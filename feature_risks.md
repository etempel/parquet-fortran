# parquet-fortran — standing risks, and how each one is (or is not) tested

**Properties of the shipped code that a future change can silently break.** This is the companion a
contributor reads *before editing an area*, not a to-do list: each entry records something that
breaks with **no test failing and no abort** — a wrong answer, a stale pointer, a corrupted heap, a
silently skipped row group — together with whether a test would actually catch it today.

`CLAUDE.md`'s "The `feature_risks.md` standing-risks register" states the rules for editing this
file; the summary is that `Risk-N` numbers are permanent, entries move between the four sections
below as their status changes, and section 4 is pruned rather than archived.

Every entry has two halves:

1. **The risk** — what breaks, and why the failure is quiet. What these share is the property that
   makes them dangerous: **their failure mode is a wrong answer or a corrupted heap, not an abort.**
2. **Test** — whether an existing test would fail if the property broke, whether one is feasible and
   worth adding, or — where no test can reach it — how to check or avoid it instead. Verdicts here
   are checked against the suite rather than inferred: several entries that once read "proposed"
   turned out to be covered already, and saying so is the point of the exercise.

## How this document is organised

Four sections, and **a risk keeps its number when it moves between them**:

| section | what belongs in it |
|---|---|
| [1. New risks](#1-new-risks) | Where a newly identified risk lands. Empty is the healthy state: an entry sits here only until someone has decided which of the three below it belongs in. |
| [2. Risks with a proposed testing scenario](#2-risks-with-a-proposed-testing-scenario) | Feasible and not written. Each entry says what to assert, and why the obvious weaker assertion would not catch anything. Some are static/lint checks rather than runtime tests, which is the right tool for a structural invariant. |
| [3. Risks not testable](#3-risks-not-testable) | No test can reach it — undefined behaviour, a process that has already aborted, a cost rather than a result, or a property of the *input*. Each entry says how to check or avoid it instead. |
| [4. Risks already covered, kept for what they still forbid](#4-risks-already-covered-kept-for-what-they-still-forbid) | A test exists that would fail if the property broke — **and the entry still tells a future change something it must not do.** |

**Numbering is permanent and independent of the section.** Risks are numbered `Risk-1` upward across
the whole document; moving one between sections (a proposal getting written, a covered property
regressing) **never** renumbers it, so a reference from `CLAUDE.md`, `feature_table.md`,
`tools/check_source_conventions.py` or a code comment stays valid for good. **A new risk takes the
next unused number — `Risk-46` today — and goes in "1. New risks"** until it has been triaged.
Numbers of deleted entries are not reused, so a stale reference resolves to nothing rather than to
the wrong risk.

**Counts today: 40 covered, 0 proposed, 5 not testable.** Section 2 being empty is the healthy
state rather than a finished one — it means every risk currently identified as testable has its
test. Nine entries are covered by something other than a unit test, deliberately: Risk-1 by a
maintainer check under `app/` with a `tools/*.sh` wrapper (it measures memory, so it needs its own
process per measurement), and Risk-2, Risk-4, Risk-5, Risk-12, Risk-13, Risk-19, Risk-43 and Risk-44 by static
checks in `tools/check_source_conventions.py`, which is the right tool for an invariant about what the code
does *not* do.

**Section 4 is pruned, not archived.** A covered entry earns its place only by still forbidding
something: a rule for the next contributor, a trap that is not visible in the code, a test whose
*design* has to be copied rather than merely kept passing. An entry that has become "this works and
is tested" is deleted outright — the test is the record at that point, and a risk register that
accumulates solved problems stops being read. One entry was removed on exactly those grounds when
this structure was introduced (the `extra: sort:` `nulls_first`/`nulls_last` token: tested, with
nothing left that could break silently).

**This file is tracked and committed**, unlike the other `feature_*.md` documents in the repo root,
which are git-ignored scratch design memory (`.gitignore` carries an explicit `!feature_risks.md`
negation for this one). So it is written for a contributor who has never seen a planning document:
no references to a conversation that produced it, and every cross-reference either into the source
tree or to `CLAUDE.md`. It does link to `feature_table.md` in a few places for the design history
behind a decision — that file is *not* tracked, so treat those as optional background rather than
something a reader is expected to have.

## Index

| risk | what a future change can break | section |
|---|---|---|
| [Risk-1](#risk-1--the-release-policy-regresses-silently) | The release policy regresses silently | 4 — covered |
| [Risk-2](#risk-2--the-schema-less-write-rests-on-three-properties-that-look-incidental) | The schema-less write rests on three properties that look incidental | 4 — covered |
| [Risk-3](#risk-3--the-screen-and-the-evaluator-can-drift-apart) | The screen and the evaluator can drift apart | 4 — covered |
| [Risk-4](#risk-4--the-sort-guards-are-one-line-each-from-returning-physically-ordered-data) | The sort guards are one line each from returning physically ordered data | 4 — covered |
| [Risk-5](#risk-5--print_stat-format-churn) | `print_stat` format churn | 4 — covered |
| [Risk-6](#risk-6--the-concurrency-guards-must-keep-agreeing-and-one-of-them-protects-a-wrong-answer) | The concurrency guards must keep agreeing, and one of them protects a wrong ANSWER | 4 — covered |
| [Risk-7](#risk-7--a-half-applied-mutation-is-unrecoverable) | A half-applied mutation is unrecoverable | 3 — not testable |
| [Risk-8](#risk-8--the-table-write-must-stay-the-same-calls-as-a-hand-written-write) | The table write must stay the same calls as a hand-written write | 3 — not testable |
| [Risk-9](#risk-9--statistics-that-are-present-but-wrong) | Statistics that are present but wrong | 3 — not testable |
| [Risk-10](#risk-10--memory-two-paths-deliberately-read-whole-columns) | Memory: two paths deliberately read whole columns | 3 — not testable |
| [Risk-11](#risk-11--a-col-pointer-is-dangling-after-a-row-structural-mutation) | A `%col` pointer is dangling after a row-structural mutation | 4 — covered |
| [Risk-12](#risk-12--the-no-target-property-is-easy-to-lose-with-no-diagnostic) | The no-`target` property is easy to lose, with no diagnostic | 4 — covered |
| [Risk-13](#risk-13--new-table-state-must-go-on-the-cache-never-on-parquet_table-itself) | New table state must go on the cache, never on `parquet_table` itself | 4 — covered |
| [Risk-14](#risk-14--the-masked-slices-two-coordinate-systems) | The masked slice's two coordinate systems | 4 — covered |
| [Risk-15](#risk-15--clone-must-reattach-whatever-the-source-reads-with) | `%clone` must reattach whatever the source reads with | 4 — covered |
| [Risk-16](#risk-16--the-two-sort-paths-share-an-engine-but-not-a-binder) | The two sort paths share an engine but not a binder | 4 — covered |
| [Risk-17](#risk-17--detach-interacts-with-the-slice-regime) | Detach interacts with the slice regime | 4 — covered |
| [Risk-18](#risk-18--validity-is-per-element-and-three-dispatch-classes-implement-it) | Validity is per element, and three dispatch classes implement it | 4 — covered |
| [Risk-19](#risk-19--two-generators-can-silently-drop-project-conventions) | Two generators can silently drop project conventions | 4 — covered |
| [Risk-20](#risk-20--kindwidth-answer-from-the-descriptor-not-the-store) | `%kind`/`%width` answer from the descriptor, not the store | 4 — covered |
| [Risk-21](#risk-21--slice-trimming-is-off-by-one-country) | Slice trimming is off-by-one country | 4 — covered |
| [Risk-22](#risk-22--a-pending-cast-is-carried-out-by-the-read-and-a-second-one-must-materialize-first) | A pending `%cast` is carried out by the read, and a second one must materialize first | 4 — covered |
| [Risk-23](#risk-23--parquet_row_index-is-derivable-only-while-the-table-still-has-its-file) | `parquet_row_index` is derivable only while the table still has its file | 4 — covered |
| [Risk-24](#risk-24--the-write-paths-own-null-and-protection-rules) | The write path's own null and protection rules | 4 — covered |
| [Risk-25](#risk-25--a-temporal-columns-stored-unit-lives-only-on-the-descriptor) | A temporal column's stored unit lives only on the descriptor | 4 — covered |
| [Risk-26](#risk-26--a-wrong-statistics-prune-silently-loses-rows) | A wrong statistics prune silently loses rows | 4 — covered |
| [Risk-27](#risk-27--the-integer-counting-sort-fast-path) | The integer counting-sort fast path | 4 — covered |
| [Risk-28](#risk-28--parquet_reader_set_filter-is-the-most-hardened-path-in-the-reader) | `parquet_reader_set_filter` is the most hardened path in the reader | 4 — covered |
| [Risk-29](#risk-29--row-group-bookkeeping-invariants-under-a-mask) | Row-group bookkeeping invariants under a mask | 4 — covered |
| [Risk-30](#risk-30--a-filtered-slice-does-not-address-physical-file-rows) | A filtered slice does NOT address physical file rows | 4 — covered |
| [Risk-31](#risk-31--an-extending-types-own-state-is-silently-lost-by-clone) | An extending type's own state is silently lost by `%clone` | 4 — covered |
| [Risk-32](#risk-32--a-hand-edit-inside-a-generated-region-survives-until-the-next-regeneration) | A hand edit inside a generated region survives until the next regeneration | 4 — covered |
| [Risk-33](#risk-33--pf_permute-through-a-col-pointer-desynchronises-a-table) | `pf_permute` through a `%col` pointer desynchronises a table | 3 — not testable |
| [Risk-34](#risk-34--pf_argsort-and-the-read-time-sort-can-drift-apart) | `pf_argsort` and the read-time sort can drift apart | 4 — covered |
| [Risk-35](#risk-35--nth_elements-determinism-rests-on-the-comparators-index-tiebreaker) | `nth_element`'s determinism rests on the comparator's index tiebreaker | 4 — covered |
| [Risk-36](#risk-36--a-binary-search-over-unsorted-input-answers-with-no-symptom) | A binary search over unsorted input answers with no symptom | 4 — covered |
| [Risk-37](#risk-37--the-merge-tie-rule-is-invisible-to-a-value-only-assertion) | The merge tie rule is invisible to a value-only assertion | 4 — covered |
| [Risk-38](#risk-38--pf_argminmaxs-two-ends-must-ask-the-same-question) | `pf_argminmax`'s two ends must ask the same question | 4 — covered |
| [Risk-39](#risk-39--a-silently-serial-threads-passes-every-correctness-test) | A silently serial `threads=` passes every correctness test | 4 — covered |
| [Risk-40](#risk-40--auto-threading-must-stay-serial-inside-a-parallel-region) | Auto threading must stay serial inside a parallel region | 4 — covered |
| [Risk-41](#risk-41--a-setting-that-is-never-read-passes-every-test-written-for-it) | A setting that is never read passes every test written for it | 4 — covered |
| [Risk-42](#risk-42--the-fortran-and-c-copies-of-a-mirrored-setting-can-drift-apart) | The Fortran and C++ copies of a mirrored setting can drift apart | 4 — covered |
| [Risk-43](#risk-43--a-second-copy-of-the-row-group-sizing-arithmetic-ignores-target_row_group_bytes) | A second copy of the row-group sizing arithmetic ignores `target_row_group_bytes` | 4 — covered |
| [Risk-44](#risk-44--a-knob-with-no-environment-variable-or-one-wired-to-the-wrong-knob-is-silent) | A knob with no environment variable, or one wired to the wrong knob, is silent | 4 — covered |
| [Risk-45](#risk-45--the-two-compilers-rules-for-the-prefetch-region-conflict-and-only-one-shape-satisfies-both) | The two compilers' rules for the prefetch region conflict, and only one shape satisfies both | 4 — covered |
| [Risk-46](#risk-46--one-validation-stands-between-the-sort-engine-and-silently-duplicated-rows) | One validation stands between the sort engine and silently duplicated rows | 4 — covered |
| [Risk-47](#risk-47--a-per-element-string-fill-is-quadratic-and-no-test-fails-when-it-comes-back) | A per-element string fill is quadratic, and no test fails when it comes back | 3 — not testable |
| [Risk-48](#risk-48--a-row-permutation-handed-to-a-caller-goes-stale-with-nothing-to-notice) | A row permutation handed to a caller goes stale, with nothing to notice | 3 — not testable |
| [Risk-49](#risk-49--a-co-ranked-merge-that-never-co-ranks-is-invisible) | A co-ranked merge that never co-ranks is invisible | 4 — covered |
| [Risk-50](#risk-50--a-co-rank-off-by-one-produces-a-non-permutation-that-nothing-on-the-raw-path-validates) | A co-rank off-by-one produces a non-permutation that nothing on the raw path validates | 4 — covered |
| [Risk-51](#risk-51--a-pre-run-error-scenario-result-can-be-consumed-as-this-runs-answer) | A pre-run error-scenario result can be consumed as this run's answer | 3 — not testable |
| [Risk-52](#risk-52--an-ab-equality-cannot-see-a-defect-the-two-paths-share) | An A/B equality cannot see a defect the two paths share | 4 — covered |
| [Risk-53](#risk-53--a-parallel-mutation-gate-that-never-engages-passes-every-correctness-test) | A parallel mutation gate that never engages passes every correctness test | 4 — covered |
| [Risk-54](#risk-54--the-parallel-rewrites-memory-cost-is-bounded-by-documentation-and-nothing-else) | The parallel rewrite's memory cost is bounded by documentation and nothing else | 3 — not testable |
| [Risk-55](#risk-55--two-readers-of-one-table-can-sample-different-rows-and-only-a-count-mismatch-shows-it) | Two readers of one table can sample different rows, and only a count mismatch shows it | 4 — covered |
| [Risk-56](#risk-56--a-per-thread-reader-that-writes-shared-cache-state-races-silently) | A per-thread reader that writes shared cache state races silently | 3 — not testable |
| [Risk-57](#risk-57--a-row-group-split-column-read-allocates-its-validity-bitmap-on-first-null-from-any-thread) | A row-group-split column read allocates its validity bitmap on first null, from any thread | 3 — not testable |
| [Risk-58](#risk-58--an-adopted-transform-is-shared-state-and-only-its-preconditions-stand-between-it-and-a-wrong-row-set) | An adopted transform is shared state, and only its preconditions stand between it and a wrong row set | 4 — covered |
| [Risk-59](#risk-59--a-shared_ptr-parameter-on-a-per-element-helper-costs-7x-and-fails-nothing) | A `shared_ptr` parameter on a per-element helper costs 7x and fails nothing | 3 — not testable |
| [Risk-60](#risk-60--a-per-element-allocatable-character-round-trip-in-a-bulk-string-operation-costs-4x-and-fails-nothing) | A per-element allocatable-character round trip in a bulk string operation costs 4x and fails nothing | 3 — not testable |
| [Risk-61](#risk-61--a-validity-split-that-is-not-byte-aligned-loses-nulls-and-no-end-to-end-test-can-be-relied-on-to-see-it) | A validity split that is not byte-aligned loses nulls, and no end-to-end test can be relied on to see it | 4 — covered |

---

## 1. New risks

*Nothing here.* A risk lands in this section when it is first identified — before anyone has
decided whether it is testable, and before any test is written. Give it the next unused number
(**Risk-62**), state what breaks and why the failure is quiet, and leave the **Test** half to whoever
triages it into one of the three sections below.

## 2. Risks with a proposed testing scenario

*Nothing here.* Every entry this section held has been implemented and moved to section 4 — which is
where a proposal goes once its test exists, carrying its number with it. A risk belongs here when
someone has decided it is testable and said what to assert, but has not written the test yet, and
only for as long as that is true.

## 3. Risks not testable

Each of these says how to check or avoid the risk instead. Most are not gaps at all — they are a
cost, a caveat about the input, a property of a process that has already aborted, or a pre-state no
test can arrange — and writing a test for them would freeze the wrong thing as a contract.

### Risk-7 — A half-applied mutation is unrecoverable

There is no undo log — deliberately, since an implicit one would double the memory of every table on
the chance someone wants to go back. `%clone` before mutating is the documented substitute. What
stands between a caller and a half-mutated table is the validate-everything-then-mutate-everything
ordering in `parquet_tables_rowmutate.f90`, and a future contributor adding a row-structural operation
could easily validate inside the per-column loop instead.

**Test.** Not testable, and the reason is worth stating because it looks testable at first glance.
The failure is *a half-mutated table left behind by an aborted call* — but the abort is an
`error stop`, which ends the process, so nothing can observe the table afterwards. An out-of-process
error scenario proves the abort happened and can assert its message; it cannot assert what the table
looked like at that moment, because the table no longer exists.

**How to avoid it instead.** The protection is entirely the ordering — every check before the first
column is touched — and it is visible in one place per operation, which is what makes it reviewable:

- A new row-structural operation goes in `src/parquet_tables_rowmutate.f90`, and its body must read
  as *validate every column, then mutate every column*. A check written inside the per-column loop
  is the defect, and it looks perfectly reasonable in a diff.
- The temptation is strongest for a check that is *cheap per column and awkward up front* (a kind
  match, a width match, a unit match). Those are exactly the ones `append_table_worker` hoists into
  its own first pass — copy that shape.
- `%clone` before mutating is the documented recovery for a caller, and is worth repeating in the
  new operation's own doc-comment rather than assumed.

### Risk-8 — The table write must stay the same calls as a hand-written write

`src/parquet_tables_write.f90`'s own file header states the design principle: reusing
`parquet_open_writer`/`parquet_write_column` rather than reimplementing them is what keeps a table
write and a hand-written write path identical in behaviour. Two things enforce it and are easy to
erode:

- **Every writer option is forwarded still absent when the caller omitted it**, so
  `parquet_open_writer` applies exactly the defaults it would for a hand-written open. There is one
  set of defaults in the library and no second copy in the table layer to drift from it. A new writer
  option costs one line here and inherits its default for free — re-supplying a locally chosen default
  instead is the change that breaks this quietly.
- **A null-free column reaches the writer with no mask at all**, because `scalar_validity`/
  `vector_validity` leave the mask unallocated and an unallocated allocatable makes an `optional`
  dummy absent. That single detail was measured as the whole of an earlier ~2.5x gap against a
  hand-written loop; the two now benchmark at parity. Passing a uniformly-`.true.` mask instead costs
  an `nrows`-long allocation *and* makes the writer build an Arrow null bitmap it did not need.

Any new option that makes the table path *diverge* from what a hand-written loop would do needs an
explicit justification in its own documentation. One asymmetry to know when testing options together:
**Arrow rejects a `compression_level` for a codec that has none** (`snappy`), so the two arguments are
not freely combinable — that is the writer's own behaviour, unchanged by the pass-through.

**Test.** The forwarding is covered; the property that actually keeps the two paths identical is not
observable from Fortran at all.

- **Covered:** `the writer options parquet_write_table forwards reach the file` and
  `release= leaves the table in the residency state the write found` (`test/test_table.f90`).
- **Not testable:** *a null-free column reaches the writer with no mask*. Passing a uniformly-`.true.`
  mask instead produces a byte-identical file — the difference is one `nrows`-long allocation and an
  Arrow null bitmap that is built and then found to be empty. It was measured as the whole of an
  earlier ~2.5x gap, and no assertion on the output can see it.
- **How to guard it instead:** a benchmark comparison of `parquet_write_table` against a hand-written
  `parquet_write_column` loop over the same null-free data. They benchmark at parity today; a
  regression here shows up as the table path becoming materially slower, and nowhere else.
  `app/benchmark_table.f90` is where that belongs.
- **When adding a writer option**, the reviewable rule is simpler than any test: forward it *still
  absent* when the caller omitted it. Re-supplying a locally chosen default is the change that breaks
  this quietly, and it is visible in the diff as a `present()` test that did not need to be there.

### Risk-9 — Statistics that are present but wrong

A file written by a tool that recorded bounds not matching its data will give a wrong answer, and
nothing can defend against it: the screen has no way to check the footer against data it deliberately
does not read. This is the same trust `parquet_column_has_nulls` already places in the recorded null
count. It is documented in `doc/pages/reading.md` and is a caveat, not a bug to fix.

**Test.** Deliberately none, and adding one would be a mistake worth naming.

A file whose footer records bounds that do not match its data will give a wrong answer, and the
screen cannot defend against it: it deliberately does not read the data it would have to compare
against. A fixture *could* be built — a debug writer can record any bounds it likes — but the test
would assert that the library returns wrong rows, freezing as a contract something that is a caveat
about the input rather than a behaviour of the library.

**How to think about it instead.** This is the same trust `parquet_column_has_nulls` already places
in the recorded null count, and it is documented in `doc/pages/reading.md`. A user who suspects a
producer can disable the screen entirely with
`parquet_debug_set_disable_statistics_prescreen(1)` and compare — which is exactly what the A/B tests
in Risk-26 do, and is the right diagnostic to point someone at. If a real producer is ever found to
write bad statistics, the response is a note naming that producer, not a change to the screen.

### Risk-10 — Memory: two paths deliberately read whole columns

- **A sort key column is always read whole.** A global order needs every row, so there is no
  row-group-scoped equivalent the way there is for filtering. This is inherent to sorting, and the
  fix is documentation rather than engineering.
- **A scoped filter re-reads its filter columns.** The row-group-scoped builder does not populate
  `column_cache`, so a filter column read afterwards comes off disk again. That is the price of the
  bounded-memory guarantee the scoped path exists to provide, and it is the right trade there — but
  it means the scoped path is not simply "the unscoped path with less memory".

**Test.** Nothing to test — this entry records two deliberate costs, not a defect, and a test would
only freeze the current shape.

- **A sort key column is always read whole.** A global order needs every row; there is no
  row-group-scoped equivalent the way there is for filtering. This is inherent to sorting, and the
  fix is documentation rather than engineering.
- **A scoped filter re-reads its filter columns**, because the row-group-scoped builder does not
  populate `column_cache`. That is the price of the bounded-memory guarantee the scoped path exists
  to provide.

**What to do instead of testing it:** state both in the user-facing documentation where a reader is
choosing between the scoped and unscoped paths, so that "the scoped path is the unscoped path with
less memory" is never assumed — it is a different trade, not a strictly better one. If either cost is
ever attacked, measure with the Arrow pool counter (Risk-1), not with RSS.

### Risk-33 — `pf_permute` through a `%col` pointer desynchronises a table

`parquet_table%col` hands back a **writable pointer into a table's live column storage**, and
`pf_permute` accepts exactly the array types those pointers have. So this compiles, runs, and is
wrong:

```fortran
call t%col("mass", p)
call pf_argsort(p, perm)
call pf_permute(p, perm)      ! reorders ONE column; every other column stays put
```

Afterwards the table's row count is unchanged, every column individually holds valid values, and
row *k* of `mass` no longer belongs with row *k* of anything else. Nothing detects it — not the
detach guard (no row-structural mutation happened), not `%validate_qc` (each value is still legal),
not a later write (the schema still matches).

**Why this is not testable.** There is no defect to assert against: every procedure involved does
exactly what it documents. A test could only demonstrate the misuse, not catch a regression — and
the API cannot be narrowed to prevent it either, since `%col`'s writability is the entire point of
having a zero-copy accessor at all (9 of its 17 specifics hand back writable pointers, and the
table layer cannot tell a legitimate in-place edit from a reorder).

**What still forbids something.** The mitigation is documentation, in the two places a reader
actually is when they are about to make this mistake, and both must be kept:
`%col`'s own doc-comment in `tools/generate_parquet_tables.py`'s template (**not** the generated
`src/parquet_tables.f90`, which is overwritten), and the callout in `doc/pages/sorting.md`'s
"Sorting a table" section. `parquet_table%sort_by` is the supported way to reorder a table, and it
reorders every column together.

Note the deliberate asymmetry: permuting a **standalone** `parquet_column` — one built in code, or
copied out — is entirely safe and is a supported operation. The hazard is the aliasing, not the
type.

### Risk-47 — A per-element string fill is quadratic, and no test fails when it comes back

`parquet_column%set_all` on a string column, and `%append_values`, must fill the packed store in
**one linear pass** (`refill_string_store` in `src/parquet_columns_string.f90`). The obvious
implementation — a loop calling `parquet_string_column%set` once per element — is O(n²), because
`set` shifts the payload tail and rewrites every later offset whenever an element's length changes,
which filling an empty column does for every element.

**Why the failure is quiet.** It is not a wrong answer. Every value is correct, every test passes,
and the only symptom is that the operation stops completing: a 15.6-million-row `character(16)`
column ran for over ten minutes without finishing, where the linear form takes seconds. Nothing in
the suite runs at a size where n²/2 offset writes is distinguishable from 2n.

**Not testable in the suite.** A timing assertion at a size that would separate the two is far past
what `fpm test` should attempt, and a timing test small enough to run would be flaky. The guard is
the comment on `refill_string_store` saying *why* the shape is what it is, plus
`tools/benchmark_table.sh`'s `sort` run, which builds a large string column and would simply stop
finishing.

**What this forbids.** Do not rewrite either `set_all` string specific as a per-element loop over
`%set`, however much simpler it reads. Do not "simplify" `refill_string_store` by dropping its
`modify_nulls = .false.` branch — that branch preserves null elements, and rebuilding from the
caller's array alone would overwrite them with no test noticing. And do not reach for `%set` in a
new bulk path for the same reason: the primitive is correct, and calling it n times is what is not.

### Risk-48 — A row permutation handed to a caller goes stale, with nothing to notice

`%argsort_by` and `%argsort_partial` hand back plain integer arrays of row indices. Nothing links
such an array back to the table it describes, so every row-structural change — `%sort_by`,
`%top_n`, `%filter_rows`, `%delete_rows`, `%truncate`, `%append`, `%append_null_rows` — silently
invalidates every permutation taken before it. **This applies to any future binding that returns row
indices**, not only to those two.

**Why the failure is quiet, and why it is worse than the pointer case.** `feature_risks.md`'s
existing `%col`-pointer risks describe the same staleness with the opposite failure direction: a
stale pointer points at freed memory and usually crashes, which is at least loud. A stale
permutation is an ordinary array of ordinary integers. Against a table that has since **shrunk**,
the indices stay in range and name the wrong rows — `%get_slice(name, parquet_slice_list(perm), v)`
returns a full-length, plausible answer built from rows the caller never selected. Against one that
has been **reordered**, the indices name rows that are no longer where the order put them. Neither
aborts, and neither is visible in the values.

**Not testable.** There is nowhere to put the check. The permutation carries no identity, no
generation stamp and no back-pointer, so the library cannot tell a fresh one from a stale one — and
adding any of those would mean returning a derived type where the whole point of the binding is that
it returns indices a caller can hand to `parquet_slice_list`, to another table, or to their own code.
A test can only assert that a deliberately stale permutation produces the wrong answer, which is
asserting the bug rather than guarding against it.

**What this forbids.** The mitigation is documentary and must stay in the doc-comments rather than
migrating into a code comment: `%argsort_by` and `%argsort_partial` both state that the permutation
describes the table **as it was**, and both name `%generation()` — already public, already bumped by
every structural change — as the way to check before reusing a saved order. A new binding that
returns row indices must carry the same paragraph. Do not add a `sort=`-style convenience that
applies a caller-supplied permutation to a table: that is the arbitrary-permutation operation
Risk-33's neighbourhood already rules out, and handing it a stale array is exactly how the silent
row-correspondence loss described there happens.

### Risk-51 — A pre-run error-scenario result can be consumed as this run's answer

`prime_error_scenarios` (`test/test_errors.f90`) runs every error scenario once, up front and in
parallel, into `test_run/.primed/<scenario>.{out,err,status}`; `run_error_scenario` then answers
from those files instead of spawning. The whole ~630-subprocess cost of `fpm test` collapses onto
that one pre-run, and so does its trustworthiness: **a triple left behind by an earlier run is
byte-indistinguishable from one this run produced.** Consuming a stale one makes every test that
depends on it assert against a binary that no longer exists — and they *pass*, because the recorded
status is whatever the old code did. That is a vacuous green suite, not a failure, which is why it
is worth writing down rather than leaving to the code.

Two things stand between the design and that outcome, and both must survive any future edit:

- `prime_error_scenarios` **wipes `prime_dir` itself** (`rm -rf` then `mkdir -p`) before writing
  anything, so no file in it predates this process.
- `g_prime_ok` is a **per-process** flag set only after that wipe-and-repopulate has completed,
  and it is the *only* thing `run_error_scenario` consults before reading a primed triple. A
  directory on disk is never sufficient on its own.

**Test.** Not testable from inside the suite, and the reason is structural rather than
accidental: the prime runs in `run_tester` *before the first test exists*, so no test can arrange
the pre-state the risk needs (a populated `prime_dir` that this process did not create). Planting
files from a test is too late — they are already wiped — and disabling priming to plant them
clears `g_prime_ok`, which is the very guard under examination. An out-of-process scenario cannot
help either: it would have to drive `run_tester`, not `error_scenarios`.

**How to avoid it instead.**

- Never read anything under `prime_dir` without `g_prime_ok`. A future helper that "just checks
  whether the capture exists" is exactly the shape that reintroduces this.
- Never make `g_prime_ok` settable from anywhere but `prime_error_scenarios`' own tail, and never
  persist it across processes (an environment variable saying "already primed", say).
- Keep the wipe and the repopulate in that one routine, in that order. Splitting them — a wipe at
  exit, or a "reuse if fresh enough" check — turns a structural guarantee into a heuristic.
- **Every failure path in the prime must fall back to spawning, never to a primed answer.** That
  is what keeps a missing or unreadable capture a performance question instead of a correctness
  one, and it is why `run_error_scenario` falls through rather than failing when a triple is
  incomplete.

### Risk-54 — The parallel rewrite's memory cost is bounded by documentation and nothing else

Each thread rewriting a column allocates a full new copy of it before releasing the old one
(`gather_storage` and friends reallocate **exact-fit**, with no headroom), so `T` concurrent columns
hold `T` transient copies where the serial loop held one. There is **no memory cap and no memory
budget knob**: that was decided deliberately, in favour of documenting the cost.

What makes the cost statable is that `colwork_threads` never returns more than the number of columns
being rewritten. So the transient copies come to at most one extra copy of the table, and the claim
in `doc/pages/settings.md`, `doc/pages/thread-safety.md` and `CHANGELOG.md` — *a whole-column rewrite
can double the table's peak memory for the duration of the call* — is exactly true rather than
approximately.

**What this forbids.**

- **Do not remove or weaken that documented paragraph without reinstating a cap.** It is the entire
  mitigation. A user near their memory ceiling is expected to have read it and to cap the threads
  with `parquet_set_table_threads(n)`, which caps the copies with them.
- **Do not add an operation to `table_colwork` whose transient is not bounded by the column count.**
  The "at most doubles" claim rests on that bound, not on any property of the three operations that
  are there today — an op that allocated per row, or that held two copies per column, would make the
  documented figure quietly wrong.
- **Do not raise the thread count above the column count** as a "why not use the spare cores"
  optimisation. There is no work for them, and it would break the bound the claim rests on.

**Test.** Still not testable as a unit test, for the reason CLAUDE.md gives about measuring Arrow
memory in reverse: peak RSS is the right instrument here (these are ordinary Fortran allocations, so
the question is the live high-water mark rather than what was returned to the OS), but a peak-memory
assertion in a test suite that runs other tests concurrently measures the whole process, not this
call. It belongs in a benchmark instead, and **it has since been measured there**:
`app/benchmark_table.f90`'s `--mode=peakmem` builds one table, sorts it exactly once, and is run at
`T = 1` and `T = max` in separate processes under an external timer, with
`tools/benchmark_table.sh` driving the two points.

**The bound holds with margin.** The transient measured **85.7 % of `(T-1)` column copies** in two
configurations — 954 MiB against a predicted 1113 at 24 columns, 1431 against 1669 at 8 columns, the
same ratio at different row counts and column sizes. The shortfall is because the copies are not
simultaneous: each lives only between its column's allocation and its `move_alloc`, and
`schedule(dynamic)` staggers when columns finish. Even at 8 threads against 9 mutable columns, where
`T` is nearly the column count and the doubling bound is tightest, the transient reached a third of
the table's resident size. `(T-1)` copies remains the right thing to document — it is a true upper
bound, and the margin is an implementation detail a different schedule or column mix would change.

**Two traps this measurement fell into first**, both of which produced a confident wrong number and
neither of which is specific to this risk. Reading a peak off a benchmark mode that *also* builds
standalone columns for another purpose measures those instead — three machines reported such a figure
and all three had to discard it. And "touching" a column with `size(ptr)` faults in no data page at
all, so an identical build reported 2828 MiB once and 5423 MiB the next time; the touch has to sum.

---

### Risk-56 — A per-thread reader that writes shared cache state races silently

`materialize_marked_parallel` (`src/parquet_tables_read.f90`) opens one `parquet_reader` per thread
through `table_open_reader_with_transform`, and that helper ends its **masked-slice** path by writing
`cache%rg_bounds` — a component of the shared cache that every read path consults. Exactly one open
may do that: the one that creates `cache%reader`. Several threads writing it concurrently is a data
race on the array the slice's own arithmetic indexes.

The guard is one line, `if (.not. present(rdr))`, and it is deliberately **derived** rather than
passed as a `set_bounds` argument: those bounds describe the cache's own reader, so "which open may
write them" and "which open created that reader" are the same question, and a separate flag would
only have created a way for the two to disagree.

**Test.** Not testable, in the way races generally are not: eight threads writing the same values
into the same array produce the correct array almost every time, so a test that removed the guard
would pass. There is nothing to assert — the failure is a torn write under a scheduler this suite
cannot force, and the values being written are identical, so even ThreadSanitizer would report it
only while the region is live rather than through a wrong answer afterwards.

**How to avoid it instead.**

- **A new statement in `table_open_reader_with_transform` that writes through `cache` must ask
  whether `rdr` is present**, exactly as the `reader_row_group_bounds` call does. Reading `cache` is
  free; writing it is not.
- **`cache` is `intent(inout)` there only because the one write needs it.** If a future change makes
  that write conditional in some other way, the intent is the signal to re-check this — an
  `intent(in)` dummy would make the whole class of mistake impossible, and is worth reaching for if
  the write ever moves out.
- **The sibling rule is already documented and is the one people meet first**: a per-thread reader
  must *attach* the slice's row range and must *not* rebuild the bounds. The attach half IS testable
  and is covered — `a masked slice prefetches in parallel with every column on one row set`
  (`test/test_table_parallel.f90`), mutation-confirmed by making the helper skip its masked branch
  when `rdr` is present, which aborts with `nrows mismatch for column: e`.

### Risk-57 — A row-group-split column read allocates its validity bitmap on first null, from any thread

`materialize_column_parallel` (`src/parquet_tables_read.f90`) reads one column by giving each thread
a row group and a reader of its own, pasting each chunk into a column sized up front. The pastes are
disjoint by construction, so nothing there needs a lock — **except the validity bitmap, which is not
part of that column until something needs it.**

`%paste` calls `ensure_bitmap` whenever its source chunk carries a null, and `ensure_bitmap`
(`src/parquet_columns_util.f90`) allocates on first use. So two threads pasting null-carrying row
groups both take the `.not. allocated(self%validity)` branch and both `allocate` it: one allocation
leaks, every null written into it is lost, and the two writes to `self%has_nulls` and to the
descriptor race with each other. Nothing about the result announces itself — the column is the right
length and full of plausible values, with some nulls silently missing.

The guard is one line, before the region:

```fortran
if (parquet_column_has_nulls(cache%reader, slot%file_name, 0_int64, 0_int64)) &
    call slot%values%ensure_validity()
```

It asks the **footer**, so a null-free column — the common case, and the one this path is fastest on
— still allocates nothing, and the query's uncertain answer ("might have nulls") is the safe
direction.

**Test.** Not testable. Removing the guard and running the null fixture — 200 000 rows, every
seventh null, eight row groups, eight threads — passed **five times out of five**. The window is a
few instructions between the `allocated` test and the `allocate`, and OpenMP hands the row groups out
fast enough that two threads rarely reach it together. A test that passes against the broken code is
worse than no test, because the next person deletes the guard and the suite agrees with them.

**How to avoid it instead.**

- **Any new parallel path that writes into one `parquet_column` from several threads must ensure
  validity before the region**, not inside it. The rule generalises past this one procedure: the
  column's *storage* is allocated by `init` and is safe to write disjointly; its *validity* is
  allocated on demand and is not.
- **The footer query is the right shape to copy** rather than allocating unconditionally.
  `parquet_column_has_nulls` is already the query the ordinary whole-column read uses to decide
  whether to build a mask at all, so this costs nothing new and keeps the null-free fast path free.
- **A residual benign race is left behind, and it is worth knowing before a ThreadSanitizer run
  reports it.** With the bitmap pre-sized, `paste`'s own `ensure_bitmap` call still runs per thread
  and still writes `self%has_nulls = .true.` — the value it already holds. Harmless in practice
  (a same-value store), but it is a genuine write race and would be the FIRST thing TSan names in
  this region. Do not let that report send anyone hunting for a different bug; and note that
  CLAUDE.md's own warning applies in reverse here — fixing the reported race would change nothing,
  because it is not the one that matters.
- **The disjointness of the pastes is what the rest of the safety rests on**, and it comes from
  `reader_row_group_bounds`: row group *rg* owns table rows `bounds(1,rg)..bounds(2,rg)` and no
  other row group owns any of them. A future change that made two row groups' ranges overlap —
  or that pasted at anything other than `bounds(1, rg)` — breaks the whole argument, not just the
  bitmap. That half IS testable and is covered (see `feature_table_parallel.md` section 17.8).

### Risk-58 — An adopted transform is shared state, and only its preconditions stand between it and a wrong row set

`parquet_reader_adopt_transform` (`src/parquet_core.f90` / `parquet_wrapper.cpp`) gives one reader
another's filter/sample mask and sort permutation. That is what makes a filtered or sorted table
readable on several threads at all — but it is also **the only place in this library where a reader
is put into a state it cannot reach on its own**, and the failure mode is a reader whose mask
describes rows other than the ones it hands back. Nothing about such a reader looks wrong: it returns
a column of exactly the length it claims, full of real values from the file.

Four checked preconditions are what stand there, and each closes a different way in:

| precondition | what it prevents |
|---|---|
| same row-group **and** row counts | a mask built for a different file, selecting real rows of the wrong ones |
| destination has no mask or permutation of its own | two transforms that would have to compose; the adopted one indexes rows the reader's own has already removed |
| no column read on the destination yet | a column read **unmasked** that can never be lined up with the adopted mask |
| the source's deferred sample draw already installed | adopting a mask that is about to be replaced |

**They are checked rather than documented on purpose.** Three of the four are cheap integer or
pointer tests, and the fourth is a boolean; a comment saying "the caller must ensure" would cost the
same and catch nothing. Each has an error scenario
(`adopt_transform_onto_transformed`, `adopt_transform_after_read`, `adopt_transform_other_file`).

**The `busy` check on the source is NOT one of them, and must not be read as one.** Several threads
adopting from one source at once is the *intended* use, so the source is deliberately read without a
`ConcurrencyGuard` — guarding it would make the second thread abort on a reader nobody is writing to.
What makes that safe is that every field read is either an immutable Arrow array or state not
written after the source's own open. The `src->busy.load()` test is a net for the case that
assumption is violated, and it is inherently racy: a reader can become busy the instant after it is
tested. **Do not delete it** (it catches the realistic misuse, a caller adopting from a reader it is
also reading), and **do not rely on it** to make a new kind of source access safe.

**What this forbids for the next change here.**

- **Any new field added to `ParquetReaderHandle` that is derived from the mask or the permutation
  must be added to the copy list**, or an adopting reader will hold a mask and stale bookkeeping
  about it. `row_group_surviving` and `row_group_live_offsets` are the existing examples, and both
  are silent when wrong — `parquet_get_chunk_size` would simply answer for the unfiltered file.
- **The `print_stat` cosmetics are part of the transfer, not decoration.** A reader that applies a
  filter while reporting none is a debugging trap, and the fields cost O(1).
- **A reader that has adopted must never then be given a transform of its own.** The destination
  check covers the ordering that exists today; a future path that adopts and *then* calls
  `parquet_reader_set_filter` would pass every check and produce two composed masks.

**Test.** The preconditions are covered by the three error scenarios above. What is **not** testable
is the concurrent-adopt safety itself: *T* threads copying two `shared_ptr`s and some vectors out of
an idle source is either correct or a race no fixture can force, and the mutations that matter (a
field left out of the copy list) are covered instead by the equality tests in
`test/test_table_parallel.f90`, which caught all three tried — see `feature_table_parallel.md`
section 17.9.

### Risk-59 — A `shared_ptr` parameter on a per-element helper costs 7x and fails nothing

Every per-element helper in `src/parquet_wrapper.cpp` — `real_family_value_at`,
`small_integer_value_at`, `decimal_value_at`, `decimal_to_int64_checked` — is called **once per
row**. Taking the array as `const std::shared_ptr<arrow::Array> &` rather than `const arrow::Array *`
means every `std::static_pointer_cast` inside builds a new `shared_ptr`: **an atomic increment and
an atomic decrement, to read one number.**

Measured on a 16-column x 2 M-row file, one filter clause: **13.6 ns per row** with the `shared_ptr`
parameter, **1.8 ns** with the raw pointer. The clause evaluation it serves was 75-93% of the cost of
installing a row filter, so the whole operation went from 0.25 s to 0.061 s at eight clauses. See
`feature_table_parallel.md` section 17.10.

**The failure is not a wrong answer — it is no signal at all.** Reverting the signature keeps every
value identical, every test passing, and every error scenario green. Only a benchmark nobody runs by
default notices, and the cost is invisible in a profile that samples by function name, because the
atomics are attributed to the helper that was already going to be hot.

**Test.** Not testable, and not worth trying to make so. A timing assertion would be the flakiest
test in the suite — it would fail on a loaded CI runner and pass on a fast one, whatever threshold
was chosen — and a call-count assertion would test the implementation rather than the property.

**How it is guarded instead.** `tools/check_source_conventions.py`'s
`check_no_per_element_shared_ptr` fails the lint stage on the SHAPE — a `static` function taking
`const std::shared_ptr<arrow::Array> &` next to an element index — rather than on a list of helper
names, so it cannot go blind to the next helper added (CLAUDE.md's "A static check that enumerates
names goes stale silently"). Confirmed to fire: reverting `real_family_value_at`'s signature makes
it fail with the line number and the measurement, and restoring it clears.

**What this forbids more generally.** The rule is about ownership, not about `shared_ptr` being
slow: the caller already holds a reference for the whole loop, so the loop needs the *pointer*, not a
share of the ownership. A genuinely per-element helper that must extend an array's lifetime would be
the exception, and would have to argue for itself in a comment and in the check — not simply be
written and merged because nothing complained.

## 4. Risks already covered, kept for what they still forbid

Every entry here has a test behind it. What keeps it in the document is the second half: a rule for
whoever edits the area next. Read the entry for the area you are about to touch before you touch
it — that is what this section is for, and it is why "covered" is not the same as "finished".

A few of these carry a suggestion of their own — Risk-18 wants a benchmark case, because the property
in question (iterate the set bits, not all 64 positions of a word) is *correct* either way and
differs only in cost, which no unit test can see. That does not move the entry into section 2: the
correctness is covered, and the suggestion improves how it is covered rather than filling a gap in
whether it is.

### Risk-1 — The release policy regresses silently

If a materialization path forgets to release the Arrow-side column after copying it into the table,
nothing fails — the table simply holds two copies of every column it reads, and only the Arrow pool
counter notices. RSS cannot answer this question at all (see CLAUDE.md, "Measuring whether Arrow
memory was actually freed"); `parquet_get_arrow_bytes_allocated` can, and each path must be measured
in its own process.

**Test.** Covered, by a maintainer check rather than a unit test — it needs its own process per
measurement, which a test-drive suite cannot give it.

- **Covered:** `tools/check_arrow_release.sh` drives `app/check_arrow_release.f90` over every
  materialization path (`%materialize_all`, `%prefetch`, a single lazy `%get`, a slice, and
  `parquet_write_table(release=.true.)`) and **exits nonzero** if any of them leaves more than a
  tolerance of one copy of the data it just read in Arrow's pool. Documented in CONTRIBUTING.md's
  "Other tools/ helpers".
- **The `control` run is not optional and goes first.** It reads a column through a plain reader —
  which caches it — and asserts the counter *rose*. Without it a measurement that silently reported
  zero (a different Arrow build, a pool that is not the default one) would print PASS for every path
  and mean nothing.
- **`materialize_all` and `prefetch` are also run under `OMP_NUM_THREADS=1`, and that is
  load-bearing rather than thorough.** The internally-parallel `%prefetch` gives each thread its own
  reader and closes it at the end of the region, and closing a reader frees whatever it cached
  whether or not the release ran — so **the parallel path passes even with every
  `parquet_release_column` call deleted**. Verified by deleting them: the parallel run reported
  0.0 of one copy and the serial run 1.0. `omp_get_max_threads() <= 1` is `parallel_prefetch_ok`'s
  first clause, which is what makes the single-thread run take the serial batch-release path.
- **When adding a materialization path**, add a mode here. The question is not "did the values
  arrive?" (any test catches that) but "was the Arrow array released after the copy?" — and RSS
  cannot answer it, because Arrow's pool keeps freed pages (CLAUDE.md, "Measuring whether Arrow
  memory was actually freed").

### Risk-2 — The schema-less write rests on three properties that look incidental

A schema-less `parquet_write_table` builds a `parquet_schema` from the resident columns' descriptors
and delegates to the ordinary write path — one write loop, and the sidecar MAML for free. Three
things hold that up, and each reads like a detail:

- **The generated schema declares `col_size:`/`array_size:` as `auto`, and must keep doing so.** The
  writer then resolves both from the data exactly as it would with no schema at all, so the
  generator cannot get a size wrong because it never computes one. A future change that "improves"
  this by measuring the column here takes on the one job the current shape avoids — and gets it
  wrong first for strings, whose declared width is a maximum over values it would have to scan.
  The sidecar still records real numbers rather than `auto`, because it is emitted at **close**,
  after the writer has resolved them.
- **The sidecar round-trip depends on the read-in MAML loader IGNORING keys it does not know.**
  What `write_maml=` emits is a Role A (write) schema; what `parquet_open_table(maml=)` consumes is
  the Role B (read-in) dialect. They interoperate because `parquet_load_qc_maml_file` never parses
  `fields:` into `%cinfo` — it scans raw lines for the keys it wants — so Role A's extra keys are
  inert rather than rejected. **Tightening that loader into a strict validator would silently break
  the round trip**, which is a feature the guide advertises. `test_write_table_schemaless_sidecar`
  is the regression test, and it is really a test of the loader's leniency.
- **A zero-column table cannot go through the generated schema at all**, because MAML requires at
  least one field, so it takes a dedicated bare-writer path. Removing that path does not fail
  visibly — it falls through to reading an unparsed schema's `%cinfo`, which segfaults. Note the
  test consequence: `run_error_scenarios.sh` alone would still report PASS there, since a segfault
  is a nonzero exit; only the stderr assertion in `test_errors.f90` distinguishes it from the clean
  abort that is meant to happen.

**Test.** Covered by a static check, which is the right tool here: the property is about what the
code *does not do*, and the difference is invisible in the output.

- **Covered:** `tools/check_source_conventions.py` (`the schema-less write declares auto sizes`)
  asserts that every `col_size`/`array_size` in `build_table_schema` is `parquet_size_auto` and
  never a measured value. It runs in `tools/run_lint_check.sh` and CI's lint stage, and is
  mutation-verified (replacing one with `slot%width` fails it).
- **Why not a runtime test:** the sidecar MAML is emitted at **close**, after the writer has
  resolved the real numbers, so a generator that measured the column here would produce a sidecar
  that still looks correct. The intermediate schema text is the only place the difference exists,
  and it is never handed to a caller.
- The other two properties are covered as before: `a schema-less write's sidecar MAML carries units
  and reopens the file` is really a test of the read-in MAML loader's **leniency** (tightening that
  loader into a strict validator will fail it, which is the intended alarm), and
  `table_write_schemaless_empty_maml` covers the zero-column path — whose stderr assertion must
  stay, since `run_error_scenarios.sh` alone would report PASS for a segfault.

### Risk-3 — The screen and the evaluator can drift apart

They answer the same question through two code paths — the screen from footer bounds, the evaluator
from decoded values — and a divergence is invisible until some specific value lands on a boundary.
The mitigations are structural and worth preserving as structure: the screen walks the *same* postfix
node list with the same stack shape as `evaluate_nodes`, takes each leaf's type family from the same
Arrow schema expression the evaluator dispatches on, and reuses the same literal parsers rather than
re-implementing them. An operator × type equality matrix is what actually catches drift.

The same hazard applies to the **NaN/Null asymmetry**: a Null is `kUnknown` and a NaN is an ordinary
`kTrue`/`kFalse`, so they behave oppositely under `not` and `/=`. A later change that "harmonizes"
the two — or makes `is_nan`/`is_not_nan` two-valued on Null — is a silent wrong answer, not a
simplification.

**Test.** Covered — the matrix was audited cell by cell and the gaps filled.

- **Covered as before:** `every operator agrees with an unpruned read` and `and/or/not and a nested
  expression agree with an unpruned read` (`test/test_filter_screen.f90`).
- **Filled since:** `an int64 column prunes on every ordering operator` (the `Int64Statistics`
  branch had never been exercised at all — every integer test used `Int32Statistics`);
  `time and timestamp columns prune on an ISO literal`; `every ordering operator on a float64 and a
  float32 column` (float32 had been reached by exactly one `>`); `every ordering operator on a
  string column`; and `the null tests prune on a column type every comparison declines` — the last
  is the sharpest, because `is_null`/`is_not_null` short-circuit **before** the type switch and so
  genuinely prune a UINT32/DECIMAL/HALF_FLOAT column whose bounds are declined. It needed a new
  fixture (`test/fixtures/screen_declined_nulls.parquet`, built by `tools/generate_fixtures.cpp`),
  since this library's writer cannot produce any of those three types.
- **The boolean ordering-reject arm is unreachable and now says so.** `screen_compare_from_bounds`
  declines `>`/`<` on a boolean, but the filter parser rejects `flag > false` at OPEN time, before
  any row group is screened — so the C++ arm is defensive code behind a Fortran-side pre-check.
  The observable behaviour is the abort, covered by the `filter_bool_ordering` scenario; a comment
  in `test/test_filter_screen.f90` records why no in-process test sits there.
- **What remains uncovered, and why it is not a gap to chase:** INT8/INT16, DATE64,
  LARGE_STRING/STRING_VIEW and the remaining declined families (UINT64, DECIMAL32/64/256,
  INT96). Each needs a hand-built Arrow fixture, and each shares its screening path with a family
  that *is* covered — INT8/16 with INT32, LARGE_STRING with STRING. Add one only if that path stops
  being shared.
- The structural mitigations (same postfix walk, same stack shape, same literal parsers) are not
  testable directly; keeping the two walks adjacent in `parquet_wrapper.cpp` is what makes a
  divergence visible in review.

### Risk-4 — The sort guards are one line each from returning physically ordered data

A sort permutation destroys row-group locality — sorted row 5 may come from row group 47 — so every
row-group-scoped operation must refuse while one is active. There are 19+ such sites (chunked reads
across the numeric/temporal/string families, `parquet_get_chunk_size`, row mode, element mode, list
width measurement). Every one of them is a single omission away from silently handing back rows in
file order.

Three rules keep this manageable:

- **All of them route through one predicate** (`reader_has_sort_permutation` in C++,
  `check_reader_no_sort` in Fortran). A new guard must go through it rather than testing the handle.
- **That predicate keys on the permutation only, never on the mask.** A mask only ever *removes*
  rows, so row groups stay contiguous and everything row-group-scoped works under one; widening the
  predicate to "any row transform" would silently re-ban everything filtering supports.
- **A new guard site is not necessarily the outermost call a user can make.** `%row_group_bounds`
  reaches `parquet_get_chunk_size`, and its refusal used to surface naming a reader-level procedure
  the caller never opened; it now has a table-level guard of its own. Before adding guard number 20,
  check whether a sibling module reaches it and prefer a guard at that layer.

The test that actually catches a missed guard is a **sorted row-mode read asserting the value**, not
the row count.

**Test.** Covered in both halves: a behavioural test for what a missed guard does, and a static
check for whether any is missing.

- **Covered:** `row mode returns the SORTED row` (`test/test_sort.f90`) is the test that actually
  catches a missed guard — asserting the **value**, not the row count, because a physically-ordered
  read returns the right *number* of rows and the wrong ones. `element mode spans the sorted rows`
  and the `table_row_group_bounds_sorted` scenario cover their own sites.
- **Covered:** `tools/check_source_conventions.py` (`row-group reads guard against a sort`) asserts
  that **every** procedure body calling `check_row_group_valid` also calls `check_reader_no_sort`.
  That pairing is what makes "did we guard all ~19 of them?" mechanical, and it extends itself: a
  new row-group-scoped read validates its row group as a matter of course and is then required to
  carry the sort guard too. All 19 sites pass today; mutation-verified by deleting one.
- **When adding guard number 20**, two rules from this risk still apply and neither is checkable:
  route it through the shared predicate rather than testing the handle, and check whether a
  *sibling module* reaches the site — `%row_group_bounds` did, and its refusal used to surface
  naming a reader-level procedure the caller never opened.

### Risk-5 — `print_stat` format churn

`parquet_reader_print_stat`'s output is asserted across several test suites and by error scenarios, so
any format change breaks assertions far from the change. It carries a `filter:` line, a `sort:` line
and a `screened:` line on top of the per-column table. Additive-only changes (a new line, no column
changes) keep the blast radius small; anything else does not. The table's own `%print_stat` was
deliberately *not* modelled on it, so it does not inherit this fragility — keep it that way, and note
its test asserts behaviour (a lazy table stays lazy, `all=.true.` still reads nothing) rather than
text.

**Test.** Covered — but the risk as originally written was **wrong**, and the correction matters
more than the fix.

- **The premise was false.** This entry claimed `print_stat`'s output was "asserted across several
  test suites and by error scenarios", so that a format change would break assertions far from the
  change. An audit found **exactly one** text assertion in the entire repository (a
  `sample: fraction=0.4 seed=42` substring in `test_print_stat_sampled_rows`); every other
  `print_stat` scenario asserts only an exit status, and `tools/run_error_scenarios.sh` discards the
  output entirely. There was no scattered fragility to consolidate — the real exposure was the
  opposite one: the format was **essentially untested**, so a regression would be caught by nothing.
- **And it had already drifted.** `doc/pages/reading.md` documented a `prefetc` column the code
  calls `fetched`, and omitted `qcmin`, `qcmax`, `qcmiss` and `filter` entirely. The documentation
  is the format's only contract, so this was the whole guard being wrong.
- **Covered:** the documentation was corrected against the code (including what `qcmin`/`qcmax`
  actually print — the operator followed by the raw bound, not a `-`), and
  `tools/check_source_conventions.py` (`print_stat's columns match its documentation`) now compares
  the C++ `headers` vector against that table in both directions. Mutation-verified by renaming a
  column: it reports both the undocumented new name and the now-absent old one.
- **Comparing the two SETS rather than asserting the printed header line is deliberate.** The header
  is padded to each column's widest cell, so its exact text depends on the data; a test matching it
  literally would be brittle in a way that teaches people to delete it.
- **Keep the table's own `%print_stat` out of this.** It was deliberately not modelled on the
  reader's, and its test asserts *behaviour* (a lazy table stays lazy, `all=.true.` still reads
  nothing) rather than text. A future change that started asserting its exact output would import
  the problem this entry describes.

### Risk-6 — The concurrency guards must keep agreeing, and one of them protects a wrong ANSWER

Three predicates decide whether an operation on a `parquet_table` is refused, and **all three
implement the same ownership test**: *a table this very thread opened inside the current parallel
region is thread-private and exempt; anything else may be shared.* `unsafe_first_touch` and
`record_open_thread` live in `src/parquet_tables_read.f90` next to the materialization path they
guard; `unsafe_shared_mutation` and everything else lives in `src/parquet_tables_parallel.f90`, which
exists so the `#ifdef _OPENMP` plumbing sits in exactly one file. If the three ever disagree, the
symptom is a guard that fires on the per-thread slice pattern (loud, and immediately obvious) or one
that does not fire when it should (silent, and not).

CLAUDE.md's "`parquet_table` concurrency" entry is the full rule set. Four properties are worth
repeating here, because each fails quietly:

- **The parallel `%prefetch` gate is a correctness boundary, not a tuning knob.**
  `parallel_prefetch_ok` refuses to parallelize when the table carries any read-time transform, and
  the sharpest clause is that an **unseeded `sample_fraction=` would make each per-thread reader draw
  a different subset** — columns read by different threads would then hold different rows, with no
  error anywhere. A sort or a filter would merely duplicate work per thread; the sample is a wrong
  answer.
- **The read path must stay free of atomics.** The cheap `append_active` check sits in
  `table_resolve` — the single choke point every value accessor passes through — and the
  `readers_active` counter is taken only around the long windows (a lazy first touch, and
  `materialize_marked`). Two atomics per cell would dominate a `%get_element` loop over a large
  column. That asymmetry is deliberate and is documented on the cache fields; do not "fix" it.
- **The append/read checks are best-effort by construction.** A read starting fractionally before an
  append publishes itself is not seen. They are a safety net over the documented append-only rule,
  not its mechanism, and a change that treats them as a mechanism will be wrong at the margin.
- **Every guard needs a NEGATIVE control, not just an error scenario.** A guard that fires
  unconditionally passes every abort test ever written for it while making the per-thread slice
  regime unusable. `test_table_private_mutation_allowed` (`test/test_openmp.f90`) is the pattern.

**One known testing gap, deliberately left:** there is no error scenario for the read-during-append
and append-during-read aborts, because triggering either deterministically needs two threads to
overlap on demand — a timing-dependent test this project's rules forbid adding. A deterministic test
would need a debug hook that holds `append_active` open, in the style of the C++ `parquet_debug_*`
hooks. Worth adding if this area is changed again.

**Test.** Covered, including the two aborts this entry previously recorded as deliberately
untested.

- **Covered:** four error scenarios (`table_mutate_shared_in_parallel`,
  `table_add_column_shared_in_parallel`, `table_set_null_no_validity_in_parallel`,
  `table_string_write_shared_in_parallel`) assert the ownership aborts, each using `!$omp single` so
  exactly one thread runs the abort. Four in-process tests in `test/test_openmp.f90` cover the
  permitted directions.
- **The negative control is the load-bearing one.** `a thread-private table may still be mutated
  inside a parallel region` is what stops an over-broad guard passing every abort scenario while
  breaking the slice regime entirely. **Any new guard needs one**, and it is the half most likely to
  be skipped, because writing the abort test feels like finishing the job.
- **Covered since:** the read-during-append and append-during-read aborts, by the
  `table_read_during_append` and `table_append_during_read` scenarios. Neither uses threads: the
  guards read a counter and do not care which thread set it, so
  `parquet_debug_table_set_inflight` — a test-only hook — forces the counter and both aborts become
  ordinary deterministic scenarios with an asserted stderr message. **Each scenario makes the
  successful call first, with the hook clear**; that negative control is what stops either passing
  against a guard that fires on every call.
- **The hook is public API, deliberately and reluctantly.** Unlike the C++ `parquet_debug_*` hooks,
  which a test reaches through its own local `bind(C)` interface, a Fortran-side hook has no such
  escape hatch: the counters live on `parquet_table_cache`, whose components are private to
  `parquet_tables`. It is excluded from README.md's API overview, no library code calls it, and its
  doc-comment says all of this. A future Fortran-side debug hook should follow the same shape rather
  than inventing a second convention.
- **Not testable at all**, and documented as such: a read through a pointer already held, and any
  threading the library cannot identify (pthreads through C interop, coarrays).

### Risk-11 — A `%col` pointer is dangling after a row-structural mutation

`%col` (and `%ref`, and a `parquet_string_column` pointer) hands back a live pointer into a column's
storage, and every row-structural mutation (`%filter_rows`, `%sort_by`, `%top_n`, `%delete_rows`,
`%truncate`, `%append`, `%append_null_rows`) reallocates that storage exact-fit. `%cast` replaces the storage too,
and so does `%evict_column` — including the eviction `parquet_write_table(release=.true.)` performs.
A pointer taken before any of them points at freed memory afterwards, and **Fortran offers no way to
detect this** — the code compiles and usually appears to work. Mitigation is documentation plus the
opt-in `%generation()` counter; it is the sharpest edge the table has.

**`%append` being thread-safe does not make it pointer-safe**, and the two are easy to conflate now
that a shared table can be appended to from several threads at once (Risk-6). The lock serialises the
appends against each other; it does nothing for a pointer some thread is still holding, because the
library cannot see a pointer dereference at all. Re-fetch after an append.

What keeps it checkable is the file split: `parquet_tables_mutate.f90` never changes the row set,
`parquet_tables_rowmutate.f90` always does. **A new row-structural mutation belongs in the latter**,
and its doc-comment must say it detaches. See CLAUDE.md, "A `parquet_table` pointer does not survive a
ROW-structural mutation", for the five guard sites a new read-after-mutation path must run through.

**The generation counter is deliberately conservative** — every column- and row-structural entry
point bumps it whether or not that particular call relocated anything, so a change means "re-fetch",
not "definitely invalidated". A missing bump gives false confidence; an unnecessary one costs a
re-fetch. `parquet_write_table` is the one bump that does not look structural: it advances the counter
when it released at least one column, and never when it released none.

**Test.** Covered. The dangling read itself is undefined behaviour and cannot be asserted on — a
test that dereferences a freed pointer may pass, crash, or return plausible garbage, and none of the
three means anything. What *is* mechanically testable is the **generation counter**, which is the
only signal a caller has, and both directions of its contract are now swept:

- `every structural entry point advances the generation counter` (`test/test_table.f90`) loops over
  all fourteen structural entry points — `%add_column`, `%drop_column`, `%rename_column`,
  `%copy_column`, `%cast`, `%evict_column`, `%reload`, `%filter_rows`, `%sort_by`, `%delete_rows`,
  `%truncate`, `%append`, `%append_null_rows` and `parquet_write_table(release=.true.)` — asserting
  the counter strictly increased, and names the operation in its failure message. Adding a mutation
  means adding a `case`, not a test.
- The **no-op half** is in the same test: six calls that change no row must leave the counter alone,
  or it starts reporting noise and callers learn to ignore it.

**This sweep found a real gap on its first run**: `%cast`'s *deferred* path (a file-backed column
nothing has read yet) rewrote `declared_kind` and returned without bumping, while the eager path
bumped. Benign for pointers — taking one would have materialized the column, which disqualifies the
deferred path — but it contradicted the counter's own documented contract ("every column- and
row-structural entry point bumps it, whether or not that particular call actually relocated
anything"), and `%kind` answers differently from that point on. Fixed in `table_cast`
(`src/parquet_tables_mutate.f90`), which now bumps on both paths.

### Risk-12 — The no-`target` property is easy to lose, with no diagnostic

`%col` needs no `target` attribute on the caller's table only because it routes through the `cache`
**pointer** — the pointer targets heap owned by the cache, not the dummy argument, so F2018 15.5.2.4's
"pointer to a dummy's target becomes undefined on return" never applies. An accessor added later that
points at `self` directly reintroduces the `target` requirement with **no compiler diagnostic and no
runtime check**: code that passes every test and corrupts memory in a caller's program.

This applies equally to `parquet_table_row`, which is returned by value from a function and must hold
only cache-derived state. The invariant is stated in `parquet_tables.f90`'s and
`parquet_tables_lifecycle.f90`'s own module doc-comments, which is where it belongs.

**Test.** Not testable at runtime — an accessor that pointed at `self` instead of at the cache would
produce a pointer that is undefined only *after return*, and reading it is the same unassertable
undefined behaviour as Risk-11. Nor does it fail to compile: `target` is a requirement on the *caller*,
so the library compiles either way and only a user's program breaks.

**Covered by a static check instead.** `tools/check_source_conventions.py` (`table pointers are
reached through %cache`) scans `src/parquet_tables*.f90` for every pointer assignment and every
`data_ptr`/`string_column` call whose source expression is rooted at `self%`, and requires `%cache`
in it. Local aliases and dummy arguments are out of scope by construction, which is what keeps it
free of false positives — the per-column statistics helpers take a `parquet_column` directly and
never match. It runs in `tools/run_lint_check.sh` and CI's `lint` stage, needs only `python3`, and
was mutation-verified by rewriting one accessor to `self%cols(idx)%values%data_ptr(p)`.

**When adding an accessor**, the question to ask is not "does this work?" — it will — but "does the
pointer I return live in the cache?" `parquet_table_row` is the case to check twice: it is returned
by value from a function, so anything it holds that is not cache-derived is undefined the moment the
function returns.

### Risk-13 — New table state must go on the cache, never on `parquet_table` itself

`parquet_table` is five scalars and one pointer, with no allocatable components at all, and that is
load-bearing rather than incidental: it is finalizable, so every allocatable component it gains makes
the compiler generate a deeper recursive walk for its `intent(out)` entry and its `FINAL` — and this
project has **three confirmed compiler bugs in exactly that machinery on exactly this type** (gfortran
leaving an OpenMP `private()` copy uninitialized; `%detached` surviving an `intent(out)` reset; ifx
segfaulting inside its own runtime on a nested derived-type component). Full detail in CLAUDE.md, "New
`parquet_table` state goes on the CACHE".

The ordering consequence is easy to get wrong: anything stored on the cache has to be assigned *after*
`allocate(table%cache)`.

**Test.** Covered by a static check — the rule is structural, so the check is structural, and it is
unusually cheap for how much it protects given three confirmed compiler bugs sit behind it.

- **Covered:** `tools/check_source_conventions.py` (`parquet_table has no allocatable component`)
  parses the `type :: parquet_table` body in `src/parquet_tables.f90` — declarations only, stopping
  at `contains` so bindings are not mistaken for components — and fails on any `allocatable`. It
  runs in `tools/run_lint_check.sh` and CI's `lint` stage. A violation is otherwise invisible: the
  code compiles, the tests pass, and the failure appears as a segfault in someone else's OpenMP
  program or on another compiler.
- The **ordering** consequence (cache state assigned after `allocate(table%cache)`) is not
  separately testable — a value assigned before the allocation is simply lost, which any test of
  that value already catches. Ordinary coverage of the feature that added the state is enough.

### Risk-14 — The masked slice's two coordinate systems

A slice opened with a filter or a sample carries its own row range inside the reader's mask, so the
table counts survivors while the file still counts physical rows. Two arrays hold the two:
`cache%rg_bounds` is **always** in the table's coordinates (what `materialize_slice` and
`resolve_width_row_groups` compare their scope against), and `cache%rg_bounds_physical` holds the
file's numbering when they differ.

Getting this backwards produces shifted data, not an error. Three specific traps:

- The physical bounds must be captured **before** any mask exists. On the masked path there is no such
  moment on the table's own reader — `sample_fraction=` installs its draw at the open — which is why
  they come from a separate footer-only reader.
- **`%row_group_bounds`'s default is table coordinates; `physical=.true.` is the file's, and it now
  answers for every file-backed table whatever transform it carries.** That uniformity is deliberate
  and was a fix: which transform happens to be present must not decide whether the question is
  answerable. The default form still refuses under a sort, correctly (a sorted row belongs to no row
  group), and refuses at the *table* level with its own message — the refusal used to surface from
  `parquet_get_chunk_size`, naming a procedure and a reader the caller never used. `%row_group_bounds`
  has four sources depending on coordinate system and open shape; the branch comments in
  `table_row_group_bounds` are the map.
- The two arrays are index-aligned, one entry per physical row group, and a row group contributing
  nothing is an **empty range** rather than a dropped entry. Dropping it would break the pairing the
  two forms exist to support.

**Test.** Covered, in both the wrong-value and the wrong-shape direction.

- **Covered:** `row_group_bounds answers in table rows by default and file rows with physical=`,
  `row_group_bounds with physical= answers under a sort too`, `a slice opened with filter= counts and
  returns only its survivors` and `an unfiltered slice cutting through row groups installs no mask`
  (all `test/test_table.f90`) pin the two coordinate systems against each other.
- **Covered:** the index-alignment invariant, in `row_group_bounds answers in table rows by default
  and file rows with physical=` — on a filtered fixture it asserts both forms return one entry per
  physical row group, that exactly one entry is an **empty range** (`lo > hi`) rather than a dropped
  entry, and that the two arrays stay index-aligned. Dropping the empty group is the natural
  "tidy-up" a future change would make, and it breaks the pairing silently.

### Risk-15 — `%clone` must reattach whatever the source reads with

A clone reopens its own reader for its own lazy reads. If it reopens *bare*, a column the source never
touched comes back with rows the source had filtered away — two different lengths inside one table,
with nothing to report it.

The mechanism that prevents this is that both `parquet_open_table` and `clone_reopen_reader` go
through one helper, `table_open_reader_with_transform`. **Any new per-table read-time state must be
copied by `table_clone` *and* consumed by that helper**, or the clone silently diverges. The clone
also copies both row-group bounds arrays and the physical slice range, because the source's own
`row_lo`/`row_hi` are table coordinates by then and cannot stand in for the physical range the scoped
filter has to be rebuilt from.

**Test.** Covered, including the specific failure mode.

- **Covered:** `a clone of a transformed table reattaches the same transform` and `a clone of a
  filtered slice reattaches the same scoped filter` (`test/test_table.f90`) assert the clone reads
  the same rows as its source — and the first one is built around exactly the case this risk
  describes: `f64` is **never touched in the source**, so the clone must read it through its own
  reader, and the test asserts both its length (4, the filtered count) and that its values line up
  with the sorted key column. A clone that reopened bare fails it.
- **When adding per-table read-time state**, the rule to test against is that it must be copied by
  `table_clone` *and* consumed by `table_open_reader_with_transform`. A test that clones a table
  carrying the new state and reads a previously-untouched column through the clone exercises both
  halves at once — copy the shape of the test above rather than writing a new one beside it.

### Risk-16 — The two sort paths share an engine but not a binder

In-memory `%sort_by` and read-time `sort_by=` produce the same order through the same C++ permutation
engine, but reach it through different binders (`sort_bind_arrow_key` for Arrow arrays, the raw-array
builder for in-memory columns). A null-tier or NaN-tier mistake in one binder produces a wrong order
with **no error**. The guard is a test asserting the two agree row for row, and it has to cover nulls
and NaNs, not just ordinary values.

**Test.** Covered, and the fixture is already built for the tiers rather than for ordinary values.

- **Covered:** `an in-memory sort matches a read-time sort row for row` (`test/test_table.f90`) is
  the A/B guard this risk asks for, run over three key columns (float, string, integer), and
  `write_sort_equiv_fixture` deliberately puts **a NaN and a null in the float key** (and a null in
  the string key) so a binder mistake in either tier shows up rather than being invisible among
  ordinary values. `test/test_sort.f90` separately covers null and NaN placement on the read-time
  path.
- **When adding a key type**, add it to that fixture with a null and — if it is floating-point — a
  NaN, rather than asserting on ordinary values only. The A/B shape is what makes the two binders
  check each other; the tiers are what the check is for.

### Risk-17 — Detach interacts with the slice regime

Detaching rewrites `regime`/`row_lo`/`row_hi`, and a row-structural mutation **skips** a column that is
not resident rather than refusing to run — which is what lets a lazy table drop rows without first
reading every column. The skipped column is then unreadable for good, and the detach guard is the only
thing that reports it.

**"Detached" means "had a file and can no longer read it", never simply "was mutated".** A table built
by `parquet_new_table` has no file to lose, so growing or reordering it must leave `%is_detached()`
answering `.false.`.

The rule is now uniform: **no rows changed ⇒ no detach.** `%truncate(n)` with `n >= %nrows()`,
`%filter_rows` with an all-`.true.` mask, `%delete_rows` with no indices, `%append` of a zero-row
table and `%append_null_rows(0)` all return without touching a column, without invalidating a pointer
and with the file still attached. **`%sort_by` is the one whose no-op case is decided by the DATA
rather than by the arguments**: sorting an already-ordered column leaves the table attached, and the
same call after an edit may not. That is the literal reading of the rule and is deliberate, but it is
the one place where "did it detach?" has to be asked rather than predicted.

Detach is also what makes `parquet_row_index` unrecoverable — see Risk-23.

**Test.** Covered in every direction, including the one decided by data.

- **Covered:** `a row mutation that changes no row does not detach` (`test/test_table.f90`) walks
  `%truncate` past the end, an all-`.true.` `%filter_rows`, `%delete_rows` with no indices,
  `%append_null_rows(0)` and a zero-row `%append`, and reads a previously-untouched column
  afterwards — which is precisely what a detach would have made impossible. `%sort_by` is in the
  same test in **both** directions: an already-ordered key leaves the table attached, and the same
  call with `descending=` still detaches. That is the one place a caller cannot predict the answer
  from the call alone, so it is also the one most likely to be "simplified" into an unconditional
  detach.
- **Covered:** `an in-memory table is never detached, however it is mutated` asserts a
  `parquet_new_table` table stays `%is_detached() == .false.` through a sort that reorders it, an
  append that grows it and a filter that shrinks it. "Detached" means "had a file and can no longer
  read it", never "was mutated" — and `table_detach` setting the flag unconditionally is the
  reasonable-looking change this catches (mutation-verified: it fails on the reordering case).
- **Covered:** the row-structural loss itself, by `a row mutation on a slice detaches it and strands
  its unread columns` plus the `table_detached_*`/`table_slice_mutate_then_read` error scenarios.

### Risk-18 — Validity is per element, and three dispatch classes implement it

`parquet_column` answers `is_null`/`set_null`/`clear_null` through three different mechanisms behind
one API — a packed bitmap, the embedded `parquet_string_column`, and the null state inside each
temporal element — which is exactly where a "fixed the bitmap, forgot the temporal case" bug lands.
**Test per dispatch class, not only per kind.**

The invariants, all one edit from a silent wrong answer:

- **Row and element forms are deliberately asymmetric.** A row *query* (`is_null(i)`) is `.true.` when
  **any** element of the row is null; a whole-row *mutation* (`set_null(i)`, `clear_null(i)`,
  `append_nulls`) acts on **every** element; `modify_nulls=.false.` protects individual null
  *elements*, not whole rows. Each operation acts at the granularity the caller named — that one
  sentence generates all three.
- **Shapes must match on every paired API.** A rank-1 `values` takes a rank-1 `is_valid`; a rank-2
  `values` takes a rank-2 `is_valid`, shaped `(width, nrows)`. No widening anywhere. The **standalone**
  mask APIs are the deliberate exception because they have no values to match: `%get_valid_mask` and
  `%set_null(mask)` accept either rank, where rank-1 is the row summary and rank-2 the true element
  state. Do not "fix" that asymmetry.
- **`%paste` replaces the pasted range's validity; `%append` merges it.** Only
  `test/test_columns.f90` covers the difference.
- **When walking the bitmap in bulk, iterate the SET BITS** (`trailz` + `ibclr`), never all 64
  positions of a nonzero word. This is not a micro-optimization: the naive form was measured **2.7x
  slower** than the pre-element-null implementation on a width-16 half-null column, while the bit-scan
  is **2.2x faster**. The zero-word skip is what keeps the null-free path free and must stay.
- **A rank-2 `is_valid` is expensive to ask for.** gfortran's default `LOGICAL` is 32 bits, so
  `width = 100`, `nrows = 10^6` is a 400 MB mask built from 1 Mbit of bitmap. The argument is optional
  precisely so this is opt-in; `%is_null(name, i, e)` is the cheap way to ask about a few elements.
- **The Q11 fallback remains available and unbuilt**: if a real workload finds the O(width) single-row
  query too slow, cache a per-row "any element null" bit alongside the element bits. It was
  deliberately *not* built, because it adds a second source of truth every mutation path must
  maintain — the exact coupling `src/parquet_columns_validity.f90`'s header records as rejected when
  the bitmap layout was chosen. Measurement is the only trigger.

**Test.** The correctness half is the best-covered area in this document; the performance half is not
covered at all and cannot be by a unit test.

- **Covered, per dispatch class rather than per kind** — which is what this risk asks for:
  `string validity delegates to the embedded column`, `temporal validity lives in the element and is
  cached`, `a null-free column allocates no validity bitmap`, `set_null allocates the bitmap lazily`,
  `element nulls are addressable on every vector kind`, `row queries mean any element null`,
  `modify_nulls= is honoured by every kind`, `modify_nulls= protects elements, not whole rows`,
  `paste replaces the pasted range's validity` (all `test/test_columns.f90`), plus
  `nulls survive a round trip for all three validity dispatch classes` and
  `one null element survives a file -> table -> file round trip` (`test/test_table.f90`).
- **Not covered, and not a unit test:** the bit-scan invariant (iterate set bits, not all 64
  positions). Both forms are *correct*; only the cost differs, by 2.7x on a half-null width-16
  column. A rewrite to the naive form would pass every test above. **Proposed:** a case in
  `app/benchmark_table.f90` reading a wide, half-null column, so the regression shows up in the
  benchmark that is already run before a release rather than in a user's profile.
- The **Q11 fallback** (a cached per-row "any element null" bit) is deliberately unbuilt; measurement
  is its only trigger, and the same benchmark case is what would supply it.

### Risk-19 — Two generators can silently drop project conventions

`tools/generate_parquet_columns.py` and `tools/generate_parquet_tables.py` emit committed source. A
missing `!>` doc-comment or `! GCOVR_EXCL_LINE` in one template multiplies across every kind it emits,
and only `ford --warn docs.md` would reveal the first. Both have `--check` modes that re-derive their
output in memory and fail if the committed copies have drifted; both are wired into CI's lint stage
and into `tools/run_lint_check.sh`. A third generator must have the same mode, and must import the
kind table rather than carrying its own copy.

**Test.** Covered — drift by the generators' own `--check` modes, template omission by a separate
static check.

- **Covered (drift):** `tools/generate_parquet_columns.py --check` and
  `tools/generate_parquet_tables.py --check` re-derive their output in memory and fail if the
  committed files have drifted. Both run in `tools/run_lint_check.sh` and in CI's lint stage.
- **Covered (omission):** `tools/check_source_conventions.py` (`generated files carry their
  conventions`) asserts, over every committed generated file, that each `module subroutine` /
  `module function` interface body is preceded by a `!>` doc-comment and that every `end module` /
  `end submodule` carries `! GCOVR_EXCL_LINE`. This is the half `--check` structurally cannot see:
  it compares the committed file against the generator, so a template missing a convention produces
  output that matches perfectly and is wrong in every kind it emits at once.
- **It found seven real violations on its first run** — six `end module`/`end submodule` lines
  emitted without the marker (`parquet_columns_access`, `parquet_columns_mutate`, `parquet_tables`,
  `parquet_tables_access`, `parquet_tables_addcol`, `parquet_tables_materialize`) and one
  undocumented interface body (`table_enumerate_columns`). All seven were fixed **in the
  generators**, never in the output.
- **A third generator** inherits the check by adding its output to `GENERATED_FILES` in that script,
  alongside the `--check` mode and the shared kind table.

### Risk-20 — `%kind`/`%width` answer from the descriptor, not the store

Under laziness these must read the descriptor, or `%kind` returns `PK_NONE` for every untouched column
— which silently makes the documented `select case (t%kind(name))` idiom take `case default` instead
of aborting. A deferred plain-`LIST` column additionally makes them *touch-triggering*, since its width
has no schema-level answer; `%unit`, `%residency` and `%is_supported` deliberately are not. Anything
reading `declared_kind` or `width` without going through `table_resolve`/`table_resolve_width` reads
`PK_NONE`/0 for a deferred column.

**Test.** Covered in both directions — the descriptor answer, and the queries that must not touch.

- **Covered:** `kind, width, unit, residency and is_supported answer without reading`
  (`test/test_table.f90`) walks every column of the fixture asserting `%kind` and `%width` return
  the right value **before any read**, then calls `%unit`, `%is_supported` and `%residency` and
  asserts residency is *still* `RES_EMPTY` afterwards. Both halves are mutation-verified: making
  `%kind` answer from the value store (`PK_NONE` until first touch) and making `%unit`
  touch-triggering each fail it. The existing round-trip tests all read their columns first, so a
  regression in either direction passes every one of them.
- **Covered:** the touch-triggering case for a deferred plain-`LIST` column, by `a plain LIST
  column's width is deferred, then resolved and proven` and `a slice measures a plain LIST column
  over its own row groups only`; and that opening classifies without reading, by `opening reads
  nothing; each accessor touches only its own column`.
- The asymmetry is what the first test pins: `%unit`, `%residency` and `%is_supported` must **not**
  join `%kind`/`%width` in becoming touch-triggering, and asserting `RES_EMPTY` after calling all
  three is the guard against a future change quietly widening the set.

### Risk-21 — Slice trimming is off-by-one country

Row groups are 1-based and not uniform, slice bounds are inclusive, and the first and last covering
groups are trimmed differently from the middle ones. The test matrix that holds this down — straddling,
aligned, inside one group, single row at each end — is worth extending rather than replacing whenever
the trim path changes.

**Test.** Covered by a matrix — extend it rather than writing a second test beside it.

- **Covered:** `a slice straddling a row-group boundary reads all 18 kinds correctly` and
  `slice bounds inside, on and across row groups all agree with the full table`
  (`test/test_table.f90`) are the matrix this risk asks for, and the `table_slice_*` error scenarios
  cover the invalid bounds.
- **Covered:** the degenerate ends. `slice bounds inside, on and across row groups...` now includes
  `row_lo == row_hi` at the first row of the file, the last row of the file, and — on a 20-row file
  in row groups of 7 — the last row of a group and the first row of the next, at **both** interior
  boundaries (7/8 and 14/15). A single-row slice is where an inclusive/exclusive mistake produces
  zero rows or two, and the boundary rows are trimmed by different ends of different groups.
- **When the trim path changes**, extend this matrix rather than writing a new test beside it: the
  value of a matrix is that a new bound is one more call, and a second parallel test is how the two
  drift.

### Risk-22 — A pending `%cast` is carried out by the read, and a second one must materialize first

`%cast` on a file-backed column **nothing has read yet** does not read anything: it rewrites the
slot's `declared_kind`, sets `cast_pending`, and lets the first touch decode straight into the
target kind through the reader's own numeric conversions — one pass instead of read-then-convert.
`table_materialize` clears the flag, which is what makes the deferral invisible.

Three properties hold that up, and each is one edit from a silent wrong answer:

- **A `%cast` over a column that is already `cast_pending` must materialize first.** Otherwise
  `%cast(x, PK_INT32)` followed by `%cast(x, PK_FLOAT64)` quietly forgets the rounding the first
  one asked for, and the column comes back with values that passed through no integer stage at all.
- **`exact=.true.` cannot defer**, because the reader does not perform the precision checks
  `exact=` promises. `%copy_column` defaults `exact` to `.true.` and `%cast` to `.false.`, so the
  two differ in which one takes the cheap path by default — deliberately: a copy is usually taken
  in order to keep something.
- **Anything that gives the slot values another way must clear `cast_pending`**, or a later
  materialize will convert values that were never read from the file.

Two consequences worth knowing rather than rediscovering: `%reload` re-reads into the column's
*current* kind, so a cast survives a reload (only value edits are discarded); and a cast replaces
the column's storage, so it invalidates any outstanding pointer exactly as a row-structural mutation
does (Risk-11), with the same absence of any way to detect it.

**Test.** Covered, including the property that fails silently.

- **Covered, and the one that matters most:** the **double cast on a column nothing has read**, in
  `cast before a first touch reads straight into the target kind` (`test/test_table.f90`).
  `%cast(big, PK_FLOAT32)` followed by `%cast(big, PK_FLOAT64)` on an untouched `int64` column must
  land on the value that went *through* float32 — the fixture's `WIDE_INT` needs more than real32's
  24 mantissa bits precisely so the intermediate stage is observable — and the test asserts both
  that it matches the through-float32 value and that it does **not** equal the exact `int64`
  conversion. If the second cast failed to materialize first, the first cast would be silently
  forgotten and only that second assertion would notice.
- **Covered:** `cast keeps nulls, unit and row count, and is a no-op on the same kind` and
  `cast(exact=.true.) refuses a loss that the default allows`, plus the `table_cast_*` error
  scenarios.
- **Covered:** `%reload` on a cast column re-reads into the column's **current** kind, in the same
  test — a value edit is discarded, the cast is not. Mutation-verified by having `table_reload` reset
  `declared_kind` to the file's kind, which the assertion catches.
- **When a path gives a slot values some other way**, it must clear `cast_pending`. The test for that
  is the same shape: cast an untouched column, then populate it by the new path, then read and assert
  no conversion was applied afterwards.
- See Risk-11 for the deferred path's generation-counter gap, found and fixed while sweeping it.

### Risk-23 — `parquet_row_index` is derivable only while the table still has its file

The automatic row-index column is virtual until asked for, and what populates it depends on the
regime: `i` for a whole unfiltered file, `row_lo + i - 1` for an unfiltered slice — both pure
arithmetic — but for a filtered, sampled or sorted table the mapping lives **only** inside the C++
`live_mask` and `sort_perm`, reachable through `parquet_get_physical_row_indices`.

**One event destroys it, under six names.** Every row-structural mutation calls `table_detach`, which
closes and deallocates the reader (the mask and permutation are gone), clears `file_backed`, rewrites
`regime`/`row_lo`/`row_hi` and deallocates `rg_bounds`. Even without those, position would no longer
imply provenance: after rows have been removed or reordered, the table's row `i` is not the *i*-th row
of anything the file knows about.

**There is no way back from the table itself**, and the loss is silent until it is too late. Three
things work, and all three must happen *before* the mutation: materialize it first (after which it
travels with every other column through `%sort_by`/`%filter_rows`/`%delete_rows`, and rows added by
`%append` get Null); `%clone` first (the clone reopens its own reader with the same transform);
or reopen the file with the same arguments — which reproduces the same rows for a filter and for a
*seeded* sample, but **silently does not work for an unseeded `sample_fraction=`**.

Two visibility rules follow from "always a slot, but virtual until requested" and must not drift:
`%has_column("parquet_row_index")` answers `.true.` while virtual although `%column_names` skips it —
the one place in the API where those two disagree — and once materialized it is listed **last**, since
it has no position in the file schema.

**Test.** Covered: the happy path, the loss, the visibility rule and the recovery users are told to
reach for.

- **Covered:** `the automatic parquet_row_index column names each row's file row` and
  `prefetch reaches the automatic row-index column` (`test/test_table.f90`), plus the
  `table_row_index_after_detach` error scenario for the loss.
- **Covered:** the **visibility disagreement** — the first of those tests asserts
  `%has_column(PARQUET_ROW_INDEX)` is `.true.` while the column is virtual, that `%column_names`
  omits it while virtual, and that once materialized it is listed **last**. A future tidy-up making
  the two agree would be a reasonable-looking change that breaks a documented contract.
- **Covered:** the **first recovery**, by `a materialized row index survives the in-memory
  mutations`. It materializes the row index, then asserts it is reordered by `%sort_by` with every
  other column (row for row against the key), filtered by `%filter_rows` down to the file rows that
  survived, and that a row added by `%append` carries **Null** — that row came from no file row, so
  a 0 or a repeat would be a wrong answer rather than a missing one. This is the recovery the guide
  tells users to reach for, so it is the one that has to keep working.
- **Not testable:** that reopening with the same arguments reproduces the same rows for an *unseeded*
  `sample_fraction=` — it does not, by design. That is a documented trap, and the test that would
  "prove" it would be asserting non-determinism.

### Risk-24 — The write path's own null and protection rules

- **`is_valid` is pre-mask-indexed**: `is_valid(i)` refers to the same position as `values(i)`/
  `mask(i)`, and only decides Null-vs-value among surviving rows. Dropping a row with a mask is
  distinct from writing a Null, which still occupies a row.
- **`protected_cols:` takes the flattened mask**, so it rejects a Null in any single *element* of a
  vector column, not merely a wholly-null row. It needed no change when validity became
  element-granular. Do not confuse it with the qc `miss:` check a few lines below it in the same file:
  `protected_cols:` **aborts**, `qc: miss:` **warns**.
- **Every column that will ever appear must appear in the first row group**, chunked writes require
  exact type agreement with the schema (no int32-into-float64 conversion), and the triplet runs on one
  thread in row-group order. These constrain any future streaming write — see the chunked-write entry
  in `feature_table.md` §2.4.

**Test.** Covered, though not all of it at the table layer — and the reason is worth knowing.

- **Covered:** `parquet_write_row_mask combined with is_valid keeps is_valid indexed pre-mask`
  (`test/test_writing.f90`) is exactly the interaction this risk describes: it asserts `is_valid(i)`
  refers to the same position as `values(i)` and `mask(i)`, that the dropped row is absent, and that
  a surviving Null row is present and null. It lives at the **writer** level because
  `parquet_write_table` has no `is_valid=` argument at all — a table's nulls come from its own null
  state — so the two cannot be combined at the table layer, and `row_mask writes a row subset and
  leaves the table untouched` (`test/test_table.f90`) covers the table side of the mask alone.
- **Covered:** `protected_cols:` rejecting a Null in a **single element** of a vector column, by the
  `write_protected_vector_element_null` error scenario (`is_valid(2,2) = .false.` on a `col_size: 3`
  column) and its `test/test_errors.f90` stderr assertion. It takes the flattened mask and needed no
  change when validity became element-granular, which is precisely why nothing would notice if it
  regressed.
- **Do not confuse `protected_cols:` with the qc `miss:` check** a few lines below it in the same
  file: `protected_cols:` **aborts**, `qc: miss:` **warns**. They have separate tests for that
  reason.

### Risk-25 — A temporal column's stored unit lives only on the descriptor

`parquet_timestamp` is seconds plus nanoseconds — lossless, but carrying **no unit of its own** — and
the descriptor's `unit` is the physical unit (`"Msun"`), not the temporal resolution. So the only
record that a column was stored as `timestamp[ns]` rather than `timestamp[us]` is `time_unit` /
`time_utc` on the descriptor, written by `record_temporal_unit` at classification time, while the
reader is still there to answer.

Lose that and a schema-less write does not quietly downgrade the column — it **fails**, because the
writer defaults to microseconds and `to_unix` refuses to truncate. Loud, but only if the recording
happens at all.

Two consequences worth keeping: **anything that adds a per-column fact the writer needs should go
the same way** — onto the descriptor at open, not derived later from values that no longer carry it;
and **a temporal column built in memory with `%add_column` has no stored unit to record**, so it
takes the writer's microsecond default and needs an explicit `timestamp[ns]`-style schema token if
it holds finer values. That asymmetry between a read column and a built one is documented, not a
defect.

**Test.** Covered, and by a test that fails loudly rather than quietly:
`a nanosecond timestamp column survives a schema-less write` (`test/test_table.f90`).

The mechanism is worth restating because it is what makes this risk self-announcing: if
`record_temporal_unit` stops recording, the schema-less write does not silently downgrade the column
— it **fails**, because the writer defaults to microseconds and `to_unix` refuses to truncate. The
test asserts the round trip; the failure mode is an abort, not a wrong value.

**When adding a per-column fact the writer needs**, put it on the descriptor at open, while the
reader is still there to answer, and copy this test's shape: write it, read it back, and assert the
value survived — the reader-side default is what would otherwise mask the loss.

### Risk-26 — A wrong statistics prune silently loses rows

`screen_row_groups` decides, from footer statistics alone, that a row group cannot contain a matching
row. If that decision is ever wrong, the row group's rows simply never appear — no error, no warning,
nothing to notice downstream.

What holds it up, all of which must survive any future edit:

- **Every uncertainty declines** (returns `kScreenAnything`), so the failure direction is always
  "prune nothing". Statistics absent, an unusable ordering, an unsupported type, an unparseable
  literal — all decline.
- **`AND`'s `may_true` is an over-approximation** and must stay one. Row-group statistics are
  per-column marginals with no joint information, so "some row satisfies `a`, and some row satisfies
  `b`" is the best available; tightening it is a bug.
- **For a `FLOAT`/`DOUBLE` leaf, `may_false` is unconditional, and so is `/=`'s `may_true`.** Parquet
  excludes NaN from min/max and records no NaN count, and a NaN is an ordinary value that compares
  *false* — so it makes every comparison false while sitting outside `[min, max]`. `NOT` consumes
  `may_false`, so an ordering-derived `may_false` prunes row groups that do match.
- **Two guard pairs are individually redundant and jointly load-bearing** (`is_stats_set()` + a null
  `statistics()`; the `sort_order() == SortOrder::UNKNOWN` check + the SIGNED/UNSIGNED `sort_order()`
  check below it). Removing either half alone changes no test result, which is exactly what makes a
  coverage-driven "cleanup" dangerous here. Note `ColumnDescriptor` has **no** `can_use_min_max()`
  method in any Parquet C++ release checked (19, 22, 23, 24) — the first guard reads `sort_order()`
  directly.

Testing this needs **both** an A/B equality against
`parquet_debug_set_disable_statistics_prescreen(1)` *and* an assertion on
`parquet_debug_get_row_groups_pruned()`. Equality alone passes just as happily against a screen that
never prunes anything, so a screen that silently stopped working would look perfectly healthy.

**Test.** Covered, and by the only test design that actually works here — worth understanding before
touching it.

- **Covered:** `test/test_filter_screen.f90` runs an A/B equality against
  `parquet_debug_set_disable_statistics_prescreen(1)` across operators, types, `and`/`or`/`not`,
  struct leaves, statistics-free files, unsupported types and full-prune cases.
- **The design point:** equality alone is *not* sufficient, and a suite built only on it would pass
  perfectly against a screen that never prunes anything — which is indistinguishable from a screen
  that silently stopped working. Every such test must also assert
  `parquet_debug_get_row_groups_pruned()`, so that "the answer is right" and "pruning actually
  happened" are checked separately.
- **When adding a leaf rule or an operator**, add both halves. The decline-by-default direction is
  free to test (assert nothing was pruned); the prune direction needs the counter.
- Both hooks are process-global, which is why `test/run_tester.f90` excludes the `filter_screen`
  suite from its per-test parallelism. A new test in that suite inherits that; a new suite using the
  same hooks needs the same exclusion.

### Risk-27 — The integer counting-sort fast path

The sort engine has two code paths producing the same answer, and the counting-sort fast path is the
one place where a wrong answer would be *fast* rather than slow — the shape of bug that survives
casual benchmarking. It keeps its own tests plus the
`parquet_debug_set_disable_sort_counting_path` hook that forces the comparator path for comparison.

The engine's ordering must also keep reproducing `arrow::compute::SortIndices` exactly (nulls and
NaNs absolute, never flipped by `descending`; ascending gives values → NaNs → nulls; ties hold file
order). That equivalence is what lets a `pyarrow` cross-check agree row for row.

The engine is also deliberately **free of reader state** — its keys arrive as plain typed vectors, and
only `sort_bind_arrow_key` touches Arrow. That is what lets the same engine serve `%sort_by` and a
possible public `parquet_sort` module. Don't reach for reader state from anything under that banner.

**Test.** Covered, and with the right mechanism: `the counting fast path matches the comparator`
(`test/test_sort.f90`) plus the `parquet_debug_set_disable_sort_counting_path` hook that forces the
comparator path so the two can be compared on identical input.

This is the shape to copy for any future fast path: a second code path producing the same answer
needs a switch that turns it off, or the two can only ever be compared by writing two programs. The
risk here is specific — a wrong answer would be *fast* rather than slow, so casual benchmarking
would not notice, and only an A/B against the slow path can.

**The `arrow::compute::SortIndices` equivalence** (nulls and NaNs absolute, never flipped by
`descending`; ascending gives values → NaNs → nulls; ties hold file order) is what lets a `pyarrow`
cross-check agree row for row. That cross-check is a manual diagnostic rather than a test — but it is
the one to reach for when a sort ordering question cannot be settled from the code.

### Risk-28 — `parquet_reader_set_filter` is the most hardened path in the reader

It carries the deferred-sample interaction, the `column_cache` re-filter, the qc re-run, the
statistics screen, the live-row-group read, and several justified `GCOVR_EXCL` blocks whose reasoning
must survive any edit rather than be deleted along with the code they annotate. Changes here have a
much wider blast radius than their diff suggests.

One specific trap: the filter's own columns are read **before** any mask exists, deliberately — a
mask installed earlier would make the filter's referenced columns come back already compacted
mid-evaluation, breaking the row-index alignment clause evaluation depends on. That is what
`has_pending_sample` exists for; removing the deferral looks safe and is not.

**Test.** Covered across several suites — `parquet_reader_set_filter matches open-time filtering`,
`parquet_reader_set_filter composes with sample_fraction`, `a scoped filter covers only its own row
groups`, `a row-bounded filter cuts inside a row group`, `a row-bounded filter with no rules selects
the range alone` (`test/test_filter.f90`), plus `a scoped filter prunes its out-of-range row groups`
and `a scoped filter's retained mask scales with its scope, not the file`
(`test/test_filter_screen.f90`).

**The one trap worth a specific note**, because it looks like dead code: the filter's own columns are
read **before** any mask exists, deliberately. A mask installed earlier would make the filter's
referenced columns come back already compacted mid-evaluation, breaking the row-index alignment
clause evaluation depends on — which is what `has_pending_sample` exists for. `parquet_reader_set_filter
composes with sample_fraction` is the test that fails if the deferral is removed, so a change here
should be run against that test specifically rather than the suite as a whole.

**Before editing this path at all:** its `GCOVR_EXCL` blocks carry reasoning that must survive the
edit rather than be deleted along with the code they annotate.

### Risk-29 — Row-group bookkeeping invariants under a mask

Two are easy to break by accident and are only checked by dedicated tests:

- **A chunked pass must still prove every row group was visited** for `parquet_reader_check_complete`,
  including row groups with zero survivors and row groups the screen pruned. A pruned row group reads
  as an empty chunk and still counts as read.
- **The per-row-group sizes must sum to `parquet_get_nrows`.** `parquet_get_chunk_size` reports the
  *surviving* count on a filtered reader, which is what a chunked loop allocates for.

**Test.** Fully covered, and by tests that name the invariants directly:

- `check_complete is satisfied by a filtered chunked pass` and `a row group with no surviving rows
  reads as an empty chunk` (`test/test_filter.f90`) pin the "every row group was visited" rule,
  including groups with zero survivors.
- `chunked reads still visit every row group under pruning` (`test/test_filter_screen.f90`) extends
  it to the pruned case — a pruned row group reads as an empty chunk and still counts as read.
- `chunk sizes sum to parquet_get_nrows under a filter` (`test/test_filter.f90`) pins the second
  invariant, which is what a chunked loop allocates against.

**When adding anything row-group-scoped**, these three assertions are the ones to extend rather than
duplicate: a new operation that skips a row group silently breaks the completeness check, and that
check is the only thing standing between "skipped" and "read and empty".

### Risk-30 — A filtered slice does NOT address physical file rows

- **An unfiltered, unsampled slice addresses physical file rows** and is trimmed out of its covering
  row groups in memory with no reader-side mask involved.
- **A filtered or sampled slice does not.** Its own row range is folded into the reader's mask — which
  is what the row-bounded form of `parquet_reader_set_filter` exists for — so the reader hands back
  exactly the surviving rows of `[row_lo, row_hi]`, and the table counts *those* from 1. `%nrows()` is
  then no longer `row_hi - row_lo + 1`.

`parquet_table_row_group_bounds` is always in file rows, and so is `%row_group_bounds(physical=.true.)`;
`%row_group_bounds`'s default is the table's own numbering (Risk-14).

A sort remains *banned* in the slice regime rather than accommodated: it reorders rows across the whole
file, so a row range would no longer name the rows the caller chose. The slice forms of
`parquet_open_table` simply have no `sort` argument, which makes it a compile error.

**Test.** Covered, with one part enforced by the compiler instead.

- **Covered:** `an unfiltered slice cutting through row groups installs no mask` establishes the
  first half (an unfiltered slice addresses physical file rows and is trimmed in memory);
  `a slice opened with filter= counts and returns only its survivors`,
  `a slice opened with sample_fraction= keeps a seeded subset of itself` and
  `row_group_bounds answers in table rows by default and file rows with physical=`
  (`test/test_table.f90`) establish the second — that a masked slice counts survivors from 1, so
  `%nrows()` is no longer `row_hi - row_lo + 1`.
- **Not a runtime test, by design:** a sort in the slice regime is a **compile** error — the slice
  forms of `parquet_open_table` simply have no `sort` argument. That is the strongest possible
  enforcement and needs no test; what it needs is for a future overload not to add one. If a slice
  form ever gains a `sort` argument, this entry is the reason it must not.

### Risk-31 — An extending type's own state is silently lost by `%clone`

`parquet_table` is designed to be extended — a [generated table type](generated-tables.md) does
exactly that, and so may hand-written code. `table_clone` knows only `parquet_table`'s own
components, so **a component the extension added arrives default-initialized in the clone, with
nothing to report it**: the clone succeeds, every column is right, and only the table parameter is
wrong. That is the shape of failure this register exists for — no abort, no failing test, a wrong
answer far from the cause.

The mechanism is `clone_extra` (`parquet_tables_clone.f90`), an overridable no-op that `%clone`
and `%clone_structure` each call as their last action, dispatching on `self`. Three things about it
must not be simplified away:

- **It is called LAST**, so an override sees a copy that is complete in every other respect.
- **An override must use `class is`, not `type is`**, or a further extension of the extending type
  silently loses this level's copy — the same bug one level down.
- **A concrete-typed override of `%clone` itself is not possible** and must not be re-attempted: an
  overriding procedure has to keep every dummy argument's characteristics, so `out` cannot be
  narrowed from `class(parquet_table)`. A `%clone_full`-style second spelling was considered and
  rejected, because it leaves the inherited `%clone` reachable and silently incomplete.

**What still forbids something:** the generator writes the assignments itself, from the
`components` user window, and **warns by name** for any component it declines to handle (a
`pointer`, or a derived type with no initializer). That warning is the only thing standing between
a user and this failure — do not remove it, and do not widen the parser to guess at the cases it
currently refuses.

**Covered by** `test_ext_clone_carries_components` and
`test_ext_clone_structure_carries_components` (`test/test_table.f90`, through a hand-written
extension type) and `test_clone_carries_user_state` (`test/test_table_codegen.f90`, through the
generated one). Mutation-verified: emptying the override fails both of the first two.

### Risk-32 — A hand edit inside a generated region survives until the next regeneration

`tools/generate_user_table_code.py` emits a module that is **deliberately user-editable**, in six
marked windows. Everything outside them is rewritten on the next run. So an edit made outside a
window works perfectly — it compiles, it passes, it ships — right up until someone regenerates,
at which point it vanishes with no diagnostic and no trace in the diff of the change that removed
it.

Three properties keep this survivable, and all three are load-bearing:

- **`--check` is a CI lint-stage check, not a convention.** It regenerates in memory and compares
  byte for byte, so an edit outside a window fails the build at the commit that made it.
- **It distinguishes the two causes.** The generated header carries a `source-maml-sha256:` digest,
  so "you edited generated text" and "the MAML changed and this file is stale" are reported
  differently. Without the digest both look identical and the message would have to guess.
- **A malformed marker set is refused BEFORE anything is rewritten.** A missing, duplicated or
  unbalanced marker is the one state in which a regeneration would destroy user code, so the
  generator errors out rather than deciding for itself where the code belonged.

**What still forbids something:** never make `--check` advisory, and never let a regeneration
"repair" a broken marker set by inferring window boundaries.

**Covered by** the generator's own `--self-test` (a hand-edited generated line, a deleted end
marker, a deleted window, a stale MAML, and idempotence), run in the lint stage alongside
`--check` on this project's own committed `src/parquet_table_example.f90`.

---

### Risk-34 — `pf_argsort` and the read-time sort can drift apart

Three entry points now order rows: `parquet_open_reader(..., sort_by=)`, `parquet_table%sort_by`,
and `pf_argsort`/`pf_sort`. They agree only because all three reduce to `sort_compare_key`
(`src/parquet_wrapper.cpp`). Nothing in the type system enforces that. A future change that gives
any one of them its own comparison — a "faster" Fortran-side path for a simple integer array, an
extra tier rule, a different tie-break — produces two orderings that are each internally
consistent, each fully tested by their own suite, and different from each other. The symptom is a
program that returns rows in a different order depending on whether it sorted on the way out of the
file or afterwards, with nothing reporting a problem.

**Covered by** `pf_argsort matches a read-time sort_by=` (`test/test_sorting.f90`), which is the
only test in either suite that compares the two paths against each other rather than against its
own expectations. Its shape is what matters and must be preserved if it is ever rewritten:

- **It sorts the same values twice, by different routes** — once by writing them to a file and
  reading it back with `sort_by=`, once by `pf_argsort` in memory — and asserts the two row orders
  are identical. Asserting either one against a hand-written expected order would let both drift
  together.
- **The fixture contains ties** (three equal values). Without them the test passes against any
  correct sort of distinct values and says nothing about stability, which is where two comparators
  most easily disagree while both looking right.
- **The `id` column is `id(k) == k`**, so the reader's own output *is* the permutation it applied
  and the comparison needs nothing decoded from the reader beyond it.

**What this still forbids:** do not give any sorting entry point its own comparison logic. New
options belong at the Fortran layer, above `sort_compare_key`, and must not change the default
ordering. The single-key one-shot C entry points
(`parquet_sort_argsort_*`/`parquet_sort_is_sorted_*`) exist to avoid a *copy*, not to avoid the
comparator — they route through it exactly as the multi-key builder does, and a future fourth entry
point must too.

---

### Risk-35 — `nth_element`'s determinism rests on the comparator's index tiebreaker

`SortRowLess` (`src/parquet_wrapper.cpp`) ends with `return a < b` on the row index. That single line
carries **two** contracts, and only one of them is obvious.

The obvious one is stability: a full tie falls back to original order, so plain `std::sort` is stable
and `std::stable_sort`'s temporary buffer is never allocated.

The quiet one is that it makes the comparator a **total order**, under which no two elements compare
equal — and that is the *only* reason `pf_nth_element` can promise "the index a full stable sort
would have put there". `std::nth_element` normally leaves an **arbitrary** member of an
equal-comparing run at the requested position. Remove the tiebreaker and `pf_nth_element` keeps
returning a valid-looking index that is merely a *different* member of the tied run — varying with
the standard-library version, the array length, even the optimisation level. Nothing aborts, and the
value is still correct; only the index is wrong.

**Covered by** `nth_element is stable on duplicates` (`test/test_sorting.f90`). **Its shape is the
point, and both halves were arrived at by a mutation surviving the test:**

- **It asserts against an INDEPENDENTLY CONSTRUCTED expectation**, not against `pf_argsort`. The
  first version compared the two, and removing the tiebreaker survived it — both go through the same
  comparator, so the mutation broke them identically and the comparison still held. The oracle now
  builds the expected permutation directly (group by value, then by original index), sorting nothing.
- **It runs the whole assertion TWICE, forcing the comparator path on the second pass** via
  `parquet_debug_set_disable_sort_counting_path`. With an independent oracle but a low-cardinality
  integer fixture the mutation *still* survived, because `sort_counting_candidate` accepts such a key
  and the counting path never calls the comparator at all. **Zero comparisons is a passing test.**
- **The fixture is 300 elements, not a handful.** libstdc++ falls back to insertion sort below ~16
  elements, which is stable with no tiebreaker whatsoever, so a small fixture cannot tell a total
  order from an accidentally-stable one either.

**What this still forbids:** never remove or weaken that final `return a < b`, and never assume a
stability test is meaningful without checking *which code path it actually reaches*. The last point
generalises past sorting — any fast path that skips the machinery under test makes a test that
exercises it vacuous while still reporting green.

---

### Risk-36 — A binary search over unsorted input answers with no symptom

`pf_lower_bound`/`pf_upper_bound`/`pf_equal_range` are the only operations in this library whose
precondition **cannot be inferred from the data they are given**. An unsorted array is a perfectly
well-formed array; the search halves it, follows whichever branch the comparison suggests, and
returns an index in range. No abort, no warning, no wrong-looking value — just a position that
happens to be meaningless.

That is why `assume_sorted` defaults to `.false.` and every call runs an **O(n) check in front of an
O(log n) search**. The default looks indefensible on complexity grounds and is not: it turns a silent
wrong answer into a named abort, and the caller who cannot afford it says so explicitly. The
documented escape is to check once with `pf_is_sorted` and pass `assume_sorted=.true.` inside the
loop — which is a deliverable of the guide page, not a footnote, because without it `m` searches cost
`O(m·N)` and the feature simply looks broken.

**Covered by** `searches agree with a counting oracle` and `assume_sorted changes no answer`
(`test/test_sorting.f90`), plus `sorting_search_unsorted` (`test/error_scenarios.f90`), whose sorted
call *before* the unsorted one is the negative control — a check that fired unconditionally would
pass the abort test just as happily while making every search unusable.

**What this still forbids:**

- **Never flip the default.** A future "optimisation" that makes `assume_sorted` default `.true.`,
  or that skips the check for a "trusted" caller, converts every misuse into a silent wrong answer.
- **`descending`/`nulls_first` select the comparison, they do not reorder anything.** They must
  describe the order the array is genuinely in; the check is what enforces that, so weakening the
  check also removes the only thing that catches a mismatched direction.
- **`pf_merge` inherits all of this, for BOTH inputs.** Its check names which argument was wrong
  (`a` or `b`), and the error-scenario test asserts that name — a merge that says only "not sorted"
  leaves the caller to guess.

---

### Risk-37 — The merge tie rule is invisible to a value-only assertion

`parquet_sort_builder_merge` takes from the first input when the two compare EQUAL (`<= 0`, not
`< 0`), which is `std::merge`'s own stability guarantee and what makes `pf_merge` agree with
`pf_sort` of the concatenation element for element.

**Flipping it is undetectable by the obvious test.** If two elements compare equal, then swapping
which one is emitted first leaves the result's *values* identical — so `all(merged == sorted_cat)`
passes, and so does any assertion on the validity mask when both tied elements are null. This was
confirmed by mutation, not reasoned about: `<= 0` → `< 0` survived the entire suite, including the
merge-equals-sort oracle and the validity test, until an assertion on the **full** merged array was
added.

Equal-comparing elements are routinely distinguishable in this library — a `character` array's
trailing blanks, a `parquet_string_column`'s empty strings, and above all a **null**, whose stored
value is arbitrary. That is what makes the rule observable at all, and what makes losing it a real
defect rather than a philosophical one.

**Covered by** `merge tracks validity` (`test/test_sorting.f90`), specifically its
`all(m == [1, 2, 3, 99, 88])` assertion over two tied nulls carrying different stored values.

**What this still forbids:** never assert a merge only through its values or only through its mask —
one full-array assertion over elements that compare equal but are distinguishable is what pins the
rule. The same trap applies to any future operation whose contract is about *which* of two equal
things is chosen.

---

### Risk-38 — `pf_argminmax`'s two ends must ask the same question

The minimum is rank 1 of the ascending order. The maximum looks like rank `n_value` of that same
order — and it names the right **value**. It names the wrong **index**: a stable ascending sort puts
the *last* member of a tied run at the end, so `imin` would report the first equal minimum while
`imax` reported the last equal maximum. One call, two different questions at its two ends, and the
values it hands back are correct throughout.

The implementation therefore flips the key's own `descending` flag and asks for **rank 1 again**, so
both ends report the first occurrence. This is safe because the tiers are absolute — `sort_tier_of`
never consults `descending`, so no null and no NaN moves into rank 1's way.

This shipped as a bug in the first version and was caught only by a test asserting first-occurrence
at *both* ends; a test checking `values(imax) == maxval(values)` would have passed.

**Covered by** `argminmax reports the first of a tie` and `argminmax accepts a parquet_column`
(`test/test_sorting.f90`), both over fixtures with duplicated extremes at both ends.

**What this still forbids:** when one procedure answers a question at two ends of an order, assert
the *index* at both ends over a tied fixture, not the value. A value assertion cannot distinguish
"first" from "last" and so cannot see the asymmetry at all.


---

### Risk-39 — A silently serial `threads=` passes every correctness test

`parquet_sorting`'s parallel sort returns a permutation **bit-identical** to the serial one, at every
thread count, on every input. That identity is what makes the feature safe — `SortRowLess` ends with
a tiebreaker on the row index, so it is a total order with no ties, and every correct sorting
algorithm must therefore agree.

**The same property makes the feature untestable by ordinary means.** A `threads=` that is ignored,
clamped to 1, or that spawns nothing returns the *correct* answer. Every assertion about values,
order, nulls, NaNs and stability passes against an implementation that never threads at all. Zero
parallelism is a passing test, exactly as zero comparisons was for the partial sort (Risk-35).

**Covered by** `threads are really created` (`test/test_sorting.f90`), which asserts
`parquet_debug_get_sort_threads_used()` directly, plus its negative control: the same call below the
minimum-work threshold must report **1**, or a hook that always answered "4" would pass too.

**Three things a test here has to get right, all found by mutation or by failing first:**

- **The fixture must reach the parallel path.** `sort_counting_candidate` keys on the value **RANGE**,
  not on cardinality, so 300 *distinct* integers under 4M still take the counting fast path, which
  performs zero comparisons and spawns nothing. Use a real or string key. This produced a probe that
  reported one thread and looked like a broken implementation.
- **The fixture must be big enough**, which for a test means lowering the threshold with
  `parquet_debug_set_sort_parallel_min_rows`. Every array in the suite is orders of magnitude below
  the real 8192.
- **The fixture needs heavy ties IN THE FULL KEY** for the identity oracle to have teeth. With the
  tiebreaker-free comparator substituted into the merge, three tied fixtures failed while
  `int64 over a wide range` (all distinct) and a two-key `pf_sort_keys` (the pair nearly unique) both
  passed. Distinctness in the composite key hides the entire class of stability defect.

**What this still forbids:** never assert a threaded sort only through its answer. One assertion on
the threads-used counter, on a fixture proven to reach the parallel path, is what separates a working
feature from a decorative argument.

---

### Risk-40 — Auto threading must stay serial inside a parallel region

With `threads=` absent, sorting is automatic: `omp_get_max_threads()` in ordinary code, and **1**
inside an OpenMP parallel region. That second half is the load-bearing one. `omp_get_max_threads()`
reads an ICV, not the current team size — inside an 8-thread region it returns **8**, not 1 — so
without the check, eight OpenMP threads would each spawn eight `std::thread`s. Sixty-four threads is
slower than not threading at all, and nothing about the result would look wrong.

`pf_sort_threads()` (`src/parquet_sorting_keys.f90`) is the single implementation, and it is public
precisely so the read-time `parquet_open_reader(..., sort_by=)` can ask the same question from
`parquet_read.f90` rather than keeping a second copy that could drift.

**This is a guard that picks a DEFAULT, not one that refuses an operation**, which is why CLAUDE.md's
"never key a guard on `omp_in_parallel()` alone" does not apply — that rule exists to stop a
*refusal* firing across a whole test suite. `parallel_prefetch_ok` (`src/parquet_tables_read.f90`)
is the standing precedent, with the same two lines and the same reason in its own comment: nested
regions are the caller's business. An **explicit** `threads=` is still honoured inside a parallel
region, because there the caller has said what they want.

**It also keeps this project's own test suite stable.** test-drive runs tests inside its own
`!$omp parallel do`, and since the read-time and table sorts are auto-parallel too, *every* suite
that sorts would otherwise oversubscribe — not just `sorting`, which is separately excluded from
that parallelism for Risk-35's comparison counter. Two independent defences; do not let either
become the stated reason for the other.

**Covered by** `auto is serial inside a parallel region` (`test/test_sorting.f90`), which asserts all
three cases from one test: auto outside a region resolves, auto inside resolves to 1, and an explicit
`threads=3` inside is still honoured.

**What this still forbids:** do not "simplify" `pf_sort_threads` to a bare `omp_get_max_threads()`,
and do not give the read-time sort its own copy of the rule.

---

### Risk-41 — A setting that is never read passes every test written for it

A `parquet_settings` knob is a module variable that some **other** file has to consult:
`cfg_sort_threads` is read in `pf_sort_threads` (`src/parquet_sorting_keys.f90`),
`cfg_prefetch_threads` in `src/parquet_tables_read.f90`, and the compression pair and
`cfg_default_use_threads` on the writer/reader open paths. If nothing reads one — the read site was
never added, or was dropped in a later refactor, or the refactor left it reading a different
variable — then `set` followed by `get` still returns exactly what was set, the factory-default
assertion still holds, and `parquet_reset_settings` still restores it. **The knob is decorative and
every obvious test passes.**

This is the same shape as Risk-39 (a `threads=` that is silently ignored returns the correct answer)
and Risk-35 (a partial sort that never calls the comparator passes a comparison-count test with zero
invocations). What makes it worse here is that a settings module invites exactly the test that cannot
detect it: **the set/get round trip is the first thing anyone writes, and it is worth nothing.**

**A second, quieter variant:** the knob is read, but at the wrong moment. A default captured at
writer *open* behaves completely differently from one consulted per *write*, and a test that sets the
knob before opening anything cannot tell the two apart. Each knob's documented capture point is part
of its contract, not an implementation detail.

**Covered by two mechanisms that do not subsume each other**, both required:

- **`check_settings_are_read`** (`tools/check_source_conventions.py`) — every `cfg_*` variable must
  be read somewhere other than the procedure that writes it. Catches "nothing reads it" statically,
  for every future knob, without anyone having to remember. Deliberately *not* scoped to "outside the
  module": `cfg_arrow_threads_initial` is written by `parquet_set_arrow_threads` and read by
  `parquet_reset_settings`, a legitimate shape that a file-scoped rule would have to exempt on day
  one.
- **A per-knob observed effect with a negative control** (`test/test_settings.f90`). The static check
  cannot tell whether the read site does the right thing. Nothing generic can observe these, so each
  knob needed its own instrument: `sort_threads` through `pf_sort_threads`, the compression pair
  through the SIZE of two files written from identical data, and `prefetch_threads` /
  `default_use_threads` through two C++ debug hooks added for exactly this purpose
  (`parquet_debug_get_prefetch_threads_used`, `parquet_debug_get_last_use_threads`), because neither
  has any observable at all otherwise.

**What this still forbids.** Never add a knob to `parquet_settings` with only a round-trip test —
that is the test this entry exists to call worthless. And the negative control is not optional: the
prefetch test asserted only `<= 1` in its first draft, which a counter that is never written
satisfies at 0, so it passed against a hook that had not been wired up at all. It has to see the
automatic case report a real thread count **first**.

**One test here is not about any single knob and must not be deleted as redundant:**
`test_argument_beats_setting`. A resolution written the wrong way round makes the setting beat an
explicit argument, and every per-knob test still passes.

---

### Risk-42 — The Fortran and C++ copies of a mirrored setting can drift apart

**Seven settings are stored twice**: in `parquet_settings` and, mirrored across the `bind(C)`
boundary, in `parquet_wrapper.cpp`. `verbosity` and `message_stream` go through
`parquet_push_output_settings`, because that side prints on its own — three warnings (two qc
soft-mode, one incomplete chunked read) plus the whole `parquet_reader_print_stat` report.
`sort_parallel_min_rows`, `sort_counting_path`, `sort_counting_bucket_limit`,
`target_row_group_bytes` and `statistics_prescreen` go through
`parquet_push_performance_settings`, because the sort engine, the row-group sizing and the row-group
statistics screen all live there. Neither side can see the other's variables.

**If the mirror stops being pushed, or the C++ side stops consulting it, the two halves disagree and
almost nothing notices.** Every Fortran-side assertion passes and every getter returns what was set;
only the behaviour is wrong. For the output pair the symptom is a setting that works for most
messages and not for some, which reads as a mystery rather than as a bug with a location. For the
performance five it is quieter still: `parquet_get_target_row_group_bytes()` answers correctly while
the files come out with the built-in row-group size, and a knob that silently does nothing is
`Risk-41` arriving by a different route — through the boundary rather than through a missing read
site, and so invisible to the `every setting is actually read` lint check, which only sees Fortran.

**Two properties keep the copy honest, and both are load-bearing:**

- **One writer.** The C++ side has no setter of its own; each push function is called only from
  `parquet_settings`' own setters and from `parquet_reset_settings`. The copy is derived, never
  independently assigned. **A future knob that C++ also needs must follow this** — a second writer
  makes the two genuinely independent and the drift becomes unfixable by inspection. This is also
  why S4 retired `parquet_debug_set_sort_parallel_min_rows`,
  `parquet_debug_set_disable_sort_counting_path` and
  `parquet_debug_set_disable_statistics_prescreen` rather than keeping them beside the settings:
  each was a second writer to the same state.
- **Resolved values cross the boundary, never tokens or sentinels.** The fold and the vocabulary
  check happen once, in Fortran, and so does the `0`-means-built-in resolution of the three numeric
  performance knobs — `parquet_wrapper.cpp` holds no `x > 0 ? x : kBuiltIn` conditional. A second
  string parser or a second default on the C++ side would be a second place for the same value to be
  spelled, and the drift would then be in the *interpretation* rather than in the value.
- **One push per group, not one per knob.** `parquet_reset_settings` restores the Fortran variables
  and calls each push once; with a setter per knob it would make twelve calls and omitting one would
  be invisible. Adding a knob to an existing group means changing its push signature, which
  `tools/check_bindc_boundary.py` checks on both sides.

**The C++ initialisers are the one genuinely duplicated value.** Each global's initialiser applies
until the first push, so `kSortParallelMinRows`/`kSortCountingBucketLimit`/`kTargetRowGroupBytes` and
`g_verbosity`/`g_message_stream`'s `0`s must equal `parquet_settings`' own `*_builtin` parameters and
factory values. A program that never touches a setting runs entirely on the C++ initialisers, so a
mismatch there changes behaviour with every knob still reporting its documented default.

**Covered by** `settings: the C++ half honours the mirrored verbosity` (`test/test_errors.f90`),
which drives `settings_cpp_warning_normal`/`_errors_only` — a qc soft-mode read whose warning is
printed **from C++** — and asserts it appears at `"normal"` and is absent at `"errors_only"`; and,
for the performance five, by the observed-effect tests in `test/test_settings.f90`, every one of
which observes something only the C++ side produces (threads used, comparisons counted, row groups
written, row groups pruned). A Fortran-side round trip would pass against a broken mirror in every
one of the seven cases.

**What this still forbids.** Do not test an output setting through a Fortran-side message alone: that
assertion passes against a C++ half that ignores the mirror completely. Any new C++-side print needs
either the shared `emit_warning_cpp` helper (for a warning) or the `output_is_suppressed()` query
(for solicited output), and a scenario that provokes it — a print site added straight to
`std::fprintf` is invisible to both this test and the `no direct printing` lint check, which can only
see Fortran.


### Risk-43 — A second copy of the row-group sizing arithmetic ignores `target_row_group_bytes`

Row groups are sized from a byte target by `chunk_size_from_bytes_per_row`
(`src/parquet_wrapper.cpp`), and it has **two callers serving two different writers**:
`close_parquet_writer`, for a whole-table write, and `estimate_chunk_size_from_schema`, for the
estimate the streaming `parquet_new_row_group` path locks in before any data exists.

Until S4 the first of those did not call it at all — it restated the entire function body inline,
all four constants included, as an `if`/`else if`/`else` chain over a local variable. Two copies of
one piece of arithmetic is an ordinary tidiness complaint right up until the byte target becomes a
setting, at which point it is a correctness one: **a re-inlined copy reads the built-in constant
instead of `g_target_row_group_bytes`**, so `parquet_set_target_row_group_bytes` governs one kind of
write and not the other.

**The failure is completely silent.** Every row is present, every value is correct, the file is
valid Parquet, `parquet_get_target_row_group_bytes()` reports exactly what was set — only the number
of row groups is wrong, in files written one particular way. Nothing aborts and nothing is checked
at runtime.

**Covered by** `the row-group sizing arithmetic exists once`
(`tools/check_source_conventions.py`'s `check_row_group_sizing_not_duplicated`), which allows each of
the four constants to be **defined** exactly once however many times it is read; and by the pair of
tests `target_row_group_bytes sizes the row groups of a whole-table write` and `target_row_group_bytes
also sizes the streaming path's estimate` (`test/test_settings.f90`), which exercise the two callers
through different observables — `parquet_get_num_row_groups` on a written file, and
`parquet_get_chunk_size(writer)` before any data.

**What this still forbids.** Both halves are needed and neither subsumes the other: the lint check
cannot see arithmetic that has been re-derived without reusing the constant names, and a test of
either caller alone passes while the other silently ignores the setting. **A third caller must call
`chunk_size_from_bytes_per_row` too, and must get its own test** — the lint check counts definitions,
not call sites, so it would say nothing about a third path that computed its own answer from
`g_target_row_group_bytes` and got the bounds wrong. The three bounds that are *not* settings
(`kMinAutoChunkSizeRows`, `kMaxAutoChunkSizeRows`, `kMaxFloorOvershootFactor`) stay declared beside
that function rather than moving in with the mirrored settings, precisely so that "what is settable"
and "what is a fixed bound" remain visibly different things.


### Risk-44 — A knob with no environment variable, or one wired to the wrong knob, is silent

`parquet_settings_from_env` applies one `PARQUET_FORTRAN_*` variable per knob, as a flat sequence of
thirteen near-identical blocks. Two things go wrong there and neither is visible.

**A knob left out of the sequence** makes its variable do nothing. `PARQUET_FORTRAN_TARGET_ROW_GROUP_BYTES=...`
is simply ignored, the program runs with the built-in value, and from the user's side that is
indistinguishable from the setting itself being broken — there is nothing to grep for and nothing
fails.

**A crossed pair** — the variable read into a neighbouring setter, which is exactly the mistake a
thirteen-entry copy-paste sequence invites — silently changes a knob nobody asked about while
leaving the named one at its default. That is worse than the first case: the program does something
the user did not ask for.

**Covered by** `every setting has an environment variable`
(`tools/check_source_conventions.py`'s `check_env_covers_every_setting`) for the omission, and by
`every environment variable reaches its own knob` (`test/test_settings.f90`) for the crossing — one
call setting all thirteen to thirteen distinguishable values, each read back through **its own**
getter, so a crossed pair fails on both knobs at once.

**What this still forbids.** The bulk test must stay thirteen assertions rather than a spot-check of
three: a crossed pair is only visible if both halves are asserted, and the sequence's uniformity is
exactly what makes a spot-check feel sufficient. The lint check deliberately derives the knob list
from `parquet_print_settings`' own rows rather than carrying its own copy — that is what makes a
future knob fail three checks together (undocumented, unread, unreachable from the environment)
instead of needing three separate people to remember three separate lists.

**A related trap that is NOT this risk, and has its own test.** The strict integer parser in
`env_int64` must not be replaced with a list-directed `read(text, *, iostat=)`: that accepts `"4 8"`
with `iostat == 0` and yields `4`, so a shell variable that expanded to two words would set the knob
to the first number and report success. `settings_env_two_numbers`
(`test/error_scenarios.f90`) is the assertion that stops it coming back.

---

### Risk-45 — The two compilers' rules for the prefetch region conflict, and only one shape satisfies both

`materialize_marked_parallel` (`src/parquet_tables_read.f90`) holds the library's **only** `!$omp
parallel` region. What may be declared inside its lexical scope is constrained from two directions
at once, and the two constraints contradict each other:

- **gfortran breaks `private()`**: it does not reliably default-initialize a private copy of a
  finalizable derived type, so the first finalization frees an undefined pointer. CLAUDE.md's
  documented workaround is to declare the variable in a `block` inside the loop body instead.
- **ifx breaks that very workaround, and is perfectly happy with `private()`.** A finalizable type
  *with allocatable components* declared in a `block` lexically nested in the region makes ifx emit
  privatization scaffolding for it (`<TYPE>.omp.mold_ctor` → `for_alloc_private` → `do_alloc_copy`
  → `copy_src_xdesc_to_dest_xdesc`) that segfaults on every thread entering the region — 100%
  reproducible with as few as 2 threads, independent of team size, so not a race.

The two forbidden shapes are opposites, so only a third one is left, and it is what the file uses:
**one shared `parquet_reader` array, indexed by thread number, allocated before the region**, so no
derived-type instance is constructed inside the parallel construct at all. Passing an element of it
on to an `optional, intent(inout)` dummy (how `table_materialize`/`table_release_one` receive
`rdr`) does not reintroduce the scaffolding either.

**Four things make the ifx half quiet, and two of them will exonerate the wrong shape if you
reproduce carelessly.** It needs `-O1`+ — at `-O0` it runs clean, so a `--profile debug` run cannot
see it. It needs the type to come from a **separately compiled module**: the identical type defined
in the same file as its user does not crash, so a quick single-file reproducer says the shape is
fine when it is not. CI builds gfortran only, so the pipeline stays green. And the backtrace names
no library code — compiler-generated frames bottoming out in libc, which reads like a heap bug
anywhere in the program.

**This is almost certainly the same bug as the `parquet_schema`-component crash** recorded in
CLAUDE.md's "New `parquet_table` state goes on the CACHE": hanging a `type(parquet_schema),
allocatable` off `parquet_table` segfaulted ifx inside its own runtime, in a **block-local table
opened inside an `!$omp parallel do`**, with a backtrace of unnamed RTL frames bottoming out in
`free()`. That component is precisely what would have given `parquet_table` its first allocatable
component, satisfying this trigger. It also explains why the library's own documented per-thread
pattern (`block` + `type(parquet_table) :: mine`) is safe today: `parquet_table` is five scalars
and a pointer, with no allocatable components — verified to survive this exact shape.

**The second failure here is the RESPONSE to the crash, and it is the one that actually happened.**
The region was disabled outright under `#ifdef __INTEL_COMPILER` — in the *same commit* that
introduced the shared-array shape which had already fixed it, so the fixed region never once ran.
The claim that the crash "reproduces identically" with the shared array was false, and one command
settles that class of question in seconds:

```
nm build/ifx_*/parquet-fortran/src_parquet_tables_read.f90.o | grep -E "for_alloc_private|mold_ctor"
```

Scaffolding absent ⇒ the source under test **cannot** produce that backtrace, so the binary that
crashed was stale (CLAUDE.md's "Stale `fpm` build cache" — `fpm clean --skip` before believing any
result). Reach for that check before reaching for a per-compiler bail-out.

**Covered by** `prefetch_threads caps the parallel prefetch` (`test/test_settings.f90`), whose
**positive control** — the automatic run must report more than one thread on a multi-thread machine
— is what reported the path being switched off. The crash half is caught by building the suite with
ifx at `-O1`+, which CI does not do; use the CI-environment image.

**What this still forbids.** Do not declare a `parquet_reader`, `parquet_writer`, `parquet_schema`
or any other finalizable type carrying allocatable components inside that region's `block` — plain
integers only, and the comment saying so must stay. Do not weaken the positive control into
something that tolerates a compiler taking the serial path; a test written that way passes against
the parallel path being disabled everywhere. Do not conclude anything about this region from a
`-O0` run, a `--profile debug` run, or a single-file reproducer. And note the coupling to
`parquet_table`'s "no allocatable components" rule: the first one added to that type would make
every block-local per-thread table in user code start crashing under ifx as well.

---

### Risk-46 — One validation stands between the sort engine and silently duplicated rows

`parquet_table%sort_by` used to call `parquet_column%reindex` on every column, and `reindex`
validates its permutation unconditionally — so a 24-column table checked the same permutation 26
times, which measured as over a third of the operation. It now validates **once**, by calling the
ordinary `%reindex` on the first mutable column and `%reindex_trusted` on every column after it.

**Why the failure is quiet.** The permutation comes from the sort engine, never from user input, so
the only way it can be invalid is a defect in `sort_build_permutation` or in whatever later feeds
this path. A duplicate index does not abort: `gather_storage` simply copies one row twice and drops
another, leaving every column individually well-formed and the table jointly wrong. Nothing reports
it, and the rows still look plausible.

**What this forbids.**

- **Do not remove the remaining validation** on the grounds that the engine is tested. It is the
  only check left, and it costs one bit-packed pass (~50 ms at 20 M rows) against the ~2 s the
  reindex phase takes at that size.
- **Do not collapse the loop to all-trusted.** Exactly one column must stay on the validating path;
  which one does not matter.
- **The mechanism is a HOIST, not a flag, now that the loop is parallel.** `table_sort_by` rewrites
  `slots(1)` serially through the validating `%reindex` and hands `slots(2:)` to `table_colwork`,
  which only ever calls `%reindex_trusted`. A `validated` flag read and written inside the parallel
  region would be a race, and a needless one — the hoist preserves the invariant by construction,
  with no shared state, and it keeps the one reachable abort here (an invalid permutation) on a
  single thread with its ordinary behaviour. **Do not restore a flag, and do not pass `slots` where
  `slots(2:)` is meant** — the latter permutes the first column twice, which is Risk-52's blind spot
  and is caught only by `check_rows_consistent`.
- **Removing the remaining validation is not caught by any test, and cannot be.** For a *valid*
  permutation `%reindex` and `%reindex_trusted` compute the same result, so substituting one for the
  other is semantically a no-op — mutation-confirmed. That is a property of the design, not a gap in
  the suite: the check exists for a permutation the sort engine got wrong, which is not constructible
  from user input.
- **Do not add a fourth copy of the check.** There are three — `check_row_permutation`
  (`src/parquet_columns_structural.f90`), `parquet_string_column%reindex` (`src/parquet_strings.f90`)
  and `check_permutation` (`src/parquet_sorting_keys.f90`) — and a table-layer validator was
  deliberately not written, which is the whole reason the first-column shape was chosen over
  validating in `table_sort_by` itself.
- **`%reindex_trusted` keeps its O(1) length check.** That guards a different invariant (a column
  whose row count disagrees with the permutation) and is what stops the gather reading outside the
  column. Only the O(n) contents walk is skippable.

**Covered by** every existing `%sort_by` test — they exercise the trusted path on each call, and a
wrong trusted path reorders columns inconsistently, which their value assertions catch. The
length check has its own scenario (`reindex_trusted_length_mismatch`), and
`pf_permute`'s route into the same path is asserted by
`assume_valid really skips the scan for a column` (`test/test_sorting.f90`), which passes a
**duplicate-bearing** index array — the only observation that separates "skips the scan" from "still
validates", since a valid permutation behaves identically either way.

---

### Risk-49 — A co-ranked merge that never co-ranks is invisible

The threaded sort merges its per-thread chunks with a **co-ranked** merge: each round is partitioned
by binary search so every thread merges a disjoint slice of the output, instead of merging pairwise
and ending in a single-threaded pass over the whole array. It is purely a wall-time change — the
permutation is bit-identical to the serial one, as it must be.

**That identity is exactly what makes the feature invisible to testing**, one level deeper than
Risk-39. A merge that silently stopped splitting — a budgeting formula that always answers one
segment, a floor raised past every real input, a refactor that drops the segment loop — returns the
*correct* permutation, and puts the O(n) serial tail straight back. Every ordering, null, NaN,
stability and identity assertion in the suite passes against it. It also keeps reporting a healthy
`parquet_debug_get_sort_threads_used`, because that counter means **phase 1's** chunk-sort threads
and knows nothing about the merge.

**The trap that actually bit, and the reason this entry exists at all: a floor of 16384 elements
means no pair below 32768 is ever segmented, so every array small enough for a unit-test sweep takes
the unsegmented path and calls the co-rank ZERO times.** Two deliberate co-rank defects (a boundary
off by one; the values ignored in favour of an even split) survived the entire suite before this was
noticed — the sweep was exercising the pairwise merge it had been written to replace. Zero
invocations is a passing test, exactly as zero comparisons was for the partial sort (Risk-35).

**Covered by** `the final merge round is really co-ranked` (`test/test_sorting.f90`), which asserts
`parquet_debug_get_sort_merge_threads_used() > 1` — the **final** round specifically, since a merge
that co-ranked only its first round would report a healthy maximum while leaving the whole tail in
place — plus two negative controls (below the minimum-work threshold, and `threads=1`, both of which
must report 1). Every other merge test lowers the floor with
`parquet_debug_set_sort_merge_min_segment` first.

**What this still forbids.**

- **Never assert a threaded merge only through its answer**, and never through phase 1's thread
  counter. Those two counters mean different things and must not be collapsed into one.
- **Never write a merge test without lowering the segment floor**, and never raise the floor without
  re-checking that the tests still reach the co-rank. A sweep that silently stops segmenting looks
  identical to one that passes.
- **Mutation-test this area against the sweep, not against the suite as a whole**, and confirm each
  mutation actually fails something. The two that survived here failed nothing at all, which reads
  like strong tests and meant the opposite.

---

### Risk-50 — A co-rank off-by-one produces a non-permutation that nothing on the raw path validates

The co-ranked segments are a partition of both input runs only because every boundary satisfies
`i + j == k` and is non-decreasing in both `i` and `j`. A boundary that drifts by one makes two
segments overlap or leaves a gap between them, so some row index is written twice and another never
written — and the result stops being a permutation at all.

**Nothing on the raw-array path would notice.** `pf_argsort` hands its answer straight to the caller,
and `pf_permute(..., assume_valid=.true.)` is documented as the way to skip validation for exactly
such a permutation, so the corruption propagates into whatever the caller reorders with it.
`parquet_table%sort_by` is protected — by the single remaining `%reindex` validation, Risk-46 — but
that is the other path, and it is not the one a `pf_argsort` user is on.

This is also the classic place for a defect that **shows up at one array size and nowhere else**: a
boundary landing one element early only matters when a run happens to end there.

**Covered by** three things, and each catches a different shape:

- `every size from 2 to 400 threads identically` and `a threaded sort is still a permutation at every
  size` (`test/test_sorting.f90`) — a *dense* sweep, every size 2..400 at every thread count 2..8,
  plus awkward larger sizes. The density is the test.
- `co-ranking survives its extreme inputs`, which drives every boundary to `i == nA` or `j == 0`
  (already-sorted, reverse-sorted, all-equal, one extreme at each end).
- **An always-on invariant check in `sort_merge_boundaries`** (`src/parquet_wrapper.cpp`), which
  verifies monotonicity and `i + j == k` on the spawning thread before anything is merged, and aborts
  through `report_fatal_error`. It costs O(segments) against the round's O(n). Mutation-confirmed:
  a boundary perturbed by +1 is caught by *this*, not by an assertion.

**What this still forbids.**

- **Do not remove the invariant check** on the grounds that the co-rank is tested. It is what turns
  a silent wrong answer into a named abort, and it is the only thing that catches the perturbation
  class at all.
- **Do not narrow the dense sweep** to a handful of round numbers to save runtime. That was decided
  explicitly, and this is the risk it was decided against.
- **Do not reuse `sort_corank` for `pf_merge`** without re-deriving its predicate. `SortRowLess` is a
  total order with no ties, so the co-rank's tie condition is unobservable there — every formulation
  of it agrees, mutation-confirmed. `pf_merge` has real ties and the opposite convention (Risk-37),
  so a helper that is provably correct here can be quietly wrong there.

---

### Risk-52 — An A/B equality cannot see a defect the two paths share

`parquet_table`'s row-structural mutations (`%sort_by`, `%filter_rows`, `%top_n`, and `%delete_rows`
and `%truncate` through `table_apply_keep`) rewrite their columns on several threads. The natural
test is an **A/B equality**: mutate a table in parallel, mutate an independent clone with
`parquet_set_table_threads(1)`, assert the two are identical. It is the right test and it catches a
great deal — a column skipped in the parallel loop, a schedule that loses work, a race on the store.

**It also has a blind spot that is easy to miss, because the test looks thorough.** It compares two
runs of the same procedure, so anything wrong *above* the point where the two paths diverge breaks
them **identically** and the comparison still holds. This is not hypothetical: replacing
`table_colwork(..., slots(2:), ...)` with `table_colwork(..., slots, ...)` in `table_sort_by` — so
the hoisted first column is reindexed a second time, permuting the sort key twice while every other
column is permuted once — **passed every A/B assertion in `test/test_table_parallel.f90`** before the
oracle below existed. Every column had been rewritten by a table whose rows were, jointly, wrong.

**What this forbids.**

- **Never let an A/B equality be the only correctness assertion for a parallel path.** Pair it with
  an oracle that does not go through the machinery under test.
- **Keep `test_table_parallel.f90`'s fixture self-checking.** Every column of it is a pure function
  of `n`, the original row number — values *and* nulls — so `check_rows_consistent` can verify each
  surviving row against the fixture's own construction, whatever the mutation did to the row set. A
  column out of step with its neighbours cannot satisfy that, however both paths were broken. The
  fixture and the oracle share one formula (`key_for`) precisely so they cannot drift apart.
- **A new operation added to `table_colwork` needs both halves**, not just the A/B one.

**Covered by** the three equality tests in `test/test_table_parallel.f90`, each of which now runs
`check_rows_consistent` after `check_tables_identical`. Mutation-confirmed in both directions: the
skipped-column mutation fails all three A/B assertions, and the double-reindex mutation above fails
only the oracle.

---

### Risk-53 — A parallel mutation gate that never engages passes every correctness test

`colwork_threads` (`src/parquet_tables_parallel.f90`) decides whether a mutation rewrites its
columns on several threads. It answers **1** — serial — for four separate reasons: fewer than two
rewritable columns, already inside a parallel region, less work than `colwork_min_elements`, or
`parquet_set_table_threads(1)`. Every one of those is correct behaviour, and every one of them makes
the whole feature disappear while every assertion about values, order and nulls keeps passing.
Zero parallelism is a passing test, exactly as zero comparisons was for the partial sort (Risk-35)
and a silently serial `threads=` was for the sort (Risk-39).

**Three things a test here has to get right, all found by getting them wrong first.**

- **The suite must run its tests sequentially.** test-drive runs a suite inside its own
  `!$omp parallel do`, and `omp_in_parallel()` is therefore `.true.` inside every test — so the
  mutation resolves to one thread and an equality test compares the serial path against itself.
  This is why `table_parallel` is its own suite and why `run_tester.f90` excludes it. Putting these
  tests in `table` instead would make all of them vacuous, silently.
- **The fixture must clear the work floor.** `colwork_min_elements` is 131072 *elements*
  (`rows * width`), which no ordinary test fixture reaches by accident. `test_table_parallel.f90`
  gets there with a width-8 vector column at 20000 rows rather than by making the table long.
- **The thread count must be asserted, with a negative control.** `check_really_parallel` asserts
  `> 1` on a fixture proven to reach the parallel path; `test_table_threads_effect`
  (`test/test_settings.f90`) asserts `1` with the cap set to 1 *and* `1` below the work floor, on
  the same machinery. Either half alone passes against a hook that always answers the same number.

**The counter is written on BOTH paths**, serial included, which is a deliberate difference from
`parquet_debug_get_prefetch_threads_used` (written only on its parallel path, so its negative
control asserts 0). Writing it always is what separates "the gate declined" from "the mutation never
reached the loop at all" — and the tests reset it to 0, a value the mutation itself never writes.

**Covered by** `check_really_parallel` in all three tests of `test/test_table_parallel.f90`, the
caller's-own-region test in the same file, and `table_threads caps the parallel per-column rewrite`
in `test/test_settings.f90`. Mutation-confirmed: a gate hard-wired to return 1 fails all three
equality tests through their thread-count assertion.

### Risk-55 — Two readers of one table can sample different rows, and only a count mismatch shows it

A `parquet_table` opened with `sample_fraction=` will open **more than one reader over its lifetime**
— `%clone` reopens the file for every column the source had not already read, and P4's parallel
prefetch gives each thread its own. Every one of them has to draw the *same* rows, or one table ends
up holding columns from two different random samples.

Nothing in the sampling machinery enforces that. `parquet_reader_set_sample` honours a seed only when
`has_seed && seed > 0` and otherwise draws fresh entropy, and `parquet_open_reader` documents
`sample_seed <= 0` as *"draw a fresh seed"* — so a reader given no seed, or given `0`, quietly
samples for itself. The single invariant standing against that is stated on
`parquet_table_cache%read_sample_seed` and established in `open_table_impl`
(`src/parquet_tables_lifecycle.f90`):

> `read_sample_seed` is allocated and **positive** whenever `read_sample_fraction` is.

**Why the failure is quiet.** `parquet_check_read_row_count` (`src/parquet_read.f90`) compares the
caller's array length against the reader's row count, so *most* disagreements abort — that is how the
original defect was found, at `sample_fraction=0.5` over 20000 rows where the two draws differed by
75 rows. But that guard is about **counts, not membership**. Two Bernoulli draws that happen to keep
the same number of rows pass it while holding entirely different rows, and the table then answers
every query with two columns that do not describe the same objects. Nothing reports it, ever. At
0.5 over 2000 rows the counts coincide roughly once in fifty.

**What this forbids, for the next change here.**

- **A new site that opens a reader for an existing table must pass `cache%read_sample_seed`** — it
  is not optional plumbing, and a site that forgets it fails exactly as `%clone` did.
- **The draw stays in C++** (`parquet_draw_sample_seed` → `resolve_sample_seed`,
  `src/parquet_wrapper.cpp`). `parquet_open_table` is reachable from several threads at once — the
  documented per-thread-slice shape — and gfortran's `RANDOM_NUMBER`/`RANDOM_SEED` state is not
  thread-safe. `resolve_sample_seed`'s own comment records that this is why the draw lives there.
- **The seed is settled BEFORE the table's reader is created**, not read back off the handle
  afterwards. Reading it back would work, and would put an ordering obligation on every site that
  opens a reader — the same obligation whose omission is this risk.
- **Do not narrow the draw to the fractions that actually sample.** It fires for any
  `sample_fraction=` at all, including `>= 1.0` (no draw installed) and negative/NaN (rejected by
  the reader). A seed is inert in those cases, and the invariant is worth more with no exceptions
  than the entropy call is worth saving — a conditional invariant is one P4's gate would have to
  re-examine.
- **A count-only assertion does not test this.** See below.

**Covered by** `a clone keeps an UNSEEDED sample_fraction's own rows` (`test/test_table.f90`), whose
*shape* is the part to preserve rather than merely keep passing. It prefetches `k` (so the clone
receives it as deep-copied values), reads `x` through the clone's own reader, and asserts
`x == 1.5*k` **row for row** — a count comparison would pass against the defect one time in fifty.
Its second half is a negative control: two separate unseeded opens must still differ, so a "fix"
that settled on a constant seed fails. Mutation-confirmed both ways — removing the draw aborts with
`row count mismatch for column x: file has 1037 rows but the values array implies 979`, and a
hard-wired constant seed fails the control.

### Risk-60 — A per-element allocatable-character round trip in a bulk string operation costs 4x and fails nothing

`parquet_string_column`'s bulk operations walk every element. Materializing each element through
`%get`/`%to_string` — which does `allocate(character(len=elen) :: res)`, fills it, hands it back, and
frees it — turns an O(payload) byte copy into **one heap allocation, one fill, one copy and one free
per row**.

Measured on 4 M elements / 70 MB, both fixed: `%to_character` **0.168 s -> 0.038 s** (4.3x), with the
allocator accounting for 71 % of the original; `%build_from` **0.305 s -> 0.033 s** (9.3x), which was the
slowest operation in the module because it paid the allocation *and* grew its destination incrementally.

**The failure is not a wrong answer — it is no signal at all.** The allocating form returns byte-identical
results, so every test passes, every error scenario stays green, and the code reads as ordinary, idiomatic
Fortran. Only a benchmark notices. This is the Fortran twin of [Risk-59](#risk-59--a-shared_ptr-parameter-on-a-per-element-helper-costs-7x-and-fails-nothing),
found the same way and for the same underlying reason: a per-element convenience that the surrounding loop
never needed.

**Test.** Not testable as such — a timing assertion would be the flakiest test in the suite. What *is*
tested is that the direct-copy path is correct, by `to_character matches %get element for element,
padded with blanks` (`test/test_parquet_string.f90`), which uses `%get` as an **independent oracle**:
the two no longer share code, so element-for-element agreement is a real cross-check rather than a
tautology. It covers the no-`null_value` path, a zero-length element, and a `null_value` longer than
any real element. Four mutations confirmed caught: dropped padding, an off-by-one on the copied
length, a width taken from element one, and a skipped `null_value` substitution.

`%build_from` is covered by `build_from: empty, all-null, zero-length and repeated handles` in the same
file, which asserts `character_size()` throughout — the one field a wrong length sum corrupts silently,
since the strings still read back correctly until the column is written or appended to. Six mutations
confirmed caught: reading every element from the first handle's column, a dropped null-row offset write,
a dropped `has_nulls`, a dropped empty-array early return, offsets written only on the non-null arm, and
a halved length sum (caught by the guard below, not by an assertion).

**What this forbids.** Do not reintroduce a `character(len=:), allocatable` intermediate inside any
per-element loop in `src/parquet_strings.f90`. The bytes are already contiguous in `data(:)` and their
bounds are one subtraction away (`elem_bounds`); a bulk operation should read them there. The rule is
about the *loop*, not about `%get`, which is exactly right for its own job of returning one element.

**A second failure mode arrives with the fix, and it is worse than the one it replaces.** Sizing the
destination once means the fill loop can no longer grow it, so a length sum that disagrees with what the
fill actually writes is a **heap overflow**, not a slow path. **No fixture this repository can build
catches it**: `ensure_data_cap` allocates `max(need, MIN_CHAR_CAP)` = at least 64 bytes, so every
test-sized column is covered by the minimum however wrong the sum is. Confirmed by mutation — halving
`want` in `%build_from` left the whole suite green. `%build_from` therefore carries an inline
`if (pos + elen > want) error stop` (one integer compare per element, `GCOVR_EXCL`'d as defensive),
which turns it into a clean abort and makes the mutation caught. **Any future bulk operation that
pre-sizes its destination needs the same guard**, and must not rely on tests to find its absence.

**Guarded by `check_no_per_element_string_alloc`** (`tools/check_source_conventions.py`), the analogue
of `check_no_per_element_shared_ptr`: it matches by SHAPE — a `%get`/`%to_string` call at `do`-loop
depth ≥ 1 — rather than by a list of procedure names, so it cannot go blind to the next bulk operation
added. Confirmed to fire: reverting `%to_character` to the allocating form fails the lint stage with
the right line, and restoring it clears.

**The check is scoped to `src/parquet_strings.f90`, and that scope is a KNOWN GAP, not a judgement
that the rest is clean.** S3's audit swept all of `src/*.f90` and found the shape alive in four
places in the module's *consumers*, on hotter paths than anything left inside it:
`extract_col_string` and `extract_strcol` (`src/parquet_sorting_keys.f90`, **generated** — the fix is
a `tools/generate_parquet_sorting.py` template edit), `stat_str`/`stat_strv`
(`src/parquet_tables_access.f90`), and `parquet_check_qc_string_compact`
(`src/parquet_write_string.f90`). Measured by differencing two real code paths at 4 M elements: one
allocation per element costs **0.11 s**, which is **19 %** of a `parquet_string_column` sort and
**33 %** of a `parquet_column` one. **Widening the check to `src/` requires fixing those first**, or
it fails the lint stage on known work — which is scheduled as `feature_string_parallel.md` **S10**,
whose last step is that widening. See S3 there for the numbers and the method.

### Risk-61 — A validity split that is not byte-aligned loses nulls, and no end-to-end test can be relied on to see it

`parquet_string_column`'s validity bitmap packs **8 rows per byte**. A threaded bulk operation whose
row ranges meet *inside* a byte has two threads doing a read-modify-write on that byte: one update is
lost, some row's null flag is wrong, the column still passes `%validate()`, and nothing aborts.

`thread_row_ranges` (`src/parquet_strings.f90`) exists to make that impossible rather than unlikely:
it divides the validity **bytes** and converts back to rows, so every range begins at `1 mod 8` and no
two threads ever touch the same byte. Every threaded phase in the module uses its ranges, including
the phases that would be safe with any split (the `old_null` capture writes disjoint array elements),
because one split shared by all phases cannot drift from itself.

**Test — and the division of labour here is the point, not an accident.**

- **The alignment rule is unit-tested deterministically**, by `thread row ranges cover every row and
  never share a validity byte` (`test/test_parquet_string.f90`), which asserts coverage, alignment and
  the legality of empty ranges over a sweep of row counts that straddle byte boundaries. Replacing the
  byte split with an even row split fails it every time.
- **The end-to-end equality tests do NOT reliably catch it**, and this was measured rather than
  assumed: the same mutation, run against `test/test_string_parallel.f90`'s threaded-vs-serial
  comparisons, **passed** — because a data race on a handful of boundary bytes, in a loop that
  finishes in microseconds, simply may not occur in any given run.

**What this forbids.** Do not treat the equality tests as cover for the alignment rule; they cover
everything *except* it. Any new threaded phase must take its ranges from `thread_row_ranges` rather
than compute its own, and any change to that helper must keep the unit test passing — it is the only
deterministic guard this risk has. A "simplification" that splits rows evenly is the exact defect, and
it will look correct in every test run that does not happen to lose a write.
