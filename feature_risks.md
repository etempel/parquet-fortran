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
next unused number — `Risk-31` today — and goes in "1. New risks"** until it has been triaged.
Numbers of deleted entries are not reused, so a stale reference resolves to nothing rather than to
the wrong risk.

**Counts today: 26 covered, 0 proposed, 4 not testable.** Section 2 being empty is the healthy
state rather than a finished one — it means every risk currently identified as testable has its
test. Seven entries are covered by something other than a unit test, deliberately: Risk-1 by a
maintainer check under `app/` with a `tools/*.sh` wrapper (it measures memory, so it needs its own
process per measurement), and Risk-2, Risk-4, Risk-5, Risk-12, Risk-13 and Risk-19 by static checks
in `tools/check_source_conventions.py`, which is the right tool for an invariant about what the code
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

---

## 1. New risks

*Nothing here.* A risk lands in this section when it is first identified — before anyone has
decided whether it is testable, and before any test is written. Give it the next unused number
(**Risk-31**), state what breaks and why the failure is quiet, and leave the **Test** half to whoever
triages it into one of the three sections below.

## 2. Risks with a proposed testing scenario

*Nothing here.* Every entry this section held has been implemented and moved to section 4 — which is
where a proposal goes once its test exists, carrying its number with it. A risk belongs here when
someone has decided it is testable and said what to assert, but has not written the test yet, and
only for as long as that is true.

## 3. Risks not testable

Each of these says how to check or avoid the risk instead. Three of the four are not gaps at all —
they are a cost, a caveat about the input, or a property of a process that has already aborted — and
writing a test for them would freeze the wrong thing as a contract.

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
storage, and every row-structural mutation (`%filter_rows`, `%sort_by`, `%delete_rows`, `%truncate`,
`%append`, `%append_null_rows`) reallocates that storage exact-fit. `%cast` replaces the storage too,
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
  thread in row-group order. These constrain any future streaming write — see the chunked-write entry in `feature_table.md` §3.4.

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
