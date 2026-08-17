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
next unused number — `Risk-106` today — and goes in "1. New risks"** until it has been triaged.
Numbers of deleted entries are not reused, so a stale reference resolves to nothing rather than to
the wrong risk.

**Counts today: 63 covered, 1 proposed, 16 not testable.** An empty section 2 is the healthy
state rather than a finished one — it means every risk currently identified as testable has its
test — so the entry sitting there is a to-do, not a milestone. Nine entries are covered by
something other than a unit test, deliberately: Risk-1 by a
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
| [Risk-60](#risk-60--a-per-element-allocatable-character-round-trip-in-a-bulk-string-operation-costs-4x-and-fails-nothing) | A per-element allocatable-character round trip in a bulk string operation costs 4x and fails nothing | 4 — covered |
| [Risk-61](#risk-61--a-validity-split-that-is-not-byte-aligned-loses-nulls-and-no-end-to-end-test-can-be-relied-on-to-see-it) | A validity split that is not byte-aligned loses nulls, and no end-to-end test can be relied on to see it | 4 — covered |
| [Risk-62](#risk-62--a-validity-run-copied-byte-wise-silently-mis-places-nulls-when-its-alignment-precondition-is-wrong) | A validity run copied byte-wise silently mis-places nulls when its alignment precondition is wrong | 4 — covered |
| [Risk-63](#risk-63--replacing-an-in-place-compactions-byte-loop-with-an-array-section-costs-a-heap-temporary-per-element) | Replacing an in-place compaction's byte loop with an array section costs a heap temporary per element | 3 — not testable |
| [Risk-64](#risk-64--two-threads-pasting-adjacent-row-groups-share-a-validity-bitmap-block-and-lose-a-null) | Two threads pasting adjacent row groups share a validity bitmap block and lose a null | 4 — covered |
| [Risk-65](#risk-65--a-guard-claimed-after-the-state-it-protects-is-a-guard-that-loses-the-race) | A guard claimed after the state it protects is a guard that loses the race | 4 — covered |
| [Risk-67](#risk-67--an-unbounded-read-of-a-parquet_column-storage-array-returns-uninitialised-slack) | An unbounded read of a `parquet_column` storage array returns uninitialised slack | 4 — covered |
| [Risk-68](#risk-68--a-qc-bound-is-parsed-into-float64-so-a-bound-past-253-is-silently-rounded) | A qc bound is parsed into `float64`, so a bound past 2^53 is silently rounded | 4 — covered |
| [Risk-69](#risk-69--a-test-that-compares-a-never-written-column-with-itself-passes-on-the-heaps-luck) | A test that compares a never-written column with itself passes on the heap's luck | 2 — proposed |
| [Risk-70](#risk-70--an-intentinout-temporal-setter-that-skips-a-component-leaves-a-reused-element-stale) | An `intent(inout)` temporal setter that skips a component leaves a REUSED element stale | 4 — covered |
| [Risk-71](#risk-71--a-stale-handle-reads-the-wrong-column-or-row-and-nothing-says-so) | A stale handle reads the wrong column or row, and nothing says so | 4 — covered |
| [Risk-72](#risk-72--the-name-form-and-the-handle-form-of-one-accessor-can-silently-disagree) | The name form and the handle form of one accessor can silently disagree | 4 — covered |
| [Risk-73](#risk-73--a-keydatatype-entry-that-disagrees-with-its-value-is-worse-than-no-entry) | A `<KEY>.datatype` entry that disagrees with its value is worse than no entry | 4 — covered |
| [Risk-74](#risk-74--discarding-a-column-the-caller-wrote-into-silently-restores-the-files-values) | Discarding a column the caller wrote into silently restores the file's values | 4 — covered |
| [Risk-75](#risk-75--the-name-indexs-linear-scan-fallback-is-a-safety-net-nothing-exercised) | The name index's linear-scan fallback is a safety net nothing exercised | 4 — covered |
| [Risk-76](#risk-76--casts-post-touch-re-check-guards-an-invariant-that-lives-in-another-file) | `%cast`'s post-touch re-check guards an invariant that lives in another file | 3 — not testable |
| [Risk-77](#risk-77--a-masked-write-compacts-the-values-and-the-validity-mask-separately) | A masked write compacts the values and the validity mask separately | 4 — covered |
| [Risk-78](#risk-78--a-temporal-columns-null-cache-is-invalidated-by-the-writer-not-by-the-reader) | A temporal column's null cache is invalidated by the writer, not by the reader | 4 — covered |
| [Risk-79](#risk-79--the-no-relocation-guarantee-rests-on-one-conditional-and-nothing-else) | The no-relocation guarantee rests on one conditional and nothing else | 4 — covered |
| [Risk-80](#risk-80--a-metadata-only-query-quietly-decodes-a-whole-column-and-the-answer-is-still-right) | A metadata-only query quietly decodes a whole column, and the answer is still right | 4 — covered |
| [Risk-82](#risk-82--a-non-nullable-field-that-receives-a-null-writes-definition-levels-that-disagree-with-its-schema) | A non-nullable field that receives a Null writes definition levels that disagree with its schema | 4 — covered |
| [Risk-81](#risk-81--two-individually-valid-ranges-that-describe-different-parts-of-the-file) | Two individually valid ranges that describe different parts of the file | 4 — covered |
| [Risk-83](#risk-83--a-write-path-that-does-not-resolve-a-declared-auto-size-emits-a-sentinel-into-the-sidecar) | A write path that does not resolve a declared `auto` size emits a sentinel into the sidecar | 4 — covered |
| [Risk-84](#risk-84--a-maml-key-matched-case-sensitively-loses-a-whole-block-in-silence) | A MAML key matched case-sensitively loses a whole block, in silence | 4 — covered |
| [Risk-85](#risk-85--the-in-code-schema-builder-now-owns-both-parsing-and-validating-its-own-text) | The in-code schema builder now owns both parsing and validating its own text | 4 — covered |
| [Risk-86](#risk-86--a-defective-quicksort-still-returns-a-correctly-sorted-answer) | A defective quicksort still returns a correctly sorted answer | 4 — covered |
| [Risk-87](#risk-87--the-counting-sorts-range-check-cannot-be-written-the-way-c-writes-it) | The counting sort's range check cannot be written the way C++ writes it | 4 — covered |
| [Risk-88](#risk-88--the-sort-comparator-silently-loses-a-third-of-its-speed-if-it-outgrows-an-inlining-budget) | The sort comparator silently loses a third of its speed if it outgrows an inlining budget | 3 — not testable |
| [Risk-89](#risk-89--the-radix-path-is-a-third-expression-of-the-ordering-and-a-wrong-answer-there-is-silent) | The radix path is a third expression of the ordering, and a wrong answer there is silent | 4 — covered |
| [Risk-90](#risk-90--the-narrow-integer-bias-is-safe-in-exactly-one-direction-and-its-guard-cannot-be-tested) | The narrow-integer bias is safe in exactly ONE direction, and its guard cannot be tested | 3 — not testable |
| [Risk-91](#risk-91--sort_radix_refine_strings-reads-one-array-while-permuting-another-and-nothing-diagnoses-passing-the-same-one) | `sort_radix_refine_strings` reads one array while permuting another, and nothing diagnoses passing the same one | 3 — not testable |
| [Risk-92](#risk-92--the-last-radix-pass-leaves-the-row-array-stale-and-only-the-string-exclusion-makes-that-safe) | The last radix pass leaves the row array STALE, and only the string exclusion makes that safe | 4 — covered |
| [Risk-93](#risk-93--the-test-log-names-a-different-test-from-the-one-that-is-stuck) | The test log names a DIFFERENT test from the one that is stuck | 3 — not testable |
| [Risk-94](#risk-94--a-compiler-may-use-an-overflowing-expressions-undefinedness-to-delete-a-branch-somewhere-else) | A compiler may use an overflowing expression's undefinedness to delete a branch somewhere else | 4 — covered |
| [Risk-95](#risk-95--parquet_randoms-remaining-wrapping-sites-rest-on-one-test-and-nothing-else) | `parquet_random`'s remaining wrapping sites rest on one test and nothing else | 4 — covered |
| [Risk-96](#risk-96--a-wide-width-integer-draw-can-be-silently-non-uniform-while-every-obvious-test-passes) | A wide-width integer draw can be silently non-uniform while every obvious test passes | 4 — covered |
| [Risk-97](#risk-97--a-wrongly-selected-route-e-fork-silently-ships-the-wrapping-kernel-on-a-capable-compiler) | A wrongly selected route (e) fork silently ships the wrapping kernel on a capable compiler | 4 — covered |
| [Risk-98](#risk-98--a-schedule-dependent-draw-reintroduces-irreproducibility-and-every-structural-test-still-passes) | A schedule-dependent draw reintroduces irreproducibility, and every structural test still passes | 4 — covered |
| [Risk-99](#risk-99--a-fatal-path-reached-by-several-threads-at-once-hangs-instead-of-terminating) | A fatal path reached by several threads at once hangs instead of terminating | 4 — covered |
| [Risk-100](#risk-100--a-lazily-computed-rejection-threshold-is-untested-by-every-width-that-does-not-reject) | A lazily computed rejection threshold is untested by every width that does not reject | 4 — covered |
| [Risk-101](#risk-101--the-wrapping-route-e-kernel-is-built-by-nothing-routine-and-is-miscompiled-under-lto) | The wrapping route (e) kernel is built by nothing routine, and is miscompiled under LTO | 4 — covered |
| [Risk-102](#risk-102--a-default-kind-size-wraps-above-231-elements-and-the-fill-fails-silently) | A default-kind `size()` wraps above 2**31 elements, and the fill fails silently | 4 — covered |
| [Risk-103](#risk-103--the-streams-high-counter-word-is-reached-by-no-ordinary-stream-index) | The stream's high counter word is reached by no ordinary stream index | 4 — covered |
| [Risk-104](#risk-104--a-thread-team-opened-one-level-down-deadlocks-libgomp) | A thread team opened one level down deadlocks libgomp | 4 — covered |
| [Risk-105](#risk-105--an-allocate-extent-from-a-default-kind-size-overflows-the-array-it-just-allocated) | An allocate extent from a default-kind `size()` overflows the array it just allocated | 4 — covered |
| [Risk-106](#risk-106--a-stream-consumed-across-loop-iterations-is-irreproducible-and-nothing-fails) | A stream consumed across loop iterations is irreproducible, and nothing fails | 4 — covered |
| [Risk-107](#risk-107--a-queue-shaped-stream-buffer-would-pull-the-buffer-state-into-the-contract) | A queue-shaped stream buffer would pull the buffer state into the contract | 4 — covered |
| [Risk-108](#risk-108--an-integer-draw-taken-off-a-block-boundary-re-reads-words-already-handed-out) | An integer draw taken off a block boundary re-reads words already handed out | 4 — covered |
| [Risk-109](#risk-109--the-bulk-permutation-and-the-scalar-entry-point-compute-the-same-function-by-different-routes) | The bulk permutation and the scalar entry point compute the same function by different routes | 4 — covered |
| [Risk-110](#risk-110--the-permutations-round-count-round-function-and-width-rule-are-frozen-and-three-rounds-looks-free) | The permutation's round count, round function and width rule are frozen, and three rounds looks free | 4 — covered |
| [Risk-111](#risk-111--a-bulk-permutation-that-silently-stopped-threading-would-fail-no-test) | A bulk permutation that silently stopped threading would fail no test | 3 — not testable |

---

## 1. New risks

*Nothing here.* A risk lands in this section when it is first identified — before anyone has
decided whether it is testable, and before any test is written. Give it the next unused number
(**Risk-112**), state what breaks and why the failure is quiet, and leave the **Test** half to whoever
triages it into one of the three sections below.

## 2. Risks with a proposed testing scenario

A risk belongs here when someone has decided it is testable and said what to assert, but has not
written the test yet, and only for as long as that is true. Once the test exists the entry moves to
section 4, carrying its number with it.

### Risk-69 — A test that compares a never-written column with itself passes on the heap's luck

**What breaks.** A `null` numeric row's *value* bytes are unspecified by design. `grow_rows`
(`src/parquet_columns_structural.f90`) sets the validity bits and writes no values, matching `%init`'s
documented contract, and its own comment says so: new rows are *"unspecified-but-valid for the numeric
ones"*. A column created all-null and never written therefore holds whatever the allocator last left
on those pages.

A test that then compares such a column **against itself** — one accessor form against another, which
is a deliberate and widely used technique in `test/test_table_codegen.f90` — is asserting `x - x == 0`
over undefined bytes. **That holds for every bit pattern except a NaN.** `NaN - NaN` is `NaN` and every
comparison against a NaN is false, so the assertion passes or fails according to what the allocator
handed back. The library is behaving exactly as documented throughout; only the test is unsound.

**Why the failure is quiet, and worse than quiet.** It is *intermittent*. The one instance found
failed **once in six full suite runs** and then passed ten reruns in a row, which is indistinguishable
from a flake and is triaged as one. Both natural explanations for an intermittent failure in this
project — a data race, and two tests sharing a fixture path (see CLAUDE.md's "Tests run concurrently")
— are documented well enough to be the obvious first guesses, and both were tried here and were wrong.
The failure also reports whichever accessor form happened to touch a NaN row, so its *message* moves
between runs, which reads as nondeterminism in the library rather than in the fixture.

**What this forbids.**

- **Do not sweep a column the library never wrote.** If a test reads a column's values at all, fill
  it first. The rule generalises past `null` rows: any storage the library documents as
  unspecified-but-valid is not comparable, including a column's spare capacity (Risk-67).
- **Fill it with ROW-DISTINCT values, never a constant.** A constant fill cures the NaN unsoundness
  and simultaneously destroys the sweep's ability to detect a misaligned range accessor — trading an
  unsound assertion for a vacuous one. Verified by mutation rather than asserted: with a constant
  fill a deliberately misaligned range accessor was **not caught**; with `0.5_real64*i` it was.
- **Do not "fix" this in the library by zero-filling `grow_rows`.** It would contradict a documented
  contract, add a `memset` to a path CLAUDE.md already flags as allocation-sensitive, and mask genuine
  "read of a null value" bugs instead of surfacing them.
- **An assertion that checks only `size(...)` or only `is_null()` is not a weaker version of this
  problem — it is a different one, and the NaN instrument cannot see it.** Six such assertions were
  found alongside the original defect; each would have passed against a *wrong answer*, not merely
  against garbage. When auditing for this risk, grade every assertion in the affected test rather than
  fixing only the one that failed.

**Test — proposed.** The instance is fixed and mutation-tested (`test/test_table_codegen.f90`, commit
`a4ff042`: `flux` is filled with row-distinct values before the accessor sweep, and six size-only /
null-only assertions were strengthened to compare values; three deliberate misalignments were
confirmed caught). **What is not covered is a future test reintroducing the pattern**, and nothing
static can see it — the assertion is well-formed and the memory is genuinely allocated.

The proposed scenario is the instrument that found it: run the **whole suite** with an `LD_PRELOAD`
shim that fills every `malloc` block with `0xFF`, so every uninitialized `real32`/`real64` reads as a
NaN. Under it the original defect was **100% reproducible** (1411 passed / exactly 1 failed, the same
test every time) and after the fix the same run is **1412 / 0**. The shim is recorded in CLAUDE.md's
"An intermittent test failure has THREE causes".

Two things to settle before implementing it, which is why this is a proposal rather than a test:

- **Where it runs.** `LD_PRELOAD` is Linux-only, so this is a CI-image job rather than something
  `fpm test` can do; the macOS equivalent (`DYLD_INSERT_LIBRARIES` + `DYLD_FORCE_FLAT_NAMESPACE`) is
  untested here. A periodic or manual job may be the right shape rather than a per-commit one.
- **What it proves.** gfortran's `allocate` goes through `malloc`, so column storage is covered, but
  stack-resident automatic arrays and anything from `calloc`/`realloc` are **not**. A clean run is
  strong evidence and not a proof, and the entry should say so rather than being read as closing the
  class.

**A related question is CLOSED and should not be re-opened.** These undefined bytes do **not** reach
the output file. Writing the same all-null column under four different heap fills produced two
byte-identical files, and the files that differed differed in exactly **4 bytes** — a creation
timestamp in the MAML metadata. Parquet stores no values for null entries (only definition levels
record nullness), confirmed with `pyarrow`: the chunk is 23 bytes with `null_count=6`, and six
`real64` values cannot fit in 23 bytes. Valgrind reports 0 uninitialised-value errors on that write
path.

## 3. Risks not testable

Each of these says how to check or avoid the risk instead. Most are not gaps at all — they are a
cost, a caveat about the input, a property of a process that has already aborted, or a pre-state no
test can arrange — and writing a test for them would freeze the wrong thing as a contract.

### Risk-90 — The narrow-integer bias is safe in exactly ONE direction, and its guard cannot be tested

`sort_radix_permutation` (`src/parquet_sorting_engine.f90`) images an integer key as `v - vmin`
instead of `ieor(v, SORT_SIGN_BIT)` when `sort_span_under_2p32` says the value range spans under
2^32, which leaves the top four bytes constant and skips four of the eight passes. It is worth 45% on
an int32 column.

**The guard's failure mode is not a wrong answer, and assuming it is leads to deleting the guard.**
`v - vmin` under wrapping arithmetic is exactly unsigned subtraction mod 2^64, and every int64 range
fits in 2^64, so the biased image is order-preserving for **any** minimum at or below every value.
Confirmed by mutation: forcing `narrow = .true.` everywhere, and replacing the two-branch range test
with the naive `hi - lo < 2^32` whose subtraction overflows on a range spanning both signs, each
leave **every permutation in the suite bit-identical and every pass count unchanged**. What those
edits really cost is **undefined behaviour** — a signed subtraction leaving int64 — plus the four
skipped passes on the columns that should have had them.

**The one direction that IS dangerous**, and the rule this entry exists to state: *a `vmin` ABOVE any
value being imaged makes the difference negative and the unsigned order wrong.* Everything in the
other direction is safe by construction, because the span test is then applied to a range that
CONTAINS the true one — a `vmin` too low, a `vmax` too high, a range widened by a null that was not
skipped. So when editing the range scan, the only question that matters is whether `vmin` can end up
above a value the image build will see.

**Test.** The dangerous direction is covered — biasing by `vmax` fails the suite immediately
(`test_radix_path_narrow_integer`, `test/test_sorting.f90`), and seeding the scan at zero is caught by
that test's far-from-zero fixture through the pass counter. The guard itself is **not testable**: no
fixture can distinguish a correct span decision from a wrong one, because the property it defends is
undefined behaviour rather than an answer. `sort_span_under_2p32`'s own doc-comment says so at its
head, so that a future reader does not delete the two-branch form on the strength of a green suite or
a surviving mutation. Both are expected.

**A fixture trap worth reusing.** The both-int64-extremes column, which looks like the obvious test
for a wide range, is degenerate here *in principle*: at `lo = -2^63` the bias and the sign flip are
the same transform, since `v - (-2^63)` is `v + 2^63` is `ieor(v, SORT_SIGN_BIT)`. And a narrow band
placed at a byte-ALIGNED base (2^40) does not discriminate either, because the unbiased image leaves
byte 4 constant too and runs the same four passes. The fixtures must straddle a byte boundary and
must avoid `-2^63`; `test_radix_path_narrow_integer` explains both.

### Risk-91 — `sort_radix_refine_strings` reads one array while permuting another, and nothing diagnoses passing the same one

`sort_radix_refine_strings` (`src/parquet_sorting_engine.f90`) takes the sorted row array `ra` as
`intent(in)` and the permutation `perm` as `intent(inout)`, walks runs of equal image in the first
and reorders the second. **They must be different arrays.** Associating one actual with a dummy that
is defined and an `intent(in)` dummy at the same time is forbidden by F2018 15.5.2.13, and neither
gfortran nor ifx diagnoses it — the compiler is entitled to optimise on the assumption, so the
symptom would be a wrong permutation that appears only under optimisation, or only on one compiler.

The trap is specific and easy to walk into: after the multi-key string pass's LSD loop the sorted rows
are sitting in `perm`'s own value block, so passing `perm` as `ra` is the obvious thing to write and
it *looks* correct — the ranges even line up. `sort_radix_string_key_pass` normalises its buffers
specifically to avoid it, copying the rows into `pb` so that a distinct array can be handed over. That
copy is not redundant and must not be removed as an optimisation.

**Test.** Not testable. It is undefined behaviour, so a build that happens to work proves nothing
about the next one. Both procedures' doc-comments state the requirement at the point where it would
be violated, which is the only defence available.

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
count. It is documented in `doc/pages/io/reading.md` and is a caveat, not a bug to fix.

**Test.** Deliberately none, and adding one would be a mistake worth naming.

A file whose footer records bounds that do not match its data will give a wrong answer, and the
screen cannot defend against it: it deliberately does not read the data it would have to compare
against. A fixture *could* be built — a debug writer can record any bounds it likes — but the test
would assert that the library returns wrong rows, freezing as a contract something that is a caveat
about the input rather than a behaviour of the library.

**How to think about it instead.** This is the same trust `parquet_column_has_nulls` already places
in the recorded null count, and it is documented in `doc/pages/io/reading.md`. A user who suspects a
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
`src/parquet_tables.f90`, which is overwritten), and the callout in `doc/pages/utilities/sorting.md`'s
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
in `doc/pages/operating/settings.md`, `doc/pages/operating/thread-safety.md` and `CHANGELOG.md` — *a whole-column rewrite
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

### Risk-63 — Replacing an in-place compaction's byte loop with an array section costs a heap temporary per element

`compact_all_serial` and `delete_by_mask_serial` (`src/parquet_strings.f90`) compact a column's payload
**in place**, copying each element's bytes with a scalar loop:

```fortran
do k = lo, hi
    wpos = wpos + 1_int64
    c%data(wpos) = c%data(k)
end do
```

That loop looks like an oversight. It is not, and the tidy-up is expensive in a way nothing reports:
`c%data(wpos+1:wpos+n) = c%data(lo:hi)` has **the same array on both sides**, so the compiler cannot
prove the two ranges do not overlap and must evaluate the right-hand side into a temporary first — one
heap allocation and free **per element**. Measured on 4 M elements: `trim_all` **0.0261 s → 0.1439 s
(5.5x slower)**, `delete_by_mask` **0.0118 s → 0.0886 s (7.5x slower)**. Every test still passes, every
answer is still correct, and the column still validates.

This is [Risk-60](#risk-60--a-per-element-allocatable-character-round-trip-in-a-bulk-string-operation-costs-4x-and-fails-nothing)'s
class arriving from a direction that risk's lint check cannot see: `check_no_per_element_string_alloc`
matches `%get`/`%to_string` calls, and there is no call here at all — the allocation is emitted by the
compiler from an assignment that mentions nothing.

**The threaded paths use exactly that section assignment, and must.** Their destination is a *different*
array, so it is provably non-overlapping and compiles to a single `memcpy` — which is most of why
`delete_by_mask` threads to 3.46x (5,800 MB/s to 19,900 MB/s) rather than to the ~3x threads alone would
buy. **The rule is about aliasing, not about sections**: same array, use the loop; different arrays, use
the section.

**Test.** None, and none is proposed: the failure is a 5.5x slowdown, not a wrong answer, and a timing
assertion in the unit suite would be flaky on a loaded machine. `tools/benchmark_strings.sh`'s `trim_all`
and `delete_by_mask` rows are where it would show, which is how it was found.

**What this forbids.** Do not "modernise" either loop, and do not copy the pattern *into* a new in-place
compaction without checking which array the destination lives in. If a future in-place bulk operation is
added here, it inherits this constraint; if it can afford a fresh destination buffer, prefer that
instead, since it removes the aliasing and buys the parallelism at the same time.

### Risk-76 — `%cast`'s post-touch re-check guards an invariant that lives in another file

**What breaks.** `table_cast` (`src/parquet_tables_mutate.f90`) compares the column's kind against the
target twice: once against `declared_kind` before it does anything, and once against
`values%kindof()` after `table_touch` has read the column. If the second check is removed and the
two ever disagree, `cast_apply` converts a column that is *already* in the target kind — it
reinterprets a real64 buffer as int64, or the reverse. That is a silent wrong answer in the values
themselves, with the kind, width, row count and null mask all still correct.

**Why it is not testable.** The two cannot disagree today, and that was established rather than
assumed: `table_materialize` (`src/parquet_tables_read.f90`) always decodes into
`slot%declared_kind`, and every path that leaves a column resident without materializing leaves the
pair in step. Six candidate sequences were run against an instrumented build — a deferred cast
followed by a second one, a cast back to the file's own kind, `exact=` on an untouched column, an
evict-then-cast, and both of those on a deferred-WIDTH `LIST` column — and none reached the branch;
nor does anything in the suite or the error scenarios. There is no public API through which the
disagreement can be arranged, and no debug hook could arrange it without writing `declared_kind`
directly, which would be testing the hook rather than the guard.

The branch body is `GCOVR_EXCL`'d with that reasoning recorded beside it.

**What this forbids.**

- **Do not delete the second check on the strength of a coverage report.** Its whole value is that
  it does not depend on a promise made one file away staying true.
- **Anything that changes what `table_materialize` decodes into must revisit this.** A read path
  that produced the file's kind and converted afterwards — a plausible optimization — would make the
  branch live, and the entry moves to section 2 with a test the moment that happens.
- **Do not "simplify" the two checks into one.** They are asking different questions: the first is
  about what the table has been told the column is, the second about what it actually holds.

### Risk-93 — The test log names a DIFFERENT test from the one that is stuck

test-drive prints its `Starting <name>` line *before* dispatching, and runs the tests of a
parallelisable suite inside its own `!$omp parallel do`. So when a run wedges, the last `Starting`
line in the log names whichever test the scheduler had reached — **not** the one on the stack.
Diagnose a hang from a live stack (`sample <pid>` on macOS, `eu-stack`/`gdb` on Linux), never from
the log's tail.

**Provenance, and why it is filed rather than fixed.** Found while diagnosing a one-off stall during
the Stage 6 engine flip: a 2000-element sort sat over 456 s inside `sort_radix_hist_threaded`'s
`!$omp parallel`, under `libgomp` in `_pthread_mutex_firstfit_lock_slow`. The log blamed a different
test, and an hour went into the wrong hypothesis because of it.

**That stall was investigated and left unexplained; it is most likely environmental.** Nesting was
refuted twice — the `sorting` suite is excluded from test-drive's parallelism, so the enclosing
region is inactive and `omp_in_parallel()` is `.false.`; and nested regions are serialised by default,
creating no team at all. It did not reproduce in **83,790** sorts mimicking the offending test, nor in
25 rounds under `OMP_MAX_ACTIVE_LEVELS=4`, nor in any full-suite run since, and the machine was
running several processes at the time. The one fact that does not fit the environmental story: it was
blocked *acquiring a mutex* rather than waiting on a condition, which is deadlock-shaped. Reopen only
on a recurrence, and capture **machine state** (load, concurrent processes) rather than more sorts —
sorts have been shown not to be the variable.

**One measurement from that investigation is worth keeping.** Opening an OpenMP team costs **~183 us**
on machine A — 0.512 s for 2793 sorts of at most 400 elements, where the sorting itself is
microseconds. That is several times what sorting 32,000 elements costs end to end, and it means
section 14's engine floor of `max(32768, 2048*nt)` is not conservative: it is roughly where a team
starts paying for itself.

**Test.** None possible for the logging trap — it is a property of a dependency's output, and a test
asserting that a log is misleading would be asserting the bug rather than guarding against it.


### Risk-111 — A bulk permutation that silently stopped threading would fail no test

`pf_random_permutation`/`pf_random_subset` resolve a thread count in `random_threads` and then branch
on it: `nth <= 1` takes a serial call, anything else opens a team. **Change that branch to always
take the serial arm and every assertion in the suite still passes** — the answer is identical by
construction, because element `k` depends only on `(seed, m, k)`, so a wholly serial implementation
is *correct*. The only symptom is that a 10**8-element permutation takes 9.6 seconds instead of
0.11.

**The near-miss that motivates this entry was exactly that shape and did ship into the working
tree.** The `!$omp parallel do` was first written without a `num_threads(nth)` clause, so OpenMP
opened the DEFAULT team — 384 threads on machine B — and then ran a loop with `nth` iterations. Every
call paid to create and destroy a full team whatever the caller asked for: `threads=2` on a
1000-element permutation cost **11.3 ms against 10 us serial**, a fixed ~6–11 ms on every call at
every size. Every test passed, including the bit-identity test, because the *values* were right. It
was found only by a probe measuring wall time per element, and it presented as an absurdly expensive
work floor rather than as a missing clause.

**Two rules follow.** `num_threads(nth)` is load-bearing on both fill sites and may not be dropped as
redundant. And a timing claim about this path is checked with `app/probe_random_perm.f90
--mode=floor`, never inferred from the suite.

**Test.** None in the suite, deliberately: the observable is wall time, and a timing assertion is the
one kind this project has consistently found to be worse than no assertion. What the suite *does*
carry is the pair that bounds the damage — `test_perm_threads` (`test/test_random_omp.f90`) asserts
the result is bit-identical at 1, 2, 3, 5, 8, 16 and 64 threads, so a threading defect can only ever
cost time; and `test_random_parallel_min_effect` (`test/test_settings.f90`) asserts the resolver
returns what the settings say, with a negative control in both directions. Neither can see whether
the fill went on to honour the count. `parquet_debug_random_bulk_threads` re-computes the rule rather
than reporting what a call did, and that limitation is stated on the hook itself.


## 4. Risks already covered, kept for what they still forbid

Every entry here has a test behind it. What keeps it in the document is the second half: a rule for
whoever edits the area next. Read the entry for the area you are about to touch before you touch
it — that is what this section is for, and it is why "covered" is not the same as "finished".

### Risk-106 — A stream consumed across loop iterations is irreproducible, and nothing fails

`pf_random_stream` carries a position, so which value an iteration receives depends on how many
draws ran before it. Seeded **once** and consumed **across** the iterations of a parallel loop, the
program runs, produces plausible numbers, and gives a different answer at a different thread count
or schedule — which is precisely the failure the whole module exists to remove, reintroduced by the
one part of it that has state.

**This is a misuse the API invites rather than a defect in it**, and no structural test can catch it:
the values are in range, the distribution is right, and a single-threaded run is perfectly
repeatable. Only comparing two *different* schedules reveals it, which is what
`test_stream_schedule` (`test/test_random_omp.f90`) does — a per-iteration-seeded stream drawing a
**data-dependent** number of values, run serially and under three schedules including a 7-thread
`dynamic,3`, asserted bit-identical, with a vacuity guard that fails if the team was one thread.

**Test:** `test_stream_schedule`. **What it still forbids:** any example, guide passage or
doc-comment that seeds a stream outside a loop and draws inside it. The seeding call is O(1) with no
warm-up specifically so that per-iteration seeding costs nothing, so there is no performance argument
for the unsafe shape — and note the tier-0 form has no way to express it at all, which is why this
risk arrives only with tier 1.

### Risk-107 — A queue-shaped stream buffer would pull the buffer state into the contract

`pf_random_stream` holds the block it last enciphered, **keyed by that block's index**. A future
contributor optimising it will find the obvious alternative — a queue of undelivered values, drained
one at a time and refilled when empty — in every generator textbook. It computes identical values and
is a contract change:

- `%position` would then have to describe how full the buffer is, not just which word is next;
- a stream saved through `%position` and restored could not be exact, because the buffer is not
  derivable from a word index;
- `%rewind` and `%jump` would each have to invalidate or rebuild it explicitly, and forgetting either
  returns **stale values that are still in range and still uniform**.

The keyed form has none of those properties: the held block either matches the wanted block or is
replaced, so it is derivable state that no part of the contract can see.

**Test:** `test_stream_position` (`test/test_random.f90`) asserts the round trip — `%position` saved, four draws taken, `%rewind` to the saved value, the same
four values returned — and `test_stream_values` asserts that a reseed drops the held block, which is
the one place the cache *must* be invalidated (`blk` indexes the previous family's words). Mutation
evidence for the shape: making the cache always miss **survives the whole suite**, which is the
correct result and is the proof that the cache cannot change an answer; making it never refresh after
the first block is caught.

**What it still forbids:** replacing the block-index key with a fill/drain counter, however much
faster it looks. The measured worth of the cache is 1.70x (gfortran) / 1.78x (ifx) on `real64` and
2.88x / 3.35x on `real32`; a queue would not beat that by enough to buy a contract change, and
`pf_random_fill_draws` already exists for callers who want the last 1.7x.

### Risk-108 — An integer draw taken off a block boundary re-reads words already handed out

The integer rule addresses a **block**, not a word pair: `int_at_impl` reads words 0 and 1 of block
`draw-1`. A stream's `%int_range` therefore aligns to the next block boundary before taking one. Drop
that alignment — it is three lines, and looks like padding — and an `%int_range` called at word 2
re-reads words 0 and 1, which an earlier `%uniform` has already handed out to the caller.

**The failure is silent and it is not merely a repeat.** The two draws are different *functions* of
the same bits (one is a scaled top-53, the other Lemire's reduction), so nothing looks duplicated;
the integers stay exactly uniform, and no containment or distribution test can see it. What is lost
is independence between two values a caller has every reason to treat as independent — the same class
of defect `int_at_impl`'s own doc-comment records for `pf_random_int_at` against `pf_random_at` at
draw 1.

**Test:** `test_stream_position` (`test/test_random.f90`) calls `%uniform32` (leaving position 2) then
`%int_range`, and asserts both the resulting position (9, i.e. aligned then four words) and that the
value equals `pf_random_int_at` at draw 2. Removing the alignment is caught.

**What it still forbids:** "simplifying" `align_to_block` away, and adding any new block-addressed
producer that does not call it. The same applies to the integer `%fill` specifics, which align once
for the whole array.

### Risk-92 — The last radix pass leaves the row array STALE, and only the string exclusion makes that safe

`sort_radix_permutation` (`src/parquet_sorting_engine.f90`) determines before its pass loop which
byte position is the last one that will execute, and has that pass scatter row indices **straight
into `perm`** rather than into `rb` — removing that pass's key write and the whole final copy, 24
bytes per element. The pass does not carry the images or the rows forward, so **after it, `ka` and
`ra` hold the order from BEFORE the last pass.**

That is sound only because nothing reads them again — and there is exactly one thing that would:
`sort_radix_refine_strings`, which finishes the runs a string key's 8-byte image could not separate
and which reads both `ka` and `ra` after the loop. A string key is therefore excluded from the direct
write (`last_p` stays `-1`), and the final copy runs for it as before.

**The rule this forbids.** *Anything new that reads `ka` or `ra` after the pass loop must either
exclude itself from the direct write the same way, or be written to read `perm` instead.* The failure
is quiet in the worst way: the stale arrays are a valid permutation of the right rows in nearly the
right order, so a refine driven from them produces a plausible, subtly wrong answer rather than an
abort or an obvious scramble.

Two further states reach the same final copy and must keep doing so: a column every digit of which is
constant executes **no pass at all**, and `nv <= 1`. Both leave `last_p` at `-1`.

**Test.** Covered, and each half by a different test. Removing the string exclusion fails
`test_radix_path_string_shapes`; running the final copy unconditionally fails three tests including
the whole-family A/B; never running it fails four. Taking `last_p` as the LOWEST non-constant digit
rather than the highest fails four, including the pass counter. All four were confirmed by mutation
(`feature_sort_improvements.md` §18.6). Note that removing the `exit` after the direct write survives
and is *correct* to survive — every later pass would `cycle` anyway, since `last_p` is by construction
the highest executing one.

### Risk-82 — A non-nullable field that receives a Null writes definition levels that disagree with its schema

`build_field` (`src/parquet_wrapper.cpp`) now applies its `nullable` argument to a **vector** column's
child `item` field, where it was previously ignored — Arrow's `fixed_size_list(value_type, size)`
convenience constructor hard-codes a nullable child, so until this change every vector column was
written with a nullable element field whatever the caller passed. The rule that decides the flag is
`resolve_chunk_nullability` for a streamed column (nullable iff the first row group passed an
`is_valid` mask) and `has_any_null` for a whole-column write.

**The invariant the whole scheme rests on is one sentence: a field declared non-nullable must never
receive an array containing nulls.** It holds by construction today — an absent mask reaches the
builder as a null `valid_bytes`, which cannot produce a null, and the kinds whose nulls come from
somewhere other than a mask (`date`/`time`/`timestamp`, and a `parquet_string_column`) are excluded
from the presence rule for exactly that reason.

**Why the failure is silent.** Break it and Parquet writes definition levels describing Nulls into a
schema that says the column has none. Nothing aborts on the write path: this library's own reader
answers from the data's own null count rather than the schema flag, so the file round-trips through
it perfectly. A *different* Arrow-based reader, trusting the schema, may skip reading definition
levels for a required field and return the wrong values — or Arrow's own `Table::Validate()` may
reject the write at close time with "Column data for field N ... is inconsistent with schema", which
is an abort thousands of rows away from the call that caused it. Which of the two you get depends on
where the mismatch enters, and neither names the write that introduced it.

**Test.** `test_vector_element_nullability_follows_mask` and
`test_streamed_nullability_follows_first_mask` (`test/test_writing.f90`) pin the flag in both
directions through `parquet_get_column_nullable`, and the vector one additionally round-trips a Null
through the masked column — so the two halves fail differently: a wrong flag fails the assertion, a
mismatched array/field pair fails by aborting inside Arrow.
`test_protected_column_is_non_nullable` covers the protected case, including an unprotected
timestamp control. The two consistency aborts have their own scenarios
(`chunk_mask_dropped_after_first_row_group`, `chunk_mask_added_after_first_row_group`).

**What this still forbids.**

- **Every `build_field` call site must pass a `nullable` argument that reflects the array it is
  paired with.** Omitting it now means `false`, and omitting it is exactly what
  `parquet_append_string_array_column` did — harmlessly, while the argument was ignored for vector
  columns, and as a live invariant break the moment it stopped being. Audit the call sites, not just
  the rule.
- **`align_array_to_field` must keep being applied wherever a field and an array are stored
  together.** A `FixedSizeListBuilder` stamps its own type on the array it finishes, with a nullable
  child, and Arrow compares that type against the schema field. The helper restamps it; without it
  a correct flag still aborts the write.
- **A new column kind whose nulls do not come from an `is_valid` mask must be added to the
  always-nullable set**, or its first null-free row group will declare a field that a later row
  group cannot fill. `date` was nearly missed here precisely because it is int32-backed and reaches
  the generic template rather than the temporal one.

### Risk-81 — Two individually valid ranges that describe different parts of the file

`parquet_reader_set_filter`'s six-argument form takes a row-group range **and** a physical row
range, and they are not independent: the rows must lie inside the rows those row groups span. Each
was validated on its own — the row groups against the file's row-group count, the rows against its
row count — and a pair that passed both could still be disjoint, at which point the reader returned
their intersection.

**Why the failure is silent.** The intersection of a valid row range with a valid row-group range is
a perfectly ordinary mask, frequently **empty** — and an empty result is exactly what a selective
filter that matched nothing produces. There is no wrong value to find, no abort, no warning: the
caller reads a row count of 0 (or a truncated one) and concludes the data did not match. The two
ranges being separately valid is what makes it hard to see, because every error message the
procedure could previously emit is about a range that is out of range *by itself*.

**Test.** `filter_row_range_outside_row_groups` (`test/error_scenarios.f90`, wrapped by
`test_filter_row_range_outside_row_groups_aborts` in `test/test_errors.f90`) writes a 12-row file at
two rows per row group, so row groups 2..3 span rows 3..6, and asserts that
`parquet_reader_set_filter(reader, filt, 2, 3, 5, 8)` aborts with a message naming both ranges and
the span. Its **negative control comes first, in the same scenario**: row groups 2..4 span rows 3..8
and the same row range 5..8 must be accepted, so a guard that fired unconditionally fails rather
than passes.

**What this still forbids.**

- **A new range argument on this call needs a cross-check against the ranges already there, not
  just its own bounds check.** That is the whole shape of this defect: three individually correct
  validations that never compared their subjects to each other.
- **Keep the message naming the row span the row groups cover.** "Out of range" is what the two
  older checks say, and this call passes both of them — an error that does not distinguish itself
  from those sends the reader to look at the wrong argument.
- **Do not move the check above the `row_group_lo <= 0` resolution.** "All row groups" is resolved
  to `1..num_row_groups` first, so that form spans the whole file and can never fail containment;
  checking earlier would reject it against an unresolved range.

### Risk-80 — A metadata-only query quietly decodes a whole column, and the answer is still right

`parquet_get_col_size` and `parquet_get_column_total_elements` are published as cheap: ask a column
its shape without reading it. That holds for a scalar column and for a `FIXED_SIZE_LIST`, whose
width is a schema constant. It cannot hold for a plain variable-length `LIST`/`LARGE_LIST` — this
library never writes one, but another producer does — because there the width is a property of the
**data**. So those columns must read something, and the only question is *how much*: a footer screen
plus a one-row-group-at-a-time proof (`list_width_verified`), or `get_single_chunk_array`, which
decodes every row group at once.

**Why the failure is silent.** Both give the *same answer* — `get_col_size` returns 0 for an empty
column, 1 for a ragged one, else the uniform width, which is `list_width_verified`'s contract
exactly. Nothing aborts, no value is wrong, no test fails. The only symptom is peak memory, on
exactly the file shape nobody here writes and therefore nobody profiles, and RSS cannot see it
either (Risk-1's rule: use the Arrow pool counter). `parquet_get_column_total_elements` shipped this
way while its sibling had already been moved to the scoped helper — an asymmetry that survived
because looking at either function alone shows nothing wrong.

**Test.** `plain_list_size_queries_avoid_whole_column_read`
(`test/error_scenarios.f90`, wrapped by `test_plain_list_size_queries_avoid_whole_column_read` in
`test/test_reading.f90`) arms `parquet_debug_set_force_whole_column_read_error` and then makes all
four calls against the `"strings"` list fixture (a uniform-width plain `LIST<utf8>`/`LARGE_LIST<utf8>`,
3 rows x 2 elements). Completing is the assertion. Its negative control,
`whole_column_read_forced_error_control`, proves the hook fires at all.

**What this still forbids.**

- **Do not add a third "cheap" column query that reaches for `get_single_chunk_array`.** If it can
  be answered from the footer, answer from the footer; if it genuinely needs data, use the
  row-group-scoped helper and add it to the scenario above. `parquet_get_string_length` is the
  deliberate exception and must stay outside this rule — the longest string cannot be known without
  reading every value, which is why it is documented as reading the column rather than pretending
  otherwise.
- **Do not "simplify" `list_width_verified`'s masked branch away.** With a filter or a sort active
  it deliberately *does* read the whole column, because a per-row-group width would answer about
  rows the caller removed. That branch is correctness, not an oversight, and it is why the two
  callers share the helper rather than each carrying their own scoping.
- **The existing `col_size_and_row_mode_avoid_whole_column_read` scenario cannot cover any of
  this**, and reading its name suggests otherwise. It writes its fixture with this library's own
  writer, so every column in it is a `FIXED_SIZE_LIST` and the plain-`LIST` branch is never entered.
  A guarantee about list columns needs a foreign-written fixture.

A few of these carry a suggestion of their own — Risk-18 wants a benchmark case, because the property
in question (iterate the set bits, not all 64 positions of a word) is *correct* either way and
differs only in cost, which no unit test can see. That does not move the entry into section 2: the
correctness is covered, and the suggestion improves how it is covered rather than filling a gap in
whether it is.

### Risk-64 — Two threads pasting adjacent row groups share a validity bitmap block and lose a null

`materialize_column_parallel` (`src/parquet_tables_read.f90`) fills one column by giving each row
group to a thread. The **rows** are disjoint by construction, and it is tempting to stop reasoning
there. The **validity bits are not**: `parquet_column` packs them `parquet_validity_block_bits` to a
block and `%paste` updates a block with a read-modify-write, so the thread finishing row group `g`
and the thread starting `g+1` read, modify and write the *same* block whenever the boundary between
them falls inside it — which is almost always, since a row-group size is chosen for I/O and has no
reason to be a multiple of 64. One update is lost. The column still validates, the row count is
right, and some row's null flag is simply wrong.

**This shipped and was found by luck.** It survived the suite until two full runs out of a few dozen
happened to fail `nulls land in the right rows when one column's read is split`; on the development
machine it never reproduced at all, not in 40 runs of the test alone and not in 25 runs of the suite
under deliberate CPU contention. The fixture's own numbers show how ordinary the condition is: eight
row groups of 25,000 rows, and **all seven** interior boundaries share a block.

The fix keeps the parallelism rather than serialising the paste — which would also be correct, and
was measured at roughly **5x slower** on a null-carrying column, because the validity write is about
two thirds of that operation. `bitmap_whole_block_rows` trims each row group to the sub-range
occupying whole blocks; that middle is pasted freely and only the ragged ends, under one block each,
go through a shared `critical`. The measured cost of the fix is nil.

**Test.** `no two row groups' pastes share a validity block` (`test/test_table_parallel.f90`), through
`parquet_debug_colread_block_rows`. **The shape is the whole point**, and it is the same division of
labour [Risk-61](#risk-61--a-validity-split-that-is-not-byte-aligned-loses-nulls-and-no-end-to-end-test-can-be-relied-on-to-see-it)
records for `parquet_string_column`: the end-to-end test that *found* this cannot be trusted to catch
a regression, because a lost update may simply not happen, so the alignment arithmetic is asserted
directly instead. Three mutations are caught deterministically — no trimming at all (the original
bug), trimming only the start, and a period that ignores the column's width.

`paste_row_group_safely`'s **third** arm — a row group too short to contain any whole block, which
is therefore serialised entire — is reached by `a row group too short to hold a whole validity block
is pasted serially` (same file), whose fixture is 40-row row groups against a 64-bit block. It is an
end-to-end test and so carries the caveat above: what it can assert is the arm's paste offsets, not
the absence of a lost update. It is worth having anyway because **the arm is unreachable at any
realistic row-group size** — the 25,000-row fixture above never enters it — so without a fixture
built for it the arm ships untested, and a mutation to it is invisible to every other test here.

**What this forbids.**

- **Do not reason from "the rows are disjoint" to "the writes are disjoint"** anywhere a bit-packed
  structure is written by more than one thread. Rows, elements and bits are three different
  granularities and only the last one is what a read-modify-write actually touches.
- **Do not copy `parquet_validity_block_bits` into another module.** It is published by
  `parquet_columns` precisely so this caller need not restate it; a second copy could drift with
  nothing to report it, and the failure is silent.
- **Any new code that fills one `parquet_column` from several threads inherits this**, including a
  future parallel `materialize_slice` — which pastes row-group pieces the same way and is serial
  today only because it was left that way. It must go through `paste_row_group_safely` or repeat its
  reasoning.

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
- **And it had already drifted.** `doc/pages/io/reading.md` documented a `prefetc` column the code
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

**The generation counter is conservative, with exactly TWO computed exceptions.** Every column- and
row-structural entry point bumps it whether or not that particular call relocated anything, so a
change means "re-fetch", not "definitely invalidated". A missing bump gives false confidence; an
unnecessary one costs a re-fetch. The exceptions are the calls that can be *proved* to have moved
nothing, and each is one predicate in one place:

- **`parquet_write_table`** advances the counter when it released at least one column, and never
  when it released none. The one bump that does not look structural.
- **`%add_column` under a NEW name, within spare column capacity** (`table_new_slot`'s `grew`
  flag). This is `%reserve_columns`' published guarantee — nothing relocates, no slot is renumbered,
  no row moves, so every outstanding pointer and handle is exactly as correct as it was. The
  `force=.true.` replace branch still bumps unconditionally, because it *clears* a column's values.
  See Risk-79 for what protects it.

**Test.** Covered. The dangling read itself is undefined behaviour and cannot be asserted on — a
test that dereferences a freed pointer may pass, crash, or return plausible garbage, and none of the
three means anything. What *is* mechanically testable is the **generation counter**, which is the
only signal a caller has, and both directions of its contract are now swept:

- `every structural entry point advances the generation counter` (`test/test_table.f90`) loops over
  all fifteen structural entry points — `%add_column` (**filled to capacity first, so the add under
  test genuinely grows the slot array**), `%drop_column`, `%rename_column`, `%copy_column`, `%cast`,
  `%evict_column`, `%reload`, `%filter_rows`, `%sort_by`, `%delete_rows`, `%truncate`, `%append`,
  `%append_null_rows`, `parquet_write_table(release=.true.)` and `%reserve_columns` past its current
  capacity — asserting the counter strictly increased, and names the operation in its failure
  message. Adding a mutation means adding a `case`, not a test.
- The **no-op half** is in the same test: seven calls that relocate nothing must leave the counter
  alone, or it starts reporting noise and callers learn to ignore it. Six change no row; the seventh
  is `%add_column` **within reserved capacity**, which is the only mechanical statement of
  `%reserve_columns`' guarantee the counter can make.

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

`parquet_table` is designed to be extended — a [generated table type](doc/pages/utilities/generated-tables.md) does
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

`materialize_marked_parallel` and `materialize_column_parallel` (`src/parquet_tables_read.f90`) hold
the `!$omp parallel` regions that declare anything at all inside their lexical scope. What may be
declared there is constrained from two directions at once, and the two constraints contradict each
other:

- **gfortran breaks `private()`**: it does not reliably default-initialize a private copy of a
  finalizable derived type, so the first finalization frees an undefined pointer. CLAUDE.md's
  documented workaround is to declare the variable in a `block` inside the loop body instead.
- **ifx breaks that very workaround, and is perfectly happy with `private()`.** A derived type
  **with allocatable components** declared in a `block` lexically nested in the region makes ifx emit
  privatization scaffolding for it (`<TYPE>.omp.mold_ctor` → `for_alloc_private` → `do_alloc_copy`
  → `copy_src_xdesc_to_dest_xdesc`) that segfaults on every thread entering the region — 100%
  reproducible with as few as 2 threads, independent of team size, so not a race.

**The allocatable components are the whole trigger; the type does NOT have to be finalizable.**
This entry (and CLAUDE.md's own note) originally said "a finalizable type with allocatable
components", and that phrasing is what let the shape back in: `materialize_column_parallel` declared
a block-local `type(parquet_column) :: chunk`, `parquet_column` has **no `FINAL`** at all, and it
crashed exactly as described — ifx 2026.1, `mold_ctor` → `for_alloc_private` → `do_alloc_copy`,
every thread, on five of the twenty-one `table_parallel` tests and so on every full `fpm test`. Read
the rule as: **no derived type with an allocatable component, finalizable or not.**

The two forbidden shapes are opposites, so only a third one is left, and it is what both regions
use: **a shared array of the type, indexed by thread number, allocated before the region** — the
`parquet_reader` array in both, plus the `parquet_column` chunk array in
`materialize_column_parallel` — so no such instance is constructed inside the parallel construct at
all. Passing an element of it on to an `optional, intent(inout)` dummy (how
`table_materialize`/`table_release_one` receive `rdr`) does not reintroduce the scaffolding either,
and neither does passing one as an ordinary `intent(inout)` argument (how
`table_materialize_chunk_kind` receives the chunk). Reusing one such slot across the iterations a
thread is handed is safe for the same reason the serial `materialize_slice` reuses one chunk across
its row groups: each call resizes it.

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

**What this still forbids.** Do not declare a `parquet_reader`, `parquet_writer`, `parquet_schema`,
`parquet_column`, `parquet_string_column` or **any other type carrying an allocatable component**
inside such a region's `block` — plain integers only, and the comment saying so must stay, in every
region, phrased as "allocatable components" rather than "finalizable" so the next reader cannot
conclude a non-finalizable type is exempt. A new parallel region needs its own per-thread array up
front, added at the same time as the region. Do not weaken the positive control into
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

**The check now covers all of `src/` as a RATCHET**, with a per-file count of the instances still to
be converted: a file gaining one fails, and a count left too high after a fix fails too, so the list
can only shrink and cannot go stale silently. **That widening revealed the S3 audit had undercounted
badly — 17 instances, not 4.** The audit's regex required the destination to be the call's last
argument, and the dominant real shape is `call store%get(i, s, allow_null=.true.)`, where it is not.
Four are now fixed (the two sort-key extractors, `stat_str`, `stat_strv`); thirteen remain and are
recorded in `KNOWN_REMAINING`. **The lesson is the general one: a hand-written audit regex is itself
untested, and the only reason this was caught is that the check was widened rather than trusted.**

**What the earlier, narrower scope was, and why it is worth remembering.** S3's audit swept all of `src/*.f90` and found the shape alive in four
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

### Risk-62 — A validity run copied byte-wise silently mis-places nulls when its alignment precondition is wrong

Risk-61's sibling, and easy to mistake for it: same corruption, no threads involved. `copy_validity_run`
and `fill_validity_valid` (`src/parquet_strings.f90`) move a contiguous run of validity bits **eight at a
time as whole bytes**, which is what removed `%slice`'s and `%append_column`'s entire null penalty
(0.0118 s to 0.0039 s, 0.0113 s to 0.0031 s on 4 M rows). The speed comes from a precondition, and if
that precondition is wrong the bytes land at the wrong bit offset: some rows come back null that were
not, `%validate()` still passes, `%size()` and the payload are untouched, and nothing aborts.

Three specific ways to get it wrong, all of which look like tidying:

- **The precondition is that BOTH sides start on a byte boundary — not that they share a bit phase.**
  Two runs at the same non-zero phase could be copied byte-wise after a ragged head, and that is a
  tempting generalisation. It must not be written: neither caller can produce it (`slice` always writes
  a destination from bit 0, `append_column` always reads a source from bit 0), so the head would be a
  branch **no test in this repository can reach**. The narrower condition costs a hypothetical future
  caller nothing but the per-bit fallback, which is already correct.
- **The null count comes from `popcnt` and must be masked to 8 bits.** `int8` is signed in Fortran, so
  `int(byte, int32)` sign-extends a set high bit into 24 further ones and the count is wrong by exactly
  that much — for a byte whose eighth row is valid, i.e. most of them.
- **The ragged tail is not optional.** Any row count that is not a multiple of 8 has one, so dropping it
  leaves the last few rows carrying whatever the destination's bitmap held before.

**Test.** `slice copies validity correctly from every bit phase` and `append_column copies validity
correctly onto every bit phase` (`test/test_parquet_string.f90`). **The shape is the point**: both sweep
the *alignment* — `first = 1..17` for the slice, a destination of 0..17 rows for the append — and assert
every element's null state and value against the original, element by element. A test that only ever
slices from row 1 or appends onto an empty column exercises the byte path alone and passes against all
three defects above. All four mutations (each bullet, plus removing the alignment guard entirely) are
caught by these two tests and by nothing else in the suite.

**What this forbids.** Do not widen the alignment precondition without a caller that needs it and a test
that reaches it; do not drop the mask; and do not replace the phase sweep in either test with a single
aligned case because "the loop is the same either way" — the fallback path is where two of the three
defects hide.

### Risk-65 — A guard claimed after the state it protects is a guard that loses the race

`ConcurrencyGuard` (`src/parquet_wrapper.cpp`) is the library's only defence against a caller sharing
one `parquet_reader`/`parquet_writer` across threads, and README's Thread safety section promises a
clean diagnostic abort when that happens. **The promise depends entirely on WHERE the claim is
taken**, and for a long time the writer took it in the wrong place.

The guard can only be claimed once a call reaches C++. On the read path that is effectively the door
— `parquet_read_column`'s second statement is `check_column_exists`, a guarded C++ call, and nothing
on the Fortran side mutates a reader. On the write path it was the **last** statement: everything
before it is Fortran mutating shared writer state, above all `parquet_check_and_mark_written_name`
(`src/parquet_write.f90`) growing `writer%written_names` with allocate/copy/`move_alloc` on every
single call. Two threads therefore raced on a Fortran allocatable — one freeing the array while the
others walked it with `trim()` — and corrupted the heap before either reached the guard.

The failure was quiet in the way this register exists to record: the process still aborted, so the
error scenario asserting an abort still passed most of the time. Measured on a 384-core machine
before the fix, **every** run of `concurrent_calls_into_shared_writer` segfaulted, the exit status
varied across 134/139/174 run to run, and the promised message reached stderr only sometimes —
occasionally accompanied by Arrow's own `cannot create default memory pool`, because the corruption
had reached the allocator. The crash never named the writer.

Four properties now keep it correct, and each is one edit from being lost:

- **The claim is taken at the FIRST statement of every write entry point**, right after
  `check_writer_open`, not at the first mutation and not at the C++ call. "First mutation" is not
  good enough: it is a moving target that a future edit silently invalidates by adding a mutation
  above it.
- **The release is a FINAL, not a paired call.** Those 38 entry points contain 64 early `RETURN`s;
  Fortran finalizes a nonpointer, nonallocatable local immediately before a `RETURN` or `END`
  (F2018 7.5.6.3), which is what makes the release automatic on every one of them. Verified on
  gfortran 15 and ifx 2026.1, including a return out of a loop. Converting `writer_lock` to explicit
  enter/leave calls reintroduces 64 chances to leak a claim.
- **`writer_lock` must stay free of allocatable components.** A finalizable type that has any is the
  shape ifx miscompiles when it is block-local inside an OpenMP parallel region (CLAUDE.md's
  "Compiler & language gotchas"), and these locks sit in exactly the procedures a misusing caller
  invokes from inside one.
- **The guard is owner-keyed, so a leaked claim does NOT fail loudly.** The owning thread may
  re-enter, so a missed release leaves everything single-threaded passing; only a later, legitimate
  hand-off to another thread aborts, in unrelated code, blaming a concurrency bug that never
  happened. This is why the test below is a hand-off rather than an assertion about a single thread.
  The same property is what allows the library's own nested C++ calls under a Fortran-held claim; the
  per-thread token behind it (`g_next_thread_token`) must stay a single process-wide instance, which
  is why CLAUDE.md's translation-unit-split section now names it alongside the `g_debug_*` family.

**Test.** Covered. `writer_guard_sequential_handoff` (`test/error_scenarios.f90`, wrapped by
`a sequential hand-off of one writer between threads succeeds` in `test/test_errors.f90`) hands one
writer from thread 0 to thread 1 across an `!$omp barrier` and requires **exit 0**. Thread 0 first
takes an early-return path (a schema column that is not `is_set`) while holding the claim, so the
early-return release is what the test actually exercises. Deleting the `parquet_writer_leave` call
from `writer_lock_release` makes it abort with the guard's own message — confirmed, and nothing else
in the suite notices.

**What this forbids.** Do not move the claim later "because the early part only reads"; do not
replace the FINAL with explicit calls; do not add an allocatable component to `writer_lock`; and do
not read a green `concurrent_calls_into_shared_*` run as evidence that this area is healthy — that
scenario passed throughout the entire period the bug existed. Its negative control is the test that
has teeth, exactly as Risk-6 records for the table layer's own guards.

### Risk-67 — An unbounded read of a `parquet_column` storage array returns uninitialised slack

`parquet_column` allocates its storage **geometrically** (`ensure_capacity`, 1.5x) so that appending
row by row is amortised O(1) rather than the O(n²) an exact-fit reallocation per append costs. The
consequence is that `size(self%i32)` is no longer `self%nrows` — it is `self%cap`, which is `>= nrows`
and usually strictly greater.

**Every read of a storage array must therefore be bounded by `1:nrows`.** They all are today
(`data_ptr_i32` is `p => self%i32(1:self%nrows)`, `set_all_i32` is `self%i32(1:self%nrows) = values`,
`copy_storage` slices both sides), and the bound was audited across all four generated
`parquet_columns_*.f90` files plus the hand-written ones when the capacity was introduced. But before
that change the bound was *decorative*: `size(...)` and `nrows` were the same number, so a procedure
that forgot it worked anyway. Any such procedure — an old one nobody re-checked, or a new one written
from the pre-capacity habit — now reads whatever the allocator happened to leave in the tail.

**The failure is quiet in the worst way.** There is no abort, no shape mismatch and no bounds
violation, because the memory is genuinely allocated and genuinely part of the array. A caller gets
extra rows that were never appended, holding arbitrary values, and a `size(p)` taken from the pointer
reports the capacity rather than the row count — so even a length check agrees with itself. Only the
values are wrong, and only sometimes: a column whose capacity happens to equal its row count (one
read from a file, or one just rebuilt by `%filter_rows`/`%sort_by`/`%gather`) behaves perfectly,
which is most columns in most tests.

Two related invariants hold the same design up, and breaking either is equally quiet:

- **`cap >= nrows` always.** `clear` resets it to 0, `move_from` moves it with the arrays, `adopt`
  takes it from the adopted allocation, `deep_copy` allocates exact-fit. A path that sets `nrows`
  without setting `cap` leaves the column claiming room it does not have, and the next write past
  the old end corrupts the heap.
- **`ensure_bitmap` sizes the validity bitmap from `cap`, not from `nrows`.** Sizing it from the row
  count reallocates the bitmap on every single append while the storage reallocates only
  geometrically — silently restoring the O(n²) behaviour on any column that carries a null, with
  every value and every null bit still correct.

**Test.** `spare capacity is invisible to every reader` and `capacity >= length holds through every
operation` (`test/test_columns.f90`) cover the first two; `a null-carrying column stays geometric
too` covers the bitmap, and it is the one whose shape matters. Asserting the null bits still read
back correctly is *not* enough — that passes against a bitmap sized from `nrows`. It asserts
`validity_bytes()*8 >= capacity()*colwidth()` instead, and the fixture is chosen so 400 rows sit at
a capacity of 474, which fall in different 64-bit blocks; without that the mutation survives the
whole suite, as it did on the first attempt.

**What this forbids.** Do not add a storage read without an explicit `1:nrows` bound, however
obviously "the whole array" is meant. Do not use `size(self%<comp>)` as a row count — `self%nrows` is
the row count and `%capacity()` is the allocation. And do not "simplify" `ensure_bitmap` back to
`bits_needed(self)`: it looks like the tighter, more careful expression, and it is the bug.

### Risk-68 — A qc bound is parsed into `float64`, so a bound past 2^53 is silently rounded

**What breaks.** `parquet_qc_numeric_bound` (`src/parquet_metadata_maml.f90`) reads a `qc: min:`/
`max:` bound out of the MAML text with `read(raw, *, iostat=ios) value` into a `real(real64)`. Every
consumer of a numeric bound therefore sees a `float64`, whatever the column's declared type. Past
2^53 that is lossy: `max: 9007199254740993` becomes `9007199254740992` with no diagnostic, because
nothing in the text distinguishes "I meant exactly this" from "I meant roughly this".

**Why it is quiet.** A qc violation is a WARNING, so the failure mode is not an abort but a *missing*
warning — the check reports compliance for data that violates its declaration. Nothing downstream can
notice: the file is written correctly, the values are correct, and the only evidence is a line that
was never printed.

**The values are no longer affected — only the bound.** `parquet_check_qc_numeric` used to widen
every element to `float64` before comparing, which rounded the *data* the same way; an `int64` column
holding 2^53 + 1 compared as though it held 2^53 and passed a `max: 2^53` bound it violates. That is
fixed: `qc_numeric_i64` converts the bound to `int64` once (`qc_bound_as_int64`) and compares
natively whenever the bound has an exact `int64` equivalent, falling back to the `float64` comparison
only for a fractional bound — which can reach an `int64` column only when the *schema's* type is a
float one, where comparing in `float64` is the correct reading.

**Test.** `qc: an int64 bound is judged in int64, not after widening to real64`
(`test/test_writing.f90`, over the `qc_int64_beyond_float64_precision` error scenario). The scenario
carries both halves in one file: an `over` column holding 2^53 + 1 against `max: 2^53`, which must
warn, and an `at` column holding 2^53 exactly against the same bound, which must not. The negative
control is what stops a checker that simply warns about every `int64` column from passing. Confirmed
by mutation: making `qc_bound_as_int64` always answer `.false.` — i.e. restoring the old widening —
fails the test.

**The four operators are now exercised, which is what makes that warning concrete.** Until
`qc: a strict min:/max: operator excludes the boundary value` (`test/test_writing.f90`, over the
`qc_int64_strict_operators` scenario), every scenario declared its bounds plainly — which parses to
the inclusive `>=`/`<=` defaults — so `qc_int64_satisfies`' two strict arms had never run. A strict
operator quietly behaving as its inclusive twin accepts exactly the boundary value the declaration
excludes, and warns about nothing. The scenario's `inside` column (one step in from each boundary) is
the negative control that separates that failure from a checker warning indiscriminately. The
fractional fallback described above has the same treatment, in
`qc: a fractional bound still checks int64 values, in real64`.

**What this forbids.** Do not "fix" the remaining half by rounding, clamping or ceiling the bound
into the value's kind: the operator makes that a different question for each of `>=`, `>`, `<=`, `<`,
and getting it wrong turns a silently-loose check into a silently-tight one, which is worse. The
honest options are to leave it (and document it, which
[quality-control.md](doc/pages/schema/quality-control.md) now does) or to carry the bound's original text
down to the comparison so an exact integer bound can be parsed as one — which would touch
`parquet_qc_numeric_bound`'s two schema-validation callers as well, and is a change to make
deliberately rather than in passing.


### Risk-70 — An `intent(inout)` temporal setter that skips a component leaves a REUSED element stale

**What breaks.** Every setter on `parquet_date`/`parquet_time`/`parquet_timestamp`
(`src/parquet_temporal.f90`) takes `class(...), intent(inout) :: self` rather than `intent(out)`.
That is deliberate and is worth real money — a *polymorphic* `intent(out)` dummy makes the compiler
default-initialise the element through the runtime on every elemental call, and a type-bound
procedure's passed-object dummy has to be polymorphic, so an elemental setter pays it per element.
Measured on machine A over a 4M-row whole-column read: the construction loop went **12.3 ms →
3.4 ms** (date, 3.6x) and **17.2 ms → 5.7 ms** (timestamp, 2.9x), taking the whole date-column read
from 23.6 ms to 14.8 ms.

The cost is an obligation moved from the compiler into the source. `intent(out)` reset **every**
component for free; under `intent(inout)` a component a setter does not assign keeps whatever the
element held **before**. So a setter that misses one silently returns a half-updated element to any
caller that reuses the variable — which the read path does for every row of every temporal column,
since it fills a caller-supplied array whose elements may already hold values.

**Why it is quiet.** Nothing fails to compile and nothing aborts. A fresh element is
default-initialised, so every test that sets a *new* variable passes; only a **reused** element
differs, and only in the component that was skipped. The read path is the worst case precisely
because it is invisible there: rows come back with plausible values, and a stale nanosecond part on
a timestamp is a real instant, just the wrong one.

**The converse is equally load-bearing.** A setter with a caught-failure path that `return`s without
assigning `self` — `%parse` with a `success` argument, and `ts_set_date_time`'s null propagation —
must **keep** `intent(out)`, because that is exactly what makes a failed or null-propagating call
yield a *null* element rather than a stale one. `date_parse`'s own doc-comment ("or null on caught
failure") is a promise made entirely by `intent(out)`. Four procedures are in this class today
(`date_parse`, `time_parse`, `ts_parse`, `ts_set_date_time`) and converting any of them for speed
would be a silent correctness regression, not an optimisation.

**Test.** Two halves, because neither covers the other.

*Dynamic:* `a setter leaves no component of a REUSED element stale` (`test/test_temporal.f90`) sets
an element to one state, sets it again to a different state, and requires the result to be
indistinguishable from the same call on a fresh element — with a second assertion pinning the
expected value, since `reused == fresh` alone passes when an assignment is deleted from both. The
sharp cases are "set a timestamp with a nanosecond part, then one without", scalar and whole-array.
Seven mutations dropping an individual component assignment are each caught.

*Static:* `check_temporal_setters_assign_all` (`tools/check_source_conventions.py`) requires every
`intent(inout)` setter to assign every component of its type, or to delegate to one that does, and
requires every `intent(out)` one to have a caught-failure return justifying it. **This is the half
that covers a component added in future**, which no existing test can. Both the component list and
the setter list are derived from `parquet_temporal.f90` itself, so a new component or a new setter is
covered with no edit to the check; it reports "gone blind" rather than passing if the declaration
shape moves out from under it. Verified to fire in all five directions, including the
component-added-later case (which fails on every setter of that type at once) and both blinding
cases.

**What this forbids.** Do not add a component to any of these three types without assigning it in
every `intent(inout)` setter — the static check will say so, and it is not a formality. Do not
convert one of the four `intent(out)` survivors for consistency or for speed. And do not "simplify"
a `%set_null` body back to an empty one: those bodies exist only because `intent(inout)` no longer
resets anything, and the three of them are the one place where the value components are **not**
observable from outside (all three raw accessors hard-code 0 for a null element, and every other
accessor aborts on null), so a mutation removing them survives the test suite by being a genuine
semantic no-op *today*. They are kept as defence for the day an accessor exposes them, and the test's
doc-comment says so rather than pretending the assertion covers them.

### Risk-71 — A stale handle reads the wrong column or row, and nothing says so

**What breaks.** `parquet_table_col` and `parquet_table_row` each capture a resolved position —
a slot index, a row index — and reuse it on every later access. Every mutation that renumbers or
reorders makes that captured position name something else. `%drop_column` shifts every slot above
the dropped one down by one, so a slot taken beforehand stays **in range** and names a different
column; `%sort_by`, `%filter_rows`, `%delete_rows`, `%truncate`, `%append` and `%append_null_rows`
move rows the same way; `%compact` and `%reserve` reallocate storage without changing the row set
at all.

**Why it is quiet.** The values that come back are individually valid — a real `real64` from a real
column at a real row. Nothing is out of bounds, nothing aborts, no assertion fails. Only the
*mapping* is wrong, and a program has no way to notice: the mass column now reads the flux column's
numbers, and both are plausible masses.

**The mitigation is the generation stamp.** Each handle records `cache%generation` when it is made
and compares on every access (`col_resolve`, `row_check_current`); `%is_valid()` asks without
aborting. The rule is deliberately **conservative** — `%append` does not move existing slots, so a
column handle could survive one, and it is refused anyway — because one total rule is checkable and
a list of exceptions is what the next mutation quietly falls outside.

**There is now exactly ONE exception, and it is computed rather than listed.** An `%add_column`
under a new name that fits in already-allocated capacity relocates no storage, renumbers no slot
and moves no row, so a handle taken beforehand is provably still correct and is *not* refused —
that is `%reserve_columns`' published guarantee (Risk-79). It survives this entry's own argument
because it is not an item on a list: it is `table_new_slot`'s `grew` flag, one predicate in one
place, which a future mutation cannot silently fall outside the way a hand-maintained list of
exempt operations could. **Do not add a second exception by analogy.** If a future operation looks
like it "obviously" moves nothing, it still bumps unless the same single predicate says otherwise.

Note the visible consequence, because a test asserting the old total rule will fail on it:
materialising `parquet_row_index` adds a slot, so within spare capacity it now leaves outstanding
handles valid where it used to invalidate them (`test_col_handle_file_backed` asserts the new
behaviour, and that the surviving handle still reads its own column).

**This entry exists because the comparison is the kind of line a cleanup deletes.** It is one
`integer(int64)` test sitting next to an `associated()` test, in a procedure whose name suggests it
is only about attachment, on a path someone will one day be profiling. "We already checked
`associated`, this is redundant" is wrong and produces exactly the silent failure above. The
`%ref` case is the worst of them: it hands back a **raw pointer** into storage a mutation may have
reallocated, so nothing downstream can catch it at all.

**Two corollaries that are not obvious from the code.** `%index()` on a row handle is `pure` and
therefore does **not** check — deliberately, because it answers about the handle rather than the
table. And a `%sort_by` whose permutation moves no row returns early without bumping the
generation, which is correct (nothing moved) but means a staleness *fixture* must actually reorder;
one written here silently failed to abort until its data was made unsorted.

**Test.** Six error scenarios, each with a negative control that uses the handle successfully first:
`col_handle_stale_after_mutation`, `col_handle_ref_after_mutation`, `col_handle_never_attached`,
`col_handle_row_out_of_range`, `row_handle_stale_after_sort`, and — for the case that corrupts a
*different* table — `row_handle_foreign_column`, where a column handle from another table is neither
stale nor detached but simply belongs somewhere else. Plus `test_col_handle_staleness` and
`test_row_handle_staleness` (`test/test_table.f90`), which assert all four `%is_valid()` states;
the third (`.false.` after a structural change) is the one that matters, since a predicate reporting
only "attached" would answer `.true.` there and send the caller into an abort.

**Mutation-verified, and one of them nearly escaped.** Deleting `col_resolve` from `col_ref_*` first
read as SURVIVED — because `fpm build` does not build test targets, so the scenario binary predated
the mutation. Check exit status, and rebuild with `fpm test`, before believing a handle mutation
survived anything.

### Risk-72 — The name form and the handle form of one accessor can silently disagree

**What breaks.** Every per-element operation on a table now has two or three spellings:
`t%get_element(name, i, v)`, `c%get(i, v)` on a column handle, and `r%get(c, v)` on a row handle.
Each kind's rules — which source kinds widen into the caller's variable, whether a write clears the
row's null, what a kind mismatch says — would have to be written once per spelling if the spellings
had their own bodies. Eighteen kinds times three spellings is fifty-four places for one rule to
live, and a fix applied to one is invisible in the others.

**Why it is quiet.** Both spellings compile, both return a number, and every existing test exercises
whichever one it was written against. A widening rule fixed in the name form and not the handle form
shows up as a handle that refuses a column the name form accepts — or worse, as one that widens
where the other does not, so two loops over the same data disagree in the last bits.

**The mitigation is that there is only one body.** `col_fetch_<tag>`/`col_store_<tag>`
(`src/parquet_tables_colaccess.f90`, generated) take the resolved pieces — cache, slot, kind, row —
rather than a handle, and **all three** spellings call them. The kind-mismatch message comes from
`cache_require_kind`, which `table_require_kind` also delegates to, so a handle and a name report a
mismatch in identical words; the pointer path's own message comes from `cache_require_ptr_kind`,
shared by `%col` and `%ref` for the same reason.

**The five-argument shape is not a style choice.** The obvious alternative — have `%get_element`
build a `parquet_table_col` and delegate through it — was built and measured at **+16.3%** on
`%get_element`, three times the 5% bar the feature was held to; the shape that ships measured
**+2.16%**, inside the cross-build noise floor. So "make the name form construct a handle" is a
refactor that has already been tried and rejected on evidence, not one waiting to be discovered.

**If a future change ever gives a spelling its own body again, this entry is what says why it must
not.** The generator once carried a `DELEGATING_KINDS` set precisely so the two shapes could
coexist while the cost was measured; it is deleted now, and the non-delegating branch with it,
because a switch between two implementations of one rule is the drift it was meant to detect.

**Test.** The equivalence sweeps in `test/test_table.f90` — `test_col_handle_get_matches_name_form`
(one kind of each scalar shape, including the widening case), `test_col_handle_allocating_kinds`
(numeric vector, temporal vector, string scalar, string vector, each read both ways) and
`test_row_handle_takes_column_handle` (all three spellings of one cell compared against each other
and against the stored value). Because there is one body, these are regression tests for the
*sharing* rather than for each kind — a second body reintroduced anywhere fails them the moment its
behaviour differs at all.

**A related trap the shared body does NOT cover, recorded here because it was met while building
it.** `parquet_column%set_elem` exists because writing one element of a vector row through
`%data_ptr` bypasses the column's own null bookkeeping — which is three different rules (a numeric
element's null is in the bitmap, a temporal element's null **is** the element, a string element's
lives in the string store). The temporal one is the quiet one: `%any_null()` answers from a cache
the column recomputes lazily, so a bypassing write leaves `%has_nulls` reporting "no nulls" for the
rest of the program. Any future per-element operation belongs in `parquet_column`, not in a caller
reaching around it. Covered by `test_col_handle_element_within_row`, whose `%has_nulls` assertion
is the one that catches it.

### Risk-73 — A `<KEY>.datatype` entry that disagrees with its value is worse than no entry

**What breaks.** A typed `schema%add_metadata` call now writes a second key-value entry naming the
value's type (`NSIDE` / `NSIDE.datatype` = `int32`), because a parquet key-value pair can only hold
text. A reader that trusts that token coerces with it — so a token that is *wrong* turns a value
that used to arrive as a harmless string into a wrong number, or into a `ValueError`, in a caller
nobody in this repository can see. Nothing on the Fortran side ever reads these entries
(`parquet_get_metadata` picks its parse from the declared type of `value`), so no test fails, no
warning prints and nothing aborts. The library is a **write-only** producer of a fact someone else
acts on, which is the whole reason this is a risk rather than a bug class the suite would catch.

Four ways a wrong token can arise, and what forbids each:

- **A mis-copied overload.** The twelve typed specifics in `src/parquet_metadata_base.f90` each
  name their own token as a literal; a copy-paste that leaves `add_metadata_int64` saying `int32`
  is invisible to every round-trip test, because the *value* still round-trips perfectly.
- **Two writers of one companion key.** A caller's own `X.datatype` entry alongside a typed `X`
  would put two same-named entries in one file. `resolve_metadata_datatype`
  (`src/parquet_write.f90`) makes the explicit one win and warns; the synthesized one is dropped.
- **The `copy_metadata=` path.** A source file carries both `NSIDE` and its companion as ordinary
  entries. When the output schema declares `NSIDE` itself, the carried `NSIDE` is dropped (the
  schema wins) and its companion must be dropped **with it**, or the output carries the schema's
  synthesized token *and* the stale carried one. `carried_companion_is_superseded`
  (`src/parquet_tables_write.f90`) does that — and it must ask whether the schema declared the key
  **before the carry loop started**, since the loop is adding to that same schema as it walks: an
  unbounded scan sees the `NSIDE` it carried one iteration earlier and drops a companion that
  should have been kept. That is not hypothetical; it is what the first implementation did.
- **The VOTable's boolean rewrite.** `T`/`F` is correct in the sidecar and **wrong** in the
  key-value entry, where this library's own `parquet_metadata_parse_logical` and every external
  reader expect `true`/`false`. This one is not silent — the existing round-trip tests fail — but
  it is one line's reach away, so it gets a named test rather than incidental cover.

**Test.** `test/test_metadata.f90`: `test_scalar_metadata_datatype_companions` and
`test_array_metadata_datatype_companions` assert the **token**, not merely the entry's presence
(asserting presence alone passes against every mis-copied overload at once), plus the absence of a
companion for a scalar string; `test_metadata_datatype_not_an_item` pins that the companion never
becomes a `%items` entry; `test_votable_declares_scalar_types` and
`test_boolean_metadata_value_is_not_rewritten` are the two halves of the boolean rule.
`test/test_table.f90`'s `test_copy_metadata_carries_datatype_once` writes the source as `int64` and
declares the output schema's own as `int32` **specifically so that counting is not enough** — a
test asserting only "exactly one" passes whichever of the two survives, which is the failure this
entry is about. `test/test_maml.f90`'s `test_maml_keyarray_key_gets_no_datatype` pins that a
MAML-declared value stays a string. The `metadata_datatype_key_collision` scenario and its
`metadata_datatype_no_collision_control` negative control cover the explicit-entry rule.

**What this still forbids.** Asserting a companion's *presence* without its *value*; adding a
thirteenth typed overload without adding its token to the two token tests; and "simplifying"
`carried_companion_is_superseded` back to an unbounded `schema_declares_key` scan.

### Risk-74 — Discarding a column the caller wrote into silently restores the file's values

**What breaks.** `%evict_column` and `%reload` both empty a column's storage and let the next touch
re-read it from the file. For a column the caller has written into — with `%set`, `%set_element`,
`%set_slice`, `%set_null`, or either handle's `%set` — that replaces the caller's values with the
file's own, and there is nothing to notice: every call succeeds, the table's shape, column list,
kinds, widths, units and row count are all unchanged, and the values that come back are valid,
plausible values from the file. The user guide's promise that eviction is "an error rather than
silent data loss" makes a reader *less* likely to check.

Both procedures now refuse such a column unless `force=.true.` is passed, keyed on
`parquet_table_column%user_populated` — a flag that was maintained correctly at every assignment
site from the day the table layer was written and **read nowhere at all** until this change. That
history is the reason this entry exists rather than being deleted as "works and is tested": a
predicate can be perfectly maintained and still be worth nothing, and nothing in a test suite
reports the difference.

**What the guard deliberately does NOT cover.** `%col(name, p)` and the column handle's `%ref(p)`
hand back a mutable pointer and mark nothing, because the library cannot tell a write through it
from a read. Marking there was considered and rejected: `%col` is overwhelmingly a *read* idiom, so
it would refuse eviction on precisely the largest columns and make `force=` the ordinary spelling —
a guard that teaches people to bypass it. The remedy is instead **opt-in**:
`%set_user_populated(name, .true.)` claims a column by hand. So the residual risk is not "this
cannot be protected" but "this is protected only if the caller remembers", and the documentation
must state the boundary as a boundary — *eviction will not silently discard values you `%set`* —
never as *eviction is safe*.

**Test.** `test/test_table.f90`: `test_user_populated_guard` is the negative control the abort
scenarios cannot supply — an unedited file-read column must still evict and still reload with **no
keyword at all**, which is what a guard written as `if (.true.)` breaks while passing every abort
test ever written for it; it also asserts that a forced eviction really does bring the file's values
back, which is what proves `force=` reached the guard rather than being accepted and ignored.
`test_user_populated_tracks_writes` walks the flag across a column's whole life (unclaimed when
read, claimed by a write, unclaimed after a forced reload) rather than asserting a single state.
`test_user_populated_pointer_gap` asserts the `%col` gap **is real** and then closes it with
`%set_user_populated`. `test_cast_leaves_user_populated_alone` covers both `%cast` paths.
`test_clone_keeps_user_populated` covers the `%clone` / `%clone_structure` split.
`test_user_populated_handle` cross-checks the name and handle forms (Risk-72's shape) and pins that
`%set_user_populated` does not bump `%generation()`. `test_reload` carries the double-reload control
for the flag-clearing half. The three abort scenarios are `table_evict_user_populated`,
`table_reload_user_populated` and `table_set_user_populated_not_resident`.

**What this still forbids.** Widening the guard to `%col`/`%ref` without re-reading the reasoning
above. Removing `%reload`'s `user_populated = .false.` line, which is invisible to everything except
`test_reload`'s second reload — a `%reload` that guards but forgets to clear passes every abort
scenario and every force= test. Giving `%cast` a guard of its own by symmetry, or letting it set the
flag again: a cast changes a column's *kind*, not whose values those are, and on the deferred path
nothing has been read yet. Moving `%clone_structure`'s reset into the shared
`clone_copy_descriptor`, which compiles, passes every eviction test, and silently stops `%clone`
carrying the flag. And documenting the protection as total.

### Risk-75 — The name index's linear-scan fallback is a safety net nothing exercised

**What breaks.** `cache_find` (`src/parquet_tables_query.f90`) bisects `cache%name_order`, and when
that search comes up empty it falls through to a **linear scan** of the slots. Its own comment says
what the scan is for: it "deliberately does NOT trust" the index, so a mutation that forgets to
maintain it costs a scan rather than returning whichever column the bisection happened to land on.
Delete the scan, or let the bisection answer without re-comparing the name at the slot it found, and
that class of mistake stops being a slowdown and becomes a **wrong column**, silently — on a table
whose shape, column list, kinds and row count are all still perfectly correct.

**Why it went untested, and why the coverage report said otherwise.** There is no route to the scan
through the public API: every column-set mutation maintains the index eagerly (`table_new_slot` and
`add_file_slot` through `cache_name_index_insert`; drop, rename, clone and reset through
`cache_name_index_rebuild`), so a *correct* library never reaches it. Direct instrumentation — a
temporary `write` on the scan's hit — measured it executing **zero times** across the whole test
suite and all error scenarios. Coverage nevertheless reported those two lines as hit on some runs
and not others, which is the mis-attributed-`return` artifact this project already documents for
Fortran gcov, and it is the part worth remembering: **a coverage report credited an unexercised
safety net, so the gap was invisible from the one instrument that was supposed to show it.**

**Test.** `column lookup falls back to a linear scan when the name index is gone`
(`test/test_table.f90`), through `parquet_debug_table_drop_name_index` — a public Fortran-side debug
hook, added for this and nothing else, for the reason
[Risk-6](#risk-6--the-concurrency-guards-must-keep-agreeing-and-one-of-them-protects-a-wrong-answer)
records for `parquet_debug_table_set_inflight`: the index lives on `parquet_table_cache`, whose
components are private to `parquet_tables`, so nothing outside can reach it. The fixture is
deliberately `test_lookup_name_index`'s own — a strict-prefix pair (`flux`/`flux_err`), three names
sharing their entire 7-byte packed sort key, and reverse-alphabetical insertion — because those are
exactly the inputs on which a scan and a bisection could disagree. Both paths must answer
identically, misses included.

Two mutations are caught deterministically: deleting the scan (every lookup then aborts with
`no column of this name`), and a hook that drops nothing (the `had_index` assertions fail).

**What this forbids.**

- **Do not delete the linear scan on the strength of a coverage report**, and do not "simplify"
  `cache_find` into trusting `name_order(mid)` without comparing the name at that slot. Both changes
  keep every current test green, because the index is currently always correct — the scan protects
  against the *future* mutation that breaks that, not against anything shipping today.
- **`had_index` must stay a required `intent(out)` argument.** Both lookup paths return the same
  answers, so a test that does not assert the before/after index state passes just as happily
  against a hook that dropped nothing. That argument *is* the negative control.
- **Do not let `cache_find` rebuild the index lazily** when it finds it missing. It takes the cache
  `intent(in)` precisely so concurrent readers need no atomics; the next *mutation* rebuilds, which
  is what makes a dropped index a slowdown rather than a permanent one, and the test asserts that
  recovery.


### Risk-77 — A masked write compacts the values and the validity mask separately

**What breaks.** `parquet_write_row_mask`/`parquet_write_chunk_row_mask` install a row-keep mask,
and every write worker's masked branch then does two independent things: it `pack`s the values down
to the kept rows, and — in a **second, separate `if (present(valid))` arm** — packs `is_valid` the
same way. Nothing ties the two together. A worker that compacts one and not the other, or that
expands the row mask into element positions differently for the two, writes a file whose Nulls sit
on rows that were never null.

There are roughly twenty such branches and they are written out one by one, not shared: five numeric
types x whole-column/chunked, three temporal types x whole-column/chunked, the padded string form
scalar/matrix x whole-column/chunked, and the compact (`parquet_string_column`) form, whose masked
branch rebuilds the column row by row rather than packing an array. The property has to hold
independently in each.

**Why it is quiet.** The row count still comes out right, every value is a value that really was in
the caller's buffer, and the file reads back without a complaint from anything. A Null that moved is
indistinguishable from a Null that was meant to be there — there is no checksum, no count, and no
abort. The vector case is quieter still: `parquet_mask_expand_block` widens each mask bit into a
whole `col_size`-element block, and for a scalar column that expansion is the identity, so a bug in
it drops *elements* rather than *rows* and is invisible until something masks a vector column.

**Test.** Four sweeps in `test/test_writing.f90`, sharing one fixture design:
`parquet_write_row_mask + is_valid compacts both, for every scalar type` and its
`parquet_write_chunk_row_mask` counterpart cover int32/int64/float32/float64/logical and the padded
string form plus the compact one; `parquet_write_row_mask compacts a masked
parquet_date/time/timestamp write` and `parquet_write_chunk_row_mask compacts a chunked
date/time/timestamp write` cover the three temporal types; and `parquet_write_row_mask expands to
whole rows of a vector column` (plus its chunked twin) covers the block expansion.

Confirmed by mutation, each caught by exactly its own test and nothing else: dropping
`vmask => valid_c` from `write_int64_flat` and from `write_int32_chunk_flat`; pointing
`write_time_chunk_flat` and `write_timestamp_flat` at the un-compacted array; and expanding along
dimension 2 instead of 1 in `parquet_mask_expand_block`, which keeps the element count right and
scrambles the order.

**What this forbids.**

- **A masked write with no `is_valid=` exercises only half of its branch.** The `present(valid)` arm
  is separate code; before these tests, several types had their masked branch covered and their
  validity compaction not. A new write specific, or a new element-type family, inherits both arms
  and needs both written.
- **The fixture's shape is the test, not the values.** Put a Null on a *dropped* row and another on
  a *surviving* one: a worker that compacted the values but left `valid` indexed against the
  pre-mask buffer then shifts the Null onto the wrong row instead of losing it, which is the failure
  a "does it still round-trip?" test cannot see. Sharing one mask and one `is_valid` across every
  column in the file is the other half — a type that gets it wrong disagrees with its five
  neighbours rather than being wrong on its own.
- **`parquet_mask_expand_block` must stay the single place a row mask becomes an element mask.** A
  worker that inlines its own expansion re-opens the vector case for itself alone, and a scalar-only
  test suite will not notice.
- **Do not test the block expansion with `col_size` values that make the two orders coincide.** A
  width of 1 is the identity and a uniform row makes a transposed expansion look correct; the tests
  use distinct per-element values so that row 3's pair cannot pass for row 1's.


### Risk-78 — A temporal column's null cache is invalidated by the writer, not by the reader

**What breaks.** A temporal `parquet_column` keeps its null state inside each element, so "does
this column hold a null?" is an O(n) scan. `nulls_cached`/`nulls_dirty` (`src/parquet_columns.f90`)
cache the answer, and the contract is one-sided: **every path that changes an element's null state
must set `nulls_dirty = .true.` itself.** Nothing checks that it did. A path that nulls elements
without marking the cache dirty leaves `%any_null` answering `.false.` for a column that demonstrably
holds nulls.

**Why it is quiet, and why it is worse than a wrong flag.** `any_null_view` — the read-only form
every bulk validity path uses — returns early on that answer, so `%row_validity`/`%element_validity`
hand back an **unallocated** mask. An unallocated allocatable passed to an `optional` dummy is an
*absent argument* (see [Risk-8](#risk-8--the-table-write-must-stay-the-same-calls-as-a-hand-written-write),
which relies on it), and that is exactly how a null-free column is meant to signal "no mask needed". So the writer emits the
column as non-nullable and every null is dropped from the file, with no abort, no warning, and a
row count that still matches. The column itself is not corrupted, which is what makes it hard to
find from the symptom: reading the file back shows plausible values where nulls should be.

**Confirmed instance.** `set_validity_elems`' six temporal arms
(`src/parquet_columns_validity.f90`) write `%dt(i)%set_null()` and friends **directly** rather than
through `%set_null(i, e)` — deliberately, because resolving the kind once instead of per element is
what makes the bulk form worth having — and so bypassed the one place that marks the cache dirty.
`set_validity_rows` was unaffected because its temporal arm still goes through `%set_null(i)`. Found
by the test below, on the first run, in code that had shipped its way through a full suite.

**Test.** `set_validity writes an element mask on every temporal kind` (`test/test_columns.f90`).
It asserts three things in order, and the third is the one that matters: that `%is_null` reproduces
the mask, that `%any_null` reports the column as holding a null, and that `%row_validity` then
returns an **allocated** mask naming the right row. Only the third describes the actual damage; the
first passes even with the bug, because the elements really were nulled.

**What this forbids.**

- **A bulk validity writer must invalidate the cache once, kind-agnostically, not per arm.** The
  fix is a single `if (is_temporal_kind(self%kind)) self%nulls_dirty = .true.` after the dispatch,
  placed so that a seventh temporal kind cannot be added without it. Six per-arm assignments would
  work today and would be one arm short the moment the kind list grows.
- **Resolving the kind once is a licence to skip the dispatch, not the bookkeeping.** Any future
  path that writes an element's null flag directly for speed inherits this obligation. The ones that
  already do are `parquet_column_set_null_row` and `parquet_column_set_null_elem`; grep
  `nulls_dirty` before adding a third.
- **Do not test a validity writer by reading back `%is_null`.** That is the assertion the bug
  passes. The cache and the elements disagree, so the test has to ask something that consults the
  cache — `%any_null`, or better a bulk mask, since that is what a writer actually calls.
- **`any_null_view` must stay read-only.** Refreshing the cache there would hide this class of bug
  rather than fix it, and it takes the column `intent(in)` precisely so that read-only consumers can
  use the bulk API at all — see its own doc-comment for the 10x measurement that motivated it.

### Risk-79 — The no-relocation guarantee rests on one conditional and nothing else

**What breaks.** `%reserve_columns` publishes a contract callers are invited to *rely on*: while
spare column capacity remains, `%add_column` under a new name relocates nothing and does not advance
`%generation()`, so a `%col` pointer and both handle types stay valid across it. The entire
enforcement is one flag in `table_new_slot` (`grew`) and one conditional bump. Nothing else in the
library holds that promise up.

A future change to the slot array's growth policy breaks it silently: exact-fit growth instead of
doubling, a rebuild that re-sorts slots, a shrink on `%drop_column`, or any new path that
reallocates `cache%cols`. **The library would still answer every query correctly and every existing
value test would still pass.** What changes is that callers who took the guarantee at its word are
now holding pointers into freed memory, with no diagnostic — the exact "undefined by the standard,
usually works" state the guarantee was introduced to remove, except that now the documentation says
it is safe.

**Why it is quiet.** A relocated `%col` pointer usually keeps working on gfortran, because
`move_alloc` preserves the payload address (see Risk-11); only the descriptor array moves. So the
symptom is not a crash but a latent, compiler- and allocator-dependent one, and it appears in *user*
code rather than in this repository's tests.

**What forbids it.** Three things, and the second is the one that actually fires:

- The bump is **one predicate in one place** — never a per-branch decision. That is what Risk-71's
  "one total rule is checkable" argument permits at all; a scattered version would not be.
- **The negative-control arm of the guarantee test.** `test_reserve_columns_guarantee`
  (`test/test_table.f90`) fills the table **to capacity before reserving**, so the reservation under
  test genuinely has to grow the array. Without that fill the test passes against a
  `%reserve_columns` that does nothing at all — which is not hypothetical: that mutation **survived**
  the first version of this test, because `parquet_new_table`'s eight slots of headroom already
  covered the adds. This is CLAUDE.md's "check WHICH code path the test actually reaches" trap, in
  its purest form.
- The **both-directions sweep** (Risk-11): a growing add must bump, an add within capacity must not.

**Test.** Covered, and mutation-verified in three directions. `test_reserve_columns_guarantee`
asserts the counter does not move AND writes through the pointer afterwards, reading the value back
through the table — a counter assertion alone would not prove the pointer still aliases anything.
`test_reserve_columns_limits` covers what the guarantee excludes (`force=.true.` still invalidates)
and what it carries (a reservation survives `%clone` and `%clone_structure`).
`test_reserve_columns_handles` covers both handle types, in both directions. Mutations checked:
making the bump unconditional (guarantee arm fails), never bumping (negative control and the sweep
both fail), and making `%reserve_columns` a no-op (guarantee arm fails — **only after** the
fill-to-capacity fix above; it survived before it).

### Risk-83 — A write path that does not resolve a declared `auto` size emits a sentinel into the sidecar

`col_size: auto`/`array_size: auto` are placeholders resolved before any data is written — explicitly
by `schema%set_col_size`/`%set_array_size`, or automatically by the first write that can supply the
value. Until then the column carries `parquet_size_auto`, which is **-1**. `parquet_close_writer`
writes the column's final `col_size`/`array_size` verbatim into a `write_maml=.true.` sidecar
(`parquet_rewrite_resolved_sizes`) and into the file's own `column.<name>.array_size` metadata entry,
so a column that was never resolved advertises `-1` in both.

**Why the failure is silent, and why it is worse than it looks.** Nothing aborts and nothing warns:
the `.parquet` file is valid and round-trips through this library perfectly, because the reader takes
each string's length from the data rather than from `array_size`. The damage is in the *sidecar*,
which is a `.maml` file that **fails `parquet_validate_maml`** — "field 'X' has an invalid array_size
(must be a positive integer or 'auto')" — so the failure surfaces in whatever program reads the
sidecar back, possibly much later and on another machine, naming a file rather than the write that
produced it. `parquet_close_writer`'s own "resolve any remaining auto to 1" loop does **not** cover
this: it lives in `parquet_write_empty_columns` and runs only when no column was written at all.

**The rule this forbids, and it is wider than the sentinel.** *Every size this library writes into a
sidecar or into the file's own metadata must describe the data that was written.* Two distinct ways
of breaking it live here, and only the first is about `auto` at all:

1. **A path that cannot resolve an `auto` size leaves the sentinel.** "This path has no declared
   length to read" is not an exemption — the confirmed instance was exactly that. The two
   `parquet_string_column` paths (`parquet_write_string_column_compact` and its chunk twin) store
   each element's own bytes and so have no `len(values)` to take; they were the only string write
   sites calling neither `parquet_resolve_or_check_col_size` nor
   `parquet_resolve_or_check_array_size`, and they shipped writing `-1`.
2. **A path that does not ENFORCE a declared size may still not repeat it.** The compact path
   deliberately does not enforce `array_size` — a reader takes each length from the data, so the
   declaration is not needed to read the column back — and a caller may legitimately write elements
   longer than the schema declares. That write is accepted, with one WARNING. What it must not
   produce is a sidecar still claiming the declaration. **Accepting inaccurate input is a policy
   choice; emitting inaccurate output is a defect**, and the two are easy to conflate because the
   same number is involved.

**The implementation is one reconciliation at close, not a per-write fix, and that placement is
load-bearing.** `resolve_compact_array_size` (`src/parquet_write_string.f90`) only *records* the
longest element each compact write carries, into `writer%observed_string_len`;
`parquet_reconcile_string_sizes` (`src/parquet_write.f90`) settles the reported value once, in
`parquet_close_writer`, before `close_parquet_writer` serializes the footer and before the sidecar is
rewritten from `writer%all_columns`. Raising the value *mid-write* instead would also raise the
ceiling `any_item_too_long` enforces on the padded (`character`-array) paths, so a column written
through both forms would have its declared limit quietly relaxed for the padded half — a second
silent failure introduced by the fix for the first. At close there are no writes left to affect.

An earlier version of this fix carried a `writer%array_size_from_data` marker so that only a
data-derived value would ever be raised, protecting a declared `array_size` from being widened. The
close-time reconciliation makes that marker unnecessary *and* makes the protection wrong: a
declaration the data has outgrown is precisely what must be corrected on the way out.

**Test.** `test_compact_write_array_size_auto_resolves` and
`test_compact_chunk_array_size_auto_grows` (`test/test_writing.f90`) both re-parse the sidecar with
`parquet_parse_maml` rather than only reading a number out of it — that is the assertion that it is a
*valid* MAML, and it is what fails (by aborting) if the sentinel comes back.
`test_flat_write_array_size_auto_resolves` covers the declared-length path the same way.
`test_compact_write_exceeds_declared_array_size` covers rule 2: the write is accepted and the sidecar
reports 20 against a declared 5.

Each carries its own control, and they are what stop the group passing against a cruder
implementation: a second column that must resolve to its *own* longest element (ruling out one width
applied to every `auto` column), a third chunk shorter than the second (ruling out "overwrite with
every chunk's own longest"), a second file written at a different declared length, and a column whose
declaration the data does *not* exceed and which must therefore come back unchanged (ruling out
overwriting every declaration with the measured width).

The warning is its own scenario, since a message can only be read from a captured run:
`compact_write_exceeds_array_size_warns` (`test/error_scenarios.f90`) asserts the write exits 0, that
the warning names the 20-character element, that a later 25-character chunk produces **no second
warning** (once per column, not once per chunk), and that the within-declaration column draws none at
all. Mutation-verified in both directions: warning per chunk → fails; warning removed → fails.

### Risk-84 — A MAML key matched case-sensitively loses a whole block, in silence

Every MAML key is case-insensitive: `parquet_find_maml_section` lowercases both sides, so
`parquet_validate_maml` accepts `Extra:`, `FIELDS:` and `Data_Type:` as the sections and sub-keys they
plainly are. The block **locators** did not follow that rule. `parquet_parse_col_map`,
`parquet_parse_protected_cols` (`src/parquet_metadata.f90`) and `locate_extra_block`
(`src/parquet_tables_maml.f90`) each compared `trim(adjustl(line))` against a lowercase literal, as
did the `fields:`/`keyarray:` scans in `parquet_parse_maml_lines`, `parquet_parse_qc_maml`,
`parquet_prune_disabled_fields` and `parquet_rewrite_resolved_sizes`.

**Why the failure is silent.** A MAML spelling its section `Extra:` passes validation — the section
name is checked case-insensitively — and then its `col_map:`, `protected_cols:`, `remap:`, `filter:`
and `sort:` are simply never found. No warning, no abort, and a file that looks entirely correct.
`protected_cols:` is the sharpest: the Null protection its author asked for is gone, so Nulls reach a
column declared free of them. `remap:` is next: a table's columns keep their physical names, so a
program looking one up by its internal name meets "column not found" from a MAML that names it.

**The rule this forbids.** *A section name and the keys nested inside it must be matched by the same
case rule, everywhere.* A validator that is case-insensitive over a locator that is not is worse than
both being strict — strictness would at least have rejected the file. Two predicates implement it,
one per subtree, because `parquet_core`'s helpers are private to its own submodule tree and
`parquet_tables_maml.f90` deliberately carries its own parsing primitives:
`parquet_maml_key_matches` and `maml_key_matches`. **Two copies of "ASCII case-insensitive equality"
cannot drift harmfully; a NEW site using neither is the real hazard**, which is why the guard is a
lint check rather than a shared symbol.

**Test.** `check_maml_keys_case_insensitive` (`tools/check_source_conventions.py`, run by CI's lint
stage) fails on any `== "<word>:"` literal in `src/*.f90` — verified to fire by reintroducing one.
`test_maml_block_headers_case_insensitive` (`test/test_maml.f90`) asserts a fully capitalized MAML
parses to the same fields and the same metadata entries as its lowercase twin, with a control that a
metadata *value* keeps its own capitalization. The `extra:` half needs an abort to be observable at
all — a block that was found is only visible through something it does — so
`extra_section_capitalized` (`test/error_scenarios.f90`) asserts that a capitalized `Extra:` with a
bogus `protected_cols:` name is still rejected, **paired with** `extra_section_lowercase_control`:
without the lowercase twin the capitalized test would pass against a library that had stopped reading
`extra:` altogether.

### Risk-85 — The in-code schema builder now owns both parsing and validating its own text

`schema%init`/`%add_field` used to write MAML *text* only, and `parquet_parse_maml` was the one
checkpoint that turned it into `%cinfo`/`%metadata` and validated it. That checkpoint is gone for an
in-code schema: `%init` parses its own header lines, and every `%add_field` parses its own field's
lines through `schema_sync_appended_lines` (`src/parquet_metadata.f90`) and validates that field
through `parquet_validate_field_rules`. Two properties now have to hold that nothing used to depend
on, and breaking either is silent.

**What breaks, first half — text without a sync.** Any future code that appends to `%maml%lines`
without calling `schema_sync_appended_lines` leaves `%cinfo` describing a schema that its own MAML
text no longer matches. The column exists in the text and not in the schema, so it is simply never
written; `parquet_close_writer`'s missing-write check iterates `%cinfo` and cannot see it either.
This is the exact bug the change removed (a `%add_field` after a parse used to do nothing, quietly),
so reintroducing it is a regression to a known failure. **`%add_col_qc`/`%set_col_qc` are a
deliberate, documented instance of the drift** and must stay one: a qc-maml declares `name` + `qc:`
and no `data_type:`, which `parquet_parse_maml_lines` rejects outright, so a qc entry cannot be
parsed by the schema parser at all — one field at a time or whole. Nothing reads `%cinfo` for a qc
schema (`parquet_open_reader` works from `%maml`, and `parquet_load_qc_maml_file` populates nothing
else), which is what makes it harmless there and only there.

**What breaks, second half — validation that stops being shared.** `parquet_validate_field_rules`
is called from two places: `parquet_validate_maml_internal`, once per field of a whole document, and
`schema_sync_appended_lines`, once per `%add_field`. Inlining it back into the document validator —
which reads as a tidy-up, since that is where it came from — silently stops validating every in-code
schema, because such a schema may now never be validated as a document at all. A `qc_min` that is
not convertible to the declared type, a `qc:` on a temporal column, a non-positive
`col_size`/`array_size` and `array_size: auto` on a non-string column would all be accepted and
carried into the written file.

**The rule this forbids.** *A schema's text, its parsed state and its validation are one object with
three faces; a change may not move one without the other two.* Concretely: a new appender to
`%maml%lines` calls `schema_sync_appended_lines`, and per-field validation stays in one procedure
reached by both routes. The whole-document rules (a missing `table:`, an empty `fields:`, an unknown
section or sub-key, `extra:`/`col_map:`/`protected_cols:`) are deliberately **not** run on the
incremental path, and that is sound only for as long as neither builder can emit them — teaching
`%add_field` to emit an `extra:` section is what would break it.

**Test.** `test_add_metadata_interleaved_with_add_field` (`test/test_metadata.f90`) builds a schema
with `%add_field` and `%add_metadata` interleaved, asserts every field and entry survives with no
explicit parse, and then runs a redundant `parquet_parse_maml` and asserts nothing was lost or
duplicated — **that call is load-bearing and must not be removed as redundant**, since without it a
parse that discards user metadata passes the test (confirmed: the mutation survived until the call
was restored). `test_schema_is_parsed_false_after_init_alone` (`test/test_maml.f90`) pins both sides
of the `%init`-alone/`%add_field` boundary. `test_in_code_sidecar_interleaved_metadata`
(`test/test_writing.f90`) covers `%metadata%source_maml_lines`, the third thing kept in step, by
writing a `write_maml=.true.` sidecar from the awkward field/metadata/field order and re-parsing it.
The validation half is `schema_add_field_validates_field_rules` (`test/error_scenarios.f90`), whose
negative control is a *valid* field accepted in the same process — without it the scenario would
pass just as happily against an `%add_field` that ran the whole-document validator and rejected
everything.

### Risk-86 — A defective quicksort still returns a correctly sorted answer

`sort_comparison_permutation` (`src/parquet_sorting_engine.f90`, `feature_sort.md` Stage 2) ends with
a **final insertion pass over the whole range**, exactly as `std::sort` does. Insertion sort is a
complete sorting algorithm, so whatever `sort_introsort_loop` leaves behind — however wrong — comes
out correctly ordered. **Every defect above that pass is therefore a performance defect and not a
wrong answer**, which is precisely what makes it invisible: the permutation is right, the conformance
tests against the C++ engine pass, and only a benchmark would ever notice.

This was measured rather than reasoned about. Of thirteen mutations applied to the engine, two
survived the entire suite, and one of them was **a sift-down with its comparison inverted** — i.e. a
completely non-functional heapsort, an entire algorithm arm, with nothing failing anywhere. The other
was a partition returning `cut + 1`, which survives because it is genuinely correct (it displaces one
element per partition slightly rightward, which the insertion pass repairs in O(1) amortised work).

The engine is currently reached only when `parquet_debug_use_fortran_sort_engine(.true.)` selects it;
**at the Stage 6 cutover it becomes the shipped sort for every `pf_argsort`/`pf_sort` call**, at which
point this stops being a property of scaffolding.

**The rule this forbids.** *Anything that changes where elements sit before the final insertion pass
must be mutation-tested against the presort invariant, never against the answer alone.* A correctness
test cannot grade this code. Concretely: a new pivot strategy, a different partition scheme, a
three-way partition, a threaded chunk sort (Stage 4) and any change to the heapsort all need the
invariant asserted, and an equality test against the C++ engine will not substitute for it. The
corollary for anyone tempted to simplify: **the final insertion pass is not merely an optimisation
for small ranges — it is the safety net that makes every other defect here quiet**, so removing it
would be a large behavioural change disguised as a cleanup.

**Test.** `test_fortran_engine_presort_invariant` (`test/test_sorting.f90`) arms
`parquet_debug_set_sort_track_shift` and asserts the largest distance the insertion pass moves any
element is at most `SORT_INSERTION_CUTOFF` — the invariant the quicksort exists to establish — and
that a forced heapsort fallback moves nothing at all, since it leaves its range fully ordered. It
also asserts the tracker recorded something nonzero, without which a tracker that never fired would
satisfy the bound trivially. It is **one-sided by construction and the code says so**: an insertion
pass only ever moves elements leftward, so a defect leaving an element too far *right* is invisible
to it — the one measured instance of that class was shown to be correct rather than merely
undetected. `test_fortran_engine_depth_limit_bites` is what proves the forced-fallback half is not
vacuous, via `parquet_debug_sort_heapsort_calls`, since both paths answer identically and no
assertion on a permutation can tell them apart.

### Risk-87 — The counting sort's range check cannot be written the way C++ writes it

`sort_counting_candidate` (`src/parquet_sorting_engine.f90`) decides whether a key's value range is
small enough to counting-sort. The C++ engine it was ported from bounds that range with
`(uint64_t)hi - (uint64_t)lo` (`sort_counting_candidate`, `src/parquet_wrapper.cpp`), which cannot
overflow whatever the two values are. **Fortran has no portable unsigned integer, and signed
overflow is undefined**, so that line cannot be repeated here. The Fortran version rearranges the
comparison instead, so that every intermediate stays in range:

- when `lo` is within `limit` of `huge(int64)`, so is `hi` (they satisfy `lo <= hi <= huge`), so the
  range is necessarily below `limit` and no arithmetic is performed at all;
- otherwise `lo + limit` cannot overflow, and `hi < lo + limit` is exactly the same test.

**What breaks.** Writing it back as the obvious `hi - lo < limit` is correct for every fixture anyone
would naturally build and wrong for a key holding values near both ends of int64: the subtraction
wraps to a negative number, the range test passes, and the placement pass then allocates a bucket
array from a meaningless count and indexes it with meaningless offsets. Confirmed by mutation — the
naive form **segfaults**, so at least it fails loudly once a fixture reaches it; what makes this a
risk rather than a bug is that no ordinary fixture does.

The reason it invites a rewrite is that the guarded form looks like defensive clutter next to a
one-line C++ original sitting a few files away, and the two are easy to "reconcile" in the wrong
direction. The code carries a comment saying so; this entry is what a reviewer should be pointed at.

**The rule this forbids.** *A ported arithmetic guard must be checked against the arithmetic of the
language it lands in, not against the source it came from.* The clause-for-clause discipline
`feature_sort.md` Stage 3 requires is about preserving the engine's decisions, not its arithmetic
idioms — and unsigned range arithmetic is exactly where those two part company.

**Test.** `test_counting_path_int64_extremes` (`test/test_sorting.f90`) drives both branches: a key
packed against `huge(int64)` whose range is 49 but whose `lo + limit` would overflow, which must be
ACCEPTED; and a key holding values near both ends at once, whose true range is about 2**64, which
must be DECLINED. Both run through `counting_ab`, which requires the counting and comparator paths to
agree with each other and with the C++ engine, and which asserts via the insertion-shift tracker that
the expected path was actually taken — without that last check the "declined" half would pass just as
happily against a counting path that accepted the key and answered correctly by luck.

### Risk-88 — The sort comparator silently loses a third of its speed if it outgrows an inlining budget

`sort_compare_key` and `sort_tier_of` (`src/parquet_sorting_engine.f90`) are called once per
comparison from the introsort's inner loops. GCC inlines them **only while they fit its default
budget**; past that it splits `sort_compare_key` into a `sort_compare_key.part.0` clone, inlines a
cheap prologue and leaves the body out of line — so the hot path, a single non-null key reaching the
value comparison, takes a **call on every comparison**, on the critical path of a dependent branch
chain.

Measured on machine A, gfortran 15.2, `--profile release`: with the split present, the serial
`f64` argsort ran at **1.28x** the C++ engine; without it, **0.91x**. Same algorithm, same
comparison count, same data — a **~39%** swing decided entirely by whether one procedure fit.

**What breaks, and why nothing notices.** Every answer stays identical, every test passes, and the
only symptom is speed. Worse, it is invisible in the obvious place to look: `sort_row_less` *is*
fully inlined into the sort's loops in both cases, so a check for "is the comparator inlined" that
greps for a call to `sort_row_less` reports success while the damage sits one level down. And the
comparator's own microbenchmark does **not** see it — `app/benchmark_sort_comparator.f90` measured
the Fortran comparator at 0.72–0.85x of C++ *with the split in place*, because a sweep's iterations
are independent and the call overlaps with them, where a quicksort partition's next iteration
depends on this comparison's branch.

The current shape was arrived at deliberately for this: `sort_tier_of` returns the **raw** tier and
`nulls_first` is applied once by `sort_compare_key`, as a negation of the tier comparison, rather
than by relabelling tiers on every call. That removed two nested three-way if-chains and is what
brought the chain back under the budget.

**The rule this forbids.** *Anything added to `sort_tier_of` or `sort_compare_key` must be paid for
by taking something else out.* Concretely: a new tier, a new key family arm inline (put it behind a
call, as `compare_bytes` already is), a validity scheme needing more than one test, or hoisting a
branch "for clarity" are all changes that can cross the threshold. They will look free.

**Test.** None — a static check, not a test, because the failure is a timing one and this project
does not put timing assertions in the suite. The check is one command against a release build:

```bash
nm <build>/.../src_parquet_sorting_engine.f90.o | grep -c 'sort_compare_key\.part'
```

It must read **0**. A nonzero count means the comparator no longer fits and the sort has lost
roughly a third of its speed. `tools/benchmark_sort_ab.sh` is what confirms the size of the loss
once the symbol is seen. Note the budget is a property of the compiler and its version, so a future
GCC may reintroduce the split without any source change — which is exactly why this is written down
rather than left to whoever next reads a disappointing benchmark.

### Risk-89 — The radix path is a third expression of the ordering, and a wrong answer there is silent

`sort_radix_image` and `sort_radix_permutation` (`src/parquet_sorting_engine.f90`,
`feature_sort_radix.md`) reproduce `sort_compare_key`'s ordering without performing a single
comparison. That makes them a **third** independent statement of what "sorted" means, beside
`sort_row_less` and `sort_keys_compare` — which is `feature_risks.md` **Risk-34** with one more
party, and worse than Risk-34 in one respect: the two comparators at least fail in the same
direction, whereas a radix defect answers a shape the comparator answers differently and nothing
compares the two unless a fixture happens to contain that shape.

Every rule the comparator applies has a counterpart here that looks nothing like it, so a change to
one does not visibly implicate the other:

| comparator rule | radix counterpart |
|---|---|
| tier: value / NaN / null, absolute | a three-block split of the output, walked in row order |
| `descending` reorders the value tier only | `not(t)` on the value image; block order untouched |
| `nulls_first` reverses tier order | which block base is which, computed before any pass |
| `-0.0 == +0.0` | both forced to one image, or the radix would order a pair the comparator calls equal |
| `compare_bytes` reads bytes unsigned, prefix < longer | a big-endian zero-padded 8-byte image, plus a refine pass |
| the row-index tiebreaker | LSD stability, which is not the same mechanism at all |

**Confirmed instance, and it is what this entry is really for.** The refine pass skipped any tied run
whose rows all fit the 8-byte window, on the stated grounds that such rows are byte-identical. They
are not — the image is zero-*padded*, so `"a"` (1 byte) and `"a"//char(0)` (2 bytes) share one image
while `compare_bytes` calls the shorter one less. A 4096-row column alternating those two values came
back **4096/4096 positions wrong**, with the whole column left in file order. Nothing aborted, the
suite stayed green, and the two string defences that existed both missed it for the same reason: the
one embedded NUL in `test_radix_path_string_shapes` sat at byte 17, *past* the window, where the
refine pass runs anyway, and `feature_sort_radix.md` §7.4's floor-forced-to-2 sweep — the strongest
evidence in that document — swept a fixture space containing no window-internal NUL at all. **A sweep
is only as exhaustive as its fixtures.**

**The rule this forbids.** *A change to any tier, `descending`, `nulls_first` or byte-comparison rule
must be applied to the radix path in the same change, and the fixture that distinguishes the old rule
from the new one must be added to the string/value shape tests.* And the sharper half, since it is
the one that actually bit: *an argument that two rows "must be identical" because they agree on a
LOSSY image is never sound* — the image is 8 bytes of a variable-length value, so agreement on it is
agreement on a projection, and the missing coordinate (here, length) has to be tested separately.

Two further consequences worth stating, because both are ways of re-entering the same hole.
`SORT_RADIX_MIN_ROWS` keeps this path off every ordinary fixture (`feature_risks.md` **Risk-49** is
the general form: a size threshold hiding a code path), so the tests that reach it are only the ones
written for it — which is why lowering the floor is real coverage work and not a tuning change. And
the radix path is currently reachable only through
`parquet_debug_use_fortran_sort_engine(.true.)`; at `feature_sort.md`'s Stage 6 cutover it becomes
the shipped answer for every single-key `pf_argsort`/`pf_sort` above the floor.

**A FOURTH party joined this since the entry was written.** `sort_radix_multi_permutation` orders a
multi-key sort by running one stable radix pass per key from the last key to the first, and it
expresses the tier / `descending` / `nulls_first` rules again in its own terms — per key rather than
per row, via `sort_radix_tier_rank`. Everything above applies to it unchanged, with one addition
specific to it: **the per-key flags must stay per key.** A pass that applied one key's `descending`
or `nulls_first` to another agrees with the comparator on every fixture where the two flags happen to
match, which is most of them; only a fixture sweeping the flags **independently per key** can see it,
and `test_radix_path_multi_key` is built that way for exactly this reason.

**Test.** `test_radix_path_string_shapes`, `test_radix_path_value_shapes`, `test_radix_path_runs`,
`test_radix_path_deep_strings`, `test_radix_path_multi_key` and `test_radix_path_alloc_fallback`
(`test/test_sorting.f90`). The first now carries the window-internal shapes that caught the instance
above — `""`/`char(0)` and `"a"`/`"a"//char(0)`/`"a"//char(0)//char(0)`, three distinct lengths under
one image — and reverting the length half of the refine test fails it, confirmed by mutation. The
second sweeps the value shapes a key transform can lose (signed zero, infinities, NaN, int64
extremes, all-null, one-valid) across all four `descending`/`nulls_first` combinations. The third is
the negative control for both: it asserts via the insertion-shift tracker that the radix path
actually ran above the floor and did not below it, without which every other radix test would pass
just as happily against a radix path that never executed.

### Risk-94 — A compiler may use an overflowing expression's undefinedness to delete a branch somewhere else

`parquet_random`'s integer draw needs the width `hi - lo + 1` as an **unsigned** 64-bit pattern.
Written that way it overflows for every width above `2**63`, and the wrapped pattern is exactly what
is wanted — which is why the design carried it as a documented, accepted wrapping site on the one
compiler with no 128-bit kind, on the strength of a Stage 0 measurement that ifx "wraps faithfully".

**It does wrap faithfully. That was never the question.** ifx 2026.1.1 computes the width correctly
and then uses the *undefinedness* of the same expression to reason about the result: `hi >= lo` holds
by construction, so absent overflow the width is positive, so `umod_2p64`'s `if (s < 0)` test — the
branch that handles every width at or above `2**63` — is provably dead and is deleted. A wide-range
draw then takes the narrow-width path, computes a wrong rejection threshold, and returns a value that
is still inside `[lo, hi]` and still looks random. Reproduced on machine B at `-O2` on the default
profile; the wrong threshold was `-6148914691236517206` where `6148914691236517205` was required, and
the affected draw retried three times before returning a plausible wrong answer.

**The rule this forbids, and it is the general one:** *never conclude that an overflowing expression
is safe because the compiler was measured to wrap it.* Wrapping is a property of one expression's
result; undefined behaviour licenses inferences **anywhere the value flows**, arbitrarily far from
the arithmetic, and a branch on the result's sign is the most inviting target there is. A wrapping
measurement is evidence about the multiply; it is not evidence about the `if` two functions away.

Fixed at the root rather than at the branch: `width_of` and `offset_by` (`src/parquet_random.f90`)
now compute on 32-bit halves, so no signed overflow occurs and no inference is available. The values
are unchanged — the golden vectors did not move — so this was a defect in how the pattern was
*obtained*, never in what it should be.

**Test.** Covered, and it is what found this: `test_agreement_int` (`test/test_random.f90`) compares
the library against `test_random_reference`'s strictly overflow-free twin across a width grid graded
by regime. Only the grid row at a width near `2**64 * 2/3` failed — the one width where the rejection
test fires often enough for a wrong threshold to change an answer — which is precisely why the grid
is graded rather than sampled. Note what did NOT catch it: the golden vectors passed on gfortran,
every containment and chi-square test passed on both compilers, and the module's own known-answer
vectors passed on both.

### Risk-95 — `parquet_random`'s remaining wrapping sites rest on one test and nothing else

Two signed-overflow sites remain in `src/parquet_random.f90` after Risk-94's fix, both deliberate,
and **both are now on the route (e) `#else` arm only**: the Philox round multiply
(`random_block`'s `#else`) and `mulhilo64`'s four 32x32 partial products. Both are recorded in the
module's own comments. **A build with a 128-bit integer kind — gfortran and flang — therefore
carries no deliberate signed overflow at all**, and the sites below are ifx's alone.

`mulhilo64` was the one that used to be carried on **both** sides, and it moved for a reason worth
keeping: the wide arm is *both* faster and overflow-free, so the trade that kept it wrapping no
longer exists. A strictly overflow-free spelling on **16-bit limbs** was what had been measured at
1.31x on gfortran and 2.05x on ifx for the whole integer path — that spelling is still too expensive
and is still not used; the 128-bit one is a different candidate and measured 26.14 -> 25.03 ns at a
narrow range on machine B.

**One spelling of it must never be adopted, and it is the fast-looking one.** Forming the product as
a single wide multiply of two unsigned-masked operands, `iand(int(a,k128), MASK64_128) *
iand(int(b,k128), MASK64_128)`, **overflows**: the full unsigned product of two 64-bit values reaches
nearly 2**128 and a signed 128-bit integer stops at 2**127 - 1. It measures *faster* than the form
that ships (one wide multiply against two) and it passes every test, because int128 wrapping happens
to give the right bits — which is exactly the reasoning the first rule below forbids. It was measured
and written up as a gain on machine B before the overflow was noticed. Split **one** operand into
32-bit halves, as the shipped code does, which bounds every intermediate below 2**97.

**What Risk-94 changes about them is not their status but their EVIDENCE.** Each was carried partly
on the reasoning that the compilers in use had been observed to wrap. That reasoning is now known to
be answering a different question than the one being asked. Neither site is more dangerous than it
was yesterday; the justification for calling them safe is simply gone, and only the agreement sweep
stands behind them.

**The rules this forbids.** Do not add a third wrapping site on the strength of a wrapping
measurement. Do not delete or weaken the strict reference in `test/test_random_reference.f90`, which
is the only independent implementation these sites are checked against. Do not put a `write`, a
recorded first mismatch or a running checksum inside any comparison loop in `test_random.f90` —
three such instruments have each been observed making a real fault vanish. And do not claim anywhere
in the documentation that this module is free of undefined behaviour.

**Test.** Covered by `test_agreement_scalar`, `test_agreement_fill` and `test_agreement_int`
(`test/test_random.f90`), which are the only checks that would notice a compiler beginning to exploit
either site. A future failure in one of them is a compiler finding to be reported, not a defect to be
worked around.

### Risk-96 — A wide-width integer draw can be silently non-uniform while every obvious test passes

`pf_random_int_at` is exactly unbiased because it rejects the last `2**64 mod s` candidates of the
range. Get that threshold wrong at a width at or above `2**63` — which the natural spelling of
`umod_2p64` does, for two thirds of such widths — and up to **half the requested range becomes
unreachable**.

The reason this needs an entry of its own is what the failure looks like from outside: every returned
value is still inside `[lo, hi]`, and the values that do occur are still uniform over themselves. A
containment test passes. A chi-square test passes, because it is binning a genuinely uniform
distribution over the reachable half. A mean/variance check passes. The exhaustive small-range count
passes, because it exercises a narrow width where the threshold is right. Nothing short of comparing
against an independent implementation over wide widths can see it.

**The rule this forbids.** *A new statistical test is not a substitute for the wide-width agreement
grid, and neither is a wider one.* If the reduction is ever re-derived, re-optimised, or ported to
another kind, the grid must be re-run — and any new width regime must be added to it rather than
assumed to behave like the ones already there.

**Test.** Covered by `test_agreement_int`'s width grid (`test/test_random.f90`), which deliberately
spans below `2**63`, exactly `2**63`, `2**64 - 1`, width 0, and ranges placed away from zero so the
width's low limb borrows — a dropped borrow once survived an entire sweep because every case had
`lo = 0`. `ref_umod_2p64` (`test/test_random_reference.f90`) derives the threshold by bitwise long
division, a different route from the library's halve-reduce-double, so the two cannot share the
signedness confusion the whole entry is about.

**See also Risk-100**, which is the same function's other half: this entry is about the threshold
being *wrong* at a wide width, that one about its arithmetic not being *executed* at a narrow one.

### Risk-97 — A wrongly selected route (e) fork silently ships the wrapping kernel on a capable compiler

Which multiply `parquet_random` compiles is decided by a cpp allowlist of compiler predefines,
because cpp cannot evaluate `selected_int_kind(38)`. An allowlist has a silent direction: a compiler
that *has* a 128-bit integer kind but is not named in it quietly gets the wrapping kernel — the exact
kernel route (e) exists to avoid, on a compiler that never needed it. Nothing about the build says
so, every value is still correct on that compiler until the day it is not, and the failure mode when
it arrives is a miscompilation, not an error.

The opposite direction is closed by cpp itself: `pf_int128_assert` divides by zero in a constant
expression if the fork is selected where `selected_int_kind(38)` is negative, so that mistake is a
compile error naming the line.

**The rule this forbids.** *Do not add a consumer-facing macro or escape hatch to select the fork.*
An override would reintroduce precisely this hazard with a second mechanism, and route (e) exists to
close it. Adding a compiler to the allowlist is fine; adding a way for a build to disagree with the
allowlist is not.

**Test.** Covered by `test_fork_selection` (`test/test_random.f90`), one assertion evaluated with the
consuming compiler at the moment the suite builds:
`parquet_debug_random_uses_int128() .eqv. (selected_int_kind(38) > 0)`. Verified to hold in both
directions on machine B — gfortran 14.2.1 takes the fork, ifx 2026.1.1 reports
`selected_int_kind(38) = -1` and does not.

### Risk-98 — A schedule-dependent draw reintroduces irreproducibility, and every structural test still passes

`parquet_random` exists for one property: the value at `(seed, i, draw)` does not depend on how many
draws came before it, so a parallel loop reproduces under any schedule. Every other test in the suite
would pass against an implementation that had quietly lost it — golden vectors, agreement sweeps,
statistics and containment are all evaluated on one thread, where there is no schedule to vary.

The realistic way to lose it is not a rewrite of the cipher but a plausible-looking optimisation: a
cached block shared between calls, a thread-local buffer, a bulk path that carries state from one
call to the next, or a future tier that derives a stream key from anything a thread can observe. Each
of those returns correct-looking numbers and breaks the only promise the module makes.

**The rule this forbids.** *No procedure in `parquet_random` may read any state that a caller did not
pass in* — not a thread number, not a cached previous block, not a saved position. The one exception
is `pf_random_seed`, which is not a draw and is documented as nondeterministic. A future tier-1
`pf_rng` will carry state by design; it must not be reachable from any tier-0 procedure.

**Test.** Covered by `test_schedule_independence` (`test/test_random_omp.f90`), which fills the same
array serially, under `schedule(static)`, under `schedule(dynamic,1)`, at 2 threads and at 7, and
requires every value to be bit-identical. Three parts of it are load-bearing and must not be
simplified away: **variable per-iteration work**, without which one thread can claim the whole loop
and the comparison is empty; a **vacuity guard** asserting the team size exceeded 1, which caught a
faulty capture during development and is the only thing standing between this test and a silent
pass; and the suite's **exclusion from test-drive's own per-test parallelism**
(`suite_is_safe_to_parallelize`, `test/run_tester.f90`), without which each region here is nested,
gets a team of one, and tests nothing.

### Risk-99 — A fatal path reached by several threads at once hangs instead of terminating

The concurrency guard in `src/parquet_wrapper.cpp` exists to catch a caller driving one
reader/writer from a parallel region — so it is **designed** to be reached by many threads at the
same instant. It used to end the process with `std::abort()`, and `abort()` takes a lock inside
glibc: when enough threads reach it together they pile up on that lock and the process never dies.

Measured on machine B under ifx at `-O0 -check all`: **192 threads** parked in `futex_wait_queue`,
every stack reading `__lll_lock_wait_private <- abort <- … <- __kmp_invoke_microtask`. The process
survived `SIGTERM` (the Fortran runtime catches it to print a traceback, and a wedged process cannot
run that handler either) and needed `SIGKILL`. The same configuration also produced an occasional
`SIGSEGV` in the same path. A full `fpm test --profile debug` under ifx could not complete.

**Why this is a silent-failure risk rather than a bug that announces itself.** The guard is the
library's only defence against shared-handle misuse, and its diagnostic is what tells a user what
they did wrong. A hang replaces that diagnostic with nothing at all — no message, no exit status, no
core — and the user's own program is what appears to be stuck. It is also **not reproducible on
demand**: 12 sequential and 24 concurrent isolated runs all terminated cleanly, and only a loaded
full-suite run hung, so an investigation that starts from "can I reproduce it" concludes there is
nothing there.

**The rules this forbids.** *No fatal path in `parquet_wrapper.cpp` may call `abort()`, `exit()` or
anything else that takes a lock.* Every one goes through `claim_fatal_path_or_park()` — an atomic
claim, so exactly one thread reports — followed by `fatal_exit()`, which is `std::_Exit(134)`: a
bare `exit_group` syscall, no lock, no `atexit` handler, defined from any thread inside or outside a
parallel region. And `g_fatal_claimed` must stay a single object: per-translation-unit copies would
admit one thread *each*, which is the pile-up itself (see CLAUDE.md's translation-unit-split note).

**Test.** Covered from two directions, because neither alone is enough.

*That exactly one thread reports*: `concurrent_calls_into_shared_writer` and its reader twin now
produce **one** stderr line where the old code produced one per colliding thread, and still exit
134. The line count is the observable that distinguishes the two implementations — the exit status
does not, which is why "it still aborts" was not sufficient evidence.

*That a hang is reported rather than waited on*: every scenario now runs under a wall-clock cap
(`tools/run_error_scenarios.sh` and `prime_error_scenarios`, overridable with
`PARQUET_SCENARIO_TIMEOUT`, default 120 s), and a scenario that trips it is a hard FAIL rather than
a nonzero exit read as a successful abort. **The cap keys on exit 124 AND 137**: `timeout` documents
124, but returns 128+9 = 137 when — as here — it kills with `SIGKILL`, which it must, because
`SIGTERM` is caught. Keying on 124 alone was written first and let a forced timeout report `[PASS]`
with `exit=137`; both sentinels were then verified by forcing a timeout in each path
(a 1-second cap on a 30-second sleep for the shell runner, a 20 ms cap for the Fortran side, which
turned 613 tests into explicit `TIMED OUT` failures). A guard that has never been made to fire is
not a guard.

### Risk-100 — A lazily computed rejection threshold is untested by every width that does not reject

`pf_random_int_at` is unbiased because it rejects the last `2**64 mod s` candidates of a range, and
`umod_2p64` computes that threshold. Risk-96 covers getting it wrong at a width at or above `2**63`.
This entry is about the other half, which is a *coverage* property rather than an arithmetic one and
is invisible for a different reason.

**The threshold is computed lazily** — only when a candidate lands in the last partial block, which
is Lemire's whole point and must not be hoisted. So a width that essentially never rejects never
computes a threshold at all. Every ordinary narrow range is such a width: the suite's own grid spans
`1..6`, `0..999999`, `-100..100` and `-3..4294967296`, whose rejection probabilities are
**2.2e-19, 3.0e-14, 8.2e-18 and 8.7e-19** respectively. Every wide width returns from the `s < 0`
arm before reaching any arithmetic. The result was that `umod_2p64`'s halve-reduce-double reduction
— the part with no early return to hide behind, and the part most easily got wrong — was executed by
**nothing in the suite**, while the module reported 87 % line coverage and every test passed.

Confirmed by mutation, which is the only reason this is stated as fact rather than suspicion:
replacing the modular fold `r = r - (s - r)` with a plain `r = r + r` was caught by **no assertion
anywhere in the suite** — not the golden vectors, not the wide-width agreement grid, not
containment, not chi-square — until the widths below were added.

**The rule this forbids.** *Do not reduce the two narrow rejecting widths to one, and do not pick a
replacement by eye.* The reduction has two arms and a correction, and which one a width takes is
decided by `q = floor(2**64 / s)`: the doubling **folds** when `q` is even and does not when `q` is
odd, and the trailing odd correction runs only when `s` is odd. One width cannot reach all of it.
More generally: *whenever a new constant or predicate gates arithmetic on a condition that is rare
by design, a test must force the rare side* — a probability of 1e-14 is zero for every practical
purpose, including coverage.

**Test.** Covered by the two narrow sweeps in `test_agreement_int` (`test/test_random.f90`), each
with its own vacuity guard that the width actually rejected:

- width `7378697629483820646` (0.4 · 2**64, even, `q = 2`) — the folding arm, rejects one draw in
  five;
- width `5534023222112865485` (0.3 · 2**64, odd, `q = 3`) — the non-folding arm and the odd
  correction, one in ten.

Both are load-bearing and were each verified by a mutation the *other* cannot catch. With them,
every line of `src/parquet_random.f90` that gfortran compiles as reachable is covered; the only
lines left uncovered are `sub64`/`add64`, which the route (e) fork compiles but never calls on a
compiler that has a 128-bit kind (they are live under ifx, and both carry a doc-comment saying so —
they are not dead code and must not be deleted on the strength of a coverage report).

### Risk-101 — The wrapping route (e) kernel is built by nothing routine, and is miscompiled under LTO

`src/parquet_random.f90` forks on whether the compiler has a 128-bit integer kind. The `#ifdef`
arm forms Philox's multiplies in that kind, where they provably cannot overflow; the `#else` arm
ships the wrapping `int64` product and is what runs wherever no such kind exists — today, ifx.

**Every compiler in the fleet except ifx takes the protected arm, so the wrapping arithmetic —
`sub64`, `add64`, and `random_block`'s `#else` multiplies — was compiled by no routine check at all,
and run by none.** Coverage cannot see it either: a gfortran-based coverage run reports those lines
uncovered *because they are unreachable in that build*, which reads identically to dead code.

**It is miscompiled, and the shape of the failure is the part worth carrying forward.** Building the
wrapping kernel at `-O3 -flto` or `-Ofast -flto`, the scalar draws return values unrelated to the
contract. Measured on gfortran 14.2.1 (Linux, Zen 4) and reported independently on gfortran 15.2
(macOS, AVX2), which produce **byte-identical wrong values**. `-fwrapv` and `-fno-strict-overflow`
each remove it; no LTO removes it; `-O2` does not exhibit it. ifx is clean on the kernel it actually
ships, at `-O0` through `-O3 -xHost -ipo`.

**flang 22.1.8 compiles the same wrapping source CORRECTLY under LTO, and that is what identifies
this as a gfortran code-generation bug rather than a property of the arithmetic.** On machine C
(x86-64, AVX2, macOS) `FC=flang-mp-22 tools/check_random_kernels.sh` passes **both** halves at all
six settings — including `-O3 -flto` and `-Ofast -flto`, where the same command under gfortran 15.2
on the same machine fails with **432** mismatches each. Before this datum the evidence was equally
compatible with a latent fragility in the wrapping arithmetic that ifx merely had not yet exploited
— which is not an idle hypothesis here, since `src/parquet_random.f90`'s own header records ifx
wrapping one expression faithfully while using the undefinedness of *another* to delete a branch
hundreds of lines away (Risk-94). It is now **two unrelated toolchains clean** (flang's LLVM
`-flto`, ifx's `-ipo`) against **two gfortran releases broken on the same source**, which supports
the code-generation explanation far better than the fragility one.

**flang is also the only compiler in the fleet that can build BOTH kernels and be checked for
agreement between them**, which is worth more than a second clean row: ifx cannot form the `int128`
arm at all (`selected_int_kind(38)` is `-1` there, and the module's `pf_int128_assert` turns that
into a compile error by design), so on ifx the strict reference and the golden vectors are the only
oracles and there is no second kernel to cross-check against.

**Per-release failure counts, for anyone comparing a future run**: gfortran 14.2.1 reports **480**
(144 from the positive stream arm, 336 from the negative — see the breakdown below); gfortran 15.2
on machine C reports **432**. The figures are driver-specific as well as release-specific, so a
count that differs is not by itself evidence of anything; the verdict is PASS versus FAIL.

**The count also moves when code OUTSIDE the fork changes, which is the most confusing way to meet
it.** The forced-wrapping build compiles the `#else` arms together with every procedure common to
both arms, so an edit to a fill or to `word_of` changes what LTO has to work with and therefore how
much it gets wrong. Confirmed: the same gfortran 14.2.1 went from **480** to **528** across the
`fill_r64`/`fill_r32`/`word_of` restructuring, with no new failure class (the labels stayed
`literal-seed at32` and `literal-seed bits`) and with every **shipped** configuration passing at
every setting including `-O3 -flto`. So a moved count after an unrelated change is expected; a moved
*verdict* on a shipped arm would not be.

**Which draws break depends on the range, and "it is a `pf_random32_at` bug" is the non-negative
half of the answer only.** Uncapped per-label counts from the driver at `-O3 -flto`, forced
wrapping: over `1..24` only `pf_random32_at` comes back wrong (144 failures), while over `-24..-1`
**all three scalar draws do** — `pf_random_bits_at`, `pf_random_at` and `pf_random32_at`, 96 each
with an explicit `draw` (336 failures). So the damage on the negative range reaches
`pf_random_bits_at`, which is the rawest form the module exposes and the one every other value is
derived from. Any future summary of this fault that names one procedure is describing one range.

**What decides whether a call is affected is the stream index's VALUE RANGE, not the call shape.**
This was measured directly, sweeping one call form and varying only the loop bounds:

| stream range | wrong |
|---|---|
| `1..1`, `0..7`, `1..8`, `1..24`, `1..64` | **all of them** |
| `-40..-1`, `-64..-1` | **both** — added later, see below |
| `-3..3`, `-8..8`, `-40..40` | none |

**The rule is ONE-SIDED versus SPANNING ZERO, not "non-negative".** The first version of this table
carried only the non-negative half and concluded that a provably non-negative range is the broken
one — a strictly negative range had simply never been tried. It was, on gfortran 15.2/macOS while
adding the in-suite sweep below, and it is broken too: `-40..-1` reports **520** mismatches against
`1..40`'s 200, deterministically across three rebuilds, with the shipped kernel clean on both. So a
sweep that covered only the non-negative half on the strength of the old table would have been
resting on an untested asymmetry.

**On gfortran 14.2.1 the trigger is narrower still: BOTH bounds must be compile-time known, and the
documented idiom is clean.** Sweeping the loop bounds alone, wrapping kernel, `-O3 -flto`, body
written out inline:

| loop bounds | wrong |
|---|---|
| `1..40` — both literal, non-negative | 40 |
| `-40..-1` — both literal, negative | 40 |
| `-40..40` — both literal, spans zero | none |
| **`1..n`, `n` opaque — `do i = 1, n`, the documented idiom** | **none** |
| `lo..hi`, both opaque | none |
| `n..-1`, opaque lower and literal `-1` | none |

A literal lower bound is **not** sufficient on its own: `do i = 1, n` over a runtime `n` measured
clean while the literal `1..40` beside it measured 40. That shape had been tested on no machine
before — the two previously measured, both-literal and both-dummy, bracket it without covering it —
and it is the shape `doc/pages/utilities/random.md` and the module's own doc-comment tell users to
write. **This is a reason to keep sweeping it, not a reason to stop:** clean on the two releases
tried is not a property of the next one.

**Two traps make a re-measurement of this silently vacuous, and both were walked into here.** The
comparison body must stay written out **inline** in each loop: factoring several shapes' shared body
into one helper, so that they "differ only in their bounds", reported **zero for every shape** on a
build that simultaneously failed at 240 — routing the body through a helper is itself a change of
compiled form. And an opaque bound must be **genuinely** opaque: an `intent(in)` dummy handed a
literal actual argument is restored to a literal by interprocedural constant propagation under
`-flto` and fires at full strength (40 mismatches), so the earlier note that dummy-argument bounds
"detect nothing at all" holds only when the actual arguments are themselves opaque. Neither the
bounds nor the body can be varied independently of the other; treat every entry above as evidence
about the exact form measured, never as a property that can be reasoned about from source.

Two consequences, and both are traps:

- **`do i = 1, n` is the module's own documented idiom**, so a broken range would be exactly the one
  a user writes. That shape is clean on both releases measured — but only because the upper bound is
  opaque, which is a property of the caller and not of the library.
- **A sweep centred on zero detects nothing.** `test_agreement_scalar` uses `-40..40` and would not
  have caught this; a literal-seed sweep first written here used `-8..8` and did not catch it either
  until the range was changed. "Cover the negatives too" is the natural instinct and it lands
  squarely in the clean range.

The earlier framing of this fault as being about *call shapes* (literal-constant seed versus
all-variable arguments) is a symptom of the same mechanism — a literal argument is a range of one —
and it is not reliable on its own. What is measured: gfortran 15.2 breaks literal-seed shapes while
all-variable ones stay correct, and **14.2.1 does the same**, not the reverse — at `-O3 -flto` all
480 of its failures are literal-seed ones, with `variable-shape`, `golden` and the integer width
regimes appearing zero times. (Verified with the driver's ten-failure print cap lifted. The cap
means grepping its ordinary output tells you which arm fires *first*, never which arms fire, and
reasoning from the printed lines is how the mistaken "did the reverse" claim survived.)
An earlier version of this paragraph, and a matching comment in the driver, claimed 14.2.1 "did the
exact reverse"; that is contradicted by a direct measurement on the machine it describes, and has
been removed rather than corrected, since it is not known which source state it was taken against.
The all-variable arm therefore has **no positive result behind it on either release** — it is swept
because the two shapes really are compiled separately, not because it has ever failed.

**The rule this forbids.** *Do not treat a green `fpm test` as evidence about the wrapping kernel,
and do not narrow the kernel check's stream ranges to a symmetric sweep.* Also do not add a
consumer-facing macro for selecting the fork: the module's header explains that the absence of one
is deliberate, and `-U__GFORTRAN__` at a standalone compile already provides everything a test
needs.

**Test.** `tools/check_random_kernels.sh` plus its driver `tools/check_random_kernels.f90` build the
module both ways across six optimisation settings including LTO, and check the golden vectors, the
strict reference over three separately-compiled stream-range shapes — both-literal non-negative,
both-literal negative, and `do i = 1, rt_n` against a `volatile` bound — and every integer width
regime. It carries a vacuity guard that the two halves really did compile different kernels —
without it, a `-U__GFORTRAN__` that stopped working would build one kernel twice and report green —
and it refuses a gfortran below the project's floor, after a first version silently used the system
11.5.0 and produced a confident set of spurious failures.

**The driver's negative arm must keep its inner `draw` loop, and this is not symmetry for its own
sake.** Written with only the three no-draw calls, that arm reported **zero** on gfortran 14.2.1
while the identical range *with* the draw loop reported 40 and the in-suite counterpart reported 520
on 15.2 — i.e. the standalone driver was not merely weaker than the suite on that arm, it was blind
to the class entirely. Do not trim it back to save three lines.

**ifx has been re-cleared against the widened trigger set**, which matters because ifx is the one
compiler that actually *ships* the wrapping kernel, so a gfortran-only finding about it is a finding
about ifx's production path. All six configurations pass including `-O3 -ipo` and `-O3 -xHost -ipo`,
and a dedicated eight-shape sweep — every row of both tables above — is clean at five ifx settings.
That clearance is not vacuous: mutating one reference constant by 1 makes the same probe report
600/600 wrong on every shape. Note also that ifx **cannot** be made to build the int128 kernel for
comparison — `selected_int_kind(38)` is `-1` there, which is exactly what the module's
`pf_int128_assert` capability assertion exists to turn into a compile error — so on ifx the strict
reference and the golden vectors are the only available oracles, and there is no second kernel to
cross-check against.

**The in-suite half is `test_agreement_scalar`'s closing block** (`test/test_random.f90`), which
sweeps `1..40` and `-40..-1` as two loops with their own literal bounds, and
`test_agreement_int`'s, which adds `-20..-1` to stream loops that were otherwise non-negative
throughout. These exist because everything else in that suite spans zero and so cannot see this
class at all; they were verified to fire — 200 and 520 mismatches against the wrapping kernel under
LTO, none on the shipped kernel — rather than merely added. **Never merge either pair back into one
symmetric range**, which is the one edit that switches them off while leaving every verdict
unchanged. `test_agreement_fill` carries the same pair for shape symmetry, but with **no** negative
control: the known instance leaves the fill procedures correct, and its comment says so rather than
implying coverage it does not have.

**CI runs `--shipped-only`**, which checks the kernel the compiler actually ships (the first LTO
coverage this project has had) and skips the forced half. That half fails today, for a gfortran bug
in a kernel gfortran never ships; a permanently red pipeline would be worse than none. Run the
script with no arguments to see it. **If the forced half ever goes green on a newer gfortran, that
is worth recording rather than assuming — and if `random_block`'s `#else` multiplies are ever made
overflow-free, the `--shipped-only` restriction should be lifted in the same change.**

### Risk-102 — A default-kind `size()` wraps above 2**31 elements, and the fill fails silently

`size(v)` asked without an explicit `kind=` returns a **default-kind** integer. For an array of
2**31 elements or more that value wraps, and in a bulk loop the wrap does not fail loudly — it
produces a length the loop then obeys. Both `pf_random_fill_draws` routines held their length that way,
and both failure modes were measured on the shipped module:

| array size | `size(v)` | what happened |
|---|---|---|
| 2**31 exactly | **−2147483648** | tripped the zero-size guard; **nothing written**, the caller's `intent(out)` array left undefined |
| 2**32 + 8 | **8** | **eight elements written**, the remaining 4.29 billion left undefined |

No error, no warning, no abort, and — because `v` is `intent(out)` — the caller reads whatever the
allocator left there, which is worse than reading zeros. A section of the *same* allocation filled
correctly, so the length is the whole story.

**Why nothing caught it.** Every fill test in the suite uses sixteen elements or fewer, and the
smallest array that reaches the boundary is 2**31 `real32` values, about 8.6 GB — far past what
`fpm test` or CI should attempt. Coverage is blind to it as well: the line executes normally, just
with a wrapped value. And it disappears entirely under `-fdefault-integer-8`, so a compiler flag can
mask it. The size is not exotic for this library: one draw per row of a three-billion-row table
lands squarely in it, and `parquet_table` handles exactly that.

**The rule this forbids.** *A length taken from a caller's array must be `size(v, kind=int64)`, and
the counters that walk it must be `integer(int64)`.* More generally: **a bulk routine's own loop
counters are part of its contract**, not an implementation detail, and the default integer kind is
the wrong choice in any routine whose input size the caller controls. The guard has to be static or
large-scale, because nothing in between can see the difference.

**Test.** `check_fill_size_kind` (`tools/check_source_conventions.py`) is the cheap always-on half:
it flags any bare `size(x)` in `src/parquet_random.f90`, matched by shape so a bulk routine added
later is covered without editing the check. It is deliberately scoped to that one file — `src/`
carries about 200 other bare `size(...)` calls, nearly all on arrays bounded by construction, and
asserting a 200-entry debt this check has not verified would be worse than leaving them; **whether
any of those takes an unbounded caller array is an open question worth its own pass.**

`tools/test_random_large_fill.sh` plus `app/test_random_large_fill.f90` are the end-to-end proof,
run by hand. Two properties of its design are load-bearing rather than incidental: it compares each
probed element against `pf_random32_at`/`pf_random_at` **at the same position**, so it verifies the
contract rather than merely that something was written; and it warns on stderr when `ELEMENTS` is
below 2**31, because a smaller run exercises the fill and **cannot detect this bug at all** — a
green run at the default size is the only one that means anything. Both failure modes were
reproduced against the unfixed module through this exact tool before the fix was accepted.

### Risk-103 — The stream's high counter word is reached by no ordinary stream index

`random_block` splits the stream index across Philox counter words `c2` and `c3`, where
`c3 = ishft(stream, -32)`. That word is **zero for every stream below 2**32 and all-ones for every
small negative one**, so a suite whose streams are all small never observes it holding anything
else — and for a long time none of them were. The golden tables use streams
`{-5, 0, 1, 2, 10**6}`, the agreement sweep ran `-40..40`, and the widest loop reached 50000.

**What that left undetectable.** Deriving `c3` from the stream's SIGN alone reproduces every value
in the rest of the suite exactly, while making streams `i` and `i + 2**32` **identical**. One draw
per row of a table with more than 4.3 billion rows is an ordinary use of this module — this library
exists for data at that size — so the failure is a silently repeated stream in exactly the case the
counter-based design is sold on. Confirmed by mutation: before the sweep below existed, that change
passed **all nineteen tests**, including the golden vectors, both agreement sweeps, containment and
chi-square.

**Why this was the last part of the counter to be pinned, and the shape to recognise.** The draw
axis had already been extended twice (Risk-100's widths, then the high-draw sweep in `test_edges`),
and the reference's index arithmetic rebuilt so that no ceiling remains on it. The stream axis got
none of that, because nothing about it looks like a boundary: no arithmetic overflows there, no
guard branches on it, and every value is equally valid. *A coordinate that is split across machine
words is exercised only up to the width a test actually uses* — that generalises past this module,
and it is the reason to reach for a mutation rather than a coverage report, which showed these lines
as fully covered throughout.

**Test.** Covered by the extreme-stream sweep at the end of `test_edges` (`test/test_random.f90`),
over `ishft(1_int64, k) + 12345` for `k = 32..62`, both signs. It carries **two** assertions per
stream and they fail for different reasons — do not reduce it to one:

- **agreement against `ref_bits`/`ref_at32`** is what catches a wrong `c3`. Unlike the draw axis
  there is no ceiling to respect: neither side derives a stream index by arithmetic, so every
  `int64` stream is directly comparable.
- **`bits(seed, s) /= bits(seed, s - 2**32)`** is what still catches a `c3` that has stopped varying
  when the reference has acquired the *same* fault. Verified to be non-redundant rather than assumed:
  with an identical fault applied to both the library and `test_random_reference` and confined to
  streams above 2**32 — so that no pre-existing assertion sees it — the agreement half passes and
  this inequality is the only thing in the suite that fails.

### Risk-104 — A thread team opened one level down deadlocks libgomp

`pf_sort_threads` used to decide whether to thread by asking `omp_in_parallel()`. That predicate
answers **"is the enclosing region ACTIVE"** — does its team have more than one thread — and it is
therefore `.false.` inside a region that exists but runs on one thread: `!$omp parallel if(cond)`
with `cond` false, or any region at all under `OMP_NUM_THREADS=1`. `omp_get_level()` is 1 there.
So the library believed itself to be at the top of the program, opened a full team, and built a
**nested** one.

**libgomp deadlocks on that shape.** Measured on gfortran 15.2 / macOS arm64: a full `fpm test`
hung roughly one run in three, always somewhere in `sorting`, at 0 % CPU with the main thread and
its workers parked on a single libgomp mutex that **nobody held** — four workers for a team of
eight, i.e. `gomp_team_start` stopped mid-spawn. Reduced to twenty lines with no library code at
all:

```fortran
!$omp parallel num_threads(1)        ! enclosing team of ONE
!$omp single
!$omp parallel do num_threads(3)     ! nested team
do j = 1, n
    b(j) = a(j)
end do
!$omp end parallel do
!$omp end single
!$omp end parallel
```

That hangs **7 runs in 8** within 20000 rounds. Every clause is load-bearing, by bisection:
`master` in place of `single` never hangs, no enclosing region never hangs, an enclosing
`parallel do` never hangs, and an enclosing team of **two or more** never hangs. The nested count
does not matter — 2, 3 and 4 all hang non-deterministically, which is what identifies it as a race
rather than a specific combination. **No environment setting fixes it**: `GOMP_SPINCOUNT=0` moves
7/8 to 2/8, `OMP_MAX_ACTIVE_LEVELS=1` and `OMP_WAIT_POLICY=passive` do nothing.

**The library's code is standard-conforming; this is a compiler-runtime defect.** What the library
can do is decline to build the shape, and that is the fix.

**The rules this forbids.**

*Do not restore `omp_in_parallel()` as the automatic predicate.* It is the wrong question. The rule
being expressed is "am I nested", and only `omp_get_level()` answers it.

*Do not widen the explicit-`threads=` clamp to every nested call.* It fires only when
`omp_get_level() > 0 .and. omp_get_active_level() == 0` — a region that exists and is running on
one thread. That is the only shape measured hanging, and narrowing to it is what lets a genuinely
parallel caller keep the documented promise that an explicit request is honoured. Widening it to
all nesting would also have silently disabled threading for the whole test suite, for the reason in
the next rule.

*Do not assume the C++ engine is exempt.* The count `resolve_thread_count` produces is the one
`drive_engine` hands to `sort_build_permutation_threaded` **and** to `parquet_sort_builder_build`,
so the clamp reaches both engines even though only the Fortran one threads with OpenMP. An earlier
draft of this fix claimed the opposite in a code comment; the tell is
`test_threads_auto_in_parallel`, which pins the C++ engine and still observes the clamp when its
enclosing region is inactive (as it is under `OMP_NUM_THREADS=1`).

*Do not run an excluded suite through `run_testsuite(..., parallel=.false.)`.* That argument keeps
test-drive's `!$omp parallel do` and disables it with an `if` clause, which still **opens** an
inactive region — so every test in every excluded suite sat at `omp_get_level() == 1`, which is
both the deadlock shape and, once the clamp existed, a blanket refusal to thread. `run_tester`'s
`run_suite` drives those suites through `run_selected` instead, which runs each test with no
enclosing region at all. It also checks the suite for duplicate test names first, because
`run_selected` finds its test **by name** and two tests sharing one would run the first twice and
the second never — silently, with the count still looking right. That check found a real instance
the moment it was written (`errors` had two "prefetching an unknown column aborts").

**Test.** `test_nested_team_guard` (`test/test_sorting.f90`) asserts the DECISION rather than the
outcome, since a deadlock cannot be asserted: `pf_sort_threads()` must return 1 inside an inactive
region, and an explicit `threads=4` reaching the Fortran engine must resolve to 1 there. Both arms
carry a level-0 negative control taken first, and both were verified by reverting their guard
independently — each mutation fails its own assertion and no other.

### Risk-105 — An allocate extent from a default-kind `size()` overflows the array it just allocated

`size(x)` without `kind=` returns a DEFAULT-kind integer and wraps above 2**31 elements
(Risk-102). In an allocate extent that is worse than the silent short fill Risk-102 describes,
because the loop that fills the array usually gets its bound **right**:

```fortran
allocate(arr(size(rows)))                       ! wraps: negative extent -> zero-length array
do k = 1, size(rows, kind=int64)                ! correct: runs the full count
    call parquet_column_get_at(..., arr(k))     ! writes past the end
end do
```

Element assignment does **not** reallocate, so this is an out-of-bounds heap write on a valid call
rather than a wrong answer. Its sibling shape is benign for a reason worth knowing: where the code
reads `allocate(arr(size(p)))` followed by the whole-array `arr = p`, intrinsic assignment to an
allocatable resizes the destination and hides the mistake entirely — so the two shapes look
identical in review and only one of them is a bug.

**Where it was.** Ten sites in `src/parquet_tables_access.f90` (`%get_slice` for every kind and
both ranks, plus the widening variants) and both `table_valid_mask_rows` variants in
`src/parquet_tables_query.f90`. In every one, the `kind=int64` on the very next line shows the
hazard was understood and the allocate was simply missed. Reachable through ordinary public API —
`%get_slice` over a slice of more than 2**31 rows — which is an unremarkable request for a library
whose tables are addressed with `int64` row counts throughout.

**Why nothing found it.** No test allocates anything near 2**31 elements, and none can: the output
array alone would be 17 GB. Coverage is blind, because the line executes normally with a wrapped
value. The generated file made it ten sites instead of one, and made it invisible to anyone reading
the generator's template, where the two lines sit in different string literals.

**The rule this forbids.** *An allocate extent taken from `size(...)` must use `kind=int64`,
everywhere, with no exemption for an array that is "obviously" small.* The uniform rule is what
makes the check maintenance-free: `kind=int64` costs nothing at a bounded extent, so there is no
list of blessed sites to go stale. The wider question — the roughly 200 other bare `size(...)` calls
in `src/`, which an audit found to be on arrays bounded by construction (column counts, MAML lines,
row-group counts, sort-key lists) — is deliberately **not** covered by this rule, because only the
allocate shape turns a wrapped length into an out-of-bounds write.

**Test.** `check_allocate_extent_kind` (`tools/check_source_conventions.py`), matched by shape
across all of `src/` and verified to fire by reverting one site. Two of the fixes are in generators
(`tools/generate_parquet_tables.py`, `tools/generate_parquet_sorting.py`) rather than in the emitted
files — a hand-edit to `src/parquet_sorting_keys.f90` was silently reverted by the next
regeneration during this very fix, and the check is what caught it.

### Risk-109 — The bulk permutation and the scalar entry point compute the same function by different routes

`pf_random_perm_at(seed, m, k)` and `pf_random_permutation(perm, seed)` must agree at every `k`, and
they reach the answer differently on purpose: the scalar form divides `x = k - 1` by `b` to recover
the Feistel halves `(l, r)`, while the bulk form gets them from its own loop indices and never
divides. **A disagreement is not a crash and not an invalid result — it is two different, equally
plausible permutations**, each internally consistent, each passing a bijectivity check, differing
only in which element lands where. Nothing downstream can notice.

**The loop bounds are the highest-risk edit in the file.** `perm_range_*` walks `r` from `r0` to a
precomputed `rhi` and advances `l` when the row is exhausted; an off-by-one anywhere in that
produces an array that is not a permutation at all, or one that is a permutation of the wrong
coordinates. `rhi` is computed before the inner loop specifically so that the `hi` bound never
becomes a branch inside it.

**One copy of the round loop, and that is the structural half of the guard.** `perm_feistel_lr` holds
the rounds; `perm_feistel` is a two-line wrapper that supplies the division. Both entry points reach
the same body, so the two cannot drift in the *cipher* — only in how they enumerate coordinates.
Splitting that body in two "for clarity" would remove the property.

**Test.** `test_perm_bulk` (`test/test_random.f90`) asserts the identity elementwise over a sweep of
`m` chosen for the bulk form's own failure modes rather than the kernel's — `100` where `a*b == m`
exactly so the fill stops on the last iteration of both loops, `5` and `7` where the cycle-walk is
entered, `1000` where it stops mid-row — for both result kinds, and separately re-asserts
bijectivity of the bulk output. `test_perm_threads` (`test/test_random_omp.f90`) extends it to every
thread count, asserting against the *scalar* form rather than against a one-thread bulk run, which is
what makes it the outermost binding of the whole construction. This test may not be deleted as
redundant on the grounds that the identity holds by construction: that is precisely the reason it
must be kept, since a restructure is what would break it.

### Risk-110 — The permutation's round count, round function and width rule are frozen, and three rounds looks free

`pf_random_perm_algorithm` (`feistel-mix2-4/zaxzb/v1`) covers five things: the construction, the width
rule (`a = ceil(sqrt(m))`, `b = ceil(m/a)`), the round count (4), the round function (`perm_mix2`) and
the key derivation. Changing any of them changes every value the module can produce for every seed.

**A future contributor optimising this will find three rounds tempting, and every marginal statistic
will agree with them.** Three rounds is 25 % cheaper and passes fixed-point chi-square, cycle counts,
position uniformity and subset membership. It is caught only by a **structural** distinguisher —
asking whether two inputs sharing a coordinate produce outputs sharing one, enumerated exhaustively
over the raw domain and calibrated against a Fisher–Yates control. Under that test three rounds leaks
in **every one of six independent key sets, always in the same relation and always in the same
direction** (z = 10–19), while four rounds wanders between relations and straddles the control. Two
rounds is broken outright and provably so: after two rounds the left output is `(l + F1(r)) mod a`, so
inputs sharing `r` can never share a left output — measured exactly 0.

**An odd round count is disqualified for a separate, structural reason** that no statistic will show:
the two factors swap every round, so after an odd count the state is in `Z_b × Z_a` and the output is
encoded against transposed factors. The real choice was only ever 2, 4 or 6.

**`perm_mix2`'s 31-bit masks are load-bearing and must not be simplified.** Removing them restores a
wrapping signed multiply, and Risk-94 records this repository being caught with exactly that — the
wrapping *measured* correct on the compiler in use, and the optimiser still used the overflow's
undefinedness to delete a branch two functions away. The masks were measured free on gfortran and
slightly faster on ifx, so there is no cost to weigh against.

**Test.** `test_perm_golden` (`test/test_random.f90`), against 27 vectors from
`tools/generate_random_perm_vectors.py` — an **arbitrary-precision Python model of the contract**,
transcribed from the specification rather than from the Fortran, whose assertions also prove no
intermediate reached 2⁶³. That file is the only thing that catches a round-count change:
`test_perm_bijection` does **not**, and this was confirmed by mutation rather than assumed — with the
vectors removed, `perm_rounds = 3` survived the entire suite, because an odd count still yields a
bijective encoding of the same domain. Mutation testing over the round count, the multiply-shift
mask, `perm_c1` and the width rule now kills all four; the conditional subtract is killed too, but as
a **hang** rather than a failure, since removing it destroys the round's bijectivity and the
cycle-walk orbit never re-enters range.
