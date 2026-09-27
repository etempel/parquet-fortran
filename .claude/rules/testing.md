---
paths:
  - "test/**/*"
  - "tools/run_error_scenarios.sh"
  - "tools/check_source_conventions.py"
  - "tools/check_*.py"
  - "tools/*.sh"
---
# Testing

## Running tests

- One suite: `fpm test run_tester -- <suite>`; one test: `fpm test run_tester -- <suite> "<name>"`.
  Prefer these while iterating; a full `fpm test` runs every OpenMP/error-scenario subprocess.
- `fpm test --profile debug` for bounds checks (`build.md`); the NAG profiles run through the
  `/nag-build` skill.
- The suites are split across five runner programs (`tools/count_tests.sh` prints the map).
  `run_tester_noundef` holds Arrow-free suites that cannot run under `-C=undefined`; its
  membership is decided by a person after reading why a result changed, never moved by a tool
  (`check_test_runner_partition` keeps the split honest).
- A fixture a test WRITES goes under `test_run/`, never the repository root
  (`check_test_fixtures_live_under_test_run`).

## Error scenarios

Procedure for adding one: the `/add-error-scenario` skill.

- An abort path is tested out of process: a `case` in the `error_scenarios_<group>.f90` module for
  its area, a wrapper in the matching `test_errors[_<group>].f90`
  (`check_scenario_exit_status_and_stderr` asserting the library's own message), and an entry in
  `tools/run_error_scenarios.sh`'s `scenarios=(...)` array (`check_scenario_list_is_complete`
  derives the names from the four `select case` blocks). A scenario needing OpenMP goes in the
  `concurrency_scenarios` bucket instead.
- **The scenarios are split four ways, for compile time**: `error_scenarios_io.f90` (writer,
  reader, settings, read-time filters), `_table.f90` (qc rules, columns, temporal, the table type),
  `_analysis.f90` (sorting, statistics, geometry, nested containers, logging, TOML) and
  `_numeric.f90` (indexes, message-stream routing, the numeric tier), with cross-group fixtures in
  `error_scenarios_support.f90`. `error_scenarios.f90` only walks the four `dispatch_*` procedures
  in turn, so a name may be dispatched by ONE of them. The test side mirrors it: `test_errors.f90`
  (machinery + the `errors` suite) and `test_table_errors/_analysis/_numeric.f90`.
- `run_tester_errors` primes every listed scenario once, in parallel from the shell (`xargs -P`,
  one fork with no OpenMP team active), into `test_run/.primed/`; every failure path degrades to
  the old on-demand spawn, never to a wrong answer. Do not move that parallelism into the test
  process. It primes for any run but a single named test. `PARQUET_TEST_NO_PRIME=1` disables
  priming; `PARQUET_TEST_PRIME_JOBS=<n>` sets the concurrency.
- Both harnesses probe `timeout` by running `timeout -s KILL 1 true` and fall through to `gtimeout`
  (a BSD `timeout` satisfies `command -v` and rejects `-s KILL`, making every scenario fail with a
  usage message). Probe with the command and flags you will actually run. Scenarios that pass by
  hand but fail in the suite: read `test_run/.primed/<name>.err`.
- Stdout and stderr are captured separately on both paths; `scenario_capture_contains` searches
  both.
- A scenario whose abort is inside a `pure` function must USE the result (print it in its trailing
  message); gfortran deletes an unused pure call at `-O1`+ and the scenario exits 0
  (`check_scenario_uses_a_pure_result`). Never consume it with another call that could itself
  abort.
- A scenario that discards an ordinary result is one `pure` keyword away from the same failure.

## Tests run concurrently

- test-drive runs a suite's tests inside `!$omp parallel do`. **Every test gets its own fixture
  filename**, even for identical contents; a shared helper takes the filename as an argument.
- `tools/run_error_scenarios.sh` is concurrent too, per scenario NAME: a helper backing several
  `case` entries derives its fixture path from its own arguments, never a `parameter`
  (`grep -hoE "call scenario_[a-z0-9_]+" test/error_scenarios*.f90 | sort | uniq -c` lists the
  multi-invoked helpers). A collision reads as a library crash (`Couldn't deserialize thrift`, exit
  134 via `std::terminate`); reproduce with forced interleaving (`taskset -c 0`), not more
  parallelism.
- **A test that writes process-global state** (a settings knob, a C++ static counter or hook) has
  its suite excluded in `test/run_tester.f90`'s `suite_is_safe_to_parallelize` (`filter_screen`,
  `sorting`, `sort`, `settings`); the failure mode is a vacuous A/B pass, and "it has always passed"
  is not evidence. Narrow a guard before excluding a suite for it.
- Under a `-fopenmp` build `omp_in_parallel()` is `.true.` inside every test (library guards that
  refuse on it fire suite-wide; `columns-tables.md`).
- An excluded suite runs through `run_selected` per test with NO enclosing region
  (`run_testsuite(..., parallel=.false.)` still opens an inactive region one level down, which
  deadlocks libgomp intermittently; `test_nested_team_guard`). Its tests may assume
  `omp_get_level() == 0`; `run_suite` refuses a suite with two tests of the same name.
- **An intermittent failure has a third cause besides a race and a shared path: a test reading
  memory the library never wrote** (a null row's value bytes are unspecified by design; `NaN - NaN`
  fails a self-comparison). Make it deterministic with an `LD_PRELOAD` `malloc` shim filling every
  block with `0xFF` (every uninitialised float becomes NaN; covers `allocate`, not stack arrays or
  `calloc`/`realloc`; Linux only):

```c
#define _GNU_SOURCE
#include <dlfcn.h>
#include <string.h>
#include <stdlib.h>
static void *(*real_malloc)(size_t);
static int fill = -1;
void *malloc(size_t n) {
    if (!real_malloc) real_malloc = dlsym(RTLD_NEXT, "malloc");
    if (fill < 0) { const char *e = getenv("FILL_BYTE"); fill = e ? (int)strtol(e, 0, 0) : 0xFF; }
    void *p = real_malloc(n);
    if (p) memset(p, fill, n);
    return p;
}
```

  A self-comparison is only as strong as the data varies (row-distinct values, verified by
  mutation). Grade a suspect test's assertions: unsound (undefined data against itself), vacuous on
  values (only `size()`/`is_null()`), weakened (periodic fixture); fix the first two. Uninitialised
  value bytes never reach the output file (closed question).

## Assertions

- Every `call check(error, condition, ...)` carries a message; the condition's own source text is
  the acceptable default.
- Never assert a compiler's `ERROR STOP` spelling or exit status (gfortran `1`/`ERROR STOP msg`,
  flang `1`/`Fortran ERROR STOP: msg`, nagfor `2`/`ERROR STOP: msg`). Assert the library's message
  text, and `/= 0` (`/= 134` where the point is telling a Fortran abort from the C++ `fatal_exit`,
  which is exactly 134 everywhere). The shared helpers already do this; grep `exitstat ==` before
  adding a comparison. A guide page may not state an exit status either.
- Reserve exact-equality assertions for values that are stored, never re-derived through a
  transcendental (`fortran-gotchas.md`, ifx). **A libm-backed bulk form and its scalar twin agree
  to a few ulp only** (a vector libm variant inside one binary): assert that, and assert CHUNK
  INVARIANCE exactly at every split size (`test_exp_cross_form`); never add `NOVECTOR` to force it.
- **A parallel-versus-serial equality needs an independent oracle beside it**: both arms share
  everything above the point they diverge, so a defect there passes the A/B. Build the fixture as a
  pure function of the row number and check every surviving row against it
  (`check_rows_consistent`, `test/test_table_parallel.f90`).
- **Test a sufficient-condition fast path by its IMPLICATION on every element it handled**, never
  by a distribution or an answer A/B: re-run the exact test on each candidate a rejection sampler's
  squeeze accepted and require agreement (`test_squeezes_are_valid`). A wrong squeeze moves no
  moment a feasible sample resolves.
- **An assertion whose whole subject is an INQUIRY does not call the procedure it names.**
  `kind(f(x))`, `len(g(s))`, `size(h(v))` and their kin are resolved from the interface, so a
  generic-resolution test written that way passes without the specific ever running and reports
  as covered nothing. Assert a VALUE from the specific as well; `tools/coverage.sh`'s
  entirely-unreached-procedure list is what finds the ones already written.
- A settings knob test asserts default, round trip and an observed effect with a negative control
  (`api-conventions.md`).
- A test asserting a REFUSAL on cost grounds says in its own doc-comment what to assert when the
  refusal lifts ("becomes an equality test, not a deletion"); a refusal comment in code reads
  "deferred until X", never "cannot be done". A refusal that disappears entirely deletes its
  scenario from all three places at once, after checking for a positive form worth asserting
  instead; run the whole suite, since other scenarios may have relied on the refused case.
- Regression fixtures for "sized/typed from the first element" bugs (character vectors) put the
  SHORTEST element first and a longer one later (`test_read_string_vector_short_first`).

## A test that asserts THREADING skips without OpenMP

```fortran
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: <what is compiled out, and why the assertion below " // &
            "would then hold for the wrong reason>")
        return
#endif
```

- Place it after the declarations and before every executable statement.
- Use the predicate the code keys on: every resolver clamps `threads=` to `omp_get_num_procs()`
  (`parquet_clamp_to_affinity`), so a test asserting a team of N also skips on
  `omp_get_num_procs() < N` -- N, not 2: a lower guard fails on a narrow runner and nowhere else
  (reproduce with `taskset -c 0-1`). A serial suite may fake the machine instead, thread count and
  mask override together (`test_index_threads_ceiling`; `borrow_threads`,
  `test/test_string_parallel.f90`).
- A test whose assertions hold without OpenMP (the C++ sort engine threads through `std::thread`;
  an unclamped resolver still reports the team it chose) takes the processor guard alone, inside
  `#ifdef _OPENMP`.
- Negative control: a normal build reports zero skips; invert the predicate once to see the expected
  tests skip.
- A test that merely USES threads is not guarded.
- The test must still COMPILE serially: every `omp_*` reference needs its own `#ifdef _OPENMP` with
  a serial arm (`check_openmp_calls_are_guarded`, `fortran-gotchas.md`).

## Mutation testing

Break the code deliberately and confirm the test fails, for anything whose failure is silent (a
fast path, a short-circuit, a cache, a guard). The `/mutation-test` skill carries the procedure
(snapshot to the scratch directory, never `git checkout` to restore, clean build, an abort counts
as caught). Rules for reading a result:

- Check which path the test reaches: a comparator mutation survives when both sides use the same
  comparator or a counting fast path never calls it (`nth_element is stable on duplicates`); a size
  threshold is the same trap (`the final merge round is really co-ranked`) — give every gating
  constant a `parquet_debug_set_*` override and lower it in tests
  (`parquet_debug_set_sort_merge_min_segment`, `parquet_set_sort_counting_path`).
  Lower EVERY floor on the path, the one deciding the decomposition runs as well as those opening
  the team. **Assert a threaded path through its team or design counter, never only through its
  answer**: the answer is identical at every team size, so a count resolved and then dropped, or a
  team that opens and runs serially, passes every answer A/B. Pair the counter with a `threads=1`
  negative control and read the one belonging to the engine under test (`threaded_split_ran()`,
  `threaded_design_was()`, `parquet_debug_sort_threads_used()`; `test/test_sorting.f90`).
- An optional argument no internal caller passes, and the degenerate shapes of an entry point
  (non-empty destination, zero-length input), are untested until enumerated deliberately.
- An argument that only FORWARDS through a layer, especially an abort-only one, needs one scenario
  per forwarding site or a static forward check (`check_join_specifics_forward_every_argument`).
- A surviving mutation may be masked by a redundant sibling guard or a later check, or be a
  semantic no-op (a total order has no ties to break); prove a mutation changes behaviour before
  reading anything into it. An IEEE-flag mutation needs a two-step underflow through a live named
  variable (`mut = z * 1.0e-300_real64; mut = mut * 1.0e-10_real64; n = n + mut`), proved with a
  standalone program; `volatile` is unavailable in a `pure` procedure.
- A procedure writing into a caller-supplied fixed-length slot needs a CANARY (pass a substring of
  a longer buffer; assert the remainder untouched), not a read-back.
- Removing work can silently reduce a documented feature to a no-op while every answer stays right;
  grep the tests and the guide for the feature's name. A counter reading 0 cannot tell "ran and did
  nothing" from "never ran"; pair it with a counter for the other branch.
- A "which path ran" observable is written by EVERY route including the fallback, in the route's
  body, not at the call sites (`parquet_debug_index_spills`).
- A branch no buildable fixture can reach is defensive: comment and `GCOVR_EXCL` it
  (`list_uniform_width`'s `IsNull`).

## Static checks (`tools/check_source_conventions.py`)

- **Match by shape, never by an enumerated list**; where a list is unavoidable, fail when it comes
  up empty. Two checks needing the same list derive it from one place
  (`parquet_print_settings`' rows).
- **A check fails rather than passes when it goes blind** — its anchor stops matching, a list comes
  back empty, or the page or source file it reads is missing. A vacuity guard naming the floor it
  expects is the usual shape (`RISK_SCANNED_MIN_FILES`).
- **Prove a new check in BOTH directions before trusting it**: plant a violation and confirm it
  fails naming the site, and blind it — delete the file it globs, empty the list it derives,
  rename its anchor — and confirm it fails there too. **The blind direction is where the holes
  are**: a check that passes when it can see nothing reports a clean tree forever, and a surviving
  blind case is more often a hole in the check than a weak mutation. Record both sets of trials
  where the check's work is written up.
- A documentation table mirroring a source-owned list is compared against the source in both
  directions (`check_env_table_matches_the_source`); read the table's rows, not the page.
- A count in prose is checkable only where the words can be expanded from the source they mirror
  (`check_set_threads_fanout_documented` spells the number out and derives the names from the
  setters; `check_module_settings_reexports_documented` expands an English phrase the same way).
  The general case is a hand count: avoid writing one where the list is the point; where a count
  must exist, have the two places carrying it name each other.
- A one-off audit regex is untested code: run it against a known instance you did not use to write
  it, and prefer turning the audit into a check. Scan an argument list with a paren counter, never
  `[^)]*`; verify a new check against a known-good tree before reading its first output as a
  finding.
- A count too large to fix at once is RATCHETED, not allow-listed: a per-file count that fails on
  growth and on a count left too high (`check_no_per_element_string_alloc`'s `KNOWN_REMAINING`).
- An environment probe matches by shape (`BASE`, `BASE-mp-*`, `BASE-[0-9]*`; `compiler_variants` in
  `tools/machine_report.sh`) and prints `SKIPPED -- '<x>' not found` rather than returning quietly.
  A tool's silence is evidence about the tool; run the thing by name before reporting it absent.
  `tools/machine_report.sh` describes the shell it ran in — run it inside each activated shell.
- **Read a file through `source_text`/`source_lines`/`stripped_lines`/`stripped_text`, never
  `path.read_text()` with a per-line `strip_comment`**; they are cached per run, and a check that
  re-derives a file's comment-stripped form makes every other check pay for the tree again. Derive
  a whole-tree structure through a cached helper too (`procedure_scopes`, `_logical_lines`,
  `_src_import_graph`) and return it frozen, so no check can mutate what the next one reads.
  `check_comment_stripper_fast_paths_agree` holds `strip_comment`'s fast paths to
  `strip_comment_by_loop`.

## The lint stage is meant to stay near a minute

`tools/run_lint_check.sh` runs its `CHECKS` `RUN_LINT_CHECK_JOBS` at a time (default: the core
count; `1` forces serial, as `--fail-fast` does). The checks write nothing and share no state,
which is what makes that safe — keep it that way.

- **Measure before touching a slow check's arithmetic.** An oracle's precision, scan density and
  cell count are what it certifies, not tuning knobs. Make a heavy generator faster by mapping its
  independent cases across processes (`tools/gen_parallel.py`, which also records the `gmpy2`
  route), and let its own `--check` prove the output did not move.
- A new `tools/*.py` file needs a row in CONTRIBUTING.md's index (`check_contributing_is_an_index`).

## `tools/*.sh` checks

- Bash 3.2 compatible (macOS): no `declare -A`, `mapfile`/`readarray`, `${var,,}`/`${var^^}`. A
  script genuinely needing bash 4 asserts `${BASH_VERSINFO[0]} -ge 4` and exits nonzero.
- A check that stops early must not exit 0: `finished=0` up front, `finished=1` after the last unit
  of work, and `trap '[ "$finished" = "1" ] || { echo "... TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT`.
  Verify by injecting an unbound variable mid-script.
- A wrapper that cannot engage the configuration it was asked for exits nonzero rather than
  degrading (`tools/fpm_lto.sh`).

## Debug hooks

- Prefer a C++ `parquet_debug_*` hook (`cpp-wrapper.md`); it stays out of the Fortran interface.
- State behind private Fortran components (`parquet_table_cache`, `parquet_strings`, which reaches
  no `bind(C)` surface) forces a PUBLIC Fortran procedure: doc-comment it as test-only and why it is
  public, call it from no library code, and never put it in a hot path
  (`parquet_debug_table_set_inflight`, `parquet_debug_table_drop_name_index`,
  `parquet_debug_set_string_min_bytes`, `parquet_debug_set_string_max_auto_threads`,
  `parquet_debug_string_row_ranges`, `parquet_debug_string_bulk_threads`).
- A hook that reports what it changed (a required `had_index` argument) beats one a test cannot
  observe. A tuning constant no test-sized fixture reaches needs an override, or every test takes
  the other path.
- Every scenario a hook enables gets a negative control: make the guarded call once with the hook
  clear before setting it.
