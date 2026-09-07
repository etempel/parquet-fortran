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
- `fpm test --profile debug` for bounds checks (`build.md`); the nagfor profiles are in
  `nagfor-builds.md`.
- A fixture a test WRITES goes under `test_run/`, never the repository root
  (`check_test_fixtures_live_under_test_run`).

## Error scenarios

- An abort path is tested out of process: a `case` in `test/error_scenarios.f90`, a wrapper in
  `test/test_errors.f90` (`check_scenario_exit_status_and_stderr` asserting the library's own
  message), and an entry in `tools/run_error_scenarios.sh`'s `scenarios=(...)` array
  (`check_scenario_list_is_complete` derives the names from the `select case`). A scenario needing
  OpenMP goes in the `concurrency_scenarios` bucket instead.
- `run_tester` primes every listed scenario once, in parallel from the shell (`xargs -P`, one fork
  with no OpenMP team active), into `test_run/.primed/`; every failure path degrades to the old
  on-demand spawn, never to a wrong answer. Do not move that parallelism into the test process.
  Only a full run and the `errors` suite prime; a named suite or test spawns on demand — do not widen
  `suite_drives_error_scenarios`. `PARQUET_TEST_NO_PRIME=1` disables priming;
  `PARQUET_TEST_PRIME_JOBS=<n>` sets the concurrency.
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
  (`grep -oE "call scenario_[a-z0-9_]+" test/error_scenarios.f90 | sort | uniq -c` lists the
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
  deadlocks libgomp intermittently — `feature_risks.md` Risk-104). Its tests may assume
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
  transcendental (`fortran-gotchas.md`, ifx).
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
- Use the predicate the code keys on: the sort engine clamps `threads=` to `omp_get_num_procs()`,
  so those tests also skip on `omp_get_num_procs() < 2`; `parquet_strings` resolves from
  `omp_get_max_threads()` and needs only `_OPENMP`.
- Negative control: a normal build reports zero skips; invert the predicate once to see the expected
  tests skip.
- A test that merely USES threads is not guarded.
- The test must still COMPILE serially: every `omp_*` reference needs its own `#ifdef _OPENMP` with
  a serial arm (`check_openmp_calls_are_guarded`, `fortran-gotchas.md`).

## Mutation testing

Break the code deliberately and confirm the test fails, for anything whose failure is silent (a
fast path, a short-circuit, a cache, a guard).

- **NEVER restore a mutation with `git checkout`** — the tree is normally uncommitted, so that
  discards the feature. Snapshot the files to the scratch directory and restore from the copy;
  verify the harness's restore before its first round. A generated file is mutated in its output,
  never in the generator.
- Detect an abort, not only a failed check: `error stop` → nonzero, SIGABRT → 134, SIGSEGV →
  139/11.
- Check which path the test reaches: a comparator mutation survives when both sides use the same
  comparator or a counting fast path never calls it (`feature_risks.md` Risk-35); a size threshold
  is the same trap (Risk-49) — give every gating constant a `parquet_debug_set_*` override and lower
  it in tests (`parquet_debug_set_sort_merge_min_segment`,
  `parquet_debug_set_disable_sort_counting_path`).
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
- A documentation table mirroring a source-owned list is compared against the source in both
  directions (`check_env_table_matches_the_source`); read the table's rows, not the page.
- A count in prose cannot be checked: avoid writing one where the list is the point; where a count
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
