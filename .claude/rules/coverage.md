---
paths:
  - "tools/coverage.sh"
  - "tools/coverage_cpp.sh"
  - ".gitlab-ci.yml"
  - "src/parquet_wrapper.cpp"
---
# Coverage

## Measuring

- `tools/coverage.sh [suite]` reports per-file and total `src/` line coverage plus uncovered
  ranges; no argument runs everything including every error scenario. It selects the `gcov`
  matching the active `gfortran` (a mismatch fails with `Invalid .gcno file!`), runs `fpm clean`
  first, passes `-fprofile-update=atomic` (test-drive's concurrent tests otherwise lose counter
  increments and report a covered line as uncovered), and runs the error scenarios serially
  (concurrent `.gcda` merges corrupt the file; `RUN_ERROR_SCENARIOS_JOBS` overrides). Keep both.
- Noise can only LOSE counts: a line reported covered in any run is covered; compare two runs
  before chasing a line.
- **`run_tester_cpp` fails roughly one run in fifteen under the INSTRUMENTED build, and has never
  been seen to fail outside it.** Measured 2026-09-08: 2 failures in ~29 instrumented runs against
  0 in every ordinary-build run of the same tree (3 consecutive full runs, plus every gate run of
  P7 and P8). The two were not the same failure — one exited 134 inside `table_container` printing
  `parquet_read_int32_column_chunk: type mismatch for column: ragged`, the other exited 1 with no
  message and no `[FAILED]` line — and neither reproduces on demand. **No root cause; do not
  assume one.** What was checked and ruled out: `list_columns=` is a per-call argument, not a
  process-global, so no setting bleeds between concurrent tests; `coverage.sh` runs the five
  runners in a plain sequential loop, so interleaved output from another runner is not possible
  and the qc warnings that appeared just before the abort are late-flushed stderr from earlier in
  the same runner; and no suite in `run_tester_cpp` spawns the `*_ragged_list_column` scenarios
  (only `test_errors` does, in `run_tester_errors`), so the message was NOT a child scenario's
  expected abort leaking into the log.
- **So a single failed coverage run is not evidence of a defect, and not evidence of its absence:
  re-run before acting on one**, and capture the failing run's whole output when it happens — a
  frequency without the message is nearly useless, and the second occurrence above was lost that
  way. A percentage from a run that aborted partway is not a measurement; discard it.
- `src/parquet_wrapper.cpp` is measured by CI's `test` job (matched apt GCC) and locally by the
  separate `tools/coverage_cpp.sh` (a dev machine's `clang++` gcov data is unreadable by GNU
  `gcov`); the two cannot share one pass.
- Both scripts report `GCOVR_EXCL`'d lines with positive hits (stale-exclusion candidates);
  `coverage_cpp.sh` also lists GCC-artifact-tagged lines that unexpectedly show zero hits.
- CI pins `gcovr>=7.1,<8.4` via `pipx`: below 7.1 the parser crashes on a 10,000+ line file
  (`UnknownLineType`, gcovr #882); 8.4+ drops every module-contained Fortran subroutine (gcovr
  #1253, an almost empty table on a clean run). Check gcovr's issue tracker before assuming a new
  crash or an empty table is this project's bug.

## Closing gaps

Procedure: the `/coverage-gaps` skill.

- An `error stop`/abort line is covered only by an out-of-process scenario (`testing.md`); a normal
  branch by extending a test.
- Not worth chasing: `end module`/`end submodule` lines, implicit finalizers, interface-only files
  (`parquet_bindings.f90`).
- A Fortran pre-check (`parquet_check_read_row_count`, `check_column_exists`) often makes a C++
  defensive branch dead; grep the Fortran call site before writing a test for an uncovered
  `report_fatal_error`/`throw`. Decimal columns always read back as `Decimal128Array`/
  `Decimal256Array`, so the `Decimal32`/`Decimal64` arms in `decimal_value_at`/
  `decimal_to_int64_checked` are permanently dead.
- **A line inside an exclusion that is actually reachable in normal operation: stop and tell the
  user**; do not silently keep or remove the marker.

## Coverage tooling never drives design

A tool's limitation is never a reason to write code differently — not a procedure becoming
`elemental`, not a rewrite moving lines, not a `GCOVR_EXCL` marker having to be added, not a
percentage moving. Record the artifact with the conventions below; never wave a genuine gap away as
an artifact without the evidence (a covered surrounding body).

## Fortran gcov attribution artifacts

Confirmed shapes where gcov attributes a line to the wrong place -- marking an excluded, dead line
as hit, or a line that demonstrably ran as unhit:

- a guard-clause `if (cond) then` inside a `GCOVR_EXCL` block (the condition is evaluated on every
  call; the body shows 0);
- a bare `return` after an `errmsg = ...` in a never-taken branch (the sibling line shows 0);
- CI-only 0% on the first executable statement of an abbreviated `module procedure` body, 100%
  locally, reproducible across ≥2 CI runs (`parquet_load_maml_file`);
- 0% on an `allocate` of a pointer to a type with allocatable or default-initialised components,
  while the procedure header and every later line show the same positive count and no branch
  separates them (`new_impl`, `src/parquet_toml.f90`); an `allocate` whose target has no such
  components is attributed normally on the next line.

Also: six `impure elemental` headers in `src/parquet_temporal.f90` never register as hit while
every body line does; excluded with a comment, and a new header with the same "0% but body covered"
pattern is treated the same way.

Tag a confirmed site with the literal phrase `gcov attribution artifact`, inline on the
`GCOVR_EXCL_START`/`GCOVR_EXCL_LINE` line or on a comment line directly above it (132-column
limit); `tools/coverage.sh`'s `gcovr_artifact_lines()` drops those from the stale-exclusion report.
Verify the raw per-line hit counts first; a genuinely stale exclusion (the code is now reachable)
has its marker removed instead.

## `src/parquet_wrapper.cpp`: GCC vs Clang gcov attribution

GCC's gcov and `llvm-cov gcov` attribute hits differently for `case`/`default:` labels, a closing
`}` after `return`, a lambda's parameter line, and continuation lines of a chained statement, so CI
and a local `coverage_cpp.sh` disagree on the same commit. Conventions:

- `GCOVR_EXCL_STOP` is on its own comment-only line, never trailing code (real gcovr does not
  exclude a `STOP` line carrying code; the local script does, so it passes locally and fails in CI).
- A `case`/`catch`/`default:` label directly outside a `START`/`STOP` block is moved inside it.
- An exclusion for a covered-but-misattributed line carries `gcov attribution artifact under GCC`
  on the SAME physical line as its marker (`coverage_cpp.sh` categorises by that substring).
- Anything that skips `atexit` (`fatal_exit`, `std::abort`, an uncaught exception into
  `std::terminate`) discards ALL coverage for that process. `.gitlab-ci.yml`'s gcovr and both
  scripts auto-exclude standalone `report_fatal_error(...)` lines; a helper called just before an
  abort needs its own reasoning.
