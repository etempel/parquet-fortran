---
name: coverage-gaps
description: "Measure line coverage with tools/coverage.sh (or tools/coverage_cpp.sh), classify every uncovered range and every stale exclusion, and propose the tests, scenarios or markers that close them. Writes no test."
argument-hint: "[suite | cpp]"
allowed-tools: Bash(tools/:*), Bash(fpm:*), Bash(git:*), Bash(grep:*), Bash(find:*), Bash(ls:*), Bash(cat:*), Bash(sed:*), Bash(awk:*), Bash(python3:*), Bash(build/:*), Bash(test_run/:*)
disable-model-invocation: true
---

# /coverage-gaps — measure and triage coverage

The rules (what the scripts do, which lines are artifacts, the gcovr version bounds) are in
`.claude/rules/coverage.md`; this is the procedure. Argument: a suite name (scoped run), nothing
(full run including every error scenario, which takes minutes) or `cpp`: $ARGUMENTS

Injected context:

- gcov/gfortran: !`gfortran --version 2>/dev/null | head -1 || echo "no gfortran"`
- Leftover instrumented trees: !`ls -d build/gcov* 2>/dev/null || echo none`

## 1 Run

- Fortran: `tools/coverage.sh [suite] 2>&1 | tee test_run/coverage-<suite|full>.log`; C++:
  `tools/coverage_cpp.sh`. Both clean first, pass `-fprofile-update=atomic`, run the error
  scenarios serially and delete their `build/gcov*` tree on exit; set nothing else.
- If a number looks wrong, run twice before chasing a line: noise only ever LOSES counts, so a line
  covered in any run is covered.

## 2 Classify every uncovered range

| class | how to recognise it | action |
|---|---|---|
| abort line | `error stop`, `report_fatal_error`, `throw` | an out-of-process scenario (`/add-error-scenario`), never an in-process test |
| ordinary branch | reachable with different data or arguments | extend the existing test, named |
| defensive or unreachable | a Fortran pre-check makes the C++ branch dead; the `Decimal32`/`Decimal64` read arms; an Arrow-guaranteed invariant | `GCOVR_EXCL` with a comment saying why; not a test |
| not worth chasing | `end module`/`end submodule`, implicit finalizers, interface-only files | none |
| attribution artifact | a guard-clause `if` inside an exclusion; a bare `return` after `errmsg =`; a CI-only miss on a `module procedure`'s first statement; the temporal `impure elemental` headers; the C++ GCC-versus-Clang shapes | verify the raw per-line counts, then tag `gcov attribution artifact` per `coverage.md` |
| harness | counters lost to concurrency, a corrupted `.gcda` | re-run |

Grep the Fortran call site before writing a test for an uncovered C++ `report_fatal_error`; the
pre-check usually makes it dead.

## 3 Stale exclusions

The report's last section lists `GCOVR_EXCL`'d lines with positive hits (and, for C++, tagged
artifact lines with zero hits). For each: verify; remove the marker if the code is now genuinely
reachable, tag it if it is an artifact. **A reachable line inside an exclusion is reported to the
user, never silently kept or un-excluded.**

## 4 Report

A per-file table ordered by uncovered lines, each range with its class and its proposal (the test
and its negative control, the scenario, or the marker); the stale-exclusion verdicts; the runs
compared if two were needed. Nothing is written unless the invocation asked for it.
