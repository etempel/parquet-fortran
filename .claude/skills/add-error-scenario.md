---
name: add-error-scenario
description: "Add an out-of-process error scenario for an abort path — the case in test/error_scenarios.f90, its test-drive wrapper, its entry in tools/run_error_scenarios.sh — and prove it fails against a broken implementation."
argument-hint: "<scenario name and the abort it must provoke>"
allowed-tools: Bash(fpm:*), Bash(git:*), Bash(grep:*), Bash(find:*), Bash(ls:*), Bash(cat:*), Bash(sed:*), Bash(awk:*), Bash(cp:*), Bash(diff:*), Bash(python3:*), Bash(tools/:*), Bash(build/:*), Bash(test_run/:*)
disable-model-invocation: true
---

# /add-error-scenario — test an abort path out of process

An `error stop` kills the test runner, so an abort path is tested by a scenario program that
provokes it and a test-drive test that asserts the exit and the message. The rules are in
`.claude/rules/testing.md`; this is the procedure. Request: $ARGUMENTS

Injected context:

- Scenario count: !`grep -cE '^\s*case \("' test/error_scenarios.f90`
- Scenario list line: !`grep -n '^scenarios=(' tools/run_error_scenarios.sh`
- Working tree: !`git status --short | head -10`

## 1 Name and place

- Name: snake_case, naming the abort (`write_undeclared_column`). Check it is unused:
  `grep -n 'case ("<name>")' test/error_scenarios.f90`.
- The abort's area decides the wrapper file: `test/test_writing_errors.f90` (`writing_errors`),
  `test/test_reading_errors.f90`, `test/test_maml_errors.f90`, `test/test_metadata_errors.f90`;
  everything else `test/test_errors.f90` (`errors`). All five suites run under
  `run_tester_errors`, the only runner that forks.

## 2 The scenario (`test/error_scenarios.f90`)

- Add `case ("<name>")` calling `scenario_<name>()` to the `select case (trim(scenario))` at the
  top, and the subroutine beside its siblings.
- The smallest fixture that provokes exactly one abort. Every fixture path is under `test_run/`
  and unique to the scenario (`test_run/error_scenario_<name>.parquet`); a helper backing several
  `case` entries derives its path from its arguments, never from a `parameter` — scenarios run
  concurrently, one process per name.
- A `parquet_debug_*` hook is reached through a local `bind(C)` interface declared inside the
  scenario, never through `src/parquet_bindings.f90`; make the guarded call once with the hook
  clear before arming it (the negative control).
- An abort inside a `pure` function: USE the result (print it in the trailing message), or the
  call is deleted at `-O1`+ and the scenario exits 0.
- Print a "... was accepted" line after the call that should have aborted, so a missing abort is
  visible in the output.
- A guard that needs OpenMP: the scenario goes in the `concurrency_scenarios` bucket (step 4) and
  its wrapper skips under `#ifndef _OPENMP`.

## 3 The wrapper

- Register `new_unittest("<what aborts>", test_<name>_aborts)` in the suite's registration array.
  In `test/test_errors.f90` the array is built in parts `p1 … pN`, because one statement may carry
  at most 255 continuation lines: add a new part rather than growing one, open it after the
  previous part's `]`, add it to the concatenation, and close a split part by removing the comma,
  not the trailing `&`.
- The test body:

```fortran
call check_scenario_exit_status_and_stderr(error, "<name>", expect_abort=.true., &
    failure_message="<what> was expected to error stop", &
    required_stderr="<the library's own message text, with its file/schema suffix>")
```

  Assert the library's message, never the runtime's `ERROR STOP` prefix and never a particular
  exit code (`check_scenario_exit_status` for status only; `/= 134` where the point is a Fortran
  abort versus the C++ `fatal_exit`). A control that must exit cleanly uses
  `expect_abort=.false.`.

## 4 Register in `tools/run_error_scenarios.sh`

Add the name to `scenarios=(...)`, or to `concurrency_scenarios=(...)` for an OpenMP-dependent one.
`check_scenario_list_is_complete` fails lint otherwise, and an unlisted scenario silently falls back
to on-demand spawning.

## 5 Verify

1. `fpm build --tests` (a plain `fpm build` does not build test targets).
2. By hand: `$(find build -type f -name error_scenarios | head -n 1) <name>` — nonzero status and
   the message on stderr; the `ok` control exits 0. More than one binary found: `fpm clean --skip`
   first.
3. The wrapper: `PARQUET_TEST_NO_PRIME=1 fpm test run_tester_errors -- <suite> "<description>"`.
4. **Prove it**: copy the guarded source file to the session scratchpad, disable the guard, rebuild
   with `fpm clean --skip` and confirm the test FAILS (the scenario prints "accepted"); restore from
   the copy (never `git checkout`), `fpm clean --skip`, confirm green. A new scenario that passes
   first time has not been shown to test anything.
5. `python3 tools/check_source_conventions.py` (`check_scenario_list_is_complete`,
   `check_statement_continuation_lines`, `check_scenario_uses_a_pure_result`,
   `check_test_fixtures_live_under_test_run`).
6. One full `fpm test`: priming runs every scenario in parallel, and a fixture collision shows only
   there.

## 6 Report

Scenario name, suite, the message asserted, the mutation that proved it, the checks run. If the
abort guards a silent-failure property, propose a `feature_risks.md` entry.
