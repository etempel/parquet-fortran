---
name: nag-build
description: "Build and test this repository with nagfor (the nag, nagdeb, nagundef and release profiles) and triage its diagnostics. Use for any NAG run or when reading NAG warnings."
argument-hint: "[profile] [suite]"
allowed-tools: Bash(fpm:*), Bash(git:*), Bash(grep:*), Bash(nagfor:*), Bash(tools/:*), Bash(ls:*), Bash(cat:*), Bash(sed:*), Bash(awk:*), Bash(sort:*), Bash(uniq:*), Bash(which:*)
disable-model-invocation: true
---

# /nag-build — run and read a nagfor build

nagfor is the only compiler here that reports unused imports and variables by default and the
only one with real runtime checking; its output needs triage, not a read-through. The codegen traps
are in `.claude/rules/fortran-gotchas.md`; the machines and activation commands in
`.claude/rules/developer-environments.md` — read that first. Arguments: $ARGUMENTS

Injected context:

- `FPM_FC`: !`echo "${FPM_FC:-unset}"`
- `nagfor` on `PATH`: !`which nagfor 2>/dev/null || echo "not found"`
- Version: !`nagfor -V 2>&1 | head -1 || true`
- `NAGFOR_OMP`: !`echo "${NAGFOR_OMP:-unset}"`
- Working tree: !`git status --short | head -10`

## 1 Activate and verify

- `export PATH="$PWD/tools/nagfor_fpm_shim:$PATH"`; `which nagfor` must be the shim. It supplies
  `-openmp` on every invocation (fpm's compile-side probe fails on `-fPIC`, so fpm puts the flag on
  the link line only); without it every threading test skips and a green run says nothing about
  the parallel paths. `NAGFOR_OMP=0` is the only supported serial build; it STRIPS any `-openmp`
  a profile or `--flag` passes.
- `FPM_CC`/`FPM_CXX` must be set explicitly; check `nagfor -V` and `$FPM_FC`. `NAGFOR_OMP=1` left
  over from an earlier shell is not evidence a NAG environment is active.
- **`fpm --verbose` and `--show-model` never show the shim's rewrites.** Verify by behaviour: run
  `string_parallel` and read the skip count (0 threaded); flip `NAGFOR_OMP=0` and watch it move.

## 2 Choose the profile

| profile | flags | sees |
|---|---|---|
| `nag` | `-f2018 -nan -colour -w=unused -Warn=...`, `-O0` | unused names, `-nan` poisoning, `-thread_safe`-class notes |
| `nagdeb` | `nag` + `-C=all -g` (`-C=intovf`, `-C=dangling`, `-C=calls` included) | every runtime check but undefined-variable |
| `nagundef` | `nagdeb` + `-C=undefined -DUNDEFINED_CHECK` | undefined variables, on the undef-safe runners only |
| `release` | optimised | the ONLY NAG configuration that can see a codegen defect |

All but `release` are `-O0`; a green `nag`/`nagdeb` run says nothing about the optimiser, and a
`release` hang is read as a possible miscompilation (`sample <pid>` names the procedure).

## 3 Run

- Own build tree, named by compiler and configuration:
  `FPM_BUILD_DIR=test_run/nag-<config> fpm test --verbose --profile <profile> --flag "-colour -w=unused"`.
  `--verbose` is required; fpm prints compiler diagnostics in no other mode. `-w=unused` clears
  the unused-name noise when reading the other classes.
- **`nagdeb`: run the WHOLE runner at `OMP_NUM_THREADS=1`** (`fpm test --profile nagdeb`), and
  expect a queue: a checked build stops at the first failure, so one defect hides every later one.
  It must stay green end to end.
- **`nagundef`: use `tools/check_nag_undefined.sh`**, which builds `run_tester_pf` and `run_tester`
  under `--profile nagundef`, runs both in full, asserts the suite count it covered, re-tries the
  `run_tester_noundef` list without moving anything, and exports the `OMP_STACKSIZE` the profile
  needs (`-C=undefined` inflates every frame; `healpix_tier_b` needs about 1 MB of OpenMP worker
  stack, and without it the run dies with no message and fpm reports `exit code 10`). One suite:
  `fpm test run_tester_pf --profile nagundef -- <suite>`. Run it after adding a suite to a runner:
  one new module can take the whole gate down, and the vacuity guard then reports
  `ran 0 suite(s) ... this run proves nothing`. `run_tester_noundef`'s membership is decided by a
  person after reading why a result changed, never moved by the tool.
- `release`: `fpm test --profile release` as its own check.

## 4 Triage the diagnostics

**`Panic:` and `Internal Error -- please report this bug` outrank every class below and are
reported to the maintainer in the same reply**: `.claude/rules/fortran-gotchas.md`, nagfor section.

**Unused-import warnings are mostly false positives**, in two classes NAG cannot see: a name
reached by HOST ASSOCIATION from an ancestor (every `parquet_read_*`, `parquet_write_*`,
`parquet_metadata_base/_get`, `parquet_tables_*`, `parquet_sorting_*`, `parquet_columns_*` file has
no `use` of its own), and a name used only inside one arm of an `#ifdef _OPENMP` (a serial build
flags the OpenMP arm; a shim build flags the `#ifndef _OPENMP` arm, such as `test/test_errors.f90`'s
`skip_test`). Remove a flagged name only when all three hold: NAG flags it; it is absent from that
file's own code with comments and string literals stripped and BOTH arms of every `#ifdef`
included; no descendant needs it through that scope. Solve the third globally (removals interact;
keep a name in exactly one ancestor), verify with a build under each compiler (gfortran compiles
the `_OPENMP` arms, serial NAG the other), and edit a generator rather than a generated file. A
residue of flagged imports is the healthy state.

**Correct by design, do not "fix"**: `Last statement of DO loop body is an unconditional
RETURN/EXIT/ERROR STOP` (the `cycle`-guard search idiom); `Expression in OpenMP IF clause is always
.FALSE.` (`test_nested_team_guard`, `test/test_sorting.f90`, opens an inactive region on
purpose); `CONTINUE statement with no label` (a deliberately empty branch); `Non-standard
intrinsic module OMP_LIB`.

**`Questionable: Variable X set but never referenced` is read every time**: a dead store (remove),
an unavoidable one (a function result that must be assigned somewhere), or a missing assertion (a
captured `parquet_debug_get_*` never checked, a discarded `setenv` status).

**`-thread_safe` is a census, not a gate**; nothing silences it, not even a correct lock. A
PROCEDURE scope or no scope at all is fine; only a MODULE scope is worth reading, and it splits into
the `cfg_*` knobs, the `parquet_debug_*` overrides and a few genuine runtime counters
(`seed_call_counter`, `affinity_clamp_claims`; re-derive):

```bash
grep '^Questionable:' <log> | grep 'thread-safe' | grep -oE 'from scope [A-Z0-9_]+' | sort | uniq -c | sort -rn
```

## 5 Checked-profile facts

- `-C=all` already implies `-C=intovf`; `src/parquet_random.f90`'s nagfor arm (`PF_SAFE64`) is
  overflow-free, so it trips nothing.
- **`-C=undefined` miscompiles every `bind(C)` call, silently** (the instrumented ABI interleaves
  definedness maps into a call with a plain C signature); no workaround exists, which is why it
  runs only through the undef-safe runners and `check_test_runner_partition` keeps the split
  honest.
- Under `UNDEFINED_CHECK` (the `nagundefined` feature only) `ensure_capacity` DEFINES null rows'
  value bytes with POISON — `huge()`, a quiet NaN from `ieee_value` (never a `transfer` bit pattern,
  which nagfor constant-folds and rejects), `.true.` — never zero; the shipped path is textually
  unchanged (preprocess with and without the macro and diff).
- The `sorting` suite is parked in `run_tester_noundef`: `pf_sort` on a `character` array
  segfaults inside NAG's own instrumentation (`extract_chr`); no source change dodges it.
- **Under test-drive's per-suite parallelism the last `Starting <test>` line is NOT the test that
  aborted.** Get the real one from a backtrace
  (`lldb -b -o "breakpoint set -n __NAGf90_rtcrash" -o run …`, `thread backtrace all`) or re-run at
  `OMP_NUM_THREADS=1`. A dangling-handle abort names the binding (`check_handle`), not the `%view`
  call that produced the handle.
- A flag blamed for a failure is a hypothesis; "it passes without the flag" is exactly what a real
  defect only a checked build can see looks like. A clean `nagundef` run says nothing about the I/O
  unit-table race (it fails only under `nag`/`nagdeb`).

## 6 Report

Profile, thread count, machine, what ran and what was not run; findings classified by the classes
above with the genuine ones first; source edits proposed, not applied, unless the invocation asked
for fixes.
