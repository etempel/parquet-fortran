# Running and reading a nagfor build

nagfor is the only compiler here that reports unused imports and unused variables by default, and
the only one with real runtime checking. Its output needs triage rather than a read-through. The
codegen traps themselves are in `fortran-gotchas.md`; machine activation is in
`tools/developer_environments.md`.

## Running it

- Put `tools/nagfor_fpm_shim/nagfor` on `PATH` (`export PATH="$PWD/tools/nagfor_fpm_shim:$PATH"`).
  The shim supplies `-openmp` on every invocation (fpm's compile-side probe fails on `-fPIC`, so fpm
  puts the flag on the link line only); without it every threading test skips and a green run says
  nothing about the parallel paths. `NAGFOR_OMP=0` is the only supported serial NAG build; it STRIPS
  any `-openmp` passed by a profile or `--flag`.
- **`fpm --verbose` and `--show-model` never show the shim's rewrites**; verify by behaviour (run
  `string_parallel` and read the skip count; flip `NAGFOR_OMP=0` and watch it move).
- Use `--verbose` — fpm prints compiler diagnostics in no other mode — and an own build tree:
  `FPM_BUILD_DIR=test_run/nag-<purpose> fpm test --verbose --flag "-colour -w=unused"`.
  `-w=unused` clears the unused-name noise when reading other classes.
- `nagfor` needs `FPM_CC`/`FPM_CXX` set explicitly.
- Profiles: `nag` (the `nagfor` feature: `-f2018 -nan -colour -w=unused -Warn=...`), `nagdeb`
  (`-C=all -g` on top), `nagundef` (`-C=undefined -DUNDEFINED_CHECK` on top). All three are `-O0`;
  `--profile release` is the only optimised NAG build and must be run as its own check.

## Unused-import warnings: mostly false positives

Two classes dominate and are invisible to NAG by construction:

- **A name reached by HOST ASSOCIATION from an ancestor** — every `parquet_read_*`, `parquet_write_*`,
  `parquet_metadata_base/_get`, `parquet_tables_*`, `parquet_sorting_*`, `parquet_columns_*` file
  has no `use` at all; the spec above imports on the subtree's behalf.
- **A name used only inside one arm of an `#ifdef _OPENMP`**: a serial build flags the OpenMP arm,
  a shim build flags the `#ifndef _OPENMP` arm (`test/test_errors.f90`'s `skip_test`). Removing
  either breaks the other build, which that NAG run cannot see.

**Remove a flagged name only when all three hold**: NAG flags it; it is absent from that file's own
code with comments *and string literals* stripped and BOTH arms of every `#ifdef` included; and no
descendant needs it through that scope. Solve the third GLOBALLY — removals interact (a name dead in
every ancestor's own text can still be needed by a leaf that imports nothing; keep it in exactly one
ancestor). Ground truth is a build with EACH compiler: gfortran compiles the `_OPENMP` arms, a
serial NAG build the other. `parquet_tables.f90`, `parquet_sorting.f90`, `parquet_columns.f90` are
generated — edit the generator. A residue of flagged imports is the healthy state.

## Other diagnostic classes

Correct-by-design, do not "fix":

- `Last statement of DO loop body is an unconditional RETURN/EXIT/ERROR STOP` — the `cycle`-guard
  search idiom.
- `Expression in OpenMP IF clause is always .FALSE.` — `!$omp parallel if(.false.)` in
  `test/test_sorting.f90` deliberately opens an inactive region (`feature_risks.md` Risk-104).
- `CONTINUE statement with no label` — the project's spelling of a deliberately empty branch.
- `Non-standard intrinsic module OMP_LIB` — inherent to OpenMP.

**`Questionable: Variable X set but never referenced` is read every time**: a genuine dead store
(remove), an unavoidable one (a function whose result must be assigned somewhere), or a **missing
assertion** (a captured `parquet_debug_get_*` never checked; a discarded `setenv` status).

**`-thread_safe` is a census, not a gate** (the `nag` profile passes it; nothing silences it, not
even a correct lock). Triage by the scope the message names: a PROCEDURE scope (host locals) and no
scope at all (a dummy passed on) are fine; only a MODULE scope is worth reading, and it splits into
the `cfg_*` knobs, the `parquet_debug_*` overrides, and a few genuine runtime counters
(`seed_call_counter`, `affinity_clamp_claims` — re-derive). One command:

```bash
grep '^Questionable:' <log> | grep 'thread-safe' | grep -oE 'from scope [A-Z0-9_]+' | sort | uniq -c | sort -rn
```

## The checked profiles

- **`nagdeb` (`-C=all -g`) is the checked profile; bare `-C=all` plus the rest is not.** `-C=all`
  already implies `-C=intovf` (NAG's `-ftrapv`); `src/parquet_random.f90`'s nagfor arm
  (`PF_SAFE64`) is overflow-free so it trips nothing. `-C=dangling` and `-C=calls` are in the set.
- **Run the WHOLE runner** (`fpm test --profile nagdeb`), at `OMP_NUM_THREADS=1`, and expect a
  queue: a checked build stops at the first failure, so one defect hides every later one. It must
  stay green end to end.
- **`-C=undefined` is excluded from `nagdeb` because it miscompiles every `bind(C)` call**
  (silently: the instrumented ABI interleaves definedness maps into a call with a plain C
  signature). No workaround exists. It runs only through the undef-safe runners: **use
  `tools/check_nag_undefined.sh`**, which builds `run_tester_pf` and `run_tester` under
  `--profile nagundef`, runs both, asserts the suite count it covered, and re-tries the
  `run_tester_noundef` list without moving anything. Read the coverage from the script's output.
  One suite: `fpm test run_tester_pf --profile nagundef -- <suite>`. Run it after adding a suite to
  a runner — one new module can take the whole gate down (the vacuity guard then reports
  `ran 0 suite(s) ... this run proves nothing`). `check_test_runner_partition` keeps the runner
  split honest.
- **Under `UNDEFINED_CHECK`** (`nagundefined` feature only) `ensure_capacity` DEFINES the value bytes
  of null rows with POISON (`huge()`, a quiet NaN from `ieee_value` — never a `transfer` bit
  pattern, which nagfor constant-folds and rejects — `.true.`), never zero; the shipped path is
  textually unchanged (preprocess with and without the macro and diff).
- The `sorting` suite is parked: `pf_sort` on a `character` array segfaults inside NAG's own
  instrumentation (`extract_chr`, null definedness-map base); no source change dodges it, and it
  does not reduce to a synthetic reproducer.
- **Under test-drive's per-suite parallelism the last `Starting <test>` line is NOT the test that
  aborted.** Get the real one from a backtrace
  (`lldb -b -o "breakpoint set -n __NAGf90_rtcrash" -o run …`, `thread backtrace all`) or re-run
  with `OMP_NUM_THREADS=1`. The abort names the handle binding (`check_handle`), not the `%view`
  call that produced a dangling handle; grep the scenario for `%view`.

## Reading a NAG finding

- A flag blamed for a failure is a hypothesis; "it passes without the flag" is exactly what a real
  defect only a checked build can see looks like.
- A clean `nagundef` run says nothing about the I/O unit-table race (it fails only under
  `nag`/`nagdeb`); a green `nag` run says nothing about the optimiser (`--profile release`).
