---
paths:
  - "bench/**/*"
  - "tools/benchmark_random_kernels.f90"
  - "tools/machine_report.sh"
  - "tools/fpm_lto.sh"
  - "feature_benchmark*.md"
---
# Benchmarking and measurement

## Where benchmark tools live

- A check needing more memory, disk or time than `fpm test` should attempt goes under `bench/`:
  the program and its thin `*.sh` wrapper side by side, env-var config forwarded as `--key=value`
  flags (`get_command_argument` in Fortran; `NAME="${NAME:-default}"` then
  `fpm run <app> -- --key="$NAME"`; `set -euo pipefail`; `cd` to the repo root). Never under
  `test/` (auto-run) or `app/` (ships only the one program). Usage goes in the script's own header.
- `bench/` is an fpm source dir, so everything `.f90` in it is built by fpm; a driver needing a bare
  compiler with forced flags stays in `tools/` (`code-style.md`). `bench/benchmark_random_kernels.sh`
  drives `tools/benchmark_random_kernels.f90` deliberately.

## Cross-machine campaigns

- The `/plan-benchmark` skill writes the run sheet (`feature_benchmark_<name>.md`, one copy per
  machine, carried by hand) and carries the runner's steps and the rules every run follows; never
  write a run sheet from scratch. Machine facts are in `developer-environments.md`.
- Everything the run EXECUTES lives in the repository under `bench/`; never paste code into the
  instruction file. **The MAINTAINER commits and pushes it before the campaign starts** — the other
  machines pull it, and `workflow.md`'s no-commit rule is not lifted here (`.claude/settings.json`
  denies `git push` outright). Report that the campaign is blocked on that push rather than
  attempting one.
- Verdicts can agree everywhere while magnitudes span 3–6x and a sub-conclusion inverts; a
  single-machine run does not size work. Read every returned copy side by side, once.
- Tooling fixed mid-campaign invalidates every figure taken before the fix.

## Measurement rules

- **Warm the data before timing** on any lazy API (`%prefetch`/`%materialize_all` first), and warm
  the result array's pages (write once through every path before the timed loop).
- **Always `--profile release`, and VERIFY optimisation for both halves.** fpm has no release
  flags for flang or nagfor's C++ half: check
  `fpm build --profile release --show-model | grep -o 'fortran_compile_flags="[^"]*"'` and the
  `cxx_compile_flags` line; a wrapper whose timed work reaches C++ asserts `-O` in both
  (`bench/benchmark_colindex.sh`) and refuses otherwise. Recovery is `FPM_FFLAGS="${FPM_FFLAGS:-} -O3"`
  / `FPM_CXXFLAGS="${FPM_CXXFLAGS:-} -O3"`, appended never assigned, and said in the report. "No
  `-O`" means `-O0` for gfortran/flang but the compiler's own default (`-O2`) for ifx/icx; a check
  carries a short evidence-based list of such compilers. A `SKIP_OPT_CHECK` override measures a
  different configuration from an appended `-O3`.
- **Per-element costs are measured with a compile-out ladder**, not phase timers (a
  `steady_clock` pair costs more than a per-cell accessor). One cpp macro per phase, one binary
  per rung, whole-operation timing; each rung's difference from baseline is an UPPER BOUND on that
  phase (removal over-reports). Rules: cross-build floor; scaffolding on a throwaway branch; the
  harness prints which rung it is; run the suite on the no-macro build; apply scaffolding with a
  script that validates every anchor and revert with `git checkout` (`bench/bench_resolve_ladder.py`).
- Two differently shaped modes over one build: a real change moves both in the same direction; a
  sign disagreement is unresolvable. A variant changing timing but not the checksum measures pure
  overhead; a variant whose checksum moves is timing-only.
- A flag-selected benchmark PRINTS its variant (`guard_variant()`) and carries a control arm the
  flag cannot touch.
- Index arithmetic in a timed loop is code: `mod` with a runtime divisor is an `idivq` (~6 ns);
  use a wrapping counter. Differences between arms survive such a defect; ratios against a floor do
  not.
- A REBUILD noise floor (two builds of the same source, untouched control arm) is far larger than a
  re-run floor and is a property of the pair of builds; measure it per campaign, never carry it
  forward. A two-commit A/B is a quieter instrument than a macro ladder.
- After a fix re-run the DIAGNOSTIC that found the problem, not only the end-to-end number; a
  prediction chained from removal-based estimates is fragile.
- A benchmark that REPLICATES library code is untested; require one row to reproduce an
  end-to-end figure before believing the others.
- Sweep the input SHAPE, not only its size (a ranking can invert); a sweep must ENGAGE the
  mechanism under test (work out the condition under which it is active; prove the effect in both
  directions with a `parquet_debug_set_*` override).
- A fixture that must be unordered uses `pf_random_permutation` (a multiplicative walk modulo `n`
  is not a shuffle and the statistics screen finds its structure); read the pruned count the
  benchmark prints.
- On a table larger than cache, hash a block of keys first and walk afterwards
  (`ix_probe_1_block`); `%probe_stats` separates a hash defect from a memory-parallelism one.
- Measure a restructure's SERIAL cost before letting the threaded form replace the original.
- A long sweep drifts: re-measure any tail figure in isolation before calling it a regression;
  check that two shapes that ought to be identical measured identically.
- Best of N rounds; never measure immediately after a heavy phase (a test suite between two arms
  biases the second); measure the arms back to back and quote the floor.
- Compare modes doing the SAME job: a `%get` allocates and copies; keep the keep-it-live checksum
  outside the timed region and take it through `%col`.
- A reference must be allocation-free (both buffers allocated and fully written before the timer);
  a thing measuring FASTER than its "floor" means the floor is wrong.
- Baseline vs working tree: `git stash push -- src tools`, rebuild, measure, `git stash pop`, and
  confirm the pop with `git status`.
- Toolchains differ by more than noise (per-row loops that dominate under ifx are nearly free under
  gfortran); when a claim cannot be reproduced, suspect the compiler before the report, and record
  which one was used.

## Memory

- RSS cannot show that Arrow buffers were freed (the pool keeps the pages); measure
  `arrow::default_memory_pool()->bytes_allocated()` through `parquet_get_arrow_bytes_allocated`
  (a local `bind(C)` interface in the consumer). Measure each path in its own process, and state in
  the output which number is the answer.

## Instrumenting phases

- Before optimising a multi-phase operation, add `parquet_debug_get_*_nanos` counters
  (`steady_clock` per phase, coarse operations only, never per row or element) and leave them in.
- A phase's SHARE bounds the prize, it does not estimate it: A/B the actual change on ONE site
  before committing to many, and carry the loop's share of the end-to-end path (a loop ratio is not
  an operation ratio).
