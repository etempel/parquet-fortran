# Build and toolchain

Machine-specific facts (what each machine is, how to activate a toolchain, what each activation
leaves in `FPM_*`) live in `developer-environments.md` only. This file carries what is true
of the toolchains wherever they are installed.

## Local verification

- **Plain `fpm test`, with nothing set, is the ordinary check.** `fpm.toml`'s `openmp = "*"`
  metapackage injects `-fopenmp` into compile and link flags for gfortran and ifx (confirm with
  `fpm build --show-model | grep -o 'fortran_compile_flags="[^"]*"'`); under flang it contributes
  nothing.
- **Profile flags come only from `--profile`.** `FPM_FFLAGS` replaces what the environment exported
  (dev machines keep Arrow's `-I` paths there) and is appended to the profile's flags, never
  substituted for them. With no `--profile` there is no `-O` and no `-fcheck=bounds` on any machine.
  `fpm test --profile debug` turns on bounds and `-Wall` checks; run it at least once for anything
  touching allocation or array shapes.
- Never assign `FPM_FFLAGS`/`FPM_CXXFLAGS`/`FPM_LDFLAGS` to CI's values locally (that drops Arrow's
  paths: `fatal error: 'arrow/api.h' file not found`). Append, never assign.
- Profiles in `fpm.toml`: fpm's `debug`/`release`, plus `nag`, `nagdeb`, `nagundef`
  (the `/nag-build` skill), `flangopt` (`-O3`) and `flangdeb`.
- To verify a no-OpenMP build, comment out `openmp = "*"`, build into a throwaway
  `FPM_BUILD_DIR`, confirm from `--show-model` that no `-fopenmp` is present, restore. Every skip
  in that run should name a threading assertion.

## Stale build cache

- **`fpm build` does not build `test/`**; use `fpm build --tests` before running a test binary by
  hand. A `print` marker on the first executable statement of a block not appearing means the
  binary is old.
- `fpm clean --skip` forces a clean rebuild without touching dependencies. Reach for it first when a
  gate change appears not to take effect, when a mutation's symptom persists after the source was
  restored (fpm misses a RESTORED file, Fortran and C++ alike), and after building with more than
  one `FPM_FFLAGS` value in a session.
- **`fpm clean` ignores `FPM_BUILD_DIR`** (fpm 0.13.0): it removes every `build/<compiler>_<hash>/`
  tree, of every compiler and profile, and leaves the tree `FPM_BUILD_DIR` names as it was. Clear
  a named tree with `rm -rf` on it instead.
- Several `build/<compiler>_<hash>/` trees exist after builds with different flags.
  `tools/run_error_scenarios.sh` resolves its binary with `find … -name error_scenarios | head -n 1`,
  so a green run there is meaningful only when `find build -type f -name error_scenarios | wc -l`
  is 1. `test/test_errors.f90` derives its sibling binary from argument 0 instead.
- Before each mutation-testing round delete the test binary
  (`find build -name run_tester -type f -delete`) or `fpm clean --skip`; "Project is up to date"
  can be wrong.
- `tools/coverage.sh`/`tools/coverage_cpp.sh` delete their own `build/gcov*` trees on exit
  (`COVERAGE_KEEP_BUILD=1` keeps them; then delete before the next plain `fpm test`).
- A wrapper building several configurations gives each its own `FPM_BUILD_DIR` outside `build/`,
  named for BOTH the compiler and the configuration (`check_build_tree_names_carry_the_compiler`).

## Hand-run compiles

- Always pass `-J<dir>` when compiling by hand from the repo root, or compile from a temporary
  directory: a stray root `.mod` (hidden by a global gitignore) shadows every standalone check's
  own module output. `ls *.mod *.smod` is the one-command check when a standalone build fails for
  no reason.
- A `tools/*.sh` script that compiles sources runs the compiler from its own work directory with
  absolute source paths (`(cd "$WORK" && $FC ... $ABS_SRC)`).

## Compilers and platforms

- Name the machine and the toolchain on every performance figure; `tools/machine_report.sh`
  prints both. On macOS the C++ half is Apple clang unless `FPM_CXX` says otherwise (fpm derives it
  from the Fortran compiler's family), so the normal macOS build is mixed-family; read the report's
  `fc`/`cc` lines rather than assuming a matched pair.
- **flang is serial-only on both macOS machines** (MacPorts ships no `omp_lib.mod`), and the
  metapackage passes it no `-fopenmp`, so every OpenMP block is preprocessed away: the whole project
  builds and `fpm test` passes serially with the threading tests skipping. A flang run says nothing
  about the parallel paths. `--profile release` does not link under flang (`-flto`,
  `LLVM ERROR: Unsupported stack probing method`); use the default profile or `--profile flangopt`.
- "Compiles and links", "runs the suite" and "is supported" are three different claims; say which.

## LTO

- Link flags: `-flto` for gfortran + g++, `-ipo` for ifx + icpx (needs `ld.lld`, resolved relative
  to `ifx` as `"$(dirname "$(command -v ifx)")/compiler/ld.lld"`, never trusted from `PATH`), and
  `-flto` for flang + clang++.
- **An LTO build across the static library needs a plugin-capable archiver, or it silently does
  nothing**: `gcc-ar` (`FPM_AR=gcc-ar-mp-<N>` on macOS, whose default `ar` cannot load the plugin).
  `tools/fpm_lto.sh` selects the archiver per family and refuses to build when it cannot;
  `tools/machine_report.sh` reports which archivers exist. On Intel there is no `xiar` (retired for
  `llvm-ar`); `ld.lld` is the load-bearing half there.
- Check the archive before reporting any LTO result in either direction: a larger archive still
  full of ordinary text symbols under `nm` means no LTO happened.
- **A mixed-family build cannot do cross-language LTO** (GCC GIMPLE vs LLVM bitcode; check with
  `file` and `otool -l | grep sectname`), while Fortran-internal LTO still works there. Say which of
  the two a measurement is about.
- `tools/machine_report.sh --lto-probe` proves the compilers can emit and consume LTO objects, not
  that the library links that way (it never builds an archive). A probe is evidence only if it
  reproduces the real build's structure.

## `-fPIC` and inlining on ELF

- Under `-fPIC` (what fpm passes) GCC cannot inline any module procedure across a call on ELF (a
  global symbol may be interposed); Mach-O has no such barrier, so a Linux-vs-macOS difference may
  be only this. `-fno-semantic-interposition` removes it and is reachable only by appending to
  `FPM_FFLAGS`; `-fvisibility=hidden` and a bigger inline budget do nothing.
- Do not move hot helpers into internal procedures to dodge it (far slower on macOS). A "same
  translation unit" argument for inlining is void under `-fPIC`; the accepted shape is writing the
  helper out inside the kernel with a keep-in-step comment (`ix_probe_1_block`,
  `src/parquet_index_map.f90`). ifx does not honour interposition, so measure on both compilers.
- One-command inlining check (Linux only; on macOS use `otool -tv` — `objdump` there reports no
  `R_X86_64` relocations and reads as "fully inlined"):

```bash
objdump -dr --no-show-raw-insn <obj>/src_parquet_argsort_engine.f90.o \
    | sed -n '/<.*mp_sort_partition_>:/,/^$/p' | grep R_X86_64
```
