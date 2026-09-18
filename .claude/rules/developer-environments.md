---
paths:
  - "bench/**/*"
  - "tools/fpm_lto.sh"
  - "tools/machine_report.sh"
  - "tools/check_random_ubsan.sh"
  - "tools/check_nag_undefined.sh"
  - "tools/nagfor_fpm_shim/**/*"
  - "feature_benchmark*.md"
---
# Developer machines

Maintainer-only: the three machines this library is built and measured on, and how to build on
each. Sole authority for machine configuration; toolchain facts that hold on any machine are in
`build.md`. The `/nag-build` and `/plan-benchmark` skills read this file first.

## The machines

| | **A — laptop** | **B — `bunyip.to.ee`** | **C — desktop** |
|---|---|---|---|
| CPU | Apple M1 Pro, 8 cores | 2 x AMD EPYC 9654, 192 physical / 384 logical, 2 sockets, 2 NUMA nodes | Intel i7-10700K, 8 physical / 16 logical |
| arch / SIMD | arm64, NEON (128-bit) | x86-64 Zen 4, AVX-512 | x86-64 Comet Lake, AVX2 (256-bit) |
| RAM | 32 GB | 1132 GB | 128 GB |
| OS | macOS 26.5 | RHEL 9.7, kernel 5.14 | macOS |
| Fortran | nagfor 7.2 (default), gfortran 15.2, flang 22.1.8 | ifx 2026.1.1; gfortran 15.2.1 (toolset-15), 14.2.1 (toolset-14), system 11.5 | nagfor 7.2 (default), gfortran 15.2, flang 22.x |
| C++ | Apple clang 21, `g++-mp-15` | icpx 2026.1.1; g++ 14.2.1 or 11.5 | Apple clang, MacPorts GCC |
| Arrow / Parquet | 25.0.0 | 24.0.0 | 25.0.0 |
| fpm | 0.13.0 alpha | 0.13.0 alpha | 0.13.0 alpha |
| use for | everyday work; quiet, so best for small deltas | ifx, threading at scale, AVX-512, gfortran-vs-ifx with everything else held constant | x86-64 twin of A: an A-vs-C comparison isolates architecture alone |

- A and C are distinguishable only by `uname -m`; `tools/machine_report.sh` prints it.
- B is on a different Arrow (24.0.0 against 25.0.0): never compare figures across it for anything
  through `src/parquet_wrapper.cpp` or an end-to-end read/write.
- B is bad for small measurements (a cold destination on a large NUMA box is dominated by page
  faults; 5.6x run-to-run at one size). Take small deltas on A.
- No machine can run the CI Docker image (A has the CLI, no daemon).

## Machines A and C (macOS): what they share

```
environment: source ~/.zprofile          # sources ~/.activate_nag.sh itself; enough for a NAG build
gfortran:    source ~/.activate_gcc.sh
nagfor:      source ~/.activate_nag.sh   # the explicit form of what ~/.zprofile already did
```

`source ~/.zprofile` leaves: `FPM_FC=nagfor`; `FPM_CC`/`FPM_CXX` = `gcc`/`g++`; `FPM_FFLAGS` =
NAG-built Healpix and cfitsio plus `-I/opt/local/include`; `FPM_CXXFLAGS` =
`-std=c++20 -stdlib=libc++ -I/opt/local/include`; `FPM_LDFLAGS=-L/opt/local/lib`; `NAGFOR_OMP=1`;
`PATH` with `tools/nagfor_fpm_shim` first (`which nagfor` is the shim). `nagfor -V` reports 7.2
Build 7244.

- The shim supplies `-openmp` on every invocation; `fpm --verbose` never shows it. Verify by
  behaviour: `string_parallel` reports 9 passed / 0 skipped threaded, 1 / 8 serial.
- `~/.activate_gcc.sh` sets `FPM_FC=gfortran-mp-15` (no bare `gfortran` on `PATH`) and leaves
  `FPM_CC`/`FPM_CXX` empty, so fpm derives Apple clang for the C++ half. That mixed-family build is
  deliberate: `FPM_CXXFLAGS` carries `-stdlib=libc++`, which MacPorts `g++` rejects.
- Healpix and cfitsio have separate NAG and GCC trees (`Healpix_3.83_nag` / `Healpix_3.83`) and the
  activation swaps which one `FPM_FFLAGS` names; assigning `FPM_FFLAGS` can leave a NAG build
  reaching for GCC-built modules.
- `NAGFOR_OMP` stays `1` after switching to gfortran; it is not evidence a NAG environment is
  active. Check `$FPM_FC`.
- flang is serial-only and `--profile release` does not link (no `omp_lib.mod` ships for it).
- `/usr/bin/ar` is Apple cctools and cannot archive LTO IR; use `gcc-ar-mp-15` or `llvm-ar-mp-22`.

## Machine A (arm64 macbook)

- `uname -m` = `arm64`; nagfor product `NPMA872NA for Apple Arm Mac OSX 64-bit`.
- `FPM_FFLAGS` under NAG:
  `-I~/usr/local/Healpix_3.83_nag/include -I~/usr/local/cfitsio-4.7.0_nag/include -I/opt/local/include`;
  `~/.activate_gcc.sh` swaps both to the non-`_nag` trees.
- No `conda activate` needed: `astro` is already active (`~/miniforge3/envs/astro/bin/python3`,
  3.13, has mpmath).
- flang: `flang-mp-22`, plus `flang-mp-devel` 23.0.0git. Archivers: `gcc-ar-mp-15`, `llvm-ar-mp-22`.

## Machine C (intel iMac)

- `uname -m` = `x86_64`.
- `conda activate astro` is required: no `python3` on the default `PATH` has mpmath.

## Machine B (AMD linux server)

```
environment: source ~/.bashrc
python:      conda activate astro
gfortran:    source /opt/fortran/activate_gcc.sh
ifx:         source /opt/fortran/activate_ifx.sh
```

- Both activations export `OMP_PLACES=sockets`; the ifx one sets `ulimit -s unlimited`. Quote the
  first when reporting any threading measurement. SMT is on: `nproc` and `omp_get_num_procs()`
  report 384 while only 192 are cores; size a thread ladder against the number you mean.
- An activation script sourced non-interactively may exit before it finishes, leaving the system
  toolchain active while its exported variables make the shell look configured: redirect its input
  and output, then check the compiler version.
- **`activate_gcc.sh` ends with `scl enable gcc-toolset-15 /bin/bash`**, an interactive subshell that
  dies at once without a tty, so sourcing it from a script leaves the system gfortran 11.5 with every
  `FPM_*` variable set. Take its exports without that line and run the command inside the toolset:

```bash
source <(sed '$d' /opt/fortran/activate_gcc.sh)   # its exports, minus the trailing `scl enable`
scl enable gcc-toolset-15 -- bash -c 'gfortran --version | head -1; fpm test'
```
- A sourced script inherits the caller's positional parameters (`./run.sh ifx` hands `$1` on):
  `set --` first.
- The system `gfortran` is 11.5.0, below the project minimum of 13, and silently miscompiles it;
  confirm the activated `gfortran --version` is the toolset's.
- The gfortran environment exports `-ffree-line-length-none`, so line length cannot be checked here.
- `set -u` kills the ifx activation silently (`PKG_CONFIG_PATH: unbound variable`; oneAPI's own
  `setvars.sh` dies on `OCL_ICD_FILENAMES`), and `>/dev/null 2>&1` swallows the diagnostic.
  `set -o pipefail` alone is safe.
- Threads beyond 64 give no meaningful gain; 64 is the sweet spot.
- B is the only machine that can run UBSan (`tools/check_random_ubsan.sh`): its gcc-toolset-15
  gfortran ships the x86-64 `libubsan`; toolset-14 shipped a 32-bit one, MacPorts gcc15 none, and
  flang rejects `-fsanitize` for Fortran. Read the toolset off `activate_gcc.sh`; it has moved once.

## Python: what needs `conda activate astro`

The `astro` environment (mpmath) is needed by two lint entries, `tools/generate_stats_vectors.py`
and `tools/generate_kde_vectors.py` (the 50-digit oracles behind `test/test_stats_golden.f90` and
`test/test_kde_golden.f90`); every other entry needs a bare `python3`. Required on B and C; already
active on A. Without it `tools/run_lint_check.sh` reports those two checks failed with `needs mpmath
(pip install mpmath)`: an environment gap, not a stale golden file. `generate_kde_vectors.py
--self-test` also needs scipy and KDEpy, which `astro` carries, so a reported KDEpy skip there is an
environment fault; it uses statsmodels when present and reports its absence by name.
