# Working environments for different developer machines

Maintainer-only. Defines **machine A**, **B** and **C** — the three machines this library is built
and measured on — and says how to build on each. Sole authority for machine configuration:
`CLAUDE.md` refers to these machines by name and carries none of it. Compiler behaviour that would
hold on any machine with the same toolchain installed belongs there, not here. Absent from
`CONTRIBUTING.md`'s tools index on purpose: no outside contributor has these machines.

## Standing rules on every machine

- **Never assign `FPM_FFLAGS`/`FPM_CXXFLAGS`/`FPM_LDFLAGS` — append.** They carry Arrow's (and on
  the macOS boxes Healpix's and cfitsio's) include and link paths; an assignment replaces them and
  the build fails at `fatal error: 'arrow/api.h' file not found`, which reads like a missing
  dependency. `FPM_FFLAGS="${FPM_FFLAGS:-} -flto"`.
- **One `FPM_BUILD_DIR` per configuration, outside `build/`, named by compiler *and* configuration.**
  Otherwise `tools/run_error_scenarios.sh`'s `find … -name error_scenarios | head -n 1` picks the
  wrong binary and reports a false green. `test_run/nag-main`, `test_run/nag-deb`.
- **Verify the activation took** — check `$FPM_FC` and the compiler's own `--version`/`-V`. Every
  script here can appear to work while leaving the system toolchain active.
- **Never edit the activation scripts**; they are outside the repository.

## The machines

| | **A — laptop** | **B — `bunyip.to.ee`** | **C — desktop** |
|---|---|---|---|
| CPU | Apple M1 Pro, 8 cores | 2 x AMD EPYC 9654, **192 physical / 384 logical**, 2 sockets, 2 NUMA nodes | Intel i7-10700K, 8 physical / 16 logical |
| arch / SIMD | **arm64, NEON (128-bit)** | **x86-64 Zen 4, AVX-512** (f/bw/dq/vl/vnni/bf16) | **x86-64 Comet Lake, AVX2 (256-bit)** |
| RAM | 32 GB | 1132 GB | 128 GB |
| OS | macOS 26.5 (Darwin 25.5.0) | RHEL 9.7, kernel 5.14 | macOS |
| Fortran | **nagfor 7.2** (default), gfortran 15.2, flang 22.1.8 | **ifx 2026.1.1**; gfortran 15.2.1 (toolset-15), 14.2.1 (toolset-14), system 11.5 | **nagfor 7.2** (default), gfortran 15.2, flang 22.x |
| C++ | Apple clang 21, `g++-mp-15` (GCC 15.2) | icpx 2026.1.1; g++ 14.2.1 or 11.5 | Apple clang, MacPorts GCC |
| Arrow / Parquet | 25.0.0 | **24.0.0** | 25.0.0 |
| fpm | 0.13.0 alpha | 0.13.0 alpha | 0.13.0 alpha |
| use for | everyday work; quiet, so best for small deltas | ifx, threading at scale, AVX-512, and gfortran-vs-ifx with everything else held constant | x86-64 twin of A: an A-vs-C comparison isolates architecture alone |

- **A and C are distinguishable only by `uname -m`** — same OS, same compilers, same Arrow.
  `tools/machine_report.sh` prints it; read it before attributing any figure.
- **B is on a different Arrow** (24.0.0 against 25.0.0): do not compare figures across it for
  anything through `src/parquet_wrapper.cpp` or an end-to-end read/write.
- **B is bad for small measurements** — on a large NUMA box a cold destination is dominated by page
  faults; 5.6x run-to-run variation recorded at one size. Take small deltas on A.
- **No machine can run the CI Docker image.** A has the CLI (`/opt/local/bin/docker`, 29.6.2) but no
  daemon.

## Machines A and C (macOS): what they share

```
environment: source ~/.zprofile          # sources ~/.activate_nag.sh itself; enough for a NAG build
gfortran:    source ~/.activate_gcc.sh
nagfor:      source ~/.activate_nag.sh   # the explicit form of what ~/.zprofile already did
```

`source ~/.zprofile` leaves:

| variable | value |
|---|---|
| `FPM_FC` | `nagfor` |
| `FPM_CC` / `FPM_CXX` | `gcc` / `g++` |
| `FPM_FFLAGS` | NAG-built Healpix and cfitsio, plus `-I/opt/local/include` |
| `FPM_CXXFLAGS` | `-std=c++20 -stdlib=libc++ -I/opt/local/include` |
| `FPM_LDFLAGS` | `-L/opt/local/lib` |
| `NAGFOR_OMP` | `1` |
| `PATH` | `tools/nagfor_fpm_shim` first, so `which nagfor` is the shim |

`nagfor -V` reports 7.2 Build 7244; fpm is 0.13.0 alpha.

- **The shim supplies `-openmp` on every invocation**, so a NAG build is threaded; `NAGFOR_OMP=0`
  opts out. **`fpm --verbose` will not show the flag** — the shim rewrites the command line after
  fpm has printed it — so verify by behaviour: `string_parallel` reports 9 passed / 0 skipped
  threaded, 1 / 8 serial.
- **`~/.activate_gcc.sh` sets `FPM_FC=gfortran-mp-15`** (no bare `gfortran` is on `PATH`) and leaves
  `FPM_CC`/`FPM_CXX` **empty**, so fpm derives `gcc`/`g++`, which on macOS is **Apple clang**. That
  mixed-family build is deliberate: `FPM_CXXFLAGS` carries `-stdlib=libc++`, which MacPorts `g++`
  rejects.
- **Healpix and cfitsio have separate NAG and GCC trees** (`Healpix_3.83_nag` against
  `Healpix_3.83`) and the activation swaps which one `FPM_FFLAGS` points at — so assigning
  `FPM_FFLAGS` can leave a NAG build reaching for GCC-built modules.
- **`NAGFOR_OMP` stays `1` after switching to gfortran.** Harmless, but its presence is not evidence
  that a NAG environment is active; check `$FPM_FC`.
- **flang is serial-only and `--profile release` does not link.** No `omp_lib.mod` ships for it —
  the only one under `/opt/local` belongs to GCC.
- **`/usr/bin/ar` is Apple cctools and cannot archive LTO IR.** Use `gcc-ar-mp-15` or
  `llvm-ar-mp-22`.

## Machine A (arm64 macbook)

As above, plus (verified 2026-08-31):

- `uname -m` = **`arm64`**; nagfor product `NPMA872NA for Apple Arm Mac OSX 64-bit`.
- `FPM_FFLAGS` under NAG is
  `-I~/usr/local/Healpix_3.83_nag/include -I~/usr/local/cfitsio-4.7.0_nag/include -I/opt/local/include`;
  `~/.activate_gcc.sh` swaps both to the non-`_nag` trees and leaves the rest unchanged.
- **No `conda activate` needed** — `astro` is already active (`CONDA_DEFAULT_ENV=astro`), so
  `python3` is `~/miniforge3/envs/astro/bin/python3` (3.13.14) and has mpmath.
- flang: `flang-mp-22`, plus `flang-mp-devel` 23.0.0git.
- Archivers present: `gcc-ar-mp-15`, `llvm-ar-mp-22`.

## Machine C (intel iMac)

As above, plus:

- `uname -m` = **`x86_64`**.
- **`conda activate astro` is required** — no `python3` on the default `PATH` has mpmath, not
  `/usr/bin/python3`, `~/.local/bin/python3` or `~/pyastro/bin/python3`.

## Machine B (AMD linux server)

```
environment: source ~/.bashrc
python:      conda activate astro
gfortran:    source /opt/fortran/activate_gcc.sh
ifx:         source /opt/fortran/activate_ifx.sh
```

Both activations export **`OMP_PLACES=sockets`**; the ifx one sets **`ulimit -s unlimited`**.
Neither is visible in a command line and the first shapes every threading measurement taken here, so
quote it when reporting one. **SMT is on**, so `nproc` and `omp_get_num_procs()` both report 384
while only 192 are cores — size a thread ladder against the number you mean.

Seven traps, all of which have cost a campaign:

- an activation script sourced **non-interactively** may exit before it finishes, leaving the system
  toolchain active while its exported variables make the shell look configured. Redirect its input
  and output, then check the compiler version;
- a **sourced script inherits the caller's positional parameters**, so a wrapper invoked as
  `./run.sh ifx` hands them on and can send the script down a different path. `set --` first;
- the **system `gfortran` is 11.5.0**, below this project's minimum of 13, and silently miscompiles
  it. Confirm the activated `gfortran --version` is the toolset's;
- the gfortran environment exports `-ffree-line-length-none`, so **line length cannot be checked
  here** — a too-long line compiles cleanly and fails everywhere else;
- **`set -u` kills the ifx activation silently**, in combination with the redirection the first trap
  calls for: it dies on `PKG_CONFIG_PATH: unbound variable` at line 11, and oneAPI's own
  `setvars.sh` dies on `OCL_ICD_FILENAMES` even if that were fixed. The diagnostic goes to stderr,
  which `>/dev/null 2>&1` swallows. `set -o pipefail` alone is safe;
- **threads beyond 64 give no meaningful gain**; 64 is the sweet spot.

**B is the only machine that can run UBSan** (`tools/check_random_ubsan.sh`): its gcc-toolset-15
gfortran ships the x86-64 `libubsan`, toolset-14 shipped only a 32-bit one, MacPorts gcc15 ships
none, and flang rejects `-fsanitize` for Fortran. The toolset `activate_gcc.sh` selects has moved
from 14 to 15 once already — read it off the script rather than assuming.

## Python: what needs `conda activate astro`

The `astro` environment (mpmath 1.4.1) is needed for **exactly one** of the 19 entries in
`tools/run_lint_check.sh` — `tools/generate_stats_vectors.py`, the 50-digit oracle behind
`test/test_stats_golden.f90`. The other eighteen need a bare `python3`. Required on **B** and **C**;
already active on **A**.

Without it the wrapper reports `1 of 19 lint check(s) FAILED` with
`needs mpmath (pip install mpmath)`. **That is an environment gap, not a stale golden file** — the
script exits at import, before comparing anything, so it is easy to misread as a regression caused
by whatever was just edited.
