# Working environments for different developer machines

Maintainer-only notes on **how to activate a toolchain** on each of the three machines this library
is developed and measured on. Nothing here is for a contributor arriving from outside — none of
them has these machines — which is why this file is deliberately absent from `CONTRIBUTING.md`'s
tools index.

**This file is the authority for the activation COMMANDS**, and wins wherever it disagrees with
anything else — it is what gets edited when a machine changes. `CLAUDE.md`'s "The three machines
available for testing" section stays the authority for what each machine *is* (CPU, architecture,
SIMD width, core count, RAM, compiler versions, Arrow version), and carries the same activation
traps at campaign length; treat that as the elaboration and this as the short answer. Put a *new*
activation command here, not there.

## Standing rules on every machine

- **Never assign `FPM_FFLAGS`, `FPM_CXXFLAGS` or `FPM_LDFLAGS`.** The activation scripts put
  Arrow's — and on the macOS boxes also Healpix's and cfitsio's — include and link paths there, and
  a command-line assignment *replaces* rather than appends. The symptom is
  `fatal error: 'arrow/api.h' file not found`, which reads like a missing dependency rather than a
  flag mistake. Append when a flag must be added:
  `FPM_FFLAGS="${FPM_FFLAGS:-} -flto"`.
- **Give each configuration its own `FPM_BUILD_DIR`, outside `build/`.** Two configurations sharing
  a tree let `tools/run_error_scenarios.sh`'s `find … -name error_scenarios | head -n 1` pick the
  wrong binary and report a false green. The name must vary by **compiler as well as by
  configuration** — fpm keeps objects apart per compiler, that lookup does not.
  `FPM_BUILD_DIR=test_run/nag-main` and `test_run/nag-deb` are the shape to copy.
- **Verify the activation took, rather than assuming it.** Every one of these scripts can appear to
  work while leaving the *system* toolchain active. Check `$FPM_FC` and the compiler's own
  `--version`/`-V` before trusting any result from a session.
- **Never edit the activation scripts or anything else outside this repository** — see `CLAUDE.md`,
  "Only modify files inside this repository". If one needs changing, say so and let the maintainer
  do it.

## Machine A (arm64 macbook)

```
environment: source ~/.zprofile
python:      conda activate astro
gfortran:    source ~/.activate_gcc.sh
nagfor:      source ~/.activate_nag.sh
```

Also the only machine with a local `flang` (`flang-mp-22`, MacPorts). A flang build here is
**serial only** — MacPorts ships no `omp_lib.mod` — and `--profile release` does not link under it;
both are LLVM/packaging limitations rather than defects in this library, and `CLAUDE.md` has the
detail.

## Machine B (AMD linux server)

```
environment: source ~/.bashrc
python:      conda activate astro
gfortran:    source /opt/fortran/activate_gcc.sh
ifx:         source /opt/fortran/activate_ifx.sh
```

The only machine for **ifx**, for **threading at scale** and for **AVX-512**, and the only one that
can compare gfortran against ifx with everything else held constant. It is **192 physical cores and
384 logical** — SMT is on, so `nproc` and `omp_get_num_procs()` both report 384 while only half of
those are cores. Size a thread ladder against the number you mean.

Both activation scripts also export **`OMP_PLACES=sockets`**, and the ifx one sets **`ulimit -s
unlimited`**. Neither is visible in a command line, and the first one shapes every threading
measurement taken here, so quote it when reporting one. Six traps live here, most of them
documented at length in `CLAUDE.md` and all of which have cost a campaign:

- an activation script sourced **non-interactively** may exit before it finishes, leaving the
  system toolchain active while its exported variables make the shell look configured — redirect
  its input and output, and check the compiler version afterwards;
- a **sourced script inherits the caller's positional parameters**, so a wrapper invoked as
  `./run.sh ifx` hands them on and can send the script down a different path. `set --` before
  sourcing;
- the **system `gfortran` is 11.5.0**, below this project's minimum of 13, and silently
  miscompiles it. Confirm the activated `gfortran --version` is the toolset's, not `11.5.0`;
- the gfortran environment exports `-ffree-line-length-none`, so **line length cannot be checked
  here** — a too-long line compiles cleanly and fails everywhere else;
- **`set -u` kills the ifx activation, silently, in combination with the redirection the first
  trap above calls for.** `source /opt/fortran/activate_ifx.sh` under `set -u` dies on
  `PKG_CONFIG_PATH: unbound variable` at its line 11, and oneAPI's own `setvars.sh` — which it
  sources — dies on `OCL_ICD_FILENAMES` even if that were fixed, so this is the whole chain rather
  than one line. The diagnostic goes to **stderr**, which `>/dev/null 2>&1` swallows, leaving a
  wrapper that exits nonzero with an empty log and no clue. `set -o pipefail` alone is safe; do not
  add `-u` to a script that sources these, or drop it around the `source` line;
- threads beyond 64 cores do not provide any meaningful speed gain. 64 cores is a sweet spot.

## Machine C (intel iMac)

```
environment: source ~/.zprofile
python:      conda activate astro
gfortran:    source ~/.activate_gcc.sh
nagfor:      source ~/.activate_nag.sh
```

**`source ~/.zprofile` alone is enough for a NAG build here** — it sources `~/.activate_nag.sh`
itself, so the separate line above is only the explicit form. Verified on 2026-08-31, it leaves:

| variable | value |
|---|---|
| `FPM_FC` | `nagfor` |
| `FPM_CC` / `FPM_CXX` | `gcc` / `g++` |
| `FPM_FFLAGS` | Healpix, cfitsio and `/opt/local` include paths |
| `FPM_CXXFLAGS` | `-std=c++20 -stdlib=libc++ -I/opt/local/include` |
| `FPM_LDFLAGS` | the matching library paths |
| `NAGFOR_OMP` | `1` |
| `PATH` | `tools/nagfor_fpm_shim` already first, so `which nagfor` is the shim |

`nagfor -V` reports **NAG Fortran Compiler Release 7.2, Build 7244**; `uname -m` is **`x86_64`**
and fpm is **0.13.0 alpha**. That architecture check is the point: **machines A and C cannot be
told apart from a compiler listing** — both are macOS with MacPorts gfortran 15.2, flang 22.x and
Arrow 25.0.0, and only `uname -m` separates them. Run `tools/machine_report.sh` (which prints it)
before attributing any measurement taken here.

The shim is what supplies `-openmp` on every invocation, so a NAG build here really is threaded;
`NAGFOR_OMP=0` opts out for a deliberately serial one. **`fpm --verbose` will not show the flag** —
the shim rewrites the command line after fpm has printed it — so verify by behaviour instead: the
`string_parallel` suite reports 9 passed / 0 skipped when threaded and 1 / 8 when not.

## Python: what needs `conda activate astro`

`conda activate astro` (mpmath 1.4.1) is needed for **exactly one** check —
`tools/generate_stats_vectors.py`, the 50-digit oracle behind `test/test_stats_golden.f90`. The
other eighteen entries in `tools/run_lint_check.sh` need nothing but a bare `python3`.

**No `python3` on the default `PATH` has mpmath** on machine C — not `/usr/bin/python3`, not
`~/.local/bin/python3`, not `~/pyastro/bin/python3`. Without the environment the lint wrapper
reports `1 of 19 lint check(s) FAILED` with `needs mpmath (pip install mpmath)`. **That is an
environment gap, not a stale golden file**: the script exits at import, before comparing anything,
so it is easy to misread as a regression caused by whatever was just edited.
