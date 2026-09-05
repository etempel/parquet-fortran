---
title: Troubleshooting
---

Symptoms and fixes for the things most likely to go wrong when you build a program against this
library: a build that cannot find Arrow, a link that cannot resolve it, a program that builds but
will not start, and one compiler bug that looks like a data problem. The last section says what to
include in a bug report. See [Environment variables](../../index.html#environment-variables) in the
README for the full list of the **build-time** variables named below, and
[Prerequisites](../../index.html#prerequisites) for the versions this library needs. The library's
own **run-time** variables are a separate family, `PARQUET_FORTRAN_*`, listed under
[Setting from the environment](settings.html#setting-from-the-environment).

## Build and compile errors

- **`<ERROR> *cmd_run* Package error: Key features is not allowed in package file`**, or fpm
  crashing with no message at all, while it is resolving `parquet-fortran` — **your fpm is older
  than 0.13.0**. This project's `fpm.toml` uses the `[features]` table and the feature-list form of
  `[profiles]`, both of which fpm 0.13.0 introduced; 0.12.0 rejects the first outright and
  segfaults on the second. The message says "package file", but the manifest it cannot parse is
  **this library's**, not yours — which is why nothing in it mentions `parquet-fortran`. Install
  fpm 0.13.0 or newer from
  [fpm's own releases page](https://github.com/fortran-lang/fpm/releases).
- **fpm fails while resolving dependencies, naming `toml-f` or a git URL it cannot reach** —
  this library has one Fortran package dependency, [toml-f](https://github.com/toml-f/toml-f), and
  `fpm.toml` pins a fork of it. fpm resolves a package's dependency tree **before** it prunes
  modules, so it is fetched for **every** consumer whether or not you import `parquet_toml` — a
  program whose only `use` is `parquet_temporal` still needs that remote reachable on its first
  build. This is a first-build problem only: once fetched, it is cached under `build/dependencies`
  and later builds do not need the network. Behind a proxy or on an air-gapped machine, clone the
  dependency yourself and point `fpm.toml` at a local path.
- **`fatal error: arrow/api.h: No such file or directory`** (GCC's wording) or
  **`fatal error: 'arrow/api.h' file not found`** (Clang's), and likewise for
  `parquet/arrow/reader.h` — `FPM_CXXFLAGS` is not pointing `-I` at Arrow's `include` directory.
  It is `FPM_CXXFLAGS` specifically: these are C++ headers, included only by
  `src/parquet_wrapper.cpp`, so an `-I` that reaches only `FPM_FFLAGS` does not fix this.
- **A compile error naming `arrow/compute/api.h`** (or another `arrow/compute/*.h` header) **when
  `arrow/api.h` itself was found** — Arrow ships its compute kernels as a *separate package*, and
  that is the one missing. On Debian/Ubuntu install `libarrow-compute-dev` alongside `libarrow-dev`
  and `libparquet-dev`. It is not optional: `src/parquet_wrapper.cpp` includes
  `<arrow/compute/api.h>` and `<arrow/compute/initialize.h>` unconditionally.
- **`error: "parquet-fortran requires C++20 ..."`**, usually followed by a cascade of
  `no template named 'optional' in namespace 'std'` from Arrow's own headers — `-std=c++20` is
  missing from `FPM_CXXFLAGS` (required on every platform — see
  [Environment variables](../../index.html#environment-variables)). This is a **compile** error,
  not a link error, and it is raised deliberately: `src/parquet_wrapper.cpp` opens with an
  `#if __cplusplus < 202002L` guard so that the failure names the flag to set rather than leaving
  you to work it out from whichever Arrow header happens to break first.

## Link errors

- **`undefined reference to arrow::...` or `cannot find -lparquet`** —
  `LIBRARY_PATH`/`FPM_LDFLAGS` is not pointing `-L` at Arrow's `lib` directory. A missing
  `link = [...]` entry in your own `fpm.toml` is **not** the cause: fpm passes on the whole of
  `parquet-fortran`'s own list — `arrow`, `arrow_compute` and `parquet` — to whatever depends on
  it, so a consuming project that lists no libraries at all still links all three. (`arrow_compute`
  is a separate library from core `arrow`, and is what
  `parquet_close_reader(..., print_stat=.true.)`'s min/max calculation needs.)
- **Undefined references to `std::__1::...` (macOS) or `std::...` (Linux) at the final link
  step** — the C++ standard library is not reaching the link. fpm normally supplies it itself
  when it links a project containing C++ sources, so this should be rare; if you do hit it, add
  `-lc++` on macOS/Clang or `-lstdc++` on Linux/GCC to `FPM_LDFLAGS`.

## Runtime errors

- **`dyld: Library not loaded` / `error while loading shared libraries` for
  `libarrow`/`libparquet`**, after a build that succeeded — the Arrow/Parquet shared libraries are
  not on the dynamic linker's search path at run time. This does not normally need setting: a
  package manager that records absolute install names (Homebrew, MacPorts) or installs into a
  standard system directory leaves nothing to do. If you built Arrow into a private prefix, add
  that directory to `DYLD_LIBRARY_PATH` (macOS) or `LD_LIBRARY_PATH` (Linux), in addition to the
  `LIBRARY_PATH` used at build time.
- **The program aborts instead of returning a status code** — this is expected. There are two
  distinct forms: your compiler's `ERROR STOP` line carrying the library's message, and a single
  line reading `parquet-fortran: <procedure>: <message>` with no backtrace. The second exits with
  exactly **134**; the first exits nonzero with whatever status your compiler chose for
  `ERROR STOP`, which the Fortran standard leaves processor-dependent — so test for 134, not for
  the other one. The message text (typically naming a missing column or file) indicates the failing
  precondition. See [Error handling](error-handling.html#the-two-failure-classes) for how to tell
  them apart, and for the `found=` argument that reports a miss instead of aborting.
- **A spurious "column not found" abort at runtime, with a plausible-looking column name that
  doesn't match anything in the schema** — this is not a real missing-column bug: gfortran 11 and
  earlier (e.g. Ubuntu 22.04's default compiler) miscompile the optional deferred-length allocatable
  `character` argument returned by `schema%add_col_qc` / `schema%set_col_qc`, corrupting the
  returned column name at runtime rather than failing to build. Use gfortran 13 or newer (see
  [Prerequisites](../../index.html#prerequisites) in the README).

## Filing a bug report

Include the output of both version calls.

`parquet_get_version` reports this library's own version — the bare call gives the release number,
`mode="internal"` the dated `"vX.Y.Z (date)"` form. It lives in the leaf module `parquet_version`,
which `use parquet` re-exports and no narrower import does, so a program built on one of the other
entry modules adds `use parquet_version` for it.

`parquet_get_arrow_version` reports the linked Arrow C++ library's runtime version, or with
`mode="parquet"` the Parquet C++ one — often the more useful half when the symptom looks
Arrow-side. It comes from `parquet_settings`, and so from `parquet_io`, `parquet_tables` and
`parquet` as well.

Say which compiler and which fpm you used, too: `fpm --version`, and your Fortran compiler's own
`--version`.
