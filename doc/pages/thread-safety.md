---
title: Thread safety
---

Concurrent use (e.g. from an OpenMP parallel region) is supported.

Rules at a glance:

- Each thread must use its own independent `parquet_writer`/`parquet_reader` instance.
- Never call into the same reader/writer instance from two threads at once.
- Independent readers may open/read the same parquet file concurrently.
- Never write to the same output file path from two threads at the same time.

Practical cases:

- Safe: many threads, each opening/writing/closing its own `parquet_writer` to a different file.
- Safe: many threads, each opening/reading/closing its own `parquet_reader` — including multiple threads independently opening their own reader on the *same* file at the same time (each thread's `parquet_open_reader` call is independent).
- Safe: parsing MAML files (`parquet_parse_maml`, `parquet_validate_maml`, etc.) concurrently across threads. Internally this path is lock-serialized for correctness, so it is thread-safe but not expected to speed up with more threads.
- **Not safe:** sharing a single `parquet_writer`/`parquet_reader` variable across threads (e.g. a module-level or `!$omp shared` instance that multiple threads call into at once).
- **Not safe:** two threads writing to the *same* output file at the same time, even with separate `parquet_writer` instances — the underlying file itself isn't safe to write from more than one place at once.

**Building for genuine multi-threaded use:** the OpenMP flag is compiler-dependent (`-fopenmp` for gfortran, `-qopenmp` for ifx, ...), so it can't be hardcoded in `fpm.toml`. If you call into this library concurrently from your own `!$omp parallel` regions, set your own project's `FPM_FFLAGS` (or equivalent) to include your compiler's OpenMP flag, e.g.:
```sh
export FPM_FFLAGS="-fopenmp"
```
Since this library compiles from source as a dependency, that flag also reaches `parquet-fortran`'s own compiled code, not just yours. Without it, the concurrency-related code paths described above silently run single-threaded rather than failing outright. (If you're developing `parquet-fortran` itself, see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md#testing-genuine-openmp-concurrency) for how this project's own tests exercise real concurrency.)

Calling into a *shared* `parquet_writer`/`parquet_reader` from more than one thread at a time (the "not safe" case above) is actively detected and rejected: the second concurrent caller triggers an immediate process abort (`std::abort()`) with a diagnostic on stderr. This is a fail-fast race guard, not a locking mechanism. Sequential, non-overlapping hand-off between threads remains allowed.

`use_threads` (see [Multi-threaded decoding/encoding](supported-data-types.html#multi-threaded-decodingencoding-use_threads-and-thread-pool-size)) is separate from OpenMP-level concurrency here. If you already parallelize with OpenMP across many readers/writers, consider `use_threads=.false.` (and/or `parquet_set_max_threads`) to avoid CPU oversubscription.
