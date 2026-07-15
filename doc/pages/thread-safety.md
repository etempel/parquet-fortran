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

**The streaming row-group write API** (`parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` — see [Streaming/chunked writes](writing.html#streamingchunked-writes)) is bound by the same "one thread at a time per writer" rule as `parquet_write_column`, with one addition: those three calls must also stay in strict row-group order for a given writer, since Parquet's row groups are laid out sequentially on disk. Wrapping that triplet itself in an `!$omp parallel`/`parallel do` region — e.g. one thread per row group — would hit the concurrency guard above (an immediate abort, not silent corruption) at best, and doesn't make sense at any rate: row groups have no ordering guarantee across threads, so even without the guard the file's row order would be nondeterministic. If you want to parallelize a streaming write, parallelize the *data preparation* for each row group (pure computation, no calls into the writer) and keep the `parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` calls themselves serial, on one thread, in order.

**The chunked read API** (`parquet_read_column_chunk` — see [Streaming/chunked reads](reading.html#streamingchunked-reads)) is bound by the same "one thread at a time per reader" rule as every other reader call, but unlike the streaming write API it has no ordering requirement: reads are stateless/random-access, so calls for different row groups (or the same one, repeatedly) can happen in any order. This means a row-group loop *can* be parallelized directly, as long as each thread uses its own `parquet_reader` instance opened on the same file (independent readers on the same file are always safe to use concurrently — see "Safe" cases above) rather than sharing one reader across threads, which would still hit the concurrency guard.

`use_threads` (see [Multi-threaded decoding/encoding](supported-data-types.html#multi-threaded-decodingencoding-use_threads-and-thread-pool-size)) is separate from OpenMP-level concurrency here. If you already parallelize with OpenMP across many readers/writers, consider `use_threads=.false.` (and/or `parquet_set_max_threads`) to avoid CPU oversubscription.
