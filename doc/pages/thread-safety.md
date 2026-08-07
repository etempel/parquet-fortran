---
title: Thread safety
---

Concurrent use (e.g. from an OpenMP parallel region) is supported.

## Rules at a glance

- Each thread must use its own independent `parquet_writer`/`parquet_reader` instance.
- Never call into the same reader/writer instance from two threads at once.
- Independent readers may open/read the same parquet file concurrently.
- Never write to the same output file path from two threads at the same time.
- A shared `parquet_table` may be **read** from many threads once its columns are resident, and
  **appended to** from many threads (the table serialises that itself). A first touch inside a
  parallel region, and every other change to a shared table, is a hard error — see
  [What a `parquet_table` allows concurrently](#what-a-parquet_table-allows-concurrently) below and
  [Reading a table from several threads](table.html#reading-a-table-from-several-threads).

## What a `parquet_table` allows concurrently

**The library enforces this table, it does not merely document it.** Everything marked "refused"
below aborts with a message naming what you did and what to do instead, rather than racing. Three
cases at the end cannot be detected at all, and are called out as such.

The design principle behind the whole table: **reading an already-resident column is free** — no
lock, no atomic, no bookkeeping, any number of threads. Everything else is arranged around not
disturbing that.

| what you do | concurrently? | what happens if you break the rule |
|---|---|---|
| Read a resident column: `%get`, `%col`, `%get_slice`, `%row`, `%get_element`, `%is_null` | **yes**, unrestricted | — |
| Read through a `%col`/`%ref` pointer you already hold | **yes**, unrestricted | — |
| Metadata: `%nrows`, `%ncols`, `%column_names`, `%kind`, `%width`, `%unit`, `%residency` | **yes** | — |
| First read of a column not yet resident, on a table **another** thread opened | no | hard error; `%prefetch` before the region |
| First read of a column, on a table **this** thread opened inside the region | **yes** | — (this is the per-thread slice pattern) |
| `%prefetch` / `%materialize_all` called from one thread | **yes, internally** — the library reads the columns on several threads for you | — |
| `%sort_by` / `%filter_rows` / `%top_n` / `%delete_rows` / `%truncate` called from one thread | **yes, internally** — the library rewrites the columns on several threads for you | — |
| `%clone` called from one thread | **yes, internally** — the library copies the columns on several threads for you | — |
| Write values into **different** resident columns | **yes** | — |
| Write values into **disjoint row ranges** of one resident fixed-width column | **yes** | — |
| Write values into a **string** column | no | hard error — its rows share one packed store, so a write can move the whole payload |
| `%set_null`/`%clear_null` when the column **already has** validity storage | **yes** (different columns, or disjoint rows) | — |
| `%set_null`/`%clear_null` when it does **not** | no | hard error naming `%ensure_validity`; call that before the region |
| `%set_null`/`%clear_null` on a **date/time/timestamp** column | **yes** | — (the null lives in the element; nothing is allocated) |
| `%append` into a shared table | **yes** — serialised by the table's own lock | — |
| **Reading** a shared table while any thread appends to it | no | hard error (best-effort — see below) |
| Any other change to a shared table: `%add_column`, `%drop_column`, `%rename_column`, `%copy_column`, `%cast`, `%evict_column`, `%reload`, `%filter_rows`, `%sort_by`, `%top_n`, `%delete_rows`, `%truncate`, `%append_null_rows`, `parquet_write_table` | no | hard error; do it before or after the region |
| The same change on a table **this** thread opened inside the region | **yes** | — |

Three things the library cannot see, which stay your responsibility:

- **A pointer you already hold.** `%append` reallocates every column's storage, so a `%col`/`%ref`
  pointer taken before an append points at freed memory afterwards. Fortran gives no way to detect
  a dangling pointer; re-fetch after an append, and use `%generation()` if you want to check.
- **Threads the library cannot identify.** The guards use OpenMP thread identity. If you thread some
  other way (pthreads through C interop, coarrays), none of them apply.
- **The exact instant a violation starts.** The read/append checks catch an overlap of any real
  duration, but a read beginning fractionally before an append publishes itself is not seen. They
  are a safety net over the append-only rule, not a substitute for it.

**Appended row order is not deterministic** — it depends on which thread got the lock first. Sort
in memory afterwards (`%sort_by`) if you need a reproducible result.

**Three groups of operations thread internally, and all of them stand down inside your own parallel
region.** `%prefetch`/`%materialize_all` read several columns at once, each on its own reader;
`%sort_by`, `%filter_rows`, `%top_n`, `%delete_rows` and `%truncate` rewrite several columns at
once; and `%clone` copies several columns at once. You do not ask for any of them and cannot get
them wrong — but two consequences are worth knowing:

- **Called from inside a parallel region of your own, both run serially.** A nested region is your
  business, not the library's: without that rule, *T* of your threads would each ask for *T* more,
  and the oversubscription is slower than not threading at all. So the per-thread-slice pattern
  below loses nothing — each thread's own table is small and there are already *T* of them running.
- **A read-time transform is carried by every per-thread reader, and two of the four decline the
  parallel read on cost.** A `qc=` or a `sample_fraction=` table (seeded or not — the table settles
  one seed when it is opened, so every reader draws the same rows) prefetches in parallel and
  returns exactly what a serial read would; a `filter=` or a `sort=` falls back to the serial
  reader, because every reader would otherwise rebuild the mask or the permutation, which measured
  slower than the parallel read saves. Nothing about the *answer* changes either way. One
  consequence worth stating because it is the question people ask: **a soft qc violation still
  warns at most once per column**, since each column is read by exactly one thread.
- **A parallel rewrite holds one transient column copy per thread**, where a serial one holds one in
  total. The thread count never exceeds the column count, so those copies come to at most one extra
  copy of the table: a `%sort_by` can double the table's peak memory for the duration of the call.
  `parquet_set_table_threads(n)` caps the threads and so caps the copies — see
  [Settings](settings.html#threads-for-mutating-a-table). **`%clone` is exempt**: it allocates a
  second copy of the table by definition, so threading it adds no transient beyond the copy you
  asked for.

## Practical cases

- Safe: many threads, each opening/writing/closing its own `parquet_writer` to a different file.
- Safe: many threads, each opening/reading/closing its own `parquet_reader` — including multiple threads independently opening their own reader on the *same* file at the same time (each thread's `parquet_open_reader` call is independent).
- Safe: parsing MAML files (`parquet_parse_maml`, `parquet_validate_maml`, etc.) concurrently across threads. Internally this path is lock-serialized for correctness, so it is thread-safe but not expected to speed up with more threads.
- **Not safe:** sharing a single `parquet_writer`/`parquet_reader` variable across threads (e.g. a module-level or `!$omp shared` instance that multiple threads call into at once).
- **Not safe:** two threads writing to the *same* output file at the same time, even with separate `parquet_writer` instances — the underlying file itself isn't safe to write from more than one place at once.

## Building for genuine multi-threaded use

`parquet-fortran`'s own `fpm.toml` declares a dependency on fpm's built-in `openmp` metapackage, which automatically supplies the right compiler-specific OpenMP flag (`-fopenmp` for gfortran, `-qopenmp` for ifx, ...) for the whole build — this project's dependency and your own project's sources alike, since fpm computes one consistent flag set across the full resolved dependency graph. You do not need to pass an OpenMP flag manually via `FPM_FFLAGS` for this library's own concurrency-related code paths to run multi-threaded. If you call into this library concurrently from your own `!$omp parallel` regions and want to be certain OpenMP is active for your own sources too, you can add the same dependency to your own `fpm.toml`:
```toml
[dependencies]
openmp = "*"
```
(If you're developing `parquet-fortran` itself, see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md#testing-genuine-openmp-concurrency) for how this project's own tests exercise real concurrency.)

## The concurrency guard

Calling into a *shared* `parquet_writer`/`parquet_reader` from more than one thread at a time (the "not safe" case above) is actively detected and rejected: the second concurrent caller triggers an immediate process abort (`std::abort()`) with a diagnostic on stderr. This is a fail-fast race guard, not a locking mechanism. Sequential, non-overlapping hand-off between threads remains allowed. (This is one of two classes of process-abort failure in this library — see [Error handling](error-handling.html#the-two-failure-classes) for the other, Fortran `error stop`.)

## Streaming/chunked writes and reads

**The streaming row-group write API** (`parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` — see [Streaming/chunked writes](writing.html#streamingchunked-writes)) is bound by the same "one thread at a time per writer" rule as `parquet_write_column`, with one addition: those three calls must also stay in strict row-group order for a given writer, since Parquet's row groups are laid out sequentially on disk. Wrapping that triplet itself in an `!$omp parallel`/`parallel do` region — e.g. one thread per row group — would hit the concurrency guard above (an immediate abort, not silent corruption) at best, and doesn't make sense at any rate: row groups have no ordering guarantee across threads, so even without the guard the file's row order would be nondeterministic. If you want to parallelize a streaming write, parallelize the *data preparation* for each row group (pure computation, no calls into the writer) and keep the `parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` calls themselves serial, on one thread, in order.

**The chunked read API** (`parquet_read_column_chunk` — see [Streaming/chunked reads](reading.html#streamingchunked-reads)) is bound by the same "one thread at a time per reader" rule as every other reader call, but unlike the streaming write API it has no ordering requirement: reads are stateless/random-access, so calls for different row groups (or the same one, repeatedly) can happen in any order. This means a row-group loop *can* be parallelized directly, as long as each thread uses its own `parquet_reader` instance opened on the same file (independent readers on the same file are always safe to use concurrently — see "Practical cases" above) rather than sharing one reader across threads, which would still hit the concurrency guard.

## Thread-pool tuning

Both `parquet_open_reader` and `parquet_open_writer` accept an optional `use_threads` (`logical`, default `.true.`):
```fortran
call parquet_open_reader(reader, "data.parquet", use_threads=.true.)
call parquet_open_writer(writer, "data.parquet", use_threads=.true.)
```
When `.true.` (the default), that reader/writer decodes or encodes column data across Arrow's internal CPU thread pool instead of a single thread — Arrow's own library default is actually `.false.`, so this library turns it on by default since the extra parallelism is normally a pure win. This is on a per-reader/per-writer basis: it costs nothing to leave it on, and there's no shared state to worry about between independent readers/writers.

The most common reason to pass `use_threads=.false.` is to avoid **oversubscription** when you're already parallelizing at a coarser level — e.g. many OpenMP threads (see "Rules at a glance" above) each opening their own reader/writer: without this, every one of those threads would *also* fan out across Arrow's thread pool, so N OpenMP threads times Arrow's pool size threads end up competing for the same cores. It's also useful for deterministic single-threaded benchmarking/profiling.

`parquet_set_arrow_threads(n)` caps the size of Arrow's thread pool itself:
```fortran
call parquet_set_arrow_threads(4)
```
Unlike `use_threads`, this is **not** a per-reader/per-writer setting — Arrow's CPU thread pool is a single, process-global resource shared by every reader/writer (in every thread) that has `use_threads` enabled. Call it once, e.g. near the start of your program, before opening readers/writers on other threads; calling it repeatedly with different values from multiple concurrent threads is a race, since each call resizes a pool everyone else is using at that same moment. `n` must be `>= 1`; values below that fail immediately with `error stop`.

(If you're developing `parquet-fortran` itself and want to measure how these two knobs actually affect write/read throughput on your own hardware, see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md#other-tools-helpers)'s `tools/benchmark_threads.sh` entry.)

## A note on functions returning `character(len=:), allocatable`

This is a library-wide compiler caveat, not specific to any one type, but it was found and root-caused via `parquet_string_column`, so it's documented here in full.

**Past gfortran/OpenMP runtime caveat — now worked around throughout this library's source.** Two fully independent threads, each with its own, separate object (no sharing at all — sharing a single instance across threads is covered by the rules above), could still silently corrupt memory under concurrent execution on some gfortran/OpenMP builds. Root-caused to a specific, still-open gfortran bug ([PR113797](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=113797); related: [PR97977](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=97977)): the compiler's codegen for *receiving* a `character(len=:), allocatable` function result uses a hidden length-tracking variable that isn't always properly thread-local. Confirmed with a minimal reproducer using no code from this library — the hazard needed nothing more than a `character(len=:), allocatable` function called concurrently on a type with two or more allocatable components; it did not depend on construction/destruction, `class()` dispatch, or sharing, and did not affect non-`character` allocatable results.

The fix, applied throughout `src/*.f90`: **every accessor that used to return `character(len=:), allocatable` as a function result is now a subroutine** that writes into an `intent(out)`/`intent(inout)` allocatable `character` argument instead — this sidesteps the defective codegen path entirely rather than working around a symptom. This was independently verified on `parquet_string_column`'s own accessors (`get`, `summary`, and the `parquet_string` handle's `to_string`): the identical reproducer converted from a function to a subroutine reproduced **zero** failures across tens of thousands of iterations, vs. hundreds of failures per 8000 for the function form. No special precaution is needed for concurrent, independent use of any type in this library as a result — the sharing rules at the top of this page remain the only thread-safety rules that apply. (`schema%add_col_qc`/`schema%set_col_qc` — see [Quality control](quality-control.html#building-a-qc-maml-in-code) — are two of the public procedures shaped by this same fix: neither is a function returning `character(len=:), allocatable`.)

## A note on Arrow's own type-singleton construction

**Past Arrow-library caveat on the very first concurrent use of a given column type — now worked around internally.** Arrow represents each column type (`int32`, `utf8`, ...) as a reference to a shared, process-wide singleton object, one per type, used by every thread. Confirmed via ThreadSanitizer that this project's apt-installed Arrow build has data races on two independent pieces of lazily-populated state hanging off that same shared object: the singleton's own one-time construction (normally safe under the standard C++ "magic statics" pattern, but not on this Arrow build), and a lazily-cached "fingerprint" Arrow computes internally the first time it needs to compare or serialize that type (used by Arrow's own type/schema equality checks, not something this library calls directly). Both have the same shape: if two independent OpenMP threads each write (or otherwise first touch) a column of the *same* type at close to the same moment — entirely possible the first time a multi-threaded program starts up and several threads open their own writer/reader within the same instant — they can race on whichever piece of that singleton's state neither thread has populated yet. The corruption this causes does not crash where it happens; it surfaces later, in whatever unrelated code next touches the heap, which is what made the original symptom (a process abort inside a completely unrelated, trivial boolean check) so misleading to trace back, and why fixing the first race alone did not fully resolve it — the second was still there.

Fixed internally, the same way this library already handles Arrow's compute-kernel registry (also a lazily-initialized, process-global Arrow resource): every singleton this library's C++ layer uses, and every piece of lazy state on it this project has found racing, is forced into existence exactly once, from a single thread, the first time any thread calls `parquet_open_reader`/`parquet_open_writer` — after that first call, every later concurrent read is just a read of already-published state, which is safe. No caller action is needed; this is purely internal to the library. If you construct `arrow::`/`parquet::` objects directly yourself in the same process (outside this library's own API) and open your first reader/writer/column of a given type from multiple threads at once, the same underlying Arrow behavior could in principle still apply to types (or to lazy state on those types) this library's own warm-up doesn't cover.
