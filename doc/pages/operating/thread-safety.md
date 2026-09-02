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
  [Reading a table from several threads](#reading-a-table-from-several-threads).
- A `pf_logger` may be **emitted through** from many threads at once; **configuring** one may not.
  See [Logging from several threads](#logging-from-several-threads) below.

## What a `parquet_table` allows concurrently

**The library enforces this table, it does not merely document it.** Everything marked "refused"
below aborts with a message naming what you did and what to do instead, rather than racing. Three
cases at the end cannot be detected at all, and are called out as such.

The design principle behind the whole table: **reading an already-resident column is free** — no
lock, no bookkeeping, any number of threads. The one thing every read does pay is a single atomic
read of a flag, which is what lets a concurrent `%append` be refused rather than silently
reallocating the storage you are reading. Everything else is arranged around not disturbing that.

| what you do | concurrently? | what happens if you break the rule |
|---|---|---|
| Read a resident column: `%get`, `%col`, `%get_slice`, `%row`, `%get_element`, `%is_null` | **yes**, unrestricted | — |
| Read through a `%col`/`%ref` pointer you already hold | **yes**, unrestricted | — |
| Metadata: `%nrows`, `%ncols`, `%column_names`, `%kind`, `%width`, `%unit`, `%residency`, `%has_nulls` | **yes** | — |
| First read of a column not yet resident, on a table **another** thread opened | no | hard error; `%prefetch` before the region |
| First read of a column, on a table **this** thread opened inside the region | **yes** | — (this is the per-thread slice pattern) |
| `%prefetch` / `%materialize_all` called from one thread | **yes, internally** — the library reads the columns on several threads for you | — |
| A single large column's first read (`%get`, `%prefetch` of one name) | **yes, internally** — split across the column's row groups instead | — |
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
  pointer taken before an append points at freed memory afterwards. Fortran gives no way to detect a
  dangling pointer; re-fetch after an append, and use `%generation()` if you want to check.
- **Threads the library cannot identify.** The guards use OpenMP thread identity. If you thread some
  other way (pthreads through C interop, coarrays), none of them apply.
- **The exact instant a violation starts.** The read/append checks catch an overlap of any real
  duration, but a read beginning fractionally before an append publishes itself is not seen. They
  are a safety net over the append-only rule, not a substitute for it.

**Appended row order is not deterministic** — it depends on which thread got the lock first. Sort in
memory afterwards (`%sort_by`) if you need a reproducible result.

**Three groups of operations thread internally, and all of them stand down inside your own parallel
region.** `%prefetch`/`%materialize_all` read several columns at once, each on its own reader;
`%sort_by`, `%filter_rows`, `%top_n`, `%delete_rows` and `%truncate` rewrite several columns at
once; and `%clone` copies several columns at once. You do not ask for any of them and cannot get
them wrong — but four consequences are worth knowing:

- **A small table is rewritten serially, and that is not a failure.** The rewrite group has a work
  floor: it threads only when there are at least **two** columns to rewrite and the largest of them
  holds at least **131072** elements (rows times width). Below either, one thread does the work,
  because splitting it would cost more than it saves. The read group has its own floor in the same
  spirit — see [Reading a table from several threads](#reading-a-table-from-several-threads).
- **Called from inside a parallel region of your own, both run serially.** A nested region is your
  business, not the library's: without that rule, *T* of your threads would each ask for *T* more,
  and the oversubscription is slower than not threading at all. So the per-thread-slice pattern
  below loses nothing — each thread's own table is small and there are already *T* of them running.
- **A read-time transform keeps the parallelism, because the readers SHARE it.** A `filter=`,
  `sort=`, `qc=` or `sample_fraction=` table still prefetches in parallel and still returns exactly
  what a serial read would: the extra readers adopt the table's own row mask and sort permutation
  rather than rebuilding them. One consequence worth stating because it is the question people ask:
  **a soft qc violation still warns at most once per column**, since each column is read by exactly
  one thread and only the table's own reader evaluates the filter.
- **A parallel rewrite holds one transient column copy per thread**, where a serial one holds one in
  total. The thread count never exceeds the column count, so those copies come to at most one extra
  copy of the table: a `%sort_by` can double the table's peak memory for the duration of the call.
  `parquet_set_table_threads(n)` caps the threads and so caps the copies — see
  [Settings](settings.html#threads-for-mutating-a-table). **`%clone` is exempt**: it allocates a
  second copy of the table by definition, so threading it adds no transient beyond the copy you
  asked for.

### Reading a table from several threads

Reading a column that is **already resident** takes no lock and runs fully in parallel. A *first
touch* inside an OpenMP parallel region is a hard error, because it allocates and publishes state
other threads may be reading at that moment. So a table shared across a parallel region must be
prefetched before it:

```fortran
call parquet_open_table(t, "catalogue.parquet")
call t%prefetch(["mass    ", "redshift"])      ! REQUIRED before the region
call t%col("mass", mass)
call t%col("redshift", z)
!$omp parallel do
do i = 1, t%nrows()
    lum(i) = luminosity(mass(i), z(i))         ! resident reads, no lock
end do
!$omp end parallel do
```

A table a thread **opens for itself inside** the region is a different case: it cannot be shared, so
its lazy reads are allowed — that is what the
[slice regime](../tables/table-open.html#reading-part-of-a-file-the-slice-regime) below is for. That
extends to changing it: a thread-private table can be filtered, sorted, renamed and dropped from
inside the region, because no other thread can see it.

`%prefetch` and `%materialize_all` **read their columns on several threads by themselves**, so a
wide file is faster to bring into memory without you writing any OpenMP at all:

```fortran
call parquet_open_table(t, "wide.parquet")
call t%materialize_all()      ! reads the columns in parallel, internally
```

Each thread drives its own reader, so nothing is shared and nothing needs a lock. This happens only
when it is both safe and worth it: at least two columns to read, and more than one thread available.
On a wide file this is several times faster than reading the columns one after another;
`bench/benchmark_table.sh` measures it on your own hardware.

**One large column is parallel too**, split across its **row groups** instead of its columns — so a
`%get` or `%prefetch` of a single name is not serial just because there is only one of it:

```fortran
call parquet_open_table(t, "one-wide.parquet")
call t%get("flux", flux)      ! read on several threads, by row group
```

This too is worth several times a serial read on a large column spread over many row groups. The two
splits are alternatives — a read is divided by column when there is more than one to read, and by
row group otherwise — and both need the same two things: more than one thread, and a file to open a
second reader on. **Neither is refused by a `filter=` or a `sort=`**; see the table below for why.
The row-group split additionally needs **more than one row group**, **enough work to pay for opening
the extra readers** — the same **131072**-element floor (rows times width) the rewrite group uses,
which is more than a team spawn here because each thread also opens its own reader and parses the
file footer — and a **non-string**
column: a string column's packed variable-length store has no fixed row slots to write row groups
into, so it keeps the ordinary whole-column read.

**A read-time transform keeps all of this.** The extra readers do not re-derive the table's
`filter=` mask or its `sort=` permutation — they **share** them, which is a refcount increment
because both are immutable Arrow arrays. So every combination is parallel, and every one returns
exactly what a serial read would:

| opened with | speedup over a serial read, 16 columns × 2 M rows, 8 threads |
|---|---|
| nothing | **several times** — the best case, and the one the others are measured against |
| `sample_fraction=` | a little below the best case, seeded or not — the table settles one seed at open and the draw rides inside the shared mask |
| `qc=` | a little below that — rules are installed per reader but checked per column, and each column is read by one thread, so nothing is checked or warned twice |
| `filter=` | lower again, and it falls further as the filter gains key columns — the filter's own evaluation is done once, by the table's reader, and stays serial |
| `sort=` | lower still — one permutation, built once |
| `filter=` + `sort=` | the lowest of the six, for both reasons at once |

Every one of them is faster than a serial read; they are ordered by how much serial work the
transform leaves in front of the parallel part. The transformed cases fall short of the best case
because that work is done once, before the parallel read begins — not because any of it is
repeated. `bench/benchmark_table.sh` measures the ratios on your own machine (see CONTRIBUTING.md).

**Qc warnings are not duplicated by this.** A `qc_soft=.true.` violation prints at most once per
column per reader, and each column is read by exactly one thread, so the parallel read prints
exactly what the serial read prints — including for a column the filter itself touches, since only
the table's own reader ever evaluates the filter.

If you open readers yourself rather than through a table, `parquet_reader_adopt_transform` is the
same mechanism, available directly — see
[Sharing a filter or a sort between your own readers](#sharing-a-filter-or-a-sort-between-your-own-readers)
below.

### Growing one table from several threads

`%append` into a **shared** table is safe and needs no `!$omp critical` of your own — the table
serialises it internally:

```fortran
call parquet_new_table(out)
call out%add_column("mass", empty)          ! give it its columns BEFORE the region

!$omp parallel do default(shared)
do g = 1, nchunks
    block
        type(parquet_table) :: mine, batch  ! in a block, never in private() -- see below
        call parquet_open_table(mine, "in.parquet", lo(g), hi(g))
        call mine%materialize_all()         ! this thread opened it: allowed
        ...
        call out%append(batch)              ! serialised for you
    end block
end do
!$omp end parallel do

call out%sort_by(["id"])                    ! arrival order is not deterministic
```

Three things to know:

- **The region is append-only.** No thread may *read* the shared table while any thread is appending
  to it — an append reallocates every column's storage. This is checked and aborts, though a read
  starting at the exact instant an append does may slip past, so treat it as a rule rather than a
  net.
- **Every pointer into the shared table dies at an append.** Re-fetch `%col`/`%ref` afterwards.
- **Prepare it first.** Adding a column is a structural change and is refused inside the region, so
  the destination needs its columns before the region starts.

Declare per-thread tables inside a `block`, **never** in an OpenMP `private()` clause:
`parquet_table` is finalizable, and a `private` copy of such a type is not reliably initialised —
the first finalization then frees an undefined pointer.

The full per-operation table is in
[What a `parquet_table` allows concurrently](#what-a-parquet_table-allows-concurrently).

### Nulling elements from several threads

Validity storage is allocated lazily — a null-free column carries no bitmap at all, which is what
keeps it cheap. That means the *first* null on a column allocates, and two threads doing that at
once would race. The library refuses it rather than racing, and tells you the fix:

```fortran
call t%ensure_validity("flags")     ! or t%ensure_validity() for every resident column
!$omp parallel do
do i = 1, n
    if (bad(i)) call t%set_null("flags", i)
end do
!$omp end parallel do
```

`%ensure_validity` changes no value and no null state — it only decides *when* the allocation
happens. It belongs **before** the region: it allocates, so calling it from inside one on a shared
table is refused for the same reason `%set_null` is. A `date`/`time`/`timestamp` column never needs
it (its null state lives in the element), and a string column cannot be written from several threads
at all.

## Sharing a filter or a sort between your own readers

The safe multi-threaded pattern is one `parquet_reader` per thread (see the cases below). When the
file is read with a `filter=` or a `sort_by=`, that pattern has a hidden cost: **every one of those
readers evaluates the same filter over the same bytes, and rebuilds the same sort permutation.** On
a large file that can cost more than the parallelism saves.

`parquet_reader_adopt_transform` hands the work over instead of repeating it:

```fortran
type(parquet_reader) :: shared
type(parquet_filter) :: filt

call filt%add("mag < 20")
call parquet_open_reader(shared, "catalogue.parquet", filter=filt)   ! evaluated ONCE

!$omp parallel default(shared)
block
    type(parquet_reader) :: mine
    real(real64), allocatable :: v(:)
    call parquet_open_reader(mine, "catalogue.parquet")              ! no filter= here
    call parquet_reader_adopt_transform(mine, shared)                ! ...it adopts one
    call parquet_read_column(mine, my_column(), v)
    call parquet_close_reader(mine)
end block
!$omp end parallel

call parquet_close_reader(shared)
```

The cost of `adopt_transform` is two atomic refcount increments — a row mask and a sort permutation
are immutable Arrow arrays, so the readers share the objects rather than copying them, however large
the file. Any number of threads may adopt from one source at once, **provided the source is idle**;
it is only ever read. Adopting while another thread is calling into the source is refused rather
than raced.

It aborts rather than producing a reader whose mask describes different rows: the two readers must
be open on files with the same row and row-group counts, the adopting reader must have no filter,
sample or sort of its own, and no column may have been read on it yet. A source carrying no
transform at all is a no-op, so there is no need to ask first.

**`parquet_table` does this for you** — `%prefetch`/`%materialize_all` on a filtered or sorted table
already share one transform across their internal readers, and you write no OpenMP at all. This
procedure is for the case where you are managing the readers yourself.

## Random numbers need no rules at all

`parquet_random` is the one part of this library with nothing to say in this page's terms. It has no
shared state to protect, so there is nothing to lock, nothing to give one instance per thread, and
no ordering to preserve: every value is a pure function of `(seed, i, [draw])`, computed from those
arguments and nothing else. Call it from any number of threads at once, on any schedule.

That is stronger than thread *safety*, and the difference matters. A conventional generator can be
made safe with a lock and still be useless in a parallel loop, because which value an iteration
receives depends on how many draws happened first — and a lock does not decide that, the schedule
does. Here a draw is **reproducible**, and being reproducible it is automatically safe. See
[Random numbers](../utilities/random.html).

The single exception is `pf_random_seed()`, which by design is not a pure function — it exists to
produce a value that has never been produced before. It increments a process-wide counter with a
lock-free atomic fetch-and-add, so concurrent calls return different seeds; that is its only
interaction with other threads, and it is handled internally.

## Logging from several threads

`parquet_logging` is safe to **emit** through concurrently and unsafe to **configure**
concurrently, and the split is deliberate rather than an omission. Emission is what happens
per record, inside a region; configuration happens once, before one.

- **Emitting is safe from any number of threads**, on a shared logger or a private one. Every
  write goes through one named critical section, so two threads cannot interleave halves of a
  line. `once=` and `every=` are decided inside that same section, so a `once=` record concurrent
  across a whole team emits exactly one line rather than one per thread.
- **Configuring is not** — `%init`, `%add_console`, `%add_file`, `%add_unit`, `%set_level`,
  `%set_format`, `%set_color`, `%set_name`, `%set_rank`, `%set_thread_mode` and `%close` all
  mutate the logger without a lock. Call them before entering a parallel region. This is the same
  rule the rest of the library follows for a reader or a writer, and for the same reason: paying
  for a lock on the once-per-run path would mean paying for it on the per-record path too.
- **A `pf_logger` is safe in a `private()` clause, in a `firstprivate()` clause, and as a
  block-local variable inside a region.** It has no allocatable components and no finalizer,
  which is what makes all three legal at once — see
  [A note on functions returning `character(len=:), allocatable`](#a-note-on-functions-returning-characterlen-allocatable)
  for the neighbouring hazard, and the type's own documentation for why those two absences are
  load-bearing rather than incidental.
- **The name stack is per thread; the logger's own name is shared.**
  `pf_log_push_name`/`pf_log_pop_name` act on the calling thread's own stack, so a subprogram
  entered on one thread tags only that thread's records. `%set_name` sets the logger-wide base and
  is configuration — set it outside the region, for the same `threadprivate` reason as the context
  base below.
- **The context stack is per thread; its base is shared.** `pf_log_push_context`/`_pop_context`
  act on the calling thread's own stack, so each thread tags its records with its own frames.
  `pf_log_set_context` sets one shared base rendered ahead of them, and is configuration — set it
  outside the region. (An OpenMP `threadprivate` copy is undefined in every thread but the initial
  one at the start of a region, so a per-thread base set *before* a region would reach thread 0 and
  no other, which reads as a bug rather than as a documented limitation.)

### Buffered mode, when interleaving matters more than immediacy

`%set_thread_mode(PF_LOG_THREAD_BUFFERED)` collects each thread's records into its own slot and
writes them out at `%flush`, so one thread's records appear together instead of interleaved with
every other thread's. The cost is that nothing is visible until the flush.

**A buffered record is not on disk until `%flush` runs**, so a program that aborts mid-region
loses whatever is still in the slots. `%fatal` flushes before it aborts for exactly this reason;
an `error stop` elsewhere in your own code does not, so call `%flush` before it if the records
matter. `%close` flushes too.

## Practical cases

- Safe: many threads, each opening/writing/closing its own `parquet_writer` to a different file.
- Safe: many threads, each opening/reading/closing its own `parquet_reader` — including multiple
  threads independently opening their own reader on the *same* file at the same time (each thread's
  `parquet_open_reader` call is independent).
- Safe: parsing MAML files (`parquet_parse_maml`, `parquet_validate_maml`, etc.) concurrently across
  threads. Internally this path is lock-serialized for correctness, so it is thread-safe but not
  expected to speed up with more threads.
- **Not safe:** sharing a single `parquet_writer`/`parquet_reader` variable across threads (e.g. a
  module-level or `!$omp shared` instance that multiple threads call into at once).
- **Not safe:** two threads writing to the *same* output file at the same time, even with separate
  `parquet_writer` instances — the underlying file itself isn't safe to write from more than one
  place at once.

## Building for genuine multi-threaded use

`parquet-fortran`'s own `fpm.toml` declares a dependency on fpm's built-in `openmp` metapackage,
which automatically supplies the right compiler-specific OpenMP flag (`-fopenmp` for gfortran,
`-qopenmp` for ifx, ...) for the whole build — this project's dependency and your own project's
sources alike, since fpm computes one consistent flag set across the full resolved dependency graph.
You do not need to pass an OpenMP flag manually via `FPM_FFLAGS` for this library's own
concurrency-related code paths to run multi-threaded.

**That holds for gfortran and ifx; NAG and flang each need something from you.** Under NAG, fpm's
own probe for the compile-side flag does not fit that compiler's spelling, so the flag reaches only
the link line — add `-openmp` to `FPM_FFLAGS` yourself. Under flang, fpm contributes no OpenMP flag
at all, and the flang installations this library is tested against ship no `omp_lib` module either;
every `!$omp` block is then compiled out and the library runs its serial paths, which give the same
answers on one thread. Check `fpm build --show-model` if you are unsure which of the two you have —
it prints the flag set fpm actually computed.

If you call into this library concurrently
from your own `!$omp parallel` regions and want to be certain OpenMP is active for your own sources
too, you can add the same dependency to your own `fpm.toml`:
```toml
[dependencies]
openmp = "*"
```
(If you're developing `parquet-fortran` itself, see
[CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md#testing-genuine-openmp-concurrency)
for how this project's own tests exercise real concurrency.)

## The concurrency guard

Calling into a *shared* `parquet_writer`/`parquet_reader` from more than one thread at a time (the
"not safe" case above) is actively detected and rejected: the second concurrent caller triggers an
immediate process exit (status 134) with a diagnostic on stderr. This is a fail-fast race guard, not
a locking mechanism. Sequential, non-overlapping hand-off between threads remains allowed. (This is
one of two classes of process-abort failure in this library — see
[Error handling](error-handling.html#the-two-failure-classes) for the other, Fortran `error stop`.)

The guard is claimed at the *first* statement of a call, before any of the writer's or reader's own
bookkeeping is touched. That ordering is what makes the diagnostic reliable rather than a race in
its own right: a colliding thread is stopped before it can write anything, so the message you get
names the actual mistake instead of a segfault somewhere further along. It also means the guard
tracks *which* thread holds a handle, so the library's own nested calls into one handle are fine —
only a genuinely different thread is rejected.

Note that the diagnostic is written by the thread that detected the collision while every other
thread is still running, so on a machine with many cores you may see the message repeated, or
interleaved with unrelated runtime output, before the process dies. The message and the nonzero exit
status are the contract; the exact surrounding output is not.

## Streaming/chunked writes and reads

**The streaming row-group write API**
(`parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` — see
[Streaming/chunked writes](../io/writing.html#streamingchunked-writes)) is bound by the same "one
thread at a time per writer" rule as `parquet_write_column`, with one addition: those three calls
must also stay in strict row-group order for a given writer, since Parquet's row groups are laid out
sequentially on disk. Wrapping that triplet itself in an `!$omp parallel`/`parallel do` region —
e.g. one thread per row group — would hit the concurrency guard above (an immediate abort, not
silent corruption) at best, and doesn't make sense at any rate: row groups have no ordering
guarantee across threads, so even without the guard the file's row order would be nondeterministic.
If you want to parallelize a streaming write, parallelize the *data preparation* for each row group
(pure computation, no calls into the writer) and keep the
`parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` calls themselves
serial, on one thread, in order.

**The chunked read API** (`parquet_read_column_chunk` — see
[Streaming/chunked reads](../io/reading.html#streamingchunked-reads)) is bound by the same "one
thread at a time per reader" rule as every other reader call, but unlike the streaming write API it
has no ordering requirement: reads are stateless/random-access, so calls for different row groups
(or the same one, repeatedly) can happen in any order. This means a row-group loop *can* be
parallelized directly, as long as each thread uses its own `parquet_reader` instance opened on the
same file (independent readers on the same file are always safe to use concurrently — see "Practical
cases" above) rather than sharing one reader across threads, which would still hit the concurrency
guard.

## Thread-pool tuning

Both `parquet_open_reader` and `parquet_open_writer` accept an optional `use_threads` (`logical`,
default `.true.`):
```fortran
call parquet_open_reader(reader, "data.parquet", use_threads=.true.)
call parquet_open_writer(writer, "data.parquet", use_threads=.true.)
```
When `.true.` (the default), that reader/writer decodes or encodes column data across Arrow's
internal CPU thread pool instead of a single thread — Arrow's own library default is actually
`.false.`, so this library turns it on by default since the extra parallelism is normally a pure
win. This is on a per-reader/per-writer basis: it costs nothing to leave it on, and there's no
shared state to worry about between independent readers/writers.

The most common reason to pass `use_threads=.false.` is to avoid **oversubscription** when you're
already parallelizing at a coarser level — e.g. many OpenMP threads (see "Rules at a glance" above)
each opening their own reader/writer: without this, every one of those threads would *also* fan out
across Arrow's thread pool, so N OpenMP threads times Arrow's pool size threads end up competing for
the same cores. It's also useful for deterministic single-threaded benchmarking/profiling.

`parquet_set_arrow_threads(n)` caps the size of Arrow's thread pool itself:
```fortran
call parquet_set_arrow_threads(4)
```
Unlike `use_threads`, this is **not** a per-reader/per-writer setting — Arrow's CPU thread pool is a
single, process-global resource shared by every reader/writer (in every thread) that has
`use_threads` enabled. Call it once, e.g. near the start of your program, before opening
readers/writers on other threads; calling it repeatedly with different values from multiple
concurrent threads is a race, since each call resizes a pool everyone else is using at that same
moment. `n` must be `>= 1`; values below that fail immediately with `error stop`.

(If you're developing `parquet-fortran` itself and want to measure how these two knobs actually
affect write/read throughput on your own hardware, see
[CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md#other-tools-helpers)'s
`bench/benchmark_threads.sh` entry.)
## A note on functions returning `character(len=:), allocatable`

**No accessor in this library returns a `character(len=:), allocatable` function result.** Every one
that would naturally be written that way — `parquet_string_column`'s `get` and `summary`, the
`parquet_string` handle's `to_string`, `schema%add_col_qc`/`schema%set_col_qc` (see
[Quality control](../schema/quality-control.html#building-a-qc-maml-in-code)) and the rest — is a
**subroutine** writing into an `intent(out)`/`intent(inout)` allocatable `character` argument
instead.

That shape is deliberate and is a thread-safety measure. gfortran's codegen for *receiving* such a
function result uses a hidden length-tracking variable that is not reliably thread-local
([PR113797](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=113797); related:
[PR97977](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=97977)), so two threads calling one
concurrently — **each on its own separate object, sharing nothing** — can corrupt memory. The
subroutine form does not go near that codegen path.

**Nothing is asked of you by this.** Concurrent, independent use of any type in this library needs
no precaution on this account; the sharing rules at the top of this page remain the only
thread-safety rules that apply. It is worth knowing only if you write your own such function and
call it from several threads.

## A note on Arrow's own type-singleton construction

Arrow represents each column type (`int32`, `utf8`, ...) as a reference to a shared, process-wide
singleton, one per type, used by every thread. On some Arrow builds two pieces of lazily-populated
state hanging off those singletons are not safe to touch concurrently for the first time — the
singleton's own construction, and a "fingerprint" Arrow caches internally the first time it compares
or serializes a type. Two threads each opening their own writer or reader for a column of the *same*
type within the same instant could race on whichever piece neither had populated yet, and the
resulting corruption surfaces later, in unrelated code that next touches the heap.

**The library forces all of it into existence once, from a single thread, on the first
`parquet_open_reader`/`parquet_open_writer` any thread makes** — the same treatment it already gives
Arrow's compute-kernel registry. Every later concurrent use is then a read of already-published
state. No caller action is needed.

The one case this does not cover: if you construct `arrow::`/`parquet::` objects **yourself**, in
the same process and outside this library's API, and first touch a given type from several threads
at once, the underlying Arrow behaviour still applies to whatever the warm-up does not reach.
