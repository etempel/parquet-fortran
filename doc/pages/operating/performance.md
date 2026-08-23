---
title: Performance and memory
---

## Memory and throughput

Worth knowing before working with very large files (tens of GB and up):

- **`parquet_write_column` materializes the whole column in memory before anything is written to
  disk** — it builds that column's full data as an in-memory Arrow array (in addition to whatever
  Fortran array you already hold), and `parquet_close_writer` only starts writing once every column
  has been added this way, so peak memory for an all-`parquet_write_column` table is roughly 2-3x
  its logical (uncompressed) size. **This does not apply to streaming/chunked writes**:
  `parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` (see
  [Streaming/chunked writes](../io/writing.html#streamingchunked-writes)) bound peak memory to one
  row group's worth of data at a time instead, specifically for a column too large to hold as one
  complete array.
- **`parquet_read_column` always materializes that entire column in memory**, regardless of [row
  group](../io/writing.html#writer-options) count — `chunk_size` only controls on-disk structure,
  not how much of a column `parquet_read_column` itself reads at once. Once read, a column stays
  cached in the `reader` for its lifetime (see [Reading only touches the columns you ask
  for](../io/reading.html#reading-only-touches-the-columns-you-ask-for)), so re-reading it is free,
  but holding many large columns open at once adds up —
  [`parquet_release_column`](../io/reading.html#releasing-a-column-you-have-finished-with) frees the
  buffers of one you have finished with. `parquet_open_reader(..., prefetch=.true.)` (see
  [Prefetching multiple columns at
  once](../io/reading.html#prefetching-multiple-columns-at-once-with-parquet_prefetch_columns))
  forces this for **every** column right at open, before you've read anything — for a wide file this
  can spike peak memory well above what your program actually goes on to use, so prefer naming the
  specific columns you need via `parquet_prefetch_columns` unless you really do intend to read the
  whole file. **The bounded-memory alternatives to `parquet_read_column` itself**:
  `parquet_read_column_chunk` reads exactly one row group at a time (see [Streaming/chunked
  reads](../io/reading.html#streamingchunked-reads)); `parquet_read_array_row_mode` reads only the
  one row group a given row falls in; `parquet_read_array_element_mode` streams the file row group
  by row group rather than materializing the whole column at once; and
  `parquet_get_col_size`/`parquet_get_column_total_elements` answer from the schema alone, reading
  no column data, for every column this library writes — but a variable-length `LIST` column from a
  foreign writer has no width in the schema, so for those two the width is screened from the footer
  and then proven one row group at a time, and is a whole-column read while a `filter=` or
  `sort_by=` transform is active.
- **Concurrency multiplies this.** Each thread's own `reader`/`writer` (see [Thread
  safety](thread-safety.html)) has its own independent memory footprint — running several large
  files through concurrently costs roughly that many times the memory of one.
- **Build with optimizations for production use.** `fpm build`'s default profile applies no
  optimization to either the Fortran or the C++ side; use `fpm build --profile release` (or your own
  equivalent `-O` flags) — this measurably affects throughput for this library's numeric read/write
  paths.

Practically: make sure available RAM comfortably covers a few times the decompressed logical size of
the largest file(s) you'll have open at once, especially under concurrency.

Two things elsewhere in the guide bear directly on this. [A table's own
memory](../tables/table.html#memory) is what `parquet_table` holds on top of the reader, and
[Settings](settings.html) carries the knobs that move these numbers — the [thread
pool](settings.html#thread-pool) and the per-area thread caps, the [writer
defaults](settings.html#writer-defaults) that fix the compression codec, [row-group size when
writing](settings.html#row-group-size-when-writing), and [row-group pruning when
reading](settings.html#row-group-pruning-when-reading), which lets a filtered read skip whole row
groups rather than decode them.

## Reaching a table's values cheaply

Everything below is about `parquet_table`. It costs nothing to apply and it is where the largest
easy wins in a value-crunching program are.

**Resolve the column once, outside the loop.** Every accessor that takes a column *name* looks that
name up on every call, and on a per-cell path that lookup is **52–77% of the total** — measured on
four toolchains across three machines. So the shape of the loop matters more than anything inside
it:

```fortran
! Slowest: one name lookup per cell.
do i = 1, t%nrows()
    call t%get_element("mass", i, m)
    total = total + m
end do

! Faster: one lookup for the whole loop, every guard still in place.
type(parquet_table_col) :: c
call t%column("mass", c)
do i = 1, t%nrows()
    call c%get(i, m)
    total = total + m
end do

! Fastest, when the kind is known at compile time and no widening is wanted.
real(real64), pointer :: p(:)
call t%col("mass", p)
total = sum(p)
```

On the shipped code a hoisted handle measured **several times** the name form's throughput on a
per-cell loop, and about the same on a realistic four-column loop with arithmetic; the pointer form
is a plain array read with no call at all and measured several times faster again.
`tools/benchmark_colindex.sh` measures all three on your own machine (see CONTRIBUTING.md). See [A
column handle](../tables/table.html#a-column-handle) for what a handle can do and the two traps to
avoid — chiefly that making one is not free, so it belongs outside the loop, and that making one
*reads* the column, so a metadata sweep should use the by-position queries instead.

**Making a handle is not free, so do not make one per cell** — re-fetching one every iteration
measured *worse than the name form it replaced*, by tens of nanoseconds per cell on every toolchain
tried. For a row handle the construction dominates its use: `r = t%row(i)` followed by one `r%get`
cost **one and a half to two and a half times** what the same `r%get` costs on a handle that already
exists (three toolchains). A loop over *rows* cannot hoist a row handle — the handle names the row —
so for a column-at-a-time sweep reach for a column handle or a `%col` pointer instead, and keep the
row handle for what it is good at: passing one row to a procedure, and reading several columns of
the same row.

## Thread placement: `OMP_PLACES` and `OMP_PROC_BIND`

Threaded sorting and the bulk random-draw paths size their teams from `omp_get_num_procs()`, which
reports the **CPU affinity of the process** rather than the machine. One common environment setting
reduces that to almost nothing.

Table prefetching and the string bulk paths size themselves from `omp_get_max_threads()` instead,
which the same setting does *not* reduce — so rather than being clamped they open more threads than
the mask has processors and time-share them, and nothing warns. The fix below covers both cases.

**Set `OMP_PLACES=sockets` if you set `OMP_PROC_BIND` at all:**

```bash
export OMP_PROC_BIND=spread
export OMP_PLACES=sockets
```

**What goes wrong with `OMP_PLACES=cores`.** With binding active, the initial thread is pinned to a
single place before your program's first statement runs, and `omp_get_num_procs()` then reports the
size of that one place rather than the machine. The library clamps every sort to that number,
because threads cannot escape the mask -- a team asked for 64 on a two-processor mask really does
land on two processors and time-share them, which is slower than not threading at all.

Where this bites, it bites hard. With `OMP_PLACES=cores` a sort was measured taking the **same time
per row at every thread count from 1 to 64** -- more than twenty times what the same sort took with
`OMP_PLACES=sockets`, and with no error and no failure. `OMP_PROC_BIND=close` with
`OMP_PLACES=cores` behaves the same way.

**The library warns when this happens**, once per process:

```
WARNING: sorting is limited to 2 thread(s) because this process's CPU affinity allows no more,
although 64 were requested. This usually means OMP_PROC_BIND is set with OMP_PLACES=cores;
OMP_PLACES=sockets avoids it.
```

The warning fires only when the clamp actually reduced the thread count, so a job deliberately
confined to a small cpuset -- or one rank pinned per core with `OMP_NUM_THREADS=1` -- stays quiet.
It follows [the verbosity setting](settings.html#terminal-output), so `parquet_set_verbosity`
suppresses it along with every other warning.

**Checking what a sort will actually use**, without waiting for a warning:

```fortran
n = pf_sort_threads()   ! the resolved count for the current context
```

This already accounts for the affinity clamp, for `parquet_set_sort_threads`, and for the rule that
a sort inside your own OpenMP parallel region stays serial. If it reports a small number on a large
machine, the placement above is the first thing to check.

**Why the library cannot simply fix this itself.** The true machine size is not recoverable from
inside a bound process: the OpenMP place list is intersected with the affinity mask too, so
`omp_get_num_places()` and `omp_get_place_num_procs()` report the mask rather than the hardware. The
environment is the only place this can be corrected.
