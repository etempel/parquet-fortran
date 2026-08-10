---
title: Performance and memory
---

## Memory and throughput

Worth knowing before working with very large files (tens of GB and up):

- **`parquet_write_column` materializes the whole column in memory before anything is written to disk** — it builds that column's full data as an in-memory Arrow array (in addition to whatever Fortran array you already hold), and `parquet_close_writer` only starts writing once every column has been added this way, so peak memory for an all-`parquet_write_column` table is roughly 2-3x its logical (uncompressed) size. **This does not apply to streaming/chunked writes**: `parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` (see [Streaming/chunked writes](../io/writing.html#streamingchunked-writes)) bound peak memory to one row group's worth of data at a time instead, specifically for a column too large to hold as one complete array.
- **`parquet_read_column` always materializes that entire column in memory**, regardless of [row group](../io/writing.html#writer-options) count — `chunk_size` only controls on-disk structure, not how much of a column `parquet_read_column` itself reads at once. Once read, a column stays cached in the `reader` for its lifetime (see [Reading only touches the columns you ask for](../io/reading.html#reading-only-touches-the-columns-you-ask-for)), so re-reading it is free, but holding many large columns open at once adds up. `parquet_open_reader(..., prefetch=.true.)` (see [Prefetching multiple columns at once](../io/reading.html#prefetching-multiple-columns-at-once-with-parquet_prefetch_columns)) forces this for **every** column right at open, before you've read anything — for a wide file this can spike peak memory well above what your program actually goes on to use, so prefer naming the specific columns you need via `parquet_prefetch_columns` unless you really do intend to read the whole file. **The bounded-memory alternatives to `parquet_read_column` itself**: `parquet_read_column_chunk` reads exactly one row group at a time (see [Streaming/chunked reads](../io/reading.html#streamingchunked-reads)); `parquet_read_array_row_mode` reads only the one row group a given row falls in; `parquet_read_array_element_mode` streams the file row group by row group rather than materializing the whole column at once; and `parquet_get_col_size`/`parquet_get_column_total_elements` answer from the schema/footer with no column data read at all.
- **Concurrency multiplies this.** Each thread's own `reader`/`writer` (see [Thread safety](thread-safety.html)) has its own independent memory footprint — running several large files through concurrently costs roughly that many times the memory of one.
- **Build with optimizations for production use.** `fpm build`'s default profile applies no optimization to either the Fortran or the C++ side; use `fpm build --profile release` (or your own equivalent `-O` flags) — this measurably affects throughput for this library's numeric read/write paths.

Practically: make sure available RAM comfortably covers a few times the decompressed logical size of the largest file(s) you'll have open at once, especially under concurrency.

## Reaching a table's values cheaply

Everything below is about `parquet_table`. It costs nothing to apply and it is where the largest easy wins in a value-crunching program are.

**Resolve the column once, outside the loop.** Every accessor that takes a column *name* looks that name up on every call, and on a per-cell path that lookup is **52–77% of the total** — measured on four toolchains across three machines. So the shape of the loop matters more than anything inside it:

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

A resolve-once loop measured between **1.5x and 4x** the name form's throughput on those four toolchains; the pointer form is a plain array read with no call at all. See [A column handle](../tables/table.html#a-column-handle) for what a handle can do and the two traps to avoid — chiefly that making one is not free, so it belongs outside the loop, and that making one *reads* the column, so a metadata sweep should use the by-position queries instead.

**Making a handle is not free, so do not make one per cell.** For a row handle the construction dominates its use: `r = t%row(i)` followed by one `r%get` cost **1.37x–2.50x** (three toolchains) what the same `r%get` costs on a handle that already exists. A loop over *rows* cannot hoist a row handle — the handle names the row — so for a column-at-a-time sweep reach for a column handle or a `%col` pointer instead, and keep the row handle for what it is good at: passing one row to a procedure, and reading several columns of the same row.
