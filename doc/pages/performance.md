---
title: Performance and memory
---

Worth knowing before working with very large files (tens of GB and up):

- **Writing materializes the whole table in memory before anything is written to disk.** Every `parquet_write_column` call builds that column's full data as an in-memory Arrow array (in addition to whatever Fortran array you already hold), and `parquet_close_writer` only starts writing once every column has been added. There is no streaming/incremental write (see [Limitations](../index.html#limitations)). Peak memory during a write is roughly 2-3x the table's logical (uncompressed) size.
- **Reading a column always materializes that entire column in memory**, regardless of [row group](supported-data-types.html#compression-and-row-group-size) count — `chunk_size` only controls on-disk structure, not how much of a column is read at once. Once read, a column stays cached in the `reader` for its lifetime (see [Reading only touches the columns you ask for](reading.html#reading-only-touches-the-columns-you-ask-for)), so re-reading it is free, but holding many large columns open at once adds up. `parquet_open_reader(..., prefetch=.true.)` (see [Prefetching multiple columns at once](reading.html#prefetching-multiple-columns-at-once-with-parquet_prefetch_columns)) forces this for **every** column right at open, before you've read anything — for a wide file this can spike peak memory well above what your program actually goes on to use, so prefer naming the specific columns you need via `parquet_prefetch_columns` unless you really do intend to read the whole file.
- **Concurrency multiplies this.** Each thread's own `reader`/`writer` (see [Thread safety](thread-safety.html)) has its own independent memory footprint — running several large files through concurrently costs roughly that many times the memory of one.
- **Build with optimizations for production use.** `fpm build`'s default profile applies no optimization to either the Fortran or the C++ side; use `fpm build --profile release` (or your own equivalent `-O` flags) — this measurably affects throughput for this library's numeric read/write paths.

Practically: make sure available RAM comfortably covers a few times the decompressed logical size of the largest file(s) you'll have open at once, especially under concurrency.

### Combined example: Nulls, quality control and compression together

This ties together `is_valid` (writing a genuine Null), `qc=.true.` (range-check warnings), and a non-default compression codec in one small program:

```fortran
program write_parquet_qc_example
    use parquet
    use iso_fortran_env, only: int32
    implicit none

    type(parquet_schema) :: schema
    type(parquet_writer) :: writer
    type(parquet_reader) :: reader
    integer(int32) :: ra(4) = [10_int32, 400_int32, 90_int32, 200_int32]  ! 400 is out of range
    integer(int32) :: ra_read(4)
    logical :: is_valid(4) = [.true., .true., .false., .true.]           ! row 3 will be written as Null
    logical :: is_valid_read(4)

    schema%maml%name = "qc_example.maml"
    schema%maml%lines = [character(len=40) :: &
        "table: qc_example_table", &
        "fields:", &
        "- name: ra", &
        "  data_type: int32", &
        "  qc:", &
        "    min: '>= 0'", &
        "    max: '< 360'" ]

    call parquet_parse_maml(schema)

    call parquet_open_writer(writer, "data.parquet", schema, qc=.true., compression="zstd")
    call parquet_write_column(writer, "ra", ra, is_valid=is_valid)
    ! prints: WARNING: qc violation for column 'ra': declared min >= 0, max < 360, ...
    call parquet_close_writer(writer)

    call parquet_open_reader(reader, "data.parquet")
    call parquet_read_column(reader, "ra", ra_read, is_valid=is_valid_read)
    call parquet_close_reader(reader)
end program write_parquet_qc_example
```
