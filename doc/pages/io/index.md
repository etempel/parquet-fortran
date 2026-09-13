---
title: Reading and writing files
ordered_subpage: reading.md
ordered_subpage: filter-sort-sample.md
ordered_subpage: writing.md
---

The two things every user of this library does: read a parquet file's columns into Fortran
arrays and write Fortran arrays out as a parquet file — with, between them, the reference for
choosing which rows a read returns and in what order (filtering, sorted order, random
downsampling).

**Parquet in, parquet out.** This library reads and writes parquet and no other format; any
conversion — FITS, CSV, HDF5, anything else — happens outside it, before the library is called or
after it returns. It is a settled scope decision rather than a gap waiting to be filled, stated in
full under [Important behavior](../../index.html#important-behavior) in the README.

- [Reading parquet files from your Fortran code](reading.html) — open a file, read whole columns,
  random access by row or element, shape/type/size queries, reading a list, map or struct column,
  prefetching, reading a file's table metadata, checking values against declared qc bounds,
  printing a reader-activity summary on close, chunked reads, and what to expect from files
  written by pandas.
- [Filtering, sorting and sampling rows](filter-sort-sample.html) — `filter=`, `sort_by=` and
  `sample_fraction=`: the rule grammar — comparisons, membership in a bound set, substring matching
  and the finiteness tests — three-valued Null logic, row-group pruning, and how the three compose.
- [Writing parquet files from your Fortran code](writing.html) — open a writer, write columns,
  declare the file's contents with a MAML schema and save that schema beside the output, writing a
  large scalar string column, writing a list, map or struct column, writer options and compression,
  row groups and chunked writes, and dropping rows on the way out with a row mask.

Prefer whole files over single columns? The [tables group](../tables/index.html) is the shortcut:
one `parquet_open_table` call presents the whole file as a `parquet_table`, with every column
available by name and read only when touched.
