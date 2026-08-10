---
title: Reading and writing files
ordered_subpage: reading.md
ordered_subpage: filter-sort-sample.md
ordered_subpage: writing.md
---

The two things every user of this library does: read a parquet file's columns into Fortran
arrays, and write Fortran arrays out as a parquet file — plus the reference for restricting a
read to a subset of the rows (filtering, sorted order, random downsampling).

- [Reading parquet files from your Fortran code](reading.html) — open a file, read whole columns,
  random access by row or element, shape/type queries, prefetching, chunked reads.
- [Filtering, sorting and sampling rows](filter-sort-sample.html) — `filter=`, `sort_by=` and
  `sample_fraction=`: the rule grammar, three-valued Null logic, row-group pruning, and how the
  three compose.
- [Writing parquet files from your Fortran code](writing.html) — open a writer, write columns,
  row-group control, writer options and compression.

Prefer whole files over single columns? The [tables group](../tables/index.html) is the shortcut:
one `parquet_open_table` call presents the whole file as a `parquet_table`, with every column
available by name and read only when touched.
