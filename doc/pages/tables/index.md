---
title: Whole tables in memory
ordered_subpage: table.md
ordered_subpage: table-open.md
ordered_subpage: table-write.md
ordered_subpage: table-mutate.md
---

The high-level layer: one `parquet_open_table` call presents a whole parquet file as a
`parquet_table`, hands its columns back as ordinary Fortran arrays or zero-copy pointers, and
reads each column only when something first touches it. A table need not come from a file at all —
it can equally be built from scratch in memory and written back out through an ordinary
`parquet_schema`.

- [Whole tables in memory: the basics](table.html) — opening a table and reaching a column with
  `%get` or `%col`, nulls, laziness and what it costs, what a table can tell you about itself, the
  row and column handles, row selections, string and container columns, changing a column's type,
  and the current limitations and memory cost.
- [Opening a table: slices, filters and renaming](table-open.html) — reading part of a file (the
  slice regime), filtering, sorting and qc-checking rows as they are read, reading a filtered file
  larger than memory one row group at a time, renaming columns and carrying units through a
  read-in MAML, finding which file row a row came from, and what a table can and cannot read.
- [Building a table and writing it out](table-write.html) — `parquet_new_table` and `%add_column`,
  then `parquet_write_table` with a schema or without one, its writer options, and carrying the
  source file's metadata into the output.
- [Changing a table](table-mutate.html) — replacing values, editing nulls, adding and dropping
  columns, filtering/sorting/ranking/appending rows and the detach rule they share, plus searching
  and ranking a column through a `%col` pointer.
