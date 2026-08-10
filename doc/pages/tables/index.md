---
title: Whole tables in memory
ordered_subpage: table.md
ordered_subpage: table-open.md
ordered_subpage: table-write.md
ordered_subpage: table-mutate.md
---

The high-level layer: one `parquet_open_table` call presents a whole parquet file as a
`parquet_table`, hands its columns back as ordinary Fortran arrays or zero-copy pointers, and
reads each column only when something first touches it.

- [Whole tables in memory: the basics](table.html) — opening a table, `%get`/`%col`, nulls,
  laziness, rows, string columns, changing a column's type, and the current limitations.
- [Opening a table: slices, filters and renaming](table-open.html) — reading part of a file (the
  slice regime), read-time filters/sorts/qc, renaming columns through a read-in MAML, and what
  the table cannot read.
- [Building a table and writing it out](table-write.html) — `parquet_new_table`, `%add_column`,
  and `parquet_write_table` with or without a schema.
- [Changing a table](table-mutate.html) — replacing values, editing nulls, adding and dropping
  columns, filtering/sorting/ranking/appending rows, and the detach rule they share.
