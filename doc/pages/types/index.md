---
title: Data types
ordered_subpage: supported-data-types.md
ordered_subpage: date-time.md
ordered_subpage: string-columns.md
---

What a column can hold: the supported Parquet/Arrow types and their Fortran counterparts, the
three temporal element types, and the compact variable-length string container.

- [Supported data types](supported-data-types.html) — the Fortran-kind-to-MAML type table, how
  nulls work and how a column is declared null-free, reading a column into a different numeric
  kind, the size limits, and what another producer's file can contain.
- [Date, time and timestamp columns](date-time.html) — `parquet_date`/`parquet_time`/
  `parquet_timestamp`: building a value from civil fields, ISO-8601 parsing and formatting,
  comparison and difference/offset arithmetic, Unix-time and MJD/JD interop, declared units, and
  nulls that live in the element itself.
- [Compact string columns with `parquet_string_column`](string-columns.html) — an
  offsets-plus-payload string store that needs no fixed width and no padding, plus the lightweight
  `parquet_string` handle that refers to one of its elements; searching, bulk row-set operations,
  and threading inside a single column.
