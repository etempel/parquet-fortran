---
title: Data types
ordered_subpage: supported-data-types.md
ordered_subpage: date-time.md
ordered_subpage: string-columns.md
ordered_subpage: list-columns.md
---

What a column can hold: the supported Parquet/Arrow types and their Fortran counterparts, the
three temporal element types, the compact variable-length string container, and the
variable-length list container.

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
- [Variable-length list columns with `parquet_list_column`](list-columns.html) — rows that hold
  different numbers of values, the offsets-plus-payload layout, the difference between a null row
  and a null element, the `parquet_list_row` handle, and how a list column is adopted into a
  `parquet_column`, and how one is read from and written to a Parquet file.
