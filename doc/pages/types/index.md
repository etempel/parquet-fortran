---
title: Data types
ordered_subpage: supported-data-types.md
ordered_subpage: date-time.md
ordered_subpage: string-columns.md
---

What a column can hold: the supported Parquet/Arrow types and their Fortran counterparts, the
three temporal element types, and the compact variable-length string container.

- [Supported data types](supported-data-types.html) — the full type table, null values,
  cross-kind reads, and what other producers' files can contain.
- [Date, time and timestamp columns](date-time.html) — `parquet_date`/`parquet_time`/
  `parquet_timestamp`: civil-time construction, Unix-time interop, units, and nulls that live in
  the element itself.
- [Compact string columns with `parquet_string_column`](string-columns.html) — an
  offsets-plus-payload string store that needs no fixed width and no padding, with its own
  builder, accessor and bulk APIs.
