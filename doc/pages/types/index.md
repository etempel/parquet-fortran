---
title: Data types
ordered_subpage: supported-data-types.md
ordered_subpage: column-storage.md
ordered_subpage: date-time.md
ordered_subpage: string-columns.md
ordered_subpage: list-columns.md
ordered_subpage: struct-columns.md
ordered_subpage: map-columns.md
---

What a column can hold: the supported Parquet/Arrow types and their Fortran counterparts, the
type-erased column container they are stored in, the three temporal element types, the compact
variable-length string container, and the three container columns.

- [Supported data types](supported-data-types.html) — the Fortran-kind-to-MAML type table, how
  nulls work and how a column is declared null-free, reading a column into a different numeric
  kind, the size limits, and what another producer's file can contain.
- [Column storage with `parquet_column`](column-storage.html) — one column whose type is not known
  at compile time: the nine element kinds and their fixed-width vector forms, filling one by
  adopting an array, per-element nulls and their row and element forms, geometric growth, assembling
  a column from pieces with `%append` and `%paste`, the row-set rebuilds including `%gather_from`,
  taking ownership of a container or a string store, and the zero-copy pointers into its storage.
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
  and a null element, the `parquet_list_row` handle, how a list column is adopted into a
  `parquet_column`, how one is read from and written to a Parquet file, and what nesting a
  container inside another does and does not support.
- [Struct columns with `parquet_struct_column`](struct-columns.html) — one value per declared
  field per row, the field set fixed at `%init`, the difference between an absent struct instance
  and a present one whose fields are null, the `parquet_struct_row` handle and its two read forms,
  and how a struct column relates to the unchanged dotted-path leaf reader.
- [Map columns with `parquet_map_column`](map-columns.html) — string-keyed `key -> value` entries
  per row, duplicate keys and their order preserved as stored, the three-way distinction between a
  null row, an empty one and a null value, the `parquet_map_row` handle with its by-key and
  positional lookups, the soft-fail arguments on every lookup, and the int32 entry ceiling a map
  has no way to widen past.
