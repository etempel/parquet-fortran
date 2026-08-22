---
title: Schemas, metadata and quality control
ordered_subpage: maml-format.md
ordered_subpage: building-schema-in-code.md
ordered_subpage: quality-control.md
ordered_subpage: combined-example.md
---

Declaring what a file contains — column names, types, units, descriptions, table metadata and
quality-control bounds — either in a MAML file on disk or built directly in code. A MAML can also
declare how a file is *read*: which columns get renamed, which rows are kept, and in what order they
come back. The group ends on two worked examples that put the pieces together.

- [The MAML metadata format](maml-format.html) — the schema file format itself: the `fields:`
  section and its data types, deferring a `col_size`/`array_size` until write time with `auto`,
  renaming columns on the way out with `col_map:` and on the way in with `extra: remap:`, declaring
  a read-time filter and sort order with `extra: filter:`/`extra: sort:`, and how a MAML's
  table-level keys become the parquet file's own metadata.
- [Building a schema in code](building-schema-in-code.html) — `schema%init`/`schema%add_field`: the
  same schema without a file on disk, choosing which of its columns a given program writes, adding
  and clearing table metadata at run time, and printing a column listing.
- [Quality control](quality-control.html) — `qc: min:`/`max:`/`miss:` bounds: what each one checks,
  building a qc-maml in code, deferring the declaration to read time with `parquet_read_qc`, and
  write-side and read-side enforcement.
- [Combined examples](combined-example.html) — two complete programs: one driving a MAML schema with
  a vector column, a dropped column and runtime metadata; one combining Nulls, qc and compression.
