---
title: Schemas, metadata and quality control
ordered_subpage: maml-format.md
ordered_subpage: building-schema-in-code.md
ordered_subpage: quality-control.md
ordered_subpage: combined-example.md
---

Declaring what a file contains — column names, types, units, descriptions, table metadata and
quality-control bounds — either in a MAML file on disk or built directly in code, ending on a
worked example that combines all of it.

- [The MAML metadata format](maml-format.html) — the schema file format itself: sections, keys,
  data types, and how a MAML becomes a parquet file's header.
- [Building a schema in code](building-schema-in-code.html) — `schema%init`/`schema%add_field`:
  the same schema without a file on disk.
- [Quality control](quality-control.html) — `qc: min:`/`max:`/`miss:` bounds, write-side
  enforcement, read-side checking, and building a qc-maml in code.
- [Combined example](combined-example.html) — MAML schema, vector columns, metadata, qc and
  compression in one complete program.
