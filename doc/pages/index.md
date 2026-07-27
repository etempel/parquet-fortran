---
title: User guide
ordered_subpage: reading.md
ordered_subpage: writing.md
ordered_subpage: supported-data-types.md
ordered_subpage: maml-format.md
ordered_subpage: building-schema-in-code.md
ordered_subpage: quality-control.md
ordered_subpage: string-columns.md
ordered_subpage: date-time.md
ordered_subpage: combined-example.md
ordered_subpage: error-handling.md
ordered_subpage: thread-safety.md
ordered_subpage: performance.md
ordered_subpage: troubleshooting.md
ordered_subpage: embedding-maml-schemas.md
---

The complete usage guide for **parquet-fortran**, beyond the quick-start overview on the
front page. For the full per-procedure API reference (every public type/function/subroutine
under `use parquet`), see the [modules](../lists/modules.html) and
[procedures](../lists/procedures.html) listings generated from source.

Ordered roughly by what a new user needs first; reference/advanced material is last. This list and
the `ordered_subpage:` frontmatter above are kept in the same order by hand — there is no automatic
check that they stay in sync, so update both together if you reorder either one.

- [Reading parquet files from your Fortran code](reading.html)
- [Writing parquet files from your Fortran code](writing.html)
- [Supported data types](supported-data-types.html)
- [The MAML metadata format](maml-format.html)
- [Building a schema in code with `schema%init`/`schema%add_field`](building-schema-in-code.html)
- [Quality control](quality-control.html)
- [Compact string columns with `parquet_string_column`](string-columns.html)
- [Date, time and timestamp columns](date-time.html)
- [Combined example: MAML schema, vector columns and metadata](combined-example.html)
- [Error handling](error-handling.html)
- [Thread safety](thread-safety.html)
- [Performance and memory](performance.html)
- [Troubleshooting](troubleshooting.html)
- [Embedding your own MAML schemas in your own project](embedding-maml-schemas.html)
