---
title: Utilities and code generation
ordered_subpage: sorting.md
ordered_subpage: generated-tables.md
ordered_subpage: embedding-maml-schemas.md
---

Things beyond the file being read or written: a general-purpose sorting API over plain Fortran
arrays, and the two generators meant to be copied into your own project.

- [Sorting arrays and columns](sorting.html) — `pf_sort`, `pf_argsort`, multi-key sorts, ranking,
  searching and set operations over plain arrays and `parquet_column`s.
- [Generated table types](generated-tables.html) — `tools/generate_user_table_code.py`: named,
  typed accessors on your own `parquet_table` extension, generated from a MAML schema — plus the
  hooks for extending a table by hand.
- [Embedding your own MAML schemas](embedding-maml-schemas.html) —
  `tools/generate_parquet_maml.sh`: compile your schemas into your project so nothing is read
  from disk at run time.
