---
title: Utilities and code generation
ordered_subpage: sorting.md
ordered_subpage: random.md
ordered_subpage: generated-tables.md
ordered_subpage: embedding-maml-schemas.md
---

Things beyond the file being read or written: a general-purpose sorting API over plain Fortran
arrays, counter-based random numbers that survive a parallel loop, and the two generators meant to
be copied into your own project.

- [Sorting arrays and columns](sorting.html) — `pf_sort`, `pf_argsort`, multi-key sorts, ranking,
  searching and set operations over plain arrays and `parquet_column`s.
- [Random numbers](random.html) — `pf_random_at` and friends: draws addressed by seed, stream and
  position, so a value does not depend on how many draws came before it and a parallel loop
  reproduces exactly under any schedule or thread count.
- [Generated table types](generated-tables.html) — `tools/generate_user_table_code.py`: named,
  typed accessors on your own `parquet_table` extension, generated from a MAML schema — plus the
  hooks for extending a table by hand.
- [Embedding your own MAML schemas](embedding-maml-schemas.html) —
  `tools/generate_parquet_maml.sh`: compile your schemas into your project so nothing is read
  from disk at run time.
