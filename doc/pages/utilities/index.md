---
title: Utilities and code generation
ordered_subpage: sorting.md
ordered_subpage: random.md
ordered_subpage: generated-tables.md
ordered_subpage: embedding-maml-schemas.md
---

Things beyond the file being read or written: a general-purpose sorting API over plain Fortran
arrays and this library's own column types, counter-based random numbers that survive a parallel
loop — with the distributions and sampling built on them — and the two generators meant to be
copied into your own project.

- [Sorting arrays and columns](sorting.html) — `pf_sort` and `pf_argsort` over eleven element
  types, from plain arrays to a `parquet_column`, with multi-key sorts and group boundaries; then
  selecting without sorting (`pf_partial_sort`, `pf_nth_element`, quantiles), searching a sorted
  array, distinct values and ranks, extremes, merging, and what threading does and does not change.
  Also `parquet_argsort`, the smaller import for `pf_argsort` over the intrinsic types alone.
- [Random numbers](random.html) — `pf_random_at` and friends: draws addressed by seed, stream and
  position, so a value does not depend on how many draws came before it and a parallel loop
  reproduces exactly under any schedule or thread count. Uniforms, raw bits and bounded integers;
  `pf_random_stream` for when you cannot say up front how many you need; four distributions
  (exponential, normal, Gamma, Poisson); then permutations, subsets and resampling, and weighted
  draws without replacement. Those last live in `parquet_sampling`, the sibling module for drawing
  from a *population* rather than drawing a number.
- [Generated table types](generated-tables.html) — `tools/generate_user_table_code.py`: named,
  typed accessors on your own `parquet_table` extension, generated from a MAML schema. Opening one
  from a file, from a slice, or from nothing at all (`%init`, `%init_slice`, `%init_empty`);
  writing it back out; the six windows in the generated file that are yours to edit; and the hooks
  for extending a table by hand.
- [Embedding your own MAML schemas](embedding-maml-schemas.html) —
  `tools/generate_parquet_maml.sh`: compile your schemas into your project so nothing is read from
  disk at run time. Includes `set_maml`, for shipping a default schema that a user can override
  with a file of their own.
