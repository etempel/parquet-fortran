---
title: Utilities and code generation
ordered_subpage: sorting.md
ordered_subpage: random.md
ordered_subpage: spatial.md
ordered_subpage: healpix.md
ordered_subpage: generated-tables.md
ordered_subpage: embedding-maml-schemas.md
---

Things beyond the file being read or written: a general-purpose sorting API over plain Fortran
arrays and this library's own column types, counter-based random numbers that survive a parallel
loop — with the distributions and sampling built on them — spatial neighbour search over
coordinate arrays, the HEALPix sphere pixelisation, and the two generators meant to be copied into your own project.

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
- [Spatial neighbour search with `pf_spatial_index`](spatial.html) — a uniform-grid index over
  plain coordinate arrays: ball and annulus search into a buffer you own, the self-join as CSR or
  as an edge list, the `k` nearest neighbours, segment, cylinder and cone shapes around an axis,
  and search on the sky by angular radius. Two or three dimensions, optional periodic boundaries,
  connected components for a Friends-of-Friends group finder, and a cell size the library measures
  for itself rather than asking you to pick.
- [Sphere pixelisation with `parquet_healpix`](healpix.html) — HEALPix: the direction a pixel
  covers and the pixel a direction falls in, in both numbering schemes, and the pixels of a disc.
  Equal-area pixels on rings of constant latitude, both integer kinds, and no floating-point
  exception raised — so a program running under `-ffpe-trap` needs no guard around a disc query.
- [Generated table types](generated-tables.html) — `tools/generate_user_table_code.py`: named,
  typed accessors on your own `parquet_table` extension, generated from a MAML schema. Opening one
  from a file, from a slice, or from nothing at all (`%init`, `%init_slice`, `%init_empty`);
  writing it back out; the six windows in the generated file that are yours to edit; and the hooks
  for extending a table by hand.
- [Embedding your own MAML schemas](embedding-maml-schemas.html) —
  `tools/generate_parquet_maml.sh`: compile your schemas into your project so nothing is read from
  disk at run time. Includes `set_maml`, for shipping a default schema that a user can override
  with a file of their own.
