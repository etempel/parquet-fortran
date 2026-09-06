---
title: Utilities and code generation
ordered_subpage: sorting.md
ordered_subpage: statistics.md
ordered_subpage: random.md
ordered_subpage: spatial.md
ordered_subpage: healpix.md
ordered_subpage: index-maps.md
ordered_subpage: logging.md
ordered_subpage: configuration-files.md
ordered_subpage: utils.md
ordered_subpage: generated-tables.md
ordered_subpage: embedding-maml-schemas.md
---

Things beyond the file being read or written: a general-purpose sorting API over plain Fortran
arrays and this library's own column types, statistical reductions over those same arrays,
counter-based random numbers that survive a parallel loop — with the distributions and sampling
built on them — spatial neighbour search over coordinate arrays, the HEALPix sphere pixelisation,
fast key-to-index lookup and a recycling allocator for index values,
leveled logging for your own program, TOML configuration files, small text and path helpers, and
the two generators meant to be copied into your own project.

- [Sorting, ranking and selection](sorting.html) — `pf_sort` and `pf_argsort` over eleven element
  types, from plain arrays to a `parquet_column`, with multi-key sorts and group boundaries; then
  selecting a few elements without ordering the rest (`pf_partial_sort`, quantiles), searching a
  sorted array, distinct values and ranks, extremes, merging, matching two arrays, and what
  threading does and does not change. Also `parquet_argsort`, the smaller import for `pf_argsort`
  over the intrinsic types alone.
- [Array statistics with the `pf_` reduction family](statistics.html) — reductions over plain
  Fortran arrays: what counts as the population (a null, a NaN and a zero weight all leave it, in
  that order), what aborts and what quietly returns nothing, and the fixed optional-argument
  order every procedure in the family shares.
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
  connected components for a Friends-of-Friends group finder, keeping an index current when the
  points move, and a cell size the library measures for itself rather than asking you to pick.
- [Sphere pixelisation with `parquet_healpix`](healpix.html) — HEALPix: the direction a pixel
  covers and the pixel a direction falls in, in both numbering schemes, and the pixels of a disc;
  `pf_healpix_grid` to carry a resolution and a scheme in one object, angular separations, grid
  arithmetic, and a `_bulk` form of every conversion that threads internally. Equal-area pixels on
  rings of constant latitude, both integer kinds, and no floating-point exception raised — so a
  program running under `-ffpe-trap` needs no guard around a disc query.
- [Key-to-index lookup with `parquet_index`](index-maps.html) — `pf_index_map`: which row holds
  this key, in a few nanoseconds, over a single integer key or a tuple of them when no one column
  is unique. Three storage backends behind one API — an array indexed by the key, an open-addressing
  hash table, and sorted keys plus a binary search — the first two chosen from the keys themselves,
  the third opt-in. Then `pf_index_pool`, which hands out and recycles unique index values so a
  program managing slots in its own arrays need not track which are free. Both are safe to mutate
  from several threads at once, and a map's lookups are lock-free.
- [Logging with `parquet_logging`](logging.html) — leveled logging for your own program:
  several destinations at once each with its own threshold and layout, ISO timestamps, colour,
  a cheap `%enabled` check before an expensive message, per-thread context tags and a buffered
  mode that keeps one thread's narrative together inside an OpenMP region. Not this library's
  own messaging, which the settings page covers.
- [Configuration files with `parquet_toml`](configuration-files.html) — reading and writing TOML
  configuration files on top of `toml-f`: checked types so a wrong-typed value never leaves your
  variable undefined, defaults applied without touching the parsed document, whole-array reads
  including string lists, diagnostics that point at the offending line, and a report for every key
  or section your program never read. Safe to call from inside an OpenMP parallel region, and it
  writes the effective configuration back out.
- [Text and path helpers with `parquet_utils`](utils.html) — ASCII case folding, turning a value
  into text with a minimum width or a format of your choosing and strictly reading one back, and
  joining and taking apart POSIX paths by CPython's `posixpath` rules. A leaf module that cannot fail: nothing in it validates,
  aborts or prints, and every result comes back allocated.
- [Generated table types](generated-tables.html) — `tools/generate_user_table_code.py`: named,
  typed accessors on your own `parquet_table` extension, generated from a MAML schema. Opening one
  from a file, from a slice, or from nothing at all (`%init`, `%init_slice`, `%init_empty`);
  writing it back out; the six windows in the generated file that are yours to edit; and the hooks
  for extending a table by hand.
- [Embedding your own MAML schemas](embedding-maml-schemas.html) —
  `tools/generate_parquet_maml.sh`: compile your schemas into your project so nothing is read from
  disk at run time. Includes `set_maml`, for shipping a default schema that a user can override
  with a file of their own.
