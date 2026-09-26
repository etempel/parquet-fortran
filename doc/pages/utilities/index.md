---
title: Utilities and code generation
ordered_subpage: sorting.md
ordered_subpage: statistics.md
ordered_subpage: kernel-density.md
ordered_subpage: solvers.md
ordered_subpage: integration.md
ordered_subpage: interpolation.md
ordered_subpage: cosmology.md
ordered_subpage: optimization.md
ordered_subpage: prima.md
ordered_subpage: root-finding.md
ordered_subpage: transforms.md
ordered_subpage: random.md
ordered_subpage: spatial.md
ordered_subpage: healpix.md
ordered_subpage: sphere.md
ordered_subpage: skycoord.md
ordered_subpage: index-maps.md
ordered_subpage: logging.md
ordered_subpage: configuration-files.md
ordered_subpage: utils.md
ordered_subpage: generated-tables.md
ordered_subpage: embedding-maml-schemas.md
---

Things beyond the file being read or written: a general-purpose sorting API over plain Fortran
arrays and this library's own column types, statistical reductions over those same arrays, kernel
density estimates of a sample's distribution, adaptive numerical integration, interpolation of
tabulated data, distances and times in an expanding universe, minimisation of a function you supply,
finding where a function of one variable crosses zero, the discrete cosine and sine transforms,
counter-based random numbers that survive a parallel loop — with the distributions and sampling
built on them — spatial neighbour search over coordinate arrays, the HEALPix sphere pixelisation,
random points in sky polygons and HEALPix masks with the RA/Dec geometry they need, celestial
coordinate systems with the separations and offsets that need none, fast key-to-index lookup and a
recycling allocator for index values, leveled logging for your own program, TOML configuration
files, small numeric, text and path helpers, and the two generators meant to be copied into your own
project.

- [Sorting, ranking and selection](sorting.html) — `pf_argsort` over eleven element types, from
  plain arrays to a `parquet_column`, and `pf_sort` over the nine of them that are not container
  types, with multi-key sorts and group boundaries; then selecting a few elements without ordering
  the rest (`pf_partial_sort`, quantiles), searching a sorted array, distinct values and their
  counts, ranks, extremes, merging, matching two arrays, mapping values through a lookup table, and
  what threading does and does not change. Also `parquet_argsort`, the smaller import for
  `pf_argsort` over the intrinsic types alone.
- [Array statistics with the `pf_` reduction family](statistics.html) — reductions over plain
  Fortran arrays: counts and moments, order statistics and quantiles, the median absolute deviation
  and the mode, two-sample covariance and correlation, the normal-probability family (rankits, the
  probit mean, the straight-line fit), the sigma clip, the running folds and bins, linear
  (cloud-in-cell) binning onto a grid, and `pf_stats`, which answers many of them off one pass. Then
  the rules they all share: what counts as the population (a null, a NaN and a zero weight all leave
  it, in that order), what aborts and what quietly returns nothing, and the fixed optional-argument
  order every procedure in the family shares.
- [Kernel density estimation with `parquet_kde`](kernel-density.html) — `pf_kde`: the density a
  one-dimensional sample was drawn from, fitted over a retained sorted copy and answered exactly
  anywhere as a density, a distribution function or a quantile, on a grid for plotting, and drawn
  from as a sample; `pf_kde_grid`: the same estimate accumulated on fixed cells from points streamed
  through it and forgotten, merged across threads. Four kernels with the bandwidth always the
  kernel's standard deviation, chosen by the Improved Sheather-Jones rule by default or by
  Silverman's, Scott's, least-squares cross-validation or a number, `adjust=`, an adaptive kernel
  narrow where the data are dense and wide where they are sparse, weights and nulls under the
  statistics family's rules with the effective sample size in the rules, and a bounded support
  corrected by reflection, renormalisation or a local linear fit.
- [Solver conventions: callbacks, tolerances, budgets and outcomes](solvers.html) — what the four
  callback-driven modules below share, stated once: the two forms a function may take and the dummy
  names an extension has to repeat, `rtol` and `atol` and what each engine measures them on,
  `max_neval` and whether a run can overshoot it, `converged=` and `info=` and the status codes the
  modules spell alike, the evaluation record, what your function may return, and what aborts. Each
  engine's own page keeps its engines, its examples and its tables.
- [Numerical integration with pf_integrate](integration.html) — adaptive quadrature of a function of
  one variable over a finite or infinite range, with the integrand supplied as an object carrying
  its own parameters or as a plain function. Tolerances as a required `rtol` and an optional `atol`,
  a budget on integrand evaluations, a status code rather than a printed warning, integration in
  `log x` for a range spanning many decades, an outward walk that finds a feature far along an
  infinite range rather than stepping over it, and a record of every evaluation on request whose
  weighted sum reproduces the integral. Nothing is printed.
- [Interpolation of tabulated data with parquet_interpolate](interpolation.html) — an interpolant
  built once over a table of abscissae and ordinates, ascending or descending, and evaluated,
  differentiated and integrated anywhere: straight lines between the points, a cubic spline with a
  natural, not-a-knot or clamped end, or a shape-preserving cubic that keeps monotone data monotone;
  the same over values on a rectilinear grid, bilinear or bicubic; a policy for queries beyond the
  table (the end value, the end segment continued, or NaN), a mask that drops points before
  building, and a one-shot form for a table queried only a few times. Every binding that reads an
  object is `pure`, so one object serves a whole team of threads. Nothing is printed.
- [Distances and times in an expanding universe with parquet_cosmology](cosmology.html) — a
  cosmology built once and evaluated over whole columns: the comoving, transverse, luminosity and
  angular diameter distances, the lookback time and the age, the comoving volume and its element,
  the distance modulus and the transverse angular scales, what the universe is made of at a redshift
  and its critical density, with the redshift at a given distance, lookback time, age, luminosity
  distance or distance modulus. Eight named cosmologies, or any flat, open, closed, `wCDM` or
  `w0waCDM` model of your own, on astropy's `w0waCDM` definition with its literals. No redshift is
  ever refused: a query beyond the table is answered by quadrature instead, so `zmax` decides only
  how fast. Every binding that reads an object is `pure`, so one object serves a whole team of
  threads. Nothing is printed.
- [Optimisation: minimising a function of one or many variables](optimization.html) — four engines
  in two tiers: Brent's method on a bracket and the Nelder-Mead simplex from a start point for a
  local minimum, differential evolution over a whole box and a multistart driver over a spread of
  starts for a global one. The objective is supplied as an object carrying its own parameters or as
  a plain function. Relative and absolute tolerances (`rtol`, `atol`) on the value spread, a soft
  budget reported through a status code rather than a printed warning, a record of the search on
  request, `threads=` with one clone of the objective per thread and the same answer at every count,
  and what `converged` does and does not promise. The one thing it can print is a thread-clamp
  notice, which `parquet_set_verbosity`, re-exported here, silences.
- [Powell's derivative-free solvers with `parquet_prima`](prima.html) — `pf_minimize_bobyqa`,
  `pf_minimize_lincoa` and `pf_minimize_cobyla`, vendored from PRIMA: a quadratic model
  interpolating a set of points and minimised inside a trust region, which on a smooth objective
  costs a fraction of what a direct-search method costs, and a linear-model simplex method for
  constraints of any shape. Which engine the constraints choose for you, bounds honoured at every
  evaluation rather than only at the end, a start that is never moved, linear constraints as arrays
  and nonlinear ones from the objective with PRIMA's sign convention, the two trust-region radii and
  what each is for, `ctol` and what an infeasible answer looks like, `scale=` for coordinates of
  different magnitudes, and `pf_bobyqa_solver` to drive the multistart driver with BOBYQA.
- [Root finding: solving f(x) = 0 in one variable](root-finding.html) — `pf_find_root`, Brent's
  method on a bracket, with the function supplied as an object carrying its own parameters or as a
  plain function. When the bracket does not yet change sign it is widened first under a growth
  policy you state — upward, downward or both ways, by a factor, within limits — and the expansion
  stops at the first sign change it meets. Absolute and relative tolerances, full precision at any
  magnitude under the defaults, an infinite function value used as a sign, a missing sign change or
  a spent budget reported through a status code rather than an abort, `info%froot` to tell a pole
  from a root, and a record of every evaluation on request. Nothing is printed.
- [Transforms: the discrete cosine and sine transforms](transforms.html) — `pf_dct`, `pf_idct`,
  `pf_dst` and `pf_idst`: the type-II discrete cosine and sine transforms of a sequence whose length
  is a power of two, and their exact inverses, in scipy's convention, factor of two included,
  unnormalised or orthonormal. `pf_is_pow2` and `pf_next_pow2` for choosing the length before the
  sequence is built, and why zero-padding one already built is not a substitute. How to pass a
  spectrum from one family to the other, which needs a shift of one position. Nothing is printed.
- [Random numbers](random.html) — `pf_random_at` and friends: draws addressed by seed, stream and
  position, so a value does not depend on how many draws came before it and a parallel loop
  reproduces exactly under any schedule or thread count. Uniforms, raw bits and bounded integers;
  `pf_random_stream` for when you cannot say up front how many you need; five distributions
  (exponential, normal, normal truncated to an interval, Gamma, Poisson); random points on a sphere:
  directions, discs about an axis, balls, a Gaussian-like scatter and rotations; then permutations,
  subsets and resampling, and weighted draws without replacement. Those last live in
  `parquet_sampling`, the sibling module for drawing from a *population* rather than drawing a
  number.
- [Spatial neighbour search with `pf_spatial_index`](spatial.html) — a uniform-grid index over plain
  coordinate arrays: ball and annulus search into a buffer you own, the self-join as CSR or as an
  edge list, the `k` nearest neighbours, segment, cylinder and cone shapes around an axis, and
  search on the sky by angular radius. Two or three dimensions, optional periodic boundaries,
  connected components for a Friends-of-Friends group finder, keeping an index current when the
  points move, and a cell size the library measures for itself rather than asking you to pick.
- [Sphere pixelisation with `parquet_healpix`](healpix.html) — HEALPix: the direction a pixel covers
  and the pixel a direction falls in, in both numbering schemes, and the pixels of a disc;
  `pf_healpix_grid` to carry a resolution and a scheme in one object, the separation of two
  directions, grid arithmetic, and a `_bulk` form of every conversion that threads internally.
  Equal-area pixels on rings of constant latitude, both integer kinds, and no floating-point
  exception raised — so a program running under `-ffpe-trap` needs no guard around a disc query.
- [Random points and geometry on the sphere with parquet_sphere](sphere.html) — points uniform per
  solid angle inside a sky polygon, with straight RA/Dec edges or great-circle edges, inside one
  HEALPix pixel or over a list of them; the polygon's containment, area and acceptance; RA/Dec
  conversions that name their declination frame, and the Fibonacci grid. Every draw is addressed
  like `pf_random_at`, so a catalogue reproduces under any schedule.
- [Celestial coordinate systems with `parquet_skycoord`](skycoord.html) — sky positions converted
  between ICRS, Galactic, ecliptic, supergalactic and FK5 J2000 coordinates, by a named procedure,
  by two selectors read at run time or by a rotation prepared once, each rotation built from the
  three angles that define it and agreeing with astropy to rounding; the RA/Dec geometry that needs
  no coordinate system: the separation of two positions, the position a separation away at a
  position angle, that angle back, a position moved by its proper motion, a position as a unit
  vector, and a field projected onto the plane tangent at its centre; positions written as
  sexagesimal text and read back strictly; and a heliocentric redshift in the CMB rest frame. Every
  conversion is `pure elemental`, so a whole column converts in one call.
- [Key-to-index lookup with `parquet_index`](index-maps.html) — `pf_index_map`: which row holds this
  key, in a few nanoseconds, over a single integer key, a tuple of them when no one column is
  unique, or a string. Three storage backends behind one API — an array indexed by the key, an
  open-addressing hash table, and sorted keys plus a binary search — the first two chosen from the
  keys themselves, the third opt-in. `pf_index_multimap` is the same over a key that repeats: every
  row holding it, as a range, and every match for a whole probe array at once as a CSR pair. Then
  `pf_index_pool`, which hands out and recycles unique index values so a program managing slots in
  its own arrays need not track which are free. All three are safe to mutate from several threads at
  once; a map's and a multimap's lookups take no lock at all, while a pool guards its queries as
  well as its mutations.
- [Logging with `parquet_logging`](logging.html) — leveled logging for your own program: several
  destinations at once each with its own threshold and layout, ISO timestamps, colour, a cheap
  `%enabled` check before an expensive message, per-thread context tags and a buffered mode that
  keeps one thread's narrative together inside an OpenMP region. Not this library's own messaging,
  which the settings page covers.
- [Configuration files with `parquet_toml`](configuration-files.html) — reading and writing TOML
  configuration files on top of `toml-f`: checked types so a wrong-typed value never leaves your
  variable undefined, defaults applied without touching the parsed document, whole-array reads
  including string lists, diagnostics that point at the offending line, and a report for every key
  or section your program never read. Safe to call from inside an OpenMP parallel region, and it
  writes the effective configuration back out. Then `parquet_cosmology_config`, the module that
  joins this one to the cosmology tier: a whole cosmology in a `[cosmology]` section, named or
  spelled out, and a record of which one a run used.
- [Numeric, text and path helpers with `parquet_utils`](utils.html) — division that does not raise a
  flag, the standard normal distribution and its quantile function, angle wrapping and conversion,
  the cross product, ASCII case folding, turning a value into text with a minimum width or a format
  of your choosing and strictly reading one back, and joining and taking apart POSIX paths by
  CPython's `posixpath` rules. A leaf module that cannot fail: nothing in it validates, aborts or
  prints, and every result comes back allocated.
- [Generated table types](generated-tables.html) — `tools/generate_user_table_code.py`: named, typed
  accessors on your own `parquet_table` extension, generated from a MAML schema. Opening one from a
  file, from a slice, or from nothing at all (`%init`, `%init_slice`, `%init_empty`); writing it
  back out; the seven windows in the generated file that are yours to edit; and the hooks for
  extending a table by hand.
- [Embedding your own MAML schemas](embedding-maml-schemas.html) — `tools/generate_parquet_maml.sh`:
  compile your schemas into your project so nothing is read from disk at run time. Includes
  `set_maml`, for shipping a default schema that a user can override with a file of their own.
