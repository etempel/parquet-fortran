# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- **Numerical integration: `parquet_integrate` and `pf_integrate`.** Adaptive quadrature of a
  function of one `real64` variable over a finite or infinite range, by the 21-point Gauss-Kronrod
  rule with adaptive bisection and Wynn-epsilon extrapolation, which is on unless
  `extrapolate=.false.` turns it off and which keeps the cost of an integrable endpoint
  singularity from growing with the tolerance. The integrand is an object extending `pf_integrand`,
  with its parameters as components and an `eval` that may update them, or a plain function;
  tolerances are `rtol` or a
  `pf_tolerance` with `atol`; `max_neval` bounds the integrand evaluations and is never exceeded;
  `log_base=` integrates a range spanning many decades in `log x`. Either bound may be
  `pf_infinity()` or its negative, spelling `[a, +inf)`, `(-inf, b]` and `(-inf, +inf)`; an
  infinite range is integrated by an outward walk that searches for a first panel the integrand is
  not negligible on and then steps outward a factor of e at a time, so a feature far along the
  range is found rather than stepped over, and `max_panels=` caps the panels one walk may use.
  `breakpoints=` cuts the range at named interior points and integrates each piece on its own,
  which is how a feature the first rule application would not sample is named rather than hunted
  for; the pieces share the evaluation budget and `atol`. `converged=` and `info=`
  (`pf_integration_info`: status, error estimate, evaluation count, panels) report the outcome and
  nothing is printed; an integrand that returns a NaN or an infinity ends the integration rather
  than the process, reporting `PF_INT_BAD_VALUE` with the offending point in `info%non_finite_at`.
  `points=` (`pf_integration_points`) records every abscissa, weight and value
  of the final partition, whose weighted sum reproduces `info%partition_integral`, and the returned
  result too whenever `info%extrapolated` is false. Reentrant: integrate from as
  many threads as you like, one integrand object per thread. An Arrow-free entry module.
  `bench/benchmark_integrate.sh` measures it. See
  [Numerical integration with pf_integrate](doc/pages/utilities/integration.md).
- **Root finding: `parquet_root` and `pf_find_root`.** Solves `f(x) = 0` for one `real64` variable
  on a bracket by Brent's method, the function given as an object extending `pf_rootfun` or as a
  plain function. `expand=` (`pf_bracket_expansion`) widens a bracket whose ends have the same sign
  under a policy the caller states — the upper end, the lower end or both, by a factor, within
  limits and a number of tries — and stops at the first sign change. `tol` and `rtol` set the
  tolerances, full precision at any magnitude by default; `max_neval` bounds the evaluations,
  expansion included; an infinite function value is used as a sign, and a NaN is an `error stop`.
  `converged=` and `info=` (`pf_root_info`: status, counts, `f` at the root, the bracket) report
  the outcome, a missing sign change or a spent budget as a status rather than an abort, and
  nothing is printed; `history=` (`pf_root_history`) records every evaluation. An Arrow-free entry
  module. See [Root finding](doc/pages/utilities/root-finding.md).
- **Discrete cosine and sine transforms: `parquet_transform`, `pf_dct`, `pf_idct`, `pf_dst` and
  `pf_idst`.** The type-II discrete cosine and sine transforms of a `real64` sequence whose length
  is a power of two, and their exact inverses, in scipy's convention (`scipy.fft.dct`, `idct`,
  `dst` and `idst`, factor of two included), unnormalised or orthonormal (`norm="ortho"`, whose
  exceptional coefficient is the first for the cosine pair and the last for the sine pair);
  `pf_is_pow2` and `pf_next_pow2` choose the length. Nothing is printed. An Arrow-free entry
  module. See [Transforms: the discrete cosine and sine
  transforms](doc/pages/utilities/transforms.md).
- **Linear binning: `pf_bin_linear`.** Deposits a sample onto a grid of points by linear
  (cloud-in-cell) assignment, splitting each value's weight between the two grid points around it
  into shares that sum to it exactly. In `parquet_stats` beside `pf_histogram`, with its
  population arguments (`is_valid`, `weights`, `skipnan` and the three exclusion counts); `mass`
  holds one entry per grid point. See
  [Array statistics](doc/pages/utilities/statistics.md#pf_bin_linear--linear-cloud-in-cell-binning-onto-a-grid).
- **Optimisation: `parquet_optimize`.** `pf_minimize_scalar` minimises a function of one variable
  on a bracket by Brent's method; `pf_minimize_simplex` minimises a function of one or many
  variables by the Nelder-Mead simplex, from a start point and a per-coordinate step, with
  fractional and absolute tolerances on the value spread. `pf_minimize_de` searches a whole box by
  differential evolution from a seed rather than a start point, with a Latin-hypercube initial
  population, `ftarget=`, an optional final simplex inside the box (`polish=`) and the final
  population on
  request; `pf_minimize_multistart` runs a local solver (`pf_simplex_solver`, or one a caller
  supplies) from a Latin hypercube of starts and counts the distinct minima. Both take `threads=`,
  evaluate through one clone of the objective per thread, and give the same answer at every thread
  count. The objective is an object extending `pf_objective`, with its parameters as components and
  an `eval` that may update them, or a plain function. `max_neval` bounds the evaluations and is
  soft by one engine step; `info=` (`pf_optimize_info`: status, convergence, counts, the final
  spread, the non-finite count, the distinct-minimum count) reports the outcome and nothing is
  printed, while a caller mistake and a non-finite objective value in a local engine are
  `error stop`; the population engines treat a non-finite value as a point outside the domain.
  `history=` (`pf_optimize_history`) records every evaluation, every generation's best, or every
  start's minimum, trimmed to the records in use. `pf_constrained_objective` is declared here so
  that every engine which does not honour nonlinear constraints refuses one. An Arrow-free entry
  module. See [Optimisation](doc/pages/utilities/optimization.md).
- **Powell's derivative-free solvers: `parquet_prima`.** `pf_minimize_bobyqa` (bounds),
  `pf_minimize_lincoa` (linear equality and inequality constraints as arrays) and
  `pf_minimize_cobyla` (nonlinear constraints from an objective extending
  `pf_constrained_objective`) minimise a function of several variables without derivatives.
  BOBYQA and LINCOA fit a quadratic model to `npt` interpolation points and minimise it in a trust
  region whose radius falls from `rhobeg` to `rhoend`; COBYLA fits linear models of the objective
  and of every constraint over a simplex. The engines are vendored from
  [PRIMA](https://github.com/libprima/prima) (Zaikun Zhang, BSD-3-Clause) at commit `43863c69`,
  reworked to this library's rules: fixed kinds, no printing layer, `pf_objective` in place of the
  procedure interface, `ieee_arithmetic` in place of the hand-rolled predicates, and a refusal in
  place of upstream's moderated extreme barrier. BOBYQA's bounds are honoured at every evaluation,
  not only at the end, and its start point is never moved — where PRIMA would move it, `rhobeg`
  shrinks instead and the radius reached comes back in `info%rho`. Constraint values follow
  PRIMA's convention, `c(x) <= 0` where feasible; `info%cstrv` is the violation at the returned
  point, measured in the caller's units, and `info%status` is `PF_OPT_INFEASIBLE` exactly when it
  exceeds `ctol`. `scale=` gives each coordinate its characteristic magnitude, so one pair of
  trust-region radii serves a problem whose variables differ by orders of magnitude, and the
  bounds and constraint matrices are transformed with it. `pf_bobyqa_solver` drives
  `pf_minimize_multistart` with BOBYQA from each start. Where PRIMA adjusts an invalid argument
  and warns, this refuses with a message. Shares `pf_objective`, `pf_constrained_objective`,
  `pf_optimize_info` and `pf_optimize_history` with `parquet_optimize` and re-exports them. An
  Arrow-free entry module. See
  [Powell's derivative-free solvers](doc/pages/utilities/prima.md).
- **Kernel density estimation: `parquet_kde`.** `pf_kde` fits a one-dimensional density to an
  array it retains, `real64` or `real32`, or to a numeric `parquet_column`, and answers `%pdf`,
  `%cdf` and `%quantile` exactly anywhere, `%curve` on equally spaced points and `%sample` from
  the estimate; `pf_kde_grid` streams points into a fixed grid and forgets them, with `%merge` for
  per-thread accumulation and `%density`, `%pdf`, `%cdf`, `%quantile` and `%sample` read from the
  grid. Four kernels (`gaussian`, `epanechnikov`, `bspline`, `box`), with the bandwidth the
  kernel's standard deviation; the Improved Sheather-Jones rule (the default), Silverman's and
  Scott's rules, a number, and `adjust=`; per-element weights and nulls under the `pf_*` family's
  rules, with `n_eff` in the rules; `lower=`/`upper=` for a bounded support, corrected by
  `boundary=` `"reflect"` (the default), `"renormalise"` or `"linear"` -- the last two the
  degree-zero and degree-one members of the local-polynomial boundary kernel, each dividing the
  summed kernels by the mass a kernel centred at the QUERY point keeps inside the support and then
  by the estimate's own integral, `"linear"` additionally setting negative values to zero.
  `adaptive=.true.` selects the sample-point adaptive kernel (each
  point's bandwidth from a pilot density, sensitivity `alpha=`, capped by `bandwidth_max=`, read
  back through `%bandwidths`, `%bandwidth_at` and `%pilot`), and the streaming form takes the same
  rule from a `pilot=` grid. A grid's lifecycle is `%init` -> `%add`* -> `%finish` -> query, with
  `%is_finished`, `finish=.true.` on `%add` and `%merge` as the one-line form, and `%clear` to
  reopen it. `threads=` on the bulk forms of both; draws addressed by `(seed, stream)`. An
  Arrow-free entry module. `bench/benchmark_kde.sh` measures it. See
  [Kernel density estimation](doc/pages/utilities/kernel-density.md).
- **A direction inside a HEALPix pixel, not just at its centre**:
  `pf_healpix_grid%pix2vec_offset(ipix, dx, dy, vec)` is `%pix2vec` generalised to any position in the
  pixel's square in the equal-area projection, so a `(dx, dy)` uniform over the unit square is a
  direction uniform over the pixel.
- **HEALPix neighbours**: `pf_neighbours_nest(nside, ipix, nb)` and `pf_neighbours_ring` return a
  pixel's eight neighbours (`-1` at a missing corner). See
  [HEALPix](doc/pages/utilities/healpix.md).
- **Interpolation of tabulated data: `parquet_interpolate`.** `pf_interp_1d` builds an interpolant
  over a table of `real64` abscissae and ordinates, ascending or descending, with `method=`
  `"linear"`, `"cubic"` (a C2 spline with `bc=` `"natural"`, `"not_a_knot"` or `"clamped"` with
  `slopes=`) or `"pchip"` (a shape-preserving cubic that keeps monotone data monotone), and answers
  `%eval`, `%derivative` and `%integral` anywhere; `outside=` says whether a point beyond the table
  is clamped to the end value, extrapolated or answered as NaN, and `is_valid=` drops points before
  building. `pf_interp_2d` is the same over values on a rectilinear grid, bilinear or bicubic.
  `pf_interp` is the one-shot form of both. Every binding that reads an object is pure, so one
  object may be shared read-only by any number of threads. Nothing is printed. An Arrow-free entry
  module. `bench/benchmark_interpolate.sh` measures it. See
  [Interpolation of tabulated data](doc/pages/utilities/interpolation.md).
- **Distances and times in an expanding universe: `parquet_cosmology`.** `pf_cosmology` is built
  once from one of eight named cosmologies (`Planck18`, `Planck15`, `Planck13`, `WMAP9`, `WMAP7`,
  `WMAP5`, `WMAP3`, `WMAP1`, matched without regard to case) or from parameters of your own —
  `h0`, `om0`, and optionally `ode0` (absent means flat, with `Ok0` exactly zero), `tcmb0`, `neff`,
  `m_nu`, `ob0`, `w0` and `wa` — on astropy's `w0waCDM` definition with astropy 8.0.1's own
  constants and Komatsu fit, so a `"Planck18"` agrees with astropy's to about `1e-8`. Every
  binding that takes a redshift is `pure elemental`, so a whole column converts in one call:
  `%comoving_distance`, `%comoving_transverse_distance`, `%luminosity_distance`,
  `%angular_diameter_distance`, `%angular_diameter_distance_z1z2`, `%lookback_time`, `%age`,
  `%efunc`, `%inv_efunc`, `%hubble`, `%distmod`, `%comoving_volume`,
  `%differential_comoving_volume`, `%kpc_proper_per_arcmin`, `%kpc_comoving_per_arcmin`,
  `%arcsec_per_kpc_proper`, `%arcsec_per_kpc_comoving`, `%lookback_distance`, the contents of the
  universe at a redshift — `%om`, `%ode`, `%ok`, `%ogamma`, `%onu`, which sum to one, with
  `%tcmb`, `%w`, `%de_density_scale` and `%critical_density` in M_sun/Mpc^3 — and the inverses
  `%z_at_comoving_distance`, `%z_at_lookback_time`, `%z_at_age`, `%z_at_luminosity_distance` and
  `%z_at_distmod`; plus the model's own parameters and derived values, `%is_flat`,
  `%has_massive_nu`, `%get_name`, `%describe`, `%m_nu` and `%clear`.
  `%init` tabulates three integrals over a grid in `zeta = ln(1+z)`; a query beyond the table is
  answered by a fixed 20-point Gauss-Legendre rule from its edge, so **no redshift is refused and
  `zmax=` decides only how fast**. The age has its own table rather than being `age(0)` minus the
  lookback time, which would keep no correct digit at high redshift. The domain runs from just
  above `z = -1` to `z = 1e10`; outside it, and for a NaN, every binding answers NaN quietly
  without raising an IEEE flag, so a catalogue's `-99` sentinels pass through an elemental call.
  `%distmod(0)` is `-Infinity`, `%arcsec_per_kpc_*(0)` is `+Infinity`, and a model whose age
  integral diverges answers `+Infinity` at every redshift. `%z_at_age` solves on `ln(age)` rather
  than on `age(0)` minus a lookback time, and `%z_at_luminosity_distance` and `%z_at_distmod`
  answer a redshift at or above zero, the smallest one where `D_L` takes the value given. `%init` and `%clear` are the only
  bindings that write an object, so one built cosmology may be evaluated from any number of
  threads at once. The free functions `pf_z2zeta`, `pf_zeta2z` and `pf_z_combine` need no
  cosmology and keep their digits where the obvious forms lose them. Arrow-free, settings-free and
  silent.
- **Random points on a sphere, in `parquet_random`.** `pf_random_direction_at` draws a uniform unit
  vector and `pf_random_radec_at` the same point as `(ra, dec)` in degrees; `pf_random_disc_at` and
  `pf_random_disc_radec_at` draw uniformly within an angular radius of a direction or a sky
  position, with `r_inner=` for a ring; `pf_random_ball_at` draws uniformly in a ball or a shell;
  `pf_random_vmf_at` and `pf_random_vmf_radec_at` draw a von Mises–Fisher direction about a centre,
  by `kappa` or by `sigma_deg`; `pf_random_rotation_at` draws a uniform rotation matrix. Each is
  addressed by seed, stream and draw like every other draw and has a `pf_random_stream` producer
  costing one block; the direction and RA/Dec forms have draw-axis fills, and `%address` returns a
  stream's seed and stream index. `pf_random_disc_cap` prepares one disc or ring once, so a loop
  over it pays the radius validation, the centre's normalisation and the frame once instead of per
  draw; `%at` is `pf_random_disc_at` to the bit. An `r_inner` above `pi` is refused rather than
  clamped. `pf_random_pair_spare_at` returns two uniforms of one enciphering plus an exactly uniform
  integer drawn from the 22 bits their conversions discard, so a composite draw need not encipher
  twice. `pf_sphere_algorithm` freezes the family's values. See
  [Random numbers](doc/pages/utilities/random.md).
- **Random points in sky regions, and sky geometry: `parquet_sphere`.** `pf_sky_polygon` holds a
  polygon given by `(ra, dec)` vertices, with straight edges in the RA/Dec chart or great-circle
  edges, answers `%contains`, `%area`, `%acceptance` and `%is_simple`, and draws points uniform per
  solid angle inside it by coordinate (`%random_at`, `%random_fill`) or along a `pf_random_stream`
  (`%random_next`). A self-intersecting polygon's `%area` and `%acceptance` are the even-odd
  quantities the sampler actually uses, measured on a fixed lattice, rather than a signed sum, and
  the acceptance floor `%init` applies is that same quantity. `%init(..., strict=.true.)` refuses a
  chart polygon that looks like an RA band written the short way across `ra = 0`.
  `pf_random_pixel_at` and `pf_random_mask_at` draw uniformly inside one HEALPix pixel or over a
  list of them on a `pf_healpix_grid`, with `_radec`, fill and stream forms; a point in a pixel
  costs one block and rejects nothing, read through `%pix2vec_offset`, and a mask draw costs the
  same one block by taking its choice of a listed pixel from the bits the point leaves spare.
  `pf_radec2vec` and `pf_vec2radec` convert between degrees and unit vectors in a named declination
  frame, and `pf_fibonacci_grid` places `n` quasi-uniform directions. `pf_sky_region_algorithm`
  freezes the samplers' values. An Arrow-free entry module. See
  [Random points and geometry on the sphere](doc/pages/utilities/sphere.md).
- **Celestial coordinate systems: `parquet_skycoord`.** `pf_icrs2gal`, `pf_gal2icrs`,
  `pf_icrs2ecl`, `pf_ecl2icrs`, `pf_gal2sgal`, `pf_sgal2gal`, `pf_icrs2sgal`, `pf_sgal2icrs`,
  `pf_icrs2fk5` and `pf_fk52icrs` convert sky positions between the ICRS, Galactic, ecliptic,
  supergalactic and FK5 J2000 systems as astropy defines them, in degrees, `pure elemental` so a
  whole column converts in one call; `pf_sky_convert` does the same with the systems named by
  `PF_COORD_*` selectors rather than by the procedure, `pf_sky_rotation` prepares such a conversion
  once and applies it elementally (`%init`, `%apply`, `%is_init`), and `pf_coord_system_name` and
  `pf_coord_system_from_name` turn a selector into a token and back for a system read out of a
  configuration file. Each rotation is built from the three angles that define it — the target
  system's north pole and the target longitude of the source's north pole — so it is orthonormal by
  construction, and the inverse is the transpose rather than a second matrix. The module also holds
  the RA/Dec geometry that needs no coordinate system: `pf_angdist_deg`, `pf_offset_radec` and
  `pf_position_angle_deg`, which offset a position by a separation at a position angle and recover
  the angle, and `pf_apply_pm`, which moves a position by its proper motion along a great circle,
  `pm_ra` being Gaia's `pmra`, the rate in right ascension times `cos(dec)`. `pf_ra2str`,
  `pf_dec2str` and `pf_radec2str` write positions as sexagesimal text, `10:21:30.550 +41:16:09.00`
  or blank- or letter-separated, and `pf_str2ra`, `pf_str2dec` and `pf_str2radec` read three-field
  text back strictly, reporting what they cannot read through an `ok` flag; `pf_deg2hms`,
  `pf_deg2dms`, `pf_hms2deg` and `pf_dms2deg` split an angle into its fields and join them, a
  declination's sign apart from its degrees. `pf_zhel2zcmb` takes a heliocentric redshift into the
  rest frame of the cosmic microwave background, with Planck 2018's dipole unless another is given.
  Total, like the rest of the sky tier: a NaN argument comes back as a NaN, or as the text `nan`,
  raising no floating-point flag, and a declination outside `[-90, 90]` is read as the direction it
  names, except as `pf_offset_radec`'s centre or `pf_apply_pm`'s starting position, where it stops
  the program. The declination frame of `parquet_healpix` is a different thing and does not enter
  here. An Arrow-free entry module. See
  [Celestial coordinate systems](doc/pages/utilities/skycoord.md).

### Changed

- **`%pairs_within_los` sweeps in one pass** and returns the same pairs in the same order, several
  times faster. **A 3D index whose points fill part of their bounding box gets a finer grid**: the
  cells-per-point ceiling counts occupied cells (0.3 per point), with 4 cells of the bounding box
  per point as the bound.
- **The index tier's automatic thread count stops at 64** (`pf_index_map`, `pf_index_multimap`,
  and a table's `%build_index`, `%find_many` and hash join), and `parquet_set_index_threads(n)`
  replaces that ceiling, raising the automatic answer as well as lowering it.
  `parquet_set_healpix_threads(n)` likewise replaces the bulk HEALPix forms' ceiling of 64 rather
  than only lowering it.
- **String keys in `pf_index_map` and `pf_index_multimap` hash each 8-byte word whole, with the
  hash's two chains cross-fed**, so keys differing only in bytes 5-8, 13-16, ... no longer share
  hashes. `%probe_stats` reports the longest run of string keys sharing one hash
  (`max_hash_chain=`), and a map warns once when 32 of its keys share one.
- **`pf_angdist_deg` moved from `parquet_healpix` to `parquet_skycoord`.** Its arguments, result
  and totality are unchanged and `use parquet` sees no difference; a program that imported
  `parquet_healpix` alone for it now imports `parquet_skycoord`. `pf_angdist`, the vector form,
  stays in `parquet_healpix`.

### Fixed

- `pf_stddev`, `pf_variance`, `pf_sem` and `pf_moments` answer a sample scaled to either end of the
  representable range. The sum of squared deviations overflows above a magnitude of about `1e150`
  and underflows to zero below about `1e-170`, so a standard deviation an ordinary `real64` holds
  was answered as `+Infinity` or, worse, as `0` for a population that has spread; where the plain
  accumulation returns something that is not finite and positive, the deviations are now scaled by
  the largest of them and the sum retaken. Every other answer is unchanged, to the last bit.
- `pf_spatial_index%within_segment`, `%within_cylinder` and `%within_cone` find the points lying
  exactly on their axis when the radius is zero, and refuse an axis whose squared length overflows
  rather than answering as a ball about its first endpoint. The zero-radius answer holds under a
  value-unsafe floating-point model too: ifx's default `-fp-model=fast`, which a FLAGLESS
  `fpm build` selects, rewrote the projection's division into a multiply by the reciprocal and
  dropped the on-axis points whose parameter is not representable (19 of 21 on a lattice row), and
  on a target whose baseline has a fused multiply-add, such as arm64, contracting the perpendicular
  offset dropped the same points (5 of 21).
- Writing from or reading into a large non-contiguous array, such as a component section
  `data(:)%x`, no longer crashes under ifx: `parquet_write_column`, `parquet_write_column_chunk`,
  `parquet_read_column`, `parquet_read_column_chunk`, `parquet_read_array_row_mode`,
  `parquet_read_array_element_mode`, and the character-array `%set_all`, `%append_values` and
  `%build_from` of `parquet_column` and `parquet_string_column`.
- `pf_index_pool%compact` releases the pool's list of freed indexes: a compacted pool holds one bit
  per index below its watermark instead of 8 bytes per free index.
- Many other minor fixes and improvements.

## [v2.4.0] - 2026-09-14

**Compatibility:** parquet files written by earlier 2.x releases are read unchanged. Library 
messages gain a class marker, the emitting procedure and file context, and every listing printed 
without `unit=` follows `message_stream`.

### Added

- **Grouping and aggregation**: `parquet_table%group_by` partitions a table's rows by key columns
  into a `parquet_grouping` that computes per-group statistics (the `parquet_stats` vocabulary or a
  procedure of your own), applies callbacks, broadcasts results back onto the rows and builds
  one-row-per-group summary tables. See [Grouping rows and aggregating per
  group](doc/pages/tables/table-group.md).
- **Writing a table incrementally**: `parquet_table_writer` keeps an output file open and appends
  tables or rows to it one row group at a time, `parquet_write_table_chunk` writes a table as one
  row group of an open writer, and `parquet_derive_schema`/`parquet_open_writer_like` derive a
  table's schema. `parquet_write_table` gains `row_index_name=`, and a schema can declare a column
  nullable with `extra: nullable_cols:`. See
  [An output file that stays open](doc/pages/tables/table-write.md#an-output-file-that-stays-open-parquet_table_writer).
- **More `parquet_table` verbs**: filling and dropping nulls (`%fillna`, `%ffill`, `%bfill`,
  `%dropna`), several columns at once (`%get_matrix`, `%set_matrix`, `%drop_columns`,
  `%keep_columns`), text/number conversion (`%parse_column`, `%format_column`), row-set operations
  (`%explode`, `%drop_duplicates`, `%sort_by_values`, `%value_counts`), filtering by expression
  (`%filter_rows`, `%row_mask`), a lookup index over one column (`%build_index`) and a row viewer
  (`%print_rows`). See [Changing a table](doc/pages/tables/table-mutate.md).
- **Richer row filters**: set membership (`in`/`not_in`, over a bound array or a literal list),
  substring matching (`starts_with`, `ends_with`, `contains`) and `is_finite`/`is_not_finite`; the
  filter, sort-key and read-QC specifications gain `%clear`. See
  [Filtering, sorting and sampling rows](doc/pages/io/filter-sort-sample.md).
- **Dictionary-encoded columns** (as pandas writes a `Categorical`) are read as ordinary columns of
  their values, and `parquet_get_column_arrow_type` reports any column's stored Arrow type. See
  [Reading dictionary columns from other tools](doc/pages/types/supported-data-types.md#reading-dictionary-columns-from-other-tools).
- **Normal-distribution statistics**: `pf_probit`, `pf_norm_cdf`, `pf_norm_sf` and `pf_norm_pdf` in
  `parquet_utils`, and `pf_normal_scores`, `pf_probit_fit`, `pf_probit_scale` and `pf_probit_mean`
  in `parquet_stats`. See
  [Statistics](doc/pages/utilities/statistics.md#pf_probit_fit-and-pf_probit_scale--the-normal-probability-plot).
- **Spatial and index-map additions**: line-of-sight cylinder searches
  (`pf_spatial_index%within_los`, `%pairs_within_los`), a `combine=` rule for per-point radii,
  string keys and bulk dictionary encoding in `pf_index_map`, the one-to-many `pf_index_multimap`,
  and `int32` answers from the bulk spatial and index queries. See
  [Spatial neighbour search](doc/pages/utilities/spatial.md) and
  [Index maps](doc/pages/utilities/index-maps.md).
- **Smaller utilities**: truncated normal draws (`rng%normal_truncated`), `pf_value_counts` and
  `pf_remap` over arrays, bulk gather and mask primitives on both column types, and `pf_from_str`,
  `pf_safe_div`, angle wrapping and conversion, `pf_cross_product` and `pf_log_now`.

### Changed

- **Library messages**: every message opens with its class marker, names the procedure it came
  from and, from a read, write or schema path, the file. Advice is a new `NOTE: ` class silenced at
  `verbosity="silent"`. Every printer given no `unit=` follows `message_stream`, and
  `%print_schema_info` with no destination writes there instead of aborting. See
  [Terminal output](doc/pages/operating/settings.md#terminal-output).
- **`parquet_set_spatial_rebuild_warning`, `parquet_get_spatial_rebuild_warning` and
  `PARQUET_FORTRAN_SPATIAL_REBUILD_WARNING` are removed**; `verbosity="silent"` silences the
  message they governed.
- `parquet_list_column%set_null` and `parquet_map_column%set_null` drop the row's elements, so a
  null row is zero-length and the column can be written.
- `parquet_table%cast` refuses a predefined column without `force=.true.`; `parquet_table%append`
  null-fills a column the appended table has not read.
- **Faster and more parallel**: `parquet_table%join` matches on a hash engine, sort-tier run
  detection and `%print_stat` thread, and `pf_index_map` builds and bulk lookups thread and are
  faster (`%get_many` is no longer `pure`). Answers are unchanged.

### Fixed

- A `string` column whose byte payload exceeds 2 GiB could not be read.
- `pf_chord2_from_angle` was wrong for angles at or above pi and below zero.
- A column's unit could be dropped from a schema-less `write_maml=.true.` sidecar, or taken from
  another column after `%add_column` followed a column drop.
- `parquet_string_column%print` failed with an I/O error on elements longer than two characters.
- Several `parquet_stats` procedures raised `IEEE_INVALID` or `IEEE_OVERFLOW` on infinities, NaNs
  or very large weights while answering correctly, aborting under trapping compilers.
- `pf_cov(x, x)` could differ from `pf_variance(x)` by one ulp under a compiler using a
  value-unsafe floating-point model, such as ifx's default `-fp-model=fast`.
- `pf_index_map%keys`/`pf_index_multimap%keys` into an `int32` list silently truncated a key below
  the `int32` range instead of aborting, in an ifx `-O0 -check all` build.
- Many other minor fixes and improvements.

## [v2.3.0] - 2026-09-06

### Added

- **`parquet_index`**: an Arrow-free entry module for fast key-to-index lookup. `pf_index_map` maps
  a single integer key, or a tuple of them, to an index value over three storage backends chosen
  automatically from the keys; `pf_index_pool` hands out and recycles unique index values. Both are
  safe to mutate from several threads at once, and map lookups are lock-free. Adds an
  `index_threads` setting (`PARQUET_FORTRAN_INDEX_THREADS`). See
  [Key-to-index lookup](doc/pages/utilities/index-maps.md).
- **`parquet_toml`**: an Arrow-free entry module for reading and writing TOML configuration files,
  built on [toml-f](https://github.com/toml-f/toml-f) — this library's first Fortran package
  dependency, so every consumer now fetches it. Checked types that quote the offending source line
  instead of leaving your variable undefined, defaults applied without writing them into the parsed
  document, whole-array and string-list reads, and a report of every key and section the program
  never read. Safe to call from inside an OpenMP parallel region. See
  [Configuration files](doc/pages/utilities/configuration-files.md).
- **Matching and joining.** `parquet_table%join` matches another table's rows against this one's on
  one or more key columns and brings that table's columns over, mutating this table in place and
  detaching it unless every row survives exactly once and in place; `how=` covers inner, left,
  right, outer, semi and anti. At the array level `pf_match`, `pf_match_all` and `pf_in` answer the
  same question over plain arrays for all eleven element types, with neither array needing to be
  sorted. A key must be of exactly the same kind on both sides, a null matches nothing including
  another null, and a NaN is a value. See [Joining two tables](doc/pages/tables/table-join.md).
- **`parquet_open_table(..., bounded=.true.)` reads a filtered file larger than memory**: the filter
  is evaluated one row group at a time and every column assembled from per-row-group chunks, so the
  peak is one row group of one column rather than one whole column. Opt-in, never faster on a file
  that fits, and refused with `sort=`. See
  [Opening a table](doc/pages/tables/table-open.md#reading-a-file-larger-than-memory-bounded).
- **`pf_lower_bound`, `pf_upper_bound` and `pf_equal_range` accept an array of targets**, answered
  against one extraction of the key, so *m* targets cost `O(n + m log n)` where *m* separate calls
  cost `O(m*n)`.

### Changed

- `parquet_get_metadata` aborts when the reader has not been opened, as every other procedure taking
  a `parquet_reader` already did. With `default=` present it previously returned that default, so a
  use-before-open was indistinguishable from a key that is genuinely absent.

### Fixed

- `pf_argsort` over a `pf_sort_keys` built from an empty array returns a zero-length permutation and
  a single sentinel `group_offsets` entry, as the array forms always did. It previously reported one
  row, naming a row that does not exist and claiming one group over no rows. `character` keys were
  unaffected.
- `threads=` is honoured by the grouped sort path, which backs `pf_argsort(..., group_offsets=)`,
  `pf_unique_count`, `pf_unique` and `pf_rank`. It was previously accepted and ignored there, so
  those operations always sorted serially; results are unchanged at every thread count.
- Many other minor fixes and improvements.

## [v2.2.0] - 2026-09-03

### Added

- **`parquet_utils`**: an Arrow-free leaf module of text and path helpers — `pf_to_lower`/
  `pf_to_upper`, `pf_to_str` for rendering a number or a `logical` as text, and `pf_join_path`,
  `pf_dirname`, `pf_basename`, `pf_path_ext`, `pf_path_stem`, `pf_split_path` and
  `pf_path_add_suffix` for taking a path apart and rebuilding it. See
  [Text and path helpers](doc/pages/utilities/utils.md).
- **`parquet_stats`**: an Arrow-free entry module of `pf_*` array statistics over `real64`,
  `real32`, `int32`, `int64` and `logical` arrays and scalar numeric `parquet_column`s, each
  taking an optional null mask, NaN policy and weights. Counts and moments (`pf_count_valid`,
  `pf_sum`, `pf_mean`, `pf_variance`, `pf_stddev`, `pf_sem`, `pf_skewness`, `pf_kurtosis`,
  `pf_moments`), order statistics (`pf_median`, `pf_quantile`, `pf_quantiles`, `pf_iqr`,
  `pf_trim_mean`, `pf_percentile_of_score`, `pf_mad`, `pf_mode`), `pf_gmean`/`pf_hmean`,
  `pf_cov`/`pf_corr` (Pearson or Spearman), `pf_zscore`, `pf_sigma_clipped_stats`, the running
  folds `pf_cumsum`/`pf_cumprod`/`pf_cummax`/`pf_cummin`, and binning with `pf_bucketize`,
  `pf_histogram` and `pf_bin_edges`. A `pf_stats` accumulator summarises a population once —
  resident, incremental (`%update`) or merged from parallel parts (`%merge`) — and answers any
  number of queries off it; `pf_describe` and `%print` render pandas' `describe()` block. The
  moment pass is threaded, with `threads=`, and is bit-identical at every thread count. See
  [Array statistics](doc/pages/utilities/statistics.md).
- **`parquet_logging`**: a general-purpose logger for the calling program — a `pf_logger` type and
  `pf_log_*` procedures on a process-wide default logger, with eight severity levels, several
  sinks at once (console, a file, or a unit you own) each with its own threshold, layout, colour
  and flush policy, templated line layouts, `once=`/`every=` deduplication, per-name level
  overrides, per-thread name and context stacks, a caller-supplied rank filter and
  `pf_log_configure_from_env`. It is a leaf module and the library itself does not use it.
- **`parquet_healpix`**: the HEALPix sphere pixelisation, Arrow-free. Direction to pixel and back
  from angles or unit vectors, RING/NEST conversion, resolution changes, disc queries
  (`pf_query_disc` and its allocating, counting and bounding forms), grid arithmetic over `nside`,
  `npix`, order, pixel area, resolution and rings, and angular separation. Every conversion has a
  `_bulk` form over whole arrays with an optional `threads=`, and `pf_healpix_grid` carries an
  `nside`, scheme and declination convention as one object with an RA/Dec layer on top.
- **`parquet_spatial`**: `pf_spatial_index`, an Arrow-free uniform-grid spatial index over plain
  coordinate arrays in two or three dimensions. Ball search, the self-join as CSR, pair-list and
  count-only forms, `k`-nearest search and `k`-th neighbour distances, segment/cylinder/cone
  searches around an axis, and angular search on the sky backed by either a 3D grid or HEALPix.
  Queries take an inner radius, can return rows ordered by distance, and take one radius or one
  per point; periodic boundaries and an automatic cell size are supported. `pf_connected_components`
  labels an edge list's components, making a Friends-of-Friends group finder one further call.
- **`parquet_list`, `parquet_struct` and `parquet_map`**: three Arrow-free container column types
  — `parquet_list_column` (variable-length rows), `parquet_struct_column` (a fixed field set of
  possibly differing types) and `parquet_map_column` (string-keyed `key -> value` entries) — each
  with a lightweight row handle, nine scalar payload kinds, per-row and per-element null tracking,
  deep copy, move and a `%gather_rows` rebuild. A `parquet_column` takes ownership of one through
  the new `%adopt_container`. `parquet_read_column`/`parquet_read_column_chunk` read `LIST`,
  `LARGE_LIST`, `STRUCT` and `MAP` columns from a file straight into them, whole or one row group
  at a time, and `parquet_write_column`/`parquet_write_column_chunk` write them back out, declared
  in a schema as `list[<elemtype>]`, `struct` or `map[<valuetype>]`. A container nested inside
  another is readable — reached through `%nested`, or by a descent path such as
  `"list_of_struct[].x"` — and writing one is refused with a message naming the column.
- **A `parquet_table` column can be a list, a map or a struct.** A `MAP` column is classified
  automatically, a variable-length `LIST` becomes a `parquet_list_column` when
  `parquet_open_table`'s new `list_columns="container"` asks for it, and a `parquet_struct_column`
  is added in memory with `%add_column`. Each gets `%col`, `%get`, `%set`, `%add_column` and a
  column handle's `%ref`; every row-structural mutation carries them along, `%print_stat` reports
  the shortest and longest row, and `parquet_write_table` writes them back out. Element-granular
  validity, `%get_slice`, `%get_element` and row-handle access are refused, since a container row
  has no fixed-width cell.
- **Two new schema queries answered from the file metadata alone**: `parquet_get_column_shape`
  reports whether a column is a `"scalar"`, `"vector"`, `"list"`, `"map"`, `"struct"` or
  `"unknown"`, and `parquet_get_map_value_type` reports a map column's value type.

### Changed

- **`pf_nth_element` and `pf_nth_quantile` are 2.5-3.9x faster on a large array**, and
  `pf_minmax`/`pf_argminmax` are faster too. `pf_quantiles`, `pf_median` and `pf_iqr` follow suit.
  Answers are unchanged.
- **`pf_minmax` and `pf_nth_quantile` take an optional `ok=`, so an all-null population can be
  reported instead of aborting.** Omitting the argument restores the abort; `pf_argminmax` is
  unchanged.
- **`parquet_write_table`'s `copy_metadata=`/`metadata_keys=` no longer carry a key the writer
  generates itself** — `DATE`, `name`, the two `IVOA.VOTable-Parquet.*` keys and every
  `column.<name>.<attr>` entry. `copy_metadata=.true.` skips them; `metadata_keys=` naming one is
  now an error.

### Fixed

- **Concurrent `parquet_open_reader`/`parquet_open_writer` calls no longer corrupt the heap while a
  file date is pinned.** Every setting mirrored to the C++ side is now atomic or mutex-guarded.
- **`parquet_get_col_size` and `parquet_get_column_total_elements` report a variable-length `list`
  column's element count** — the sum of its rows' own lengths, rather than its row count — **and
  see through an Arrow extension, dictionary or run-end-encoded wrapper** to the width of the
  fixed-size list underneath.
- **`schema%get_field` and `schema%add_field_from` keep a temporal column's unit and UTC flag.** A
  `timestamp[ns,utc]` column copied with `%add_field_from` became a bare `timestamp`.
- **A NaN is left out of a float column's reported minimum and maximum.** `%print_stat` and a
  write-time `qc: min:`/`max:` violation warning both report the range over the values that can be
  ordered, and a column whose every value is a NaN reports `NaN`. Either previously gave a
  processor-dependent answer, and aborted under a compiler running with the IEEE traps unmasked.
- Many other minor fixes and improvements.

## [2.0.0] - 2026-08-24

**Toolchain floor:** gfortran ≥ 13 (13 on CI; 15.2.0 the primary development target), and —
confirmed by hand rather than by CI — Intel Fortran (ifx) 2026.1.0/2026.1.1, NAG Fortran 7.2, and
LLVM flang 22.1.8 (serial builds only). Also fpm ≥ 0.13.0, a C++20-capable C++ compiler, and
Arrow/Parquet C++ ≥ 24.0.0 (validated locally against 25.0.0). See
[Prerequisites](README.md#prerequisites) for the full detail.

**SemVer scope:** the promise covers the whole `use parquet` surface — every public
type/procedure/constant reachable that way, including a public type's own type-bound procedures and
operators — and, new in this release, each advertised entry module *in its own right*
(`parquet_io`, `parquet_tables`, `parquet_columns`, `parquet_strings`, `parquet_temporal`,
`parquet_sorting`, `parquet_argsort`, `parquet_sampling`, `parquet_random`, `parquet_settings`,
`parquet_version`, `parquet_maml_base`), since a module offered as an entry point has to stay usable
through that import alone. `parquet_core`, `parquet_bindings` and every `*_base`/`*_engine`/
`*_kernel` module are explicitly outside it.

**Compatibility:** three source-incompatible changes and two behaviour changes, each listed under
Changed — `parquet_set_max_threads` is renamed, `sample_seed=` widens to `integer(int64)`, and
`parquet_get_version(v, mode=)` is removed; a file written without an explicit `compression=` now
uses zstd rather than snappy, and a schema field declaring no `qc: miss:` no longer means "no Nulls
allowed". Parquet files written by 1.0.0 are read unchanged.

### Added

- **`parquet_tables` (`parquet_table`)**: a whole parquet file as one in-memory object.
  `parquet_open_table` reads each column on first use; `%get` hands one back as an ordinary Fortran
  array and `%col` as a zero-copy typed pointer. Tables can be built in memory (`parquet_new_table`,
  `%add_column`, `%append`), mutated (`%sort_by`, `%filter_rows`, `%top_n`, `%delete_rows`,
  `%truncate`, `%cast`), opened as a row slice, cloned, written back out (`parquet_write_table`),
  and used from several threads with the library enforcing the rules rather than documenting them.
- **`parquet_columns` (`parquet_column`)**: type-erased whole-column value storage over 18 scalar
  and vector kinds, with per-element null tracking and the full value and structural instruction
  set.
- **`parquet_sorting`**: sorting for plain Fortran arrays and column types over eleven element
  types — `pf_argsort`, `pf_sort`, `pf_permute`, `pf_is_sorted`, `pf_partial_argsort`, multi-key
  sorting through `pf_sort_keys`, binary search, unique and reduction operations, and group
  boundaries from the same pass as the sort. Read-time sorting, `parquet_table%sort_by` and
  `pf_argsort` are one engine, so they cannot disagree about null placement, NaN placement or tie
  order.
- **`parquet_random` and `parquet_sampling`**: a counter-based generator whose value at
  `(seed, index)` is a pure function of its coordinates rather than of call order, so a parallel
  loop returns the same numbers at any thread count or schedule. Uniform, integer, exponential,
  normal, gamma and Poisson draws; permutations, subsets and resampling; and weighted sampling
  without replacement (`pf_weighted_draw`, `pf_weighted_subset`, `pf_weighted_permutation`).
- **`parquet_settings`**: process-global settings — Arrow's thread pool, per-area thread caps,
  default compression codec and level, terminal verbosity and message stream, the sort's integer
  counting fast path, target row-group size and the statistics prescreen — with
  `parquet_print_settings`, `parquet_reset_settings` and one `PARQUET_FORTRAN_*` environment
  variable per knob.
- **Reproducible file output**: `parquet_set_file_date`/`parquet_get_file_date`, and
  `PARQUET_FORTRAN_FILE_DATE`, pin the `DATE` metadata key to a given `YYYY-MM-DDTHH:MM:SS` instead
  of reading the clock, so the same data written twice produces byte-identical files. An empty
  value, the default, reads the clock as before.
- **Every layer of the library is now an entry module in its own right**, each covered by the
  version promise and each far cheaper to compile against than `use parquet`: `parquet_io`,
  `parquet_tables`, `parquet_columns`, `parquet_sorting`, `parquet_argsort`, `parquet_sampling`,
  `parquet_random`, `parquet_strings`, `parquet_temporal`, `parquet_settings` and
  `parquet_version`. Eight of them no longer reach the Parquet C++ bindings at all. `use parquet`
  is unchanged and remains the recommended import.
- **Row filters are boolean expressions** over a file's columns — `and`/`or`/`not`, parentheses and
  precedence — rather than AND-combined clauses only, and a filter can be installed after open with
  `parquet_reader_set_filter`.
- **Read-time sorting**: `parquet_open_reader(..., sort_by=)` and `parquet_reader_set_sort` return a
  file's rows ordered by one or more columns, with per-key direction and null placement. Streaming
  and chunked reads now work on a filtered, sampled or sorted reader.
- **Generated table types**: `tools/generate_user_table_code.py` turns a MAML schema into a named
  `parquet_table` extension with one accessor per declared column.
- **`parquet_read_qc` and `parquet_compose_read_qc`**: read-time quality control declared in code
  and composed against whatever a file's own qc-MAML declares.
- **Reader queries answered without materializing a column**: `parquet_get_column_names`,
  `parquet_get_metadata_items`, `parquet_get_physical_row_indices`, `parquet_get_qc_columns`,
  `parquet_get_column_nullable`, `parquet_column_has_nulls`, `parquet_measure_list_width` and
  `parquet_release_column`.
- **Typed table metadata records its own type in the written file**, as a companion
  `<KEY>.datatype` entry, so a value written with `schema%add_metadata("NSIDE", 1024_int32)` reads
  back as an `int32` rather than as the indistinguishable string `1024`.
- **NAG Fortran 7.2 and LLVM flang are tested toolchains**, alongside gfortran and ifx. flang builds
  are serial only; see [Prerequisites](README.md#prerequisites) for what each one needs.

### Changed

- **BREAKING: `parquet_set_max_threads` is renamed to `parquet_set_arrow_threads`.** The old name is
  removed rather than kept as an alias, so a call to it no longer compiles.
- **BREAKING: `sample_seed=` is now `integer(int64)`**, and for a given seed
  `parquet_open_reader(..., sample_fraction=)` selects a different set of rows than it did in 1.0.0.
- **BREAKING: `parquet_get_version(v, mode="arrow")` and `mode="parquet"` are removed.**
  `parquet_get_arrow_version` reports the linked Arrow and Parquet C++ versions instead.
- **`parquet_open_writer`'s default compression is now `zstd` at level 3**, changed from `snappy`,
  and `float32`/`float64` columns are written with BYTE_STREAM_SPLIT encoding and dictionary
  encoding disabled.
- **A schema built with `schema%init`/`schema%add_field` no longer needs a `parquet_parse_maml`
  call**, and those calls no longer have a required order. `%add_field` now applies the same
  per-field validation `parquet_validate_maml` applies.
- **`qc: miss:` has three states**, and a field that declares none no longer means "no Nulls
  allowed" — it now says nothing about Nulls, and none are checked on either the write or the read
  side.
- **`parquet_write_column_chunk` converts a chunk's values to the schema's declared numeric type**,
  exactly as `parquet_write_column` always has.
- **A streamed column's nullability comes from its first row group's `is_valid` mask**, and every
  row group must agree. A vector column's element field is written non-nullable when nothing can
  put a Null in it.
- **Closing a schema-enforced writer that had nothing written to it produces a valid empty file**
  instead of aborting.
- **`parquet_get_column_type` and `parquet_column_exists(types=)` report the narrowest lossless
  Fortran kind** a column can be read into, rather than whether its physical type is one of nine
  names.
- **Reads, writes, filtering, sorting and string columns are substantially faster**, in many cases
  by several times: temporal reads, string reads and writes, quality-control checks, filter
  evaluation, sorts over columns containing nulls, validity handling, and column lookup on a wide
  table. A `parquet_table`'s per-column work and one `parquet_string_column`'s bulk work now also
  run on several threads.
- **The user guide is reorganised into six groups**, so every published page URL changed from
  `page/<name>.html` to `page/<group>/<name>.html`.

### Fixed

- **An integer `qc: min:`/`max:` bound is exact at any magnitude.** Bounds were parsed and compared
  through `float64`, so past 2^53 a bound could round and a violating value be accepted.
- **An unrecognised `qc: miss:` value in a schema MAML is rejected** instead of silently meaning the
  opposite of what it says.
- **A MAML key is case-insensitive everywhere, including block headers.** A capitalised `Extra:`
  silently lost the whole block.
- **MAML files are read correctly**: lines beyond 1024 characters are no longer silently truncated,
  a CRLF file no longer produces a misleading type error, and a spurious length abort under heavy
  thread contention is gone.
- **`parquet_date`/`parquet_timestamp` offset arithmetic and `%to_unix` no longer misjudge their own
  overflow guards under nagfor**, which could abort on an ordinary date or wrap silently past
  `int64`.
- **Sharing one `parquet_reader`/`parquet_writer` across threads aborts with the documented
  diagnostic**, instead of segfaulting, or hanging when several threads trip the guard at once.
- **Two heap-corruption races on first concurrent use of Arrow's own type singletons are fixed**
  (confirmed with ThreadSanitizer).
- **A struct-nested string column read through the compact buffer path no longer returns every row
  as Null** (a use-after-free).
- **The library no longer kills a process whose IEEE traps are unmasked**: both
  `parquet_close_reader(print_stat=.true.)` and writing a NaN to an int-declared column trapped.
- Many other minor fixes and improvements.

## [1.0.0] - 2026-07-27

**Toolchain floor:** gfortran ≥ 13 (13 on CI; 15.2.0 the primary development target), Intel
Fortran (ifx) 2026.1.0 (confirmed manually; not exercised by CI), a C++20-capable C++ compiler,
and Arrow/Parquet C++ ≥ 24.0.0 (validated locally against 25.0.0). See
[Prerequisites](README.md#prerequisites) for the full detail.

**SemVer scope:** the stability promise covers the whole `use parquet` surface documented under
README's [API stability](README.md#important-behavior) — every public type/procedure/constant reachable
that way, including a public type's own type-bound procedures and operators; anything not
reachable via `use parquet` (private module internals, the C++ surface, file/module layout) can
change in a minor or patch release.

**Compatibility:** 1.0.0 is the first published release (no prior `0.9.x` version was ever
tagged or published to the fpm registry), so there is no prior-release file format to remain
compatible with.

### Added

- Read and write parquet columns for `int32`/`int64`/`float32`/`float64`/`logical`/`character`
  (MAML: `boolean`/`string`), as plain 1D columns or fixed-length vector columns.
- Read and write `DATE`/`TIME`/`TIMESTAMP` columns (`parquet_date`/`parquet_time`/
  `parquet_timestamp`), nanosecond-precision, with ISO-8601 and Unix-time/MJD/JD interop,
  same-type difference/offset arithmetic (`operator(-)`/`operator(+)`, `%diff_seconds`, and the
  `parquet_ns_per_sec`/`parquet_ns_per_day`/`parquet_ns_to_sec`/`parquet_ns_to_day` conversion
  constants), and reading back a column's stored unit/timezone (`parquet_get_column_time_info`).
- Compact `parquet_string_column` type for variable-length string columns without pre-sizing,
  with search/mutation (`find`, `contains`/`startswith`/`endswith`/`equals`, `append`/`set`/
  `erase`), zero-copy element handles (`view`/`view_all`/`view_slice`), gathering handles back
  into a column (`build_from`), an owning row-range copy (`slice`), `clone`/`move`/`swap`, and
  summary statistics.
- Schema and metadata definition/validation from a MAML file, or built directly in code
  (`schema%init`/`add_field`/`add_metadata`), including column renaming (`col_map:`),
  quality-control range checks (`qc:`), Null-protected columns (`protected_cols:`), automatic
  `col_size`/`array_size` resolution at write time (`parquet_size_auto`), inspecting/copying an
  existing field (`schema%get_field`/`add_field_from`), and a human-readable schema dump
  (`schema%print_schema_info`).
- Read-time quality control: checking an already-written file's data against a qc-maml
  (`parquet_load_qc_maml_file`, `qc=`/`qc_soft=` on `parquet_open_reader`), independent of the
  `qc:` enforcement performed on write.
- Reading a file's stored metadata back (`parquet_get_metadata`) and prefetching columns
  (`parquet_prefetch_columns`).
- Genuine Parquet Null support on read/write, via value substitution or a validity mask.
- Row filtering on read (`parquet_filter`), random downsampling (`sample_fraction`/`sample_seed`),
  and row masking on write (`parquet_write_row_mask`/`parquet_write_chunk_row_mask`, dropping rows
  entirely rather than substituting Null), plus streaming/chunked reads and writes for large
  columns and row-mode/element-mode random-access reads.
- Reading a column into a widened numeric kind (including `int8`/`int16`/unsigned integers/
  `half_float`/`decimal32..256`), `STRING_VIEW`, foreign `LIST`/`LARGE_LIST` columns, and nested
  `STRUCT` fields via a dot-separated path.
- Reader queries answered from the file's schema/footer without reading column data: a column's
  existence/type (`parquet_column_exists`/`parquet_get_column_type`), row-group count
  (`parquet_get_num_row_groups`), vector-column width/total element count (`parquet_get_col_size`/
  `parquet_get_column_total_elements`), and a string column's longest stored value
  (`parquet_get_string_length`).
- Control over output compression codec, compression level, and row group size; opting out of
  silently overwriting an existing file (`overwrite=.false.` on `parquet_open_writer`); printing
  per-column read statistics on close (`print_stat=` on `parquet_close_reader`).
- Thread-safe concurrent use (e.g. from OpenMP), with control over Arrow's internal thread-pool
  size (`parquet_set_max_threads`).
- Support for embedding your own MAML schemas into a downstream project.

[2.4.0]: https://github.com/etempel/parquet-fortran/releases/tag/v2.4.0
[2.0.0]: https://github.com/etempel/parquet-fortran/releases/tag/v2.0.0
[1.0.0]: https://github.com/etempel/parquet-fortran/releases/tag/v1.0.0
