---
title: Choosing a module: what each entry module costs to import
---

`use parquet` brings the whole library into scope and is the right answer for most programs. It is
also the largest: a project that imports it compiles **156** of this library's Fortran files.

Every layer underneath is importable on its own, and several of them cost a great deal less. This
page says what each entry module gives you, what it costs, and — the part that is easy to get wrong
— what "costs less" does and does not mean.

## The one caveat, first

**No import makes the *package* Arrow-free.** `link` is a package-level key in `fpm.toml`, and fpm
cannot prune a C++ translation unit, so a project that depends on parquet-fortran compiles
`src/parquet_wrapper.cpp` and links `-larrow -larrow_compute -lparquet` **whichever module it
imports** — including `use parquet_temporal`. If Arrow's development headers are not installed, the
build fails at `arrow/api.h` regardless of what you wrote in your `use` statement.

What the tiers below labelled *Arrow-free* do guarantee is narrower and is about the **Fortran**
graph: no module fpm compiles for that import names `parquet_bindings`, so none of your Fortran
code path crosses the C++ boundary. That is what keeps those modules fast to compile, independently
testable, and free of the reader/writer machinery — and it is enforced, not merely intended.
`tools/check_source_conventions.py` carries one check per Arrow-free tier and fails the build if
that tier's graph — submodules included — ever reaches `parquet_bindings`, and
`tools/check_argsort_standalone.sh` proves it the other way round, compiling the argsort tier with
a bare compiler and no Arrow on the system at all. A check per tier rather than one for the group
is deliberate: several of these modules sit in each other's graphs, so a single check would go on
passing for a tier that had stopped being reachable from it.

If you need a genuinely Arrow-free *package*, this is not it, and no `use` statement will make it
one.

## The entry modules

Sizes are the number of this library's Fortran source files fpm compiles for a project whose only
import is that module, measured with `tools/check_module_footprints.sh` and recorded in
`tools/module_footprints.txt`. `src/parquet_wrapper.cpp` is excluded from the count, being present
in every one of them.

| Import | Files | Fortran graph reaches Arrow? | What it gives you |
|---|---|---|---|
| `parquet_version` | 2 | no | `parquet_get_version`: which parquet-fortran this is |
| `parquet_temporal` | 1 | no | `parquet_date`, `parquet_time`, `parquet_timestamp` and their unit constants |
| `parquet_strings` | 2 | no | `parquet_string_column` / `parquet_string`: packed, null-aware string storage |
| `parquet_random` | 3 | no | counter-based random numbers, the distributions and the points-on-a-sphere family |
| `parquet_argsort` | 4 | no | `pf_argsort` over the six intrinsic types, plus `pf_sort_threads` |
| `parquet_sampling` | 8 | no | permutations, subsets, resampling and weighted draws |
| `parquet_spatial` | 15 | no | `pf_spatial_index`: neighbour and k-nearest search on a uniform grid or a HEALPix pixelisation, including on the sky |
| `parquet_healpix` | 7 | no | `pf_query_disc` and friends: the HEALPix sphere pixelisation |
| `parquet_sphere` | 15 | no | `pf_sky_polygon`, `pf_random_pixel_at` and `pf_random_mask_at`: uniform random points in sky polygons and HEALPix pixels and masks, and RA/Dec conversions in a named declination frame and the Fibonacci grid |
| `parquet_skycoord` | 6 | no | `pf_icrs2gal`, `pf_sky_convert`, `pf_sky_rotation` and the other rotations: sky positions converted between ICRS, Galactic, ecliptic, supergalactic and FK5 J2000 coordinates; `pf_angdist_deg`, `pf_offset_radec`, `pf_position_angle_deg` and `pf_apply_pm`, the RA/Dec geometry that needs no coordinate system, proper motion included; `pf_radec2str`, `pf_str2radec` and their kin, positions as sexagesimal text and back; and `pf_zhel2zcmb`, a redshift in the CMB rest frame |
| `parquet_index` | 13 | no | `pf_index_map`: which row holds this key, over a single integer key, a tuple of them or a string, with three storage backends, two chosen from the keys and one opt-in; `pf_index_multimap`: every row holding a key that repeats, as ranges; and `pf_index_pool`, which hands out and recycles unique index values |
| `parquet_columns` | 10 | no | `parquet_column`: a typed, null-aware column container |
| `parquet_list` | 11 | no | `parquet_list_column` / `parquet_list_row`: variable-length list storage |
| `parquet_struct` | 11 | no | `parquet_struct_column` / `parquet_struct_row`: one value per declared field per row |
| `parquet_map` | 11 | no | `parquet_map_column` / `parquet_map_row`: string-keyed `key -> value` entries per row |
| `parquet_logging` | 1 | no | `pf_logger` and the `pf_log_*` procedures: leveled logging to several destinations at once, with a layout you choose and correct behaviour inside an OpenMP parallel region |
| `parquet_toml` | 2 | no | `pf_toml`: reading and writing TOML configuration files on top of `toml-f`, with checked types, a report for every key nobody read, and diagnostics that point at the offending line |
| `parquet_utils` | 1 | no | `pf_to_lower`, `pf_to_str`, `pf_join_path` and the path splitters: ASCII case folding, value-to-text, and POSIX path handling |
| `parquet_sorting` | 22 | no | the whole sorting API, every element type, including `pf_sort_keys` |
| `parquet_stats` | 29 | no | the `pf_*` array-statistics family: reductions over plain Fortran arrays |
| `parquet_kde` | 40 | no | `pf_kde`: a kernel density estimate of a one-dimensional sample, its density, distribution function and quantiles answered anywhere; `pf_kde_grid`: the same estimate streamed into fixed cells |
| `parquet_integrate` | 3 | no | `pf_integrate`: adaptive quadrature of a function of one variable over a finite or infinite range |
| `parquet_interpolate` | 4 | no | `pf_interp_1d`, `pf_interp_2d` and `pf_interp`: linear, cubic-spline and shape-preserving interpolation of tabulated data, in one dimension and on a rectilinear grid |
| `parquet_optimize` | 14 | no | `pf_minimize_scalar`, `pf_minimize_simplex`, `pf_minimize_de` and `pf_minimize_multistart`: minimising a function of one or many variables, on a bracket, from a start point, or globally over a box |
| `parquet_prima` | 24 | no | `pf_minimize_bobyqa`, `pf_minimize_lincoa` and `pf_minimize_cobyla`: Powell's derivative-free solvers, vendored from PRIMA — a function of several variables with bounds, linear constraints or nonlinear ones |
| `parquet_root` | 2 | no | `pf_find_root`: where a function of one variable crosses zero, by Brent's method on a bracket, widened first under a growth policy you state |
| `parquet_transform` | 2 | no | `pf_dct` and `pf_idct`: the discrete cosine transform of a sequence whose length is a power of two, and its inverse |
| `parquet_settings` | 3 | **yes** | the process-global knobs, and `parquet_get_arrow_version` |
| `parquet_io` | 63 | **yes** | reading and writing Parquet files, and nothing else |
| `parquet_tables` | 99 | **yes** | the `parquet_table` container, and the statistics tier its `%agg` runs on |
| `parquet` | 156 | **yes** | everything above, through one `use` |

Four rows deserve a note.

**`parquet_version` is the only route to `parquet_get_version`**, apart from `use parquet`. No other
entry module re-exports it, deliberately: a version string is fixed at compile time and nothing in
the library reads it, so making every tier carry it would grow every tier's graph for a name none of
them needs. A program built on one of the Arrow-free modules that wants to report its library
version writes a second `use parquet_version` line — two files, no C++ boundary. The Arrow and
Parquet C++ versions are a different question with a different answer: `parquet_get_arrow_version`,
in `parquet_settings` (and so in `parquet_io`, `parquet_tables` and `parquet`), because reading them
means calling into the C++ half.

**`parquet_tables` costs most of `parquet`'s files**, so importing it instead of the
facade buys little beyond a narrower namespace. The table layer sits on the reader, the column
container, the sorting engine and the statistics tier (`parquet_grouping%agg` is that tier's
vocabulary called once per group), which between them are almost the whole library; what it
leaves behind is the two facades themselves and the utility tiers nothing in the table layer
reaches — `parquet_sampling`, `parquet_spatial`, `parquet_healpix`, `parquet_sphere`,
`parquet_skycoord`, `parquet_logging`, `parquet_toml` and `parquet_version`.

**`parquet_io` is the one real saving on the Arrow side** — see the table above for the two
counts: it drops the entire table layer, the outer facade, the statistics tier and the eight
utility modules listed just above. Reach for it when your program opens files, moves columns in and out,
and never builds a `parquet_table`.

It does **not** drop `parquet_random`: `parquet_open_reader(..., sample_fraction=)` picks its rows
with this library's own generator, so the reader genuinely depends on it. Those three files are a
closed set — `parquet_random` and the two leaves it reads, `parquet_expkey` and `parquet_ziggurat`,
which import nothing but `iso_fortran_env` — so the graph cannot grow further through them.

**`parquet_settings` is the cheapest import that reaches Arrow**, at three files, and that is the
point of listing it: what naming it costs you is not compile time, it is the C++ boundary. Those
three are the knob state, this module, and `parquet_bindings` — which it imports because it is the
module that mirrors `verbosity`, `message_stream`, the four performance knobs and `file_date` across
to the C++ half. Everything below explains why the Arrow-free tiers cannot import it, and re-export
the knobs they read instead.

## Why the numbers jump the way they do

fpm prunes at **module** granularity, and a submodule is never pruned separately from the module it
belongs to. So each row above is a union of *whole modules*, not of the procedures you actually
call. `parquet_sorting` costs 22 files whether you use one specific or all of them, because its
eight submodules come as a set.

That is also why the argsort tier exists at all. `parquet_sampling` needs exactly one sorting
specific — `pf_argsort` over a `real64` array, for `pf_weighted_permutation` — and taking it from
`parquet_sorting` would cost that import the whole 22-file sorting graph for one procedure. The
intrinsic-type `pf_argsort` and its engine live in `parquet_argsort` instead, which is what keeps
`use parquet_sampling` at 8 files.

The practical consequence for your own code: **one added `use` line can multiply what a consumer
compiles.** If you contribute to this library, `tools/check_module_footprints.sh` is what notices.

## Settings: each module carries its own

A process-global setting is normally reached through `parquet_settings` — but that module owns the
C++ boundary, so importing it would pull the Arrow stack into an otherwise Arrow-free build. Each
entry module therefore re-exports, **getter and setter both, every knob its own code reads**, so a
program that imports one module for one capability can configure that capability without importing
anything else.

| Import | Settings it re-exports |
|---|---|
| `parquet_version` | `verbosity` and `message_stream` — it can print, see below |
| `parquet_temporal` | none — it reads none |
| `parquet_utils` | none — it reads none, and must not: it sits below `parquet_settings_base` |
| `parquet_random` | none — it reads none; the thread rule lives in `parquet_sampling` |
| `parquet_columns` | none — it reads none |
| `parquet_list` | none — it reads none |
| `parquet_struct` | `verbosity` and `message_stream` |
| `parquet_map` | `verbosity` and `message_stream` |
| `parquet_logging` | none — deliberately. It is not the library's own messaging system and reads neither knob; `pf_log_configure_from_env` is its own configuration route |
| `parquet_toml` | none — it reads none; its output is governed by `parquet_logging`, not by `verbosity`/`message_stream` |
| `parquet_strings` | `string_threads`, plus `verbosity` and `message_stream` |
| `parquet_sampling` | `random_threads`, `random_parallel_min_elements` |
| `parquet_spatial` | `spatial_threads`, the four sorting knobs, plus `verbosity` and `message_stream` |
| `parquet_healpix` | `healpix_threads`, plus `verbosity` and `message_stream` — it can warn from a thread clamp |
| `parquet_sphere` | none — it reads none, and prints nothing at all |
| `parquet_skycoord` | none — it reads none, and prints nothing at all |
| `parquet_index` | `index_threads`, the four sorting knobs (a `method="sorted"` build sorts through `pf_argsort`), plus `verbosity` and `message_stream` — it can warn from a thread clamp |
| `parquet_argsort` | `sort_threads`, `sort_radix_path`, `sort_counting_path`, `sort_counting_bucket_limit`, plus `verbosity` and `message_stream` |
| `parquet_sorting` | the same six as `parquet_argsort` |
| `parquet_stats` | `verbosity` and `message_stream` — `pf_stats%print` writes solicited output |
| `parquet_kde` | `verbosity` and `message_stream` — `pf_kde%print` and `pf_kde_grid%print` write solicited output |
| `parquet_integrate` | none — it reads none, and prints nothing at all |
| `parquet_interpolate` | none — it reads none, and prints nothing at all |
| `parquet_prima` | none — it reads none, and prints nothing at all. The one emitter it can reach is the thread clamp inside `pf_minimize_multistart`, which a caller reaches through `parquet_optimize` and silences there |
| `parquet_optimize` | `verbosity` and `message_stream` — it can warn from a thread clamp |
| `parquet_root` | none — it reads none, and prints nothing at all |
| `parquet_transform` | none — it reads none, and prints nothing at all |
| `parquet_settings`, and so `parquet_io`, `parquet_tables`, `parquet` | all of them |

The output pair (`verbosity`, `message_stream`) appears wherever a module can print something: a
user who imports `parquet_argsort` alone must still be able to silence its thread-clamp warning.

`parquet_version` is in that table for exactly that reason, and it is the one row where the rule is
easy to miss: `parquet_get_version` prints a remark on a development build, so a program whose only
import is `use parquet_version` has to be able to quiet it. Two files, still no C++ boundary.

**`parquet_get_version` itself is deliberately outside this rule** and is re-exported by none of
these modules — it is not a setting, nothing in the library reads it, and it has its own entry
module. See the note under [The entry modules](#the-entry-modules).

`parquet_settings` remains available and is what `use parquet` gives you; naming it directly is
only a problem for a build that is deliberately staying clear of Arrow. See
[Settings](settings.html) for what every knob does, and note in particular that the knobs the C++
half mirrors reach it when a reader or writer is **opened** rather than when you set them — which is
exactly what makes this per-module re-export possible.

## Which module do I import?

- **Most programs: `use parquet`.** One import, everything reachable, no decisions.
- **Reading and writing files, with no `parquet_table`: `use parquet_io`.**
- **A numerical or utility library that must not depend on the reader/writer machinery**: import the
  Arrow-free module you actually need — `parquet_sorting`, `parquet_argsort`, `parquet_sampling`,
  `parquet_random`, `parquet_columns`, `parquet_strings`, `parquet_temporal`. Remember the caveat at
  the top: your *package* still links Arrow.
- **Reporting which library you built against: `use parquet_version`**, alongside whatever else you
  import. `parquet_get_version` is there and, apart from `use parquet`, nowhere else.
- **Never `use parquet_core`.** It is internal, undocumented and may change in any release;
  `parquet_io` is its supported face.

A narrow import is an ordinary program — there is nothing to configure and no facade to go through.
This one compiles four of this library's Fortran files — the count the table above gives for
`parquet_argsort` — and reaches no reader, no writer and no `parquet_table`:

```fortran
program narrow_import
    use parquet_argsort
    use iso_fortran_env, only: int32
    implicit none
    integer(int32) :: v(5) = [30, 10, 50, 20, 40]
    integer(int32), allocatable :: perm(:)

    call pf_argsort(v, perm)
    print *, perm            ! 2 4 1 5 3
end program narrow_import
```

Swap `parquet_argsort` for any other row of the table and the shape is the same.

## What the stability promise covers

Every module in the table above is public API and is covered by the library's versioning promise. A
module advertised as an entry point makes its own surface a promise separate from `use parquet`, and
`parquet_columns` is the sharpest case: a change to `parquet_column`'s bindings is a public API
change even when nothing reachable through `use parquet` moves.

`parquet_core`, `parquet_bindings`, `parquet_settings_base`, `parquet_expkey`, `parquet_ziggurat`,
`parquet_sorting_oracle` and every `*_engine`/`*_kernel` submodule are **not** covered. They are
accessible because Fortran has no package scope, not because they are meant to be imported.

**One module is importable and promised but deliberately absent from the table**: `parquet_maml_base`,
which holds the MAML schemas bundled with this library and the shared `parquet_maml_file` type. It is
left out because you would reach for it for what it *holds* rather than for what it costs to compile
— and because most of its public names are this library's own embedded fixtures rather than API,
which is why `use parquet` imports three types from it with an `only:` list instead of re-exporting
it whole. See [Embedding your own MAML schemas](../utilities/embedding-maml-schemas.html).

**The table above is the authority on what costs what.** It is the list
`tools/check_module_footprints.sh` measures and the list this promise covers; anywhere else in the
repository that appears to enumerate entry modules is describing it, not defining it. A module added
to that table needs a section in `tools/module_footprints.txt` and, if its Fortran graph is meant to
stay clear of Arrow, its own check in `tools/check_source_conventions.py` — both of which fail
loudly when they are missing, which is how the list stays one list.
