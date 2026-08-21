---
title: Choosing a module
---

`use parquet` brings the whole library into scope and is the right answer for most programs. It is
also the largest: a project that imports it compiles **64** of this library's Fortran files.

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
testable, and free of the reader/writer machinery — and it is enforced, not merely intended:
`tools/check_source_conventions.py` fails the build if one of those graphs ever reaches
`parquet_bindings`, and `tools/check_argsort_standalone.sh` compiles the argsort tier with a bare
compiler and no Arrow on the system at all.

If you need a genuinely Arrow-free *package*, this is not it, and no `use` statement will make it
one.

## The entry modules

Sizes are the number of this library's Fortran source files fpm compiles for a project whose only
import is that module, measured with `tools/check_module_footprints.sh` and recorded in
`tools/module_footprints.txt`. `src/parquet_wrapper.cpp` is excluded from the count, being present
in every one of them.

| Import | Files | Fortran graph reaches Arrow? | What it gives you |
|---|---|---|---|
| `parquet_temporal` | 1 | no | `parquet_date`, `parquet_time`, `parquet_timestamp` and their unit constants |
| `parquet_strings` | 2 | no | `parquet_string_column` / `parquet_string`: packed, null-aware string storage |
| `parquet_random` | 3 | no | counter-based random numbers and the four distributions |
| `parquet_argsort` | 4 | no | `pf_argsort` over the six intrinsic types, plus `pf_sort_threads` |
| `parquet_sampling` | 8 | no | permutations, subsets, resampling and weighted draws |
| `parquet_columns` | 10 | no | `parquet_column`: a typed, null-aware column container |
| `parquet_sorting` | 21 | no | the whole sorting API, every element type, including `pf_sort_keys` |
| `parquet_io` | 40 | **yes** | reading and writing Parquet files, and nothing else |
| `parquet_tables` | 64 | **yes** | the `parquet_table` container |
| `parquet` | 64 | **yes** | everything above, through one `use` |

Two rows deserve a note.

**`parquet_tables` costs the same as `parquet`**, so importing it instead of the facade buys
nothing but a narrower namespace. The table layer sits on the reader, the column container and the
sorting engine, which between them are almost the whole library.

**`parquet_io` is the one real saving on the Arrow side**, at 40 files against 64: it drops the
entire table layer, the random-number and sampling modules, and the facade. Reach for it when your
program opens files, moves columns in and out, and never builds a `parquet_table`.

## Why the numbers jump the way they do

fpm prunes at **module** granularity, and a submodule is never pruned separately from the module it
belongs to. So each row above is a union of *whole modules*, not of the procedures you actually
call. `parquet_sorting` costs 21 files whether you use one specific or all of them, because its
eleven submodules come as a set.

That is also why the argsort tier exists at all. `parquet_sampling` needs exactly one sorting
specific — `pf_argsort` over a `real64` array, for `pf_weighted_permutation` — and taking it from
`parquet_sorting` cost the whole 21-file graph, Arrow included. Splitting the intrinsic-type
`pf_argsort` and its engine into `parquet_argsort` took `use parquet_sampling` from 24 files to 8
and off the C++ boundary entirely.

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
| `parquet_temporal` | none — it reads none |
| `parquet_random` | none — it reads none; the thread rule lives in `parquet_sampling` |
| `parquet_columns` | none — it reads none |
| `parquet_strings` | `string_threads`, plus `verbosity` and `message_stream` |
| `parquet_sampling` | `random_threads`, `random_parallel_min_elements` |
| `parquet_argsort` | `sort_threads`, `sort_radix_path`, `sort_counting_path`, `sort_counting_bucket_limit`, plus `verbosity` and `message_stream` |
| `parquet_sorting` | the same six as `parquet_argsort` |
| `parquet_io`, `parquet_tables`, `parquet` | all of them, via `parquet_settings` |

The output pair (`verbosity`, `message_stream`) appears wherever a module can print something: a
user who imports `parquet_argsort` alone must still be able to silence its thread-clamp warning.

`parquet_settings` remains available and is what `use parquet` gives you; naming it directly is
only a problem for a build that is deliberately staying clear of Arrow. See
[Settings](settings.html) for what every knob does, and note in particular that four of them reach
the C++ half when a reader or writer is **opened** rather than when you set them — which is exactly
what makes this per-module re-export possible.

## Which module do I import?

- **Most programs: `use parquet`.** One import, everything reachable, no decisions.
- **Reading and writing files, with no `parquet_table`: `use parquet_io`.**
- **A numerical or utility library that must not depend on the reader/writer machinery**: import the
  Arrow-free module you actually need — `parquet_sorting`, `parquet_argsort`, `parquet_sampling`,
  `parquet_random`, `parquet_columns`, `parquet_strings`, `parquet_temporal`. Remember the caveat at
  the top: your *package* still links Arrow.
- **Never `use parquet_core`.** It is internal, undocumented and may change in any release;
  `parquet_io` is its supported face.

## What the semantic-versioning promise covers

Every module in the table above is public API and is covered by the library's versioning promise —
which is a wider commitment than it used to be, because a module advertised as an entry point makes
its own surface a promise separate from `use parquet`. `parquet_columns` is the sharpest case: a
change to `parquet_column`'s bindings is a public API change even when nothing reachable through
`use parquet` moves.

`parquet_core`, `parquet_bindings`, `parquet_settings_base`, `parquet_expkey`, `parquet_ziggurat`,
`parquet_sorting_oracle` and every `*_engine`/`*_kernel` submodule are **not** covered. They are
accessible because Fortran has no package scope, not because they are meant to be imported.
