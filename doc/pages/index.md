---
title: User guide
ordered_subpage: io
ordered_subpage: types
ordered_subpage: schema
ordered_subpage: tables
ordered_subpage: utilities
ordered_subpage: operating
---

The complete usage guide for **parquet-fortran**, beyond the quick-start overview on the [front
page](../index.html). For the full per-procedure API reference (every public
type/function/subroutine under `use parquet`), see the [modules](../lists/modules.html) and
[procedures](../lists/procedures.html) listings generated from source.

**One `use parquet` brings in the whole library** — readers and writers, schemas and MAML, the
`parquet_table` container, the `parquet_column` foundation and its `PK_*` kind constants, compact
string columns, the date/time/timestamp types, sorting, random numbers and sampling, and the
process-global settings. Every page below assumes that single import. The individual modules
(`parquet_io`, `parquet_tables`, `parquet_columns`, `parquet_strings`, `parquet_temporal`,
`parquet_sorting`, `parquet_argsort`, `parquet_sampling`, `parquet_random`, `parquet_settings`,
`parquet_version`, `parquet_maml_base`) are still there and can be named directly when you want a
narrower import — see [Choosing a module](operating/choosing-a-module.html) for what each one costs
to compile against and which of them are covered by the library's API stability promise.
`parquet_core` is the exception in the other direction: it is internal and is covered by no promise.
Because `parquet` re-exports rather than defines, the generated
[procedures](../lists/procedures.html) and [types](../lists/types.html) listings are the reliable
place to look a name up, rather than the `parquet` module's own page.

**Parquet in, parquet out.** This library reads and writes parquet and no other format. Any
conversion — FITS, CSV, HDF5, anything else — happens outside it, before the library is called or
after it returns. That is a settled scope decision rather than a gap waiting to be filled.

The guide is organised into six groups, ordered roughly by what a new user needs first;
reference/advanced material is last. If you only want a file's columns as arrays with the least
ceremony, skip straight to [Whole tables in memory](tables/index.html) — it is the easiest entry
point in the library.

- [Reading and writing files](io/index.html) — read columns into arrays, write arrays out, and
  choose which rows a read returns and in what order (filter, sort, random sample).
- [Data types](types/index.html) — what a column can hold: the supported types, date/time/
  timestamp elements, and compact string columns.
- [Schemas, metadata and quality control](schema/index.html) — declare a file's contents in MAML
  or in code, with qc bounds; and declare how a file is *read* — which columns are renamed, which
  rows are kept and in what order they come back. Ends on two worked examples.
- [Whole tables in memory](tables/index.html) — the high-level `parquet_table` layer: a whole
  file as one object, columns by name as ordinary arrays or zero-copy pointers, read only when
  touched — or built in memory and written out.
- [Utilities and code generation](utilities/index.html) — sorting for arrays and columns,
  reproducible random numbers and the sampling built on them, generated table types, and embedding
  your own MAML schemas.
- [Operating the library](operating/index.html) — the cross-cutting concerns you reach for when
  something is wrong or has to be tuned: which module to import, what an abort means, why a build
  or read fails, what may run concurrently, what memory and speed to expect, and the process-global
  knobs.

### Every page at a glance

All 24 pages, in reading order. (This list, each group's own page list and the
`ordered_subpage:` frontmatter are kept consistent by
`tools/check_source_conventions.py`, which fails the CI lint stage when they drift.)

**Reading and writing files**

- [Reading parquet files from your Fortran code](io/reading.html)
- [Filtering, sorting and sampling rows](io/filter-sort-sample.html)
- [Writing parquet files from your Fortran code](io/writing.html)

**Data types**

- [Supported data types](types/supported-data-types.html)
- [Date, time and timestamp columns](types/date-time.html)
- [Compact string columns with `parquet_string_column`](types/string-columns.html)

**Schemas, metadata and quality control**

- [The MAML metadata format](schema/maml-format.html)
- [Building a schema in code with `schema%init` and `schema%add_field`](schema/building-schema-in-code.html)
- [Quality control](schema/quality-control.html)
- [Combined examples: schemas, vector columns, metadata and quality control](schema/combined-example.html)

**Whole tables in memory**

- [Whole tables in memory: the basics](tables/table.html)
- [Opening a table: slices, filters and renaming](tables/table-open.html)
- [Building a table and writing it out](tables/table-write.html)
- [Changing a table](tables/table-mutate.html)

**Utilities and code generation**

- [Sorting arrays and columns with `pf_sort`/`pf_argsort`](utilities/sorting.html)
- [Random numbers and sampling with `pf_random_at`](utilities/random.html)
- [Generated table types: named accessors from a MAML schema](utilities/generated-tables.html)
- [Embedding your own MAML schemas in your own project](utilities/embedding-maml-schemas.html)

**Operating the library**

- [Choosing a module: what each entry module costs to import](operating/choosing-a-module.html)
- [Error handling](operating/error-handling.html)
- [Troubleshooting](operating/troubleshooting.html)
- [Thread safety](operating/thread-safety.html)
- [Performance and memory](operating/performance.html)
- [Settings](operating/settings.html)
