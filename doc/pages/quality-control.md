---
title: Quality control
---

Quality control (`qc:`) checks a column's values against declared `min:`/`max:` bounds and/or a
`miss:` Null-expectation flag — on write (against what's about to be written) and, independently,
on read (against an existing file, via a qc-maml). This page covers both, plus the two ways to
build a qc-maml without a `.maml` file on disk.

## The qc: block

A MAML field can declare a `qc:` block with `min:`/`max:` bounds and/or a `miss:` Null-expectation flag:
```
- name: ra
  data_type: float64
  qc:
    min: '>= 0'
    max: '< 360'
    miss: Null
```
A plain number (`min: 1`) is treated as inclusive (`>=` for `min:`, `<=` for `max:`); a quoted value with an explicit leading operator uses that comparison instead. The operator must match the bound's direction: `min:` accepts only `>=` or `>` (a lower bound) and `max:` accepts only `<=` or `<` (an upper bound); a reversed operator (e.g. `min: '< 5'`) is a nonsensical bound and fails `parquet_validate_maml`. Either bound may be omitted (only `min:` or only `max:` is fine). `parquet_validate_maml` also checks that every declared bound actually converts to a value usable for that field's `data_type`: for `int32`/`int64` it must be an exact integer within that type's range; for `float32`/`float64` it must be finite (not `NaN`/`Infinity`); a `string` field's bound is used as a literal string (nothing to convert, so nothing can fail there); `qc:` on a `boolean` field is accepted but never enforced (silently ignored, and so exempt from the operator-direction check too). `miss:` is `Null`/`NA` (case-insensitive) or empty; anything else fails `parquet_validate_maml`.

`qc:`/`parquet_filter` are not yet supported for `date`/`time`/`timestamp` columns — see [Date, time and timestamp columns](date-time.html#not-yet-supported).

## Building a qc-maml in code

Instead of authoring a `.maml` file, you can build the qc-maml in memory a column at a time from a compact, comma-separated string, then pass it straight to `parquet_open_reader(..., schema=)`. The `type(parquet_schema)` type provides two forms of the same builder — they append the identical field entry and differ only in how the parsed column name comes back:

- **`call schema%add_col_qc(qc_input [, col_name])`** — the parsed name is returned in the optional `col_name` argument (`character(len=:), allocatable, intent(out)`) when present; omit it to just add the entry.
- **`call schema%set_col_qc(col)`** — in-place: `col` is `intent(inout)` and must already be `character(len=:), allocatable`. It holds the compact `qc_input` string on entry and the parsed column name on exit, so a single variable is reused rather than assigning a result back into it.

```fortran
type(parquet_schema) :: qc
type(parquet_reader) :: reader
character(len=:), allocatable :: col

! two-argument form: name comes back in the (optional) second argument
call qc%add_col_qc("ra, >=0, <=360, Null", col)  ! col is returned as "ra"
call qc%add_col_qc("dec, , <=90")                ! col_name omitted: just add

! in-place form: col holds qc_input on entry, the parsed name on exit
col = "mag, 5"
call qc%set_col_qc(col)                           ! col becomes "mag" (bare 5 => >= 5)

call parquet_open_reader(reader, "data.parquet", schema=qc)
call parquet_read_column(reader, col, mag)        ! reuse the returned name
```

> **Why two forms?** `add_col_qc`'s `col_name` is `intent(out)`, so you must **not** pass the same variable as both arguments (`call maml%add_col_qc(x, x)`) — aliasing an `intent(out)` argument is undefined and corrupts the input. When you want the in-place `x` reused as both input and output, use `set_col_qc`, whose single argument is `intent(inout)` for exactly that — it still **mutates** `maml` (it adds the entry, like `add_col_qc`); it's a builder that also returns the name, not a pure query, hence `set_` rather than `get_`. (Neither form is a function returning `character(len=:), allocatable` — see [Thread safety](thread-safety.html#a-note-on-functions-returning-characterlen-allocatable) for why that matters on some compilers.)

Both forms share the same input format and validation:

- **`qc_input` is `"col_name, qc_min, qc_max, qc_miss"`** — at most four comma-separated fields, matched **positionally**. Only `col_name` (the first field) is required and must be non-empty; any of the last three may be empty or omitted (`"ra, >0"` sets just a min; `"ra, , <=10"` sets just a max; `"ra,,, Null"` sets just miss). The `fields:` header is created automatically on the first call.
- **`qc_min`/`qc_max`** may carry a leading operator (`>=`/`>` for `min`, `<=`/`<` for `max`) or be a bare number (inclusive, i.e. `>=` for min and `<=` for max — same convention as the [write-side `qc:`](#write-side-enforcement) above). A reversed operator (e.g. a `<` on `min`), or an operator with no value after it, fails immediately with `error stop`.
- **`qc_miss`** may only be empty, `Null`/`null`, or `NA`/`na`; anything else fails with `error stop`. (Meaning is exactly as for a file-based qc-maml: `Null`/`NA` ⇒ Nulls expected, empty ⇒ not expected.)
- Adding a column already present in this maml, or supplying more than four fields, also fails immediately with `error stop` — invalid input is never partially applied.
- An **empty (or all-blank) `qc_input` is a no-op**: `col_name` is returned as an empty string and nothing is added to the maml. This is distinct from a leading comma (e.g. `", >0"`), which does have content — an empty *first field* — and is the error above (a missing column name).

`schema%add_field`'s own `qc_min`/`qc_max`/`qc_miss` arguments (see [Building a schema in code](building-schema-in-code.html#building-a-schema-with-init-and-add_field)) build the identical kind of `qc:` block directly on a from-scratch schema-authoring field, with the same operator/`miss:` rules — a schema built that way works directly as `parquet_open_reader(..., schema=)`, without needing a separate `add_col_qc` call.

- `schema` is optional, `type(parquet_schema)`; `parquet_load_qc_maml_file(filename)` loads one from disk (a separate function from `parquet_parse_maml`, since a qc-maml has different, lighter requirements and is never parsed into `cinfo`/`metadata` — see [The MAML metadata format](maml-format.html#the-fields-section)). `qc` is optional `logical`: if omitted, it defaults to `.true.` whenever `schema` is supplied and `.false.` otherwise; an explicit `qc=` always wins (so `qc=.false.` with a `schema=` present disables checking entirely, and `qc=.true.` with no `schema=` at all is a harmless no-op, nothing to check).
- `qc_soft` is optional `logical`, default `.false.` (hard: a violation aborts the process). It only ever takes effect when qc is active; with `qc=.false.` (or no `schema=`) it is irrelevant.
- A qc-maml's only required field attribute is `name` — `data_type` and everything else (including `qc:` itself) are optional, unlike a schema-authoring MAML. `qc: min:`/`max:` bounds are parsed against the column's actual Parquet type at read time, not any `data_type` the maml might declare. A qc-maml may declare fields that don't exist in the parquet file at all (they're silently ignored) or that already have a value in the file's own physical type different from the maml — validation only requires that field names not repeat, that a `qc: miss:` value (if present) is `Null`/`NA` (case-insensitive) or empty, and that any `qc: min:`/`max:` operator points the right way (`min:` a lower bound with `>=`/`>`, `max:` an upper bound with `<=`/`<` — the same rule the write side enforces; a reversed operator aborts `parquet_open_reader`).

## Deferring qc declarations with `parquet_read_qc`

`schema%add_col_qc` emits its MAML text immediately, which is exactly what you want when the schema is the final word. It is the wrong shape when the declarations have to be held **unresolved** — when the column names still need translating, or when a MAML that arrives with the file may already have declared qc for some of the same columns and should win.

`type(parquet_read_qc)` is that carrier. It takes the identical `"col, min, max, miss"` string, one column per `%add` call, and validates nothing until the declarations are composed into a real schema:

```fortran
type(parquet_read_qc) :: qc
call qc%add("mass, >0, <=1000, Null")
call qc%add("flag, , , NA")
```

Two things can then be done with it, in this order:

- **`call qc%remap_column_names(from, to)`** renames the column each entry declares — the qc sibling of [`parquet_filter%remap_column_names`](reading.html#renaming-the-columns-a-filter-or-sort-refers-to), and subject to the same rules. Only the entry's first field is touched, so a *bound* that happens to spell a column name is never rewritten.
- **`call parquet_compose_read_qc(schema, qc, composed, ncolumns)`** merges a MAML-declared qc (`schema`, optional) and a code-declared one (`qc`, optional) into the single `composed` schema that `parquet_open_reader(..., schema=)` takes. Pass `composed` on only when `ncolumns` is greater than zero — a schema with no rules still switches qc on for no benefit.

### The merge rule: per column, not per bound

**If the MAML's `fields:` entry for a column carries a `qc:` key at all, the MAML wins in full for that column and the code's entry for it is dropped entirely.** Not merged bound by bound — dropped. If the MAML says nothing about a column, or merely *names* it without a `qc:` key, the code's entry applies instead.

```
MAML declares:              min: <empty>   max: 100        miss: <empty>
Code declares:              min: 0         max: 10         miss: Null

Active rule:                min: <empty>   max: 100        miss: <empty>
```

The code's `min:` and `miss:` are **not** filled into the gaps the MAML left. "The file's own description wins the whole column if it says anything at all" is one rule to hold in your head; a bound-by-bound merge would need a rule for each of the eight present/absent combinations across three bounds from two sources, for a benefit nothing here asks for.

An **empty** `qc:` block counts as saying something. `miss:` defaults to "Nulls are not expected", so a bare

```yaml
- name: column_x
  qc:
```

already means "no Nulls in this column" — a real restriction the MAML's author stated, and it wins like any other.

`composed` carries only the qc-bearing field entries: a MAML entry with no `qc:` key is not copied across. Nothing else is read from this schema — `parquet_open_reader` hands it to the qc parser and nowhere else — and it is what keeps the two sources from colliding over a column the MAML merely names.

## Write-side enforcement

`qc` defaults to `.true.` whenever `parquet_open_writer` is given a `schema=` (pass `qc=.false.` to opt out); it's a no-op without a schema:
```fortran
call parquet_open_writer(writer, "data.parquet", schema)          ! qc active by default (schema given)
call parquet_open_writer(writer, "data.parquet", schema, qc=.false.) ! explicitly disabled
```
With qc active, every `parquet_write_column` call runs two independent checks per column:

- **Range** — if `min:`/`max:` is declared, every element for which `is_valid` is `.true.` (or every element, if `is_valid` wasn't passed at all — see [Null values](supported-data-types.html#null-values)) is checked against the bound(s). String columns are compared lexicographically using Fortran's native string comparison. Vector columns are checked element-wise.
- **Miss (Null expectation)** — applies to any column written with an `is_valid=`/null-carrying mask (numeric/logical `is_valid=`, a `parquet_string_column`'s own null tracking, or a `parquet_date`/`time`/`timestamp` element's null state). If `miss:` is `Null`/`NA`, Nulls are expected and never warned about. If `miss:` is absent or empty (**the default**), Nulls are NOT expected: finding any is a violation. `min:`/`max:` always run only over non-null elements regardless of `miss:`.

Neither check ever stops the write — each prints its own one-line `WARNING` to stdout naming the column, e.g.:
```
WARNING: qc violation for column 'ra': declared min >= 0, max < 360, data range [-1.5, 359.9], 3 of 1000 valid element(s) out of range
WARNING: qc violation for column 'ra': 2 of 1000 element(s) are Null (qc: miss: not declared)
```
This only applies when writing against a MAML-derived schema (`parquet_open_writer(..., schema, ...)`); the range check only fires for columns that actually declare `qc: min:`/`max:`, and the miss check only fires for a column actually written with a null-carrying mask. `qc=.false.` skips both checks entirely.

## Read-side enforcement

`parquet_open_reader(reader, filename, schema=..., qc=..., qc_soft=...)` checks column values against `qc: min:`/`max:`/`miss:` bounds declared in a MAML file — mirroring the write-side `qc:` check above, but on the read side. By default a violation is a **hard error** (`qc_soft=.false.`): the process aborts with a diagnostic on stderr, the same class of clean, deliberate abort as the read-side Null/type-mismatch checks (see [Limitations](../index.html#limitations)). Pass `qc_soft=.true.` to instead **warn and continue**: a `WARNING` is printed to stdout and reading proceeds. Either way, the existing strict-by-default Null behavior (`error stop` on a genuine Null unless `null_value=`/`is_valid=` is passed — see [Null values](supported-data-types.html#null-values)) is completely unchanged.

```fortran
type(parquet_reader) :: reader
integer(int32), allocatable :: ra_back(:)

! qc.maml:
!   fields:
!   - name: ra
!     qc:
!       min: 0
!       max: 360
!   - name: id
!     qc:
!       miss: Null   ! this column is expected to contain genuine Nulls

! Default: a range violation aborts the process.
call parquet_open_reader(reader, "data.parquet", schema=parquet_load_qc_maml_file("qc.maml"))
call parquet_read_column(reader, "ra", ra_back)
! aborts: parquet-fortran: qc hard check: qc violation for column 'ra' (based on incomplete column information): declared min >= 0, max <= 360, data range [...], N of M valid element(s) out of range
call parquet_close_reader(reader)

! qc_soft=.true.: the same violation only prints a WARNING to stdout, then continues.
call parquet_open_reader(reader, "data.parquet", schema=parquet_load_qc_maml_file("qc.maml"), qc_soft=.true.)
call parquet_read_column(reader, "ra", ra_back)
! prints: WARNING: qc violation for column 'ra' (based on incomplete column information): declared min >= 0, max <= 360, data range [...], N of M valid element(s) out of range
call parquet_close_reader(reader)
```

- `qc: miss: Null` (or `NA`) means Nulls are *expected* for that field: no violation is raised if the column contains one. Leaving `miss:` empty or omitting it (the default) means Nulls are *not* expected: reading a Null in that column is a violation — regardless of whether you also pass `null_value=`/`is_valid=` to actually read it. A field with no `qc:` block at all (just a bare `name:`) gets no checking whatsoever, the same as a field never mentioned in the maml.
- qc only ever runs for a column this reader actually touches — read (`parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode`, scalar or vector), prefetched (`parquet_prefetch_columns`), or referenced by a [`parquet_filter`](reading.html#row-filtering-with-parquet_filter) — and, when a filter is active, only ever sees the already-filtered rows. Boolean columns skip the min/max check entirely (never meaningful there) but still get the Null-presence check.
- In soft mode, each of the two violation categories (Null-presence, range) prints **at most once per column** for the whole lifetime of the reader, even if that column is read multiple times (in hard mode the first violation aborts, so this never comes up); the message notes it's based on incomplete (whatever's been decoded so far) column information.
- **"Once per column" survives a `parquet_table`'s internally-parallel `%prefetch`/`%materialize_all`.** That path gives each thread its own reader, and a warning is deduplicated per *reader* — but it is also *raised* per column read, and each column is read by exactly one thread, so a qc-checked table warns exactly as often on the parallel path as on the serial one. The one shape that would duplicate is a column a `filter=` also touches while evaluating its own clauses, since every reader evaluates the filter; that combination does not arise, because a filtered table [prefetches serially](table.html#reading-a-table-from-several-threads) for unrelated (cost) reasons.
- Requires linking `arrow_compute` (see [Printing reader statistics](reading.html#printing-reader-statistics-with-parquet_close_reader-print_stattrue)) — same as `print_stat`, and for the same reason (min/max calculation).
- **Read-time qc and streaming/chunked reads:** if the reader was opened with `qc=.true.`, each `parquet_read_column_chunk` call (see [Streaming/chunked reads](reading.html#streamingchunked-reads)) runs the usual `qc: min:`/`max:`/`miss:` checks against just that row group's own data, not the whole column. In hard mode (`qc_soft=.false.`, the default), a violation aborts immediately, naming the offending row group (`qc violation for column 'name [row group N]'...`). In soft mode (`qc_soft=.true.`), a violation prints a `WARNING` — still at most once per column for the reader's whole lifetime, so reading many violating row groups in soft mode doesn't spam one warning per chunk.
