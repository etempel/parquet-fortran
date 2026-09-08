---
title: Supported data types
---

The following intrinsic Fortran kinds (from `iso_fortran_env`) are supported throughout the write,
read and metadata APIs, both as scalars/1D arrays and as 2D vector columns:

| Fortran kind         | MAML `data_type` |
|-----------------------|-------------------|
| `integer(int32)`      | `int32`           |
| `integer(int64)`      | `int64`           |
| `real(real32)`        | `float32`         |
| `real(real64)`        | `float64`         |
| `logical`             | `boolean`         |
| `character(len=*)`    | `string`          |

Vector column entries use the shape convention `(col_size, nrows)` for arrays passed to
`parquet_write_column` or produced by `parquet_read_column`.

## Container columns: `LIST`, `MAP` and `STRUCT`

Three further column types hold a *variable* amount of data per row rather than one value or a
fixed-width vector. Each has its own type, its own MAML token and its own page:

| Fortran type | MAML `data_type` | every row holds |
|---|---|---|
| [`parquet_list_column`](list-columns.html) | `list[<elemtype>]` | zero or more values, a different count per row |
| [`parquet_map_column`](map-columns.html) | `map[<valuetype>]` | zero or more `key -> value` entries; keys are strings |
| [`parquet_struct_column`](struct-columns.html) | `struct` | one value per declared field, the fields differing in type |

All three are read with `parquet_read_column` and written with `parquet_write_column`, whole or one
row group at a time, exactly as the scalar types are. Two properties set them apart from everything
else on this page, and both follow from a row's contents being variable:

- **They carry their own nullness**, so they take neither `null_value=` nor `is_valid=` — see
  [Null values](#null-values) below. Each tracks two levels separately: whether the row itself is
  absent, and whether a value within it is missing. For a list or a map an absent row is also
  distinct from a *present but empty* one; a struct row has a fixed field set and so has no empty
  form.
- **A container may hold another container on READ but not on WRITE.** A list of structs, a map of
  lists and a struct with a list field all read from a file produced by any tool; writing one is
  refused, naming the offending payload, value or field. Each type's own page has the detail.

`col_size:` and `array_size:` do not apply to any of them, because both declare a width that is the
same in every row — which is exactly the property a container does not have.

## Date, time and timestamp columns

Three additional element-level types — `parquet_date`, `parquet_time`, `parquet_timestamp` (module
`parquet_temporal`, re-exported from `use parquet`) — read and write Parquet `DATE`/`TIME`/
`TIMESTAMP` columns, one value per element (`type(parquet_timestamp) :: ts(nrows)` for a scalar
column, `ts(col_size, nrows)` for a vector column), through the same `parquet_write_column`/
`parquet_read_column`/chunked/row-mode/element-mode calls as every type above. Unlike every type in
the table above, these carry their own null state (no `is_valid=`/`null_value=` argument, and a
null-containing column reads without the error-on-Null the numeric/string readers apply by default)
and their write-time unit/timezone is declared via a MAML token
(`timestamp[ns,utc]`/`time[ms]`/`date`) rather than inferred. See [Date, time and timestamp
columns](date-time.html) for the full guide — types, null semantics, units, Unix-time/MJD/JD
interop, and reading files from other tools (legacy `INT96`, arbitrary timezones). `qc:` bounds are
not yet supported for these three types; `parquet_filter` rules are, against a double-quoted
ISO-8601 literal — see [Filtering `date`, `time` and `timestamp`
columns](../io/filter-sort-sample.html#filtering-date-time-and-timestamp-columns).

## Large string columns

A `string` (scalar or vector-of-strings) column's underlying Arrow representation is chosen
automatically based on size. Normally it's Arrow's default `utf8` type, which caps a single column's
total string byte payload at 2^31-1 bytes (~2 GiB) — but if writing a column would exceed that, this
library transparently switches that column to `large_utf8` (64-bit offsets, no such limit) instead.
This is fully automatic and requires no action from either the writer or reader side:
`parquet_write_column`/`parquet_read_column` and every other read function behave identically either
way, including row filtering (`parquet_filter`) and `qc:` range checks. The only place the
difference is visible is `parquet_close_reader(print_stat=.true.)`'s `parquet_type` column, which
shows `large_string`/`list<large_string>` instead of `string`/`list<string>` for a column that was
promoted.

Two write paths skip the size test and always use `large_utf8`, however small the column is. A
**streamed** column (`parquet_write_column_chunk`) has its Arrow field fixed the moment the first
row group locks the file's schema — long before the whole column's byte payload has been seen — so
it takes the 64-bit form unconditionally rather than risk a later row group overflowing an
already-fixed `utf8`. A column written from a **`parquet_string_column`** takes it because that
container's own storage already carries 64-bit offsets, so there is no int32-offset variant to
choose between. Both therefore report `large_string` in `parquet_type`; nothing else about them
differs.

The same ceiling exists on the read side, and the same widening handles it. A `string` column whose
stored byte payload exceeds the `utf8` limit — one written by another tool without the promotion
above, say — cannot be assembled as a single `utf8` array, so it is widened to `large_utf8` as it
is read. Nothing is visible to the caller: every read function, row filtering, sorting and
`parquet_table` behave as for any other `string` column, and `parquet_type`,
`parquet_get_column_type` and `parquet_get_column_arrow_type` go on reporting the file's own
`string`. The cost is one pass over the column's offsets, paid only by a column over the limit.

## Reading `string_view` columns from other tools

This library's own writer never produces Arrow's `string_view` representation — it only ever appears
when reading a Parquet file written by another Arrow-based tool whose stored Arrow schema declared a
column as `utf8_view()`. Reading such a column works transparently through
`parquet_read_column`/`parquet_get_string_length` and every other scalar/vector string read
function, exactly like `string`/`large_string` above, including `qc:` range checks and row filtering
(`parquet_filter`). `parquet_close_reader(print_stat=.true.)`'s `parquet_type` column shows
`string_view` for such a column, and goes on showing it whatever else is in force — that cell
reports the file's own stored schema, which nothing on the read side rewrites. Filtering does cast
the column to `large_utf8` internally first, because Arrow's row-filter compute kernel has no
`string_view` support to call directly, but the cast is invisible: it changes no read result and no
diagnostic.

The compact `parquet_string_column` (see [String columns](string-columns.html)) read path applies
that same cast for its own reason. Its buffer handoff needs the offsets-plus-payload layout that
only `string`/`large_string` have — a view array's values are inlined or spread across several data
buffers — so a `string_view` column is converted to `large_utf8` first and then handed over. It
costs one pass over the column's bytes, and the result replaces the cached column, so reading it a
second time pays nothing. Values, nulls and reported types are unchanged either way.

## Reading dictionary columns from other tools

A **dictionary column** is Arrow's encoded representation of a column with few distinct values: the
values are stored once in a dictionary, and each row holds a small integer index into it. It is what
**pandas writes for a `Categorical`**, and — like `string_view` above — it only ever arrives when
reading a file written by another Arrow-based tool, since this library's own writer never produces
one. (Parquet's own page-level dictionary *encoding* is unrelated and always in play; it is a
compression detail this library, and every other reader, never sees.)

Such a column is **decoded to its values as it is read**, once, at the point every read path funnels
through. So it behaves exactly like an ordinary column of the value type: `parquet_get_column_type`
reports that type, `parquet_read_column` and the compact `parquet_string_column` read hand back the
values, `parquet_get_string_length` measures them, and `filter=`, `sort_by=`, `qc:` and
`parquet_table` all accept the column. A null index reads as a null row.

Three things follow, and are worth knowing before relying on it:

- **The encoding is not carried.** Fortran has no categorical type, so the category *codes*, the
  dictionary's *order* and pandas' `ordered` flag are all dropped — a decoded column is its values
  and nothing else. To recover codes, factorise the strings yourself: `pf_index_map`'s
  `%get_or_add_many` assigns each distinct value an index in one call.
- **Only what Arrow restores arrives as a dictionary at all.** Arrow's Parquet reader rebuilds a
  stored dictionary type only over `string` and `binary` values, so a pandas categorical over
  *integers* is handed back as a plain `int64` column and never reaches this path. A
  `dictionary<binary>` does arrive, and stays unreadable — `parquet_get_column_type` answers
  `"unknown"` and a read of it aborts, exactly as for a plain `binary` column. Decoding widens
  nothing.
- **`parquet_close_reader(print_stat=.true.)`'s `parquet_type` cell still shows
  `dictionary<values=string, indices=int8, ordered=0>`**, the same way it goes on showing
  `string_view` above: that cell reports the file's own stored schema, which nothing on the read
  side rewrites. The column reads as a string column and describes itself as what it is on disk.
  [`parquet_get_column_arrow_type`](../io/reading.html#what-a-column-is-actually-stored-as-parquet_get_column_arrow_type)
  reports the same thing for one column on demand, which is how to see the index type and the
  `ordered` flag the decode drops.

The decode costs one pass over the column (each row's value copied out of the dictionary) and the
dense values in memory, which is what any read of the column would have cost had it not been
encoded.

Everything else a pandas-written file does differently — the `__index_level_0__` column, the
`large_string` default, the frame description under the `pandas` metadata key — is in
[Files written by pandas](../io/reading.html#files-written-by-pandas).

## Vector-column width (`col_size`) limit

Unlike a *scalar* column's row count, which this library supports beyond Fortran's default-integer
`huge(1)` (2,147,483,647) for every data type, a single row's own vector width (`col_size`) is
capped at that same number. This is a hard limit of Arrow's `FixedSizeListType` itself — its
`list_size` is a plain `int32_t`, and unlike Arrow's string type (see [Large string
columns](#large-string-columns) above), there is no "large" fixed-size-list variant to fall back to.
Writing a vector column whose `col_size` would exceed this aborts the process (a C++-level abort
with a diagnostic on stderr, the same class of failure as the physical-type-mismatch case in
[Limitations](../../index.html#limitations)) rather than silently truncating `col_size` and
corrupting the written column.

## Vector-column per-row-group element count limit

Parquet's own repetition/definition-level generation for list-typed columns walks every flattened
element *of a single row group* with a plain `int32_t` counter, so a row group's own
`row_group_rows * col_size` is capped at 2,147,483,647 — but this is scoped to one row group, not
the whole file. `parquet_close_writer`'s row-group auto-sizing (see [Writer
options](../io/writing.html#writer-options)) already knows each column's `col_size` and silently
picks a smaller row-group size whenever a wide vector column needs it, so a column's *total*
`nrows * col_size` can exceed 2,147,483,647 — a real, hittable case (e.g. 2.5 billion rows at
`col_size=2`) — without any special handling: it is transparently split across multiple row groups
and round-trips normally. The only case that still aborts is an *explicitly* chosen `chunk_size`
(`parquet_open_writer(..., chunk_size=)`) that conflicts with a vector column's `col_size` —
silently shrinking a caller's explicit request would be a surprising, hard-to-notice performance
change, so this aborts the process instead (a C++-level abort with a diagnostic on stderr, the same
class of failure as the `col_size` case above) rather than either silently overriding the request or
letting Arrow itself throw an uncaught `IOError: List index overflow` mid-write.

## Column-count limit

A file is capped at 2,147,483,647 columns — the third and last of Arrow's hard `int32` ceilings,
alongside the two above. `arrow::Schema::num_fields()` and `GetFieldIndex()` return a plain
`int32_t`, and unlike a column's row count there is no 64-bit or "large" variant to fall back to.
Writing a column that would push the file's column count past this aborts the process (a C++-level
abort with a diagnostic on stderr, the same class of failure as the two cases above) rather than
letting Arrow's own field bookkeeping wrap silently. Reaching it takes an enormous amount of memory
and time first — every column carries its own name, type and metadata — so unlike the `col_size`
ceiling it is not a limit you meet by accident.

## Reading a column into a different numeric kind

`parquet_read_column` (and `parquet_read_array_row_mode`/`parquet_read_array_element_mode`) dispatch
on the *declared type/kind of the `values` array you pass in*, not the column's own stored type — so
`values`' kind doesn't have to match the file's `data_type` exactly, as long as the conversion is
one of the following:

| Requested `values` kind | Convertible **from** stored `data_type` |
|--------------------------|-------------------------------------------|
| `integer(int32)`         | `int32`, `int64` (checked for overflow — see below); `int8`, `int16`, `uint8`, `uint16` (always exact); `uint32`, `uint64` (checked for overflow); `float32`, `float64`, `half_float`, and `decimal` (checked for a fractional part and for overflow — see below) |
| `integer(int64)`         | `int64`, `int32`, `int8`, `int16`, `uint8`, `uint16`, `uint32` (always exact); `uint64` (checked for overflow); `float32`, `float64`, `half_float`, and `decimal` (checked for a fractional part and for overflow — see below) |
| `real(real32)`           | `float32`, `float64`, `int32`, `int64`, `int8`, `int16`, `uint8`, `uint16`, `uint32`, `uint64`, `half_float`, `decimal` |
| `real(real64)`           | `float64`, `float32`, `int32`, `int64`, `int8`, `int16`, `uint8`, `uint16`, `uint32`, `uint64`, `half_float`, `decimal` |
| `logical`                | `boolean` only |
| `character(len=*)`       | `string` only — but in any physical form: `utf8`, `large_utf8` and `utf8_view` alike, per the two sections above |

`int8`/`int16`/`uint8`/`uint16`/`uint32`/`uint64`/`half_float`/`decimal` here are physical
Parquet/Arrow storage types this library's own writer never produces (writing stays limited to the
six types in the table at the top of this page, plus `date`/`time`/`timestamp` — in scalar, vector,
and the three container forms `LIST`, `MAP` and `STRUCT`) — they only ever
arise from a file written by some other tool. There is nothing to declare for them in a MAML schema
or anywhere else: the conversion is purely internal to the read path, triggered automatically by
whatever physical type the column already has on disk.

To find out which physical type a file's column actually holds — and so which kind to declare —
ask `parquet_get_column_type`, which reports the narrowest lossless Fortran kind for it; see
[What a column is read as](../io/reading.html#what-a-column-is-read-as).

Anything not listed for a given `values` kind (e.g. requesting `logical`/`string` from anything
else) fails immediately, naming the column and the type mismatch. Every failure described in this
section is a C++-level abort with a diagnostic printed to stderr, not a Fortran `error stop` — the
same class of failure as the physical-type-mismatch case in
[Limitations](../../index.html#limitations).

Conversions that lose information silently, with no warning:

- `int64` → `integer(int32)`, and `uint32`/`uint64` → `integer(int32)`/`integer(int64)` where the
  value doesn't fit, are checked for overflow: a stored value outside the target's range aborts
  (e.g. `"...int64->int32 overflow..."`, `"...uint64->int64 overflow..."`) rather than wrapping.
- `int32`/`int64`/`int8`/`int16`/`uint8`/`uint16`/`uint32`/`uint64` → `real32`/`real64`, `float64` →
  `real32`, and `half_float`/`decimal` → `real32`/`real64`, are **not** checked for precision loss —
  a large `int64` (beyond ~2^53) or `int32` (beyond ~2^24) read into `real32` silently loses
  exact-integer precision, the same way a plain Fortran `real(int_value, kind=real32)` conversion
  would.

A `float32`/`float64`/`half_float`/`decimal` value read into an `integer(int32)`/`integer(int64)`
array is **never** silently truncated, unlike the narrowing above: a value with a nonzero fractional
part aborts immediately (e.g.
`"...double value has a fractional part, cannot convert to int32 for column: ..."`), and an
otherwise-integral value that doesn't fit the target width aborts with the same kind of overflow
message as the integer conversions above (e.g. `"...double->int64 overflow for column: ..."`).
This exists specifically to support reading a column that is intrinsically an integer quantity but
happens to have been written as a floating-point or decimal type by whatever tool produced the file
— not to allow lossy float-to-int rounding.

`qc: min:`/`max:` range checks and row filtering (`parquet_filter`) cover every type in the table
above the same way they already cover `int32`/`int64`/`float32`/`float64` — both compare against the
column's raw physical value regardless of its stored type, so no MAML/filter-side change is needed
to use them against one of these extended types.

**The write side converts in the opposite direction, and the asymmetry is worth holding onto.** A
read dispatches on the array *you* declare and converts the file's values into it; a write against a
schema converts the values *you* pass into the `data_type` the schema declares (`int32`, `int64`,
`float32` and `float64` in any combination, on both `parquet_write_column` and
`parquet_write_column_chunk`), so the file holds the declared type rather than the kind of the array
you handed over. See [Streaming/chunked
writes](../io/writing.html#streamingchunked-writes) for the rules, including which conversions fail
rather than round.

## Null values

> `date`/`time`/`timestamp` columns are the exception to this whole section: `parquet_date`/
> `parquet_time`/`parquet_timestamp` carry their own null state per element, so there is no
> `null_value=`/`is_valid=` argument for them anywhere, and a null-containing column reads without
> the error-on-Null described below — see [Null values are part of the
> element](date-time.html#null-values-are-part-of-the-element-not-a-separate-mask).

Fortran has no per-element representation for a missing/Null value. On the **read** side, if a
column contains any genuine Parquet Null (e.g. a file produced by another tool), the default
behavior of `parquet_read_column`, `parquet_read_array_row_mode`, and
`parquet_read_array_element_mode` is to abort the process immediately, rather than silently
returning undefined data. This is a C++-level abort with a diagnostic printed to stderr (e.g.
`parquet-fortran: parquet_read_int32_column: column contains Null value(s), which is not supported: <column>`),
not a Fortran `error stop` — the same class of failure as the physical-type-mismatch case in
[Limitations](../../index.html#limitations).

To read a Null-containing column instead of erroring, pass one or both of these optional keyword
arguments, supported by all three of the read families above for every data type in the table at
the top of this page:

- `null_value=nullval` — a scalar of the same type as `values`; every Null in the column is replaced
  with `nullval` in the returned `values`.
- `is_valid=mask` — a `logical` array of the same shape as `values`; `.false.` wherever the Parquet
  value was Null, `.true.` otherwise.

**Every type that carries its own null state takes neither**, because there is nothing for an
external mask to add: the temporal types above, `parquet_string_column` below, and the three
container types — `parquet_list_column`, `parquet_map_column` and `parquet_struct_column` — whose
read specifics take `(reader, name, values)` and nothing else. A container's row nullness and its
per-element or per-field nullness both travel inside the column; see
[Null rows, empty rows and null values](map-columns.html#null-rows-empty-rows-and-null-values) for
how those two levels differ.

If only `is_valid` is given (no `null_value`), Null slots in `values` are still filled with a safe
type-appropriate default (`0` / `.false.` / blank string) rather than left as undefined data — check
`is_valid` to know which entries are real. If both are given, Nulls are replaced with `nullval`
*and* `is_valid` still reports which entries were originally Null. Omitting both keeps the default
strict (error-on-Null) behavior.

A complete round trip through all three of those, writing two genuine Nulls and reading them back
twice:

```fortran
program null_values_example
    use parquet
    use iso_fortran_env, only: real64
    implicit none

    type(parquet_writer) :: writer
    type(parquet_reader) :: reader
    real(real64) :: flux(5), got(5), filled(5)
    logical :: written(5), present(5)

    flux = [1.5_real64, 2.5_real64, 3.5_real64, 4.5_real64, 5.5_real64]
    written = [.true., .true., .false., .true., .false.]

    ! Rows 3 and 5 are written as genuine Parquet Nulls; flux(3)/flux(5) are ignored.
    call parquet_open_writer(writer, "flux.parquet")
    call parquet_write_column(writer, "flux", flux, is_valid=written)
    call parquet_close_writer(writer)

    call parquet_open_reader(reader, "flux.parquet")
    ! is_valid= alone: the Null slots come back as 0.0, and `present` says which those are.
    call parquet_read_column(reader, "flux", got, is_valid=present)
    ! null_value= instead: the same slots come back as -99.0, with no mask to consult.
    call parquet_read_column(reader, "flux", filled, null_value=-99.0_real64)
    call parquet_close_reader(reader)

    print *, present   ! T T F T F
    print *, got       ! 1.5  2.5    0.0  4.5    0.0
    print *, filled    ! 1.5  2.5  -99.0  4.5  -99.0
end program null_values_example
```

Reading `flux` with neither argument would abort instead, because the column really does contain
Nulls — that is the strict default the two arguments opt out of.

For vector columns, a slot is reported/treated as Null if either the whole row is missing or that
specific element within the row is missing (Parquet's list columns track these independently);
`is_valid(j, i)` reflects the combination of both.

A scalar `string` column read into a [`parquet_string_column`](string-columns.html) takes neither
argument either, for the same reason the temporal types don't: that container carries a null state
per element of its own, so each Null in the file lands as `%append_null()` and `%is_null(i)` is what
you check afterwards. There is nothing for a mask to report or a sentinel to fill.

`parquet_get_string_length` is unaffected by any of this: it always silently skips Nulls when
computing the maximum string length, since sizing an output buffer shouldn't depend on how you plan
to handle Nulls when reading.

On the **write** side, `parquet_write_column(writer, name, values, is_valid=mask)` accepts the same
kind of `logical` mask (same shape as `values`, `.false.` = write a genuine Null there); there is no
`null_value` on the write side, since a value used to *detect* a Null (rather than substitute one,
as on read) would risk misclassifying a legitimate value that happens to equal the sentinel.
Whatever is in `values` at a `.false.` slot is ignored — a real Parquet Null is written there
regardless. For vector columns, `is_valid` is element-level only: an entire row's vector can never
be Null, only individual elements within it. A column only becomes nullable in the file's schema if
`is_valid` is actually passed and contains at least one `.false.` entry; omitting `is_valid` (or
passing an all-`.true.` mask) writes the column exactly as it would with no mask at all, keeping it
non-nullable.

**A streamed column's nullability comes from its FIRST row group, and from whether you passed a
mask — not from what the mask said.** `parquet_write_column_chunk` fixes the column's Arrow field the
moment the first row group locks the file's schema, long before any later row group exists. So the
question it can answer is *"did the caller pass an `is_valid` mask?"*, and the column is nullable
exactly when the answer is yes — an all-`.true.` mask still makes it nullable, because you have said
that Nulls are possible for this column even if this chunk holds none.

**Every later row group must then use the same form**, and mixing them is a hard error rather than a
silently ignored mask:

```fortran
call parquet_new_row_group(writer, 3)
call parquet_write_column_chunk(writer, "flux", a, is_valid=mask)   ! nullable from here on
call parquet_finish_row_group(writer)

call parquet_new_row_group(writer, 3)
call parquet_write_column_chunk(writer, "flux", b)                  ! aborts: the mask was dropped
```

In practice this costs nothing, because a chunked write is normally one loop body: the mask is
either there on every call or on none. If you cannot know in advance whether a Null will turn up,
pass an all-`.true.` mask in the first row group and the column stays nullable for the rest of the
file.

Two exceptions, for the same reason in both: `date`/`time`/`timestamp` columns and a
`parquet_string_column` carry their null state *inside the element*, with no mask to pass, so a
null-free first row group would say nothing about the seventh. Both are written nullable when
streamed — unless the column is protected, which is how you declare one of them null-free (see
[Null values](#null-values) below).

This differs from a whole-column `parquet_write_column`, which sees every value before it writes
anything and so decides from the values themselves: a column with no Null in it is written
non-nullable whether or not a mask was passed. `call parquet_get_column_nullable(reader, name,
is_nullable)` reports what a file actually ended up with, for either path.

To forbid Nulls in specific columns even when a caller passes `is_valid`, list them under a MAML
schema's `extra:` section as `protected_cols:`, either as a semicolon-separated scalar or a
dash-list:
```
extra:
  protected_cols: col1;col2;col3
```
or equivalently:
```
extra:
  protected_cols:
  - col1
  - col2
  - col3
```
Every name listed must be one of this same MAML file's own declared `fields:` (checked by
`parquet_validate_maml`; unknown names fail validation), and it is matched by the name written to
the file — so where a `col_map:` entry renames a field, `protected_cols:` must carry the **output**
name, not the internal one. If a user MAML overrides a base MAML, `protected_cols:` is taken from
whichever MAML is actually used to build the writer's schema (the user MAML if one is provided,
otherwise the base MAML) — not merged across both.

**A protected column may hold no Null at all, whatever kind it is and however the Null was
expressed.** Writing an `is_valid` mask with any `.false.` entry for one fails immediately with
`error stop` — and so does writing a `date`/`time`/`timestamp` column containing a null element, or
a `parquet_string_column` containing an `%append_null()`, even though neither of those takes an
`is_valid` argument at all. The rule is about the values reaching the file, not about which argument
carried them. This only applies when writing against a MAML-derived schema
(`parquet_open_writer(..., schema, ...)`); a schema-less writer has no `protected_cols:` to enforce.

**A protected column is written non-nullable**, on every path — which is also the only way to
declare a *streamed* `date`/`time`/`timestamp` or `parquet_string_column` column null-free, since
those carry their nulls in the element and are otherwise nullable whenever they are streamed.
Passing an all-`.true.` `is_valid` mask for a protected column is fine and changes nothing: it is
checked and then discarded, so declaring a column protected never makes the `is_valid` keyword
unusable. Because the mask is discarded before the same-form rule above is applied, a protected
column is also exempt from that rule — a mask cannot say anything its protection has not already
settled. Keep the form uniform anyway; nothing enforces it there.

**You can protect a column from code as well as from a MAML**, with
`call schema%set_protected(name [, protected])` — `protected` defaults to `.true.`, and `.false.`
lifts protection. Call it before `parquet_open_writer`, since the writer takes its own copy of the
schema at open time. Unprotecting a column the MAML itself declared protected is allowed but prints
a warning naming it: someone wrote that declaration down deliberately, and overriding it in code
should be visible in the program's output. See
[Building a schema in code](../schema/building-schema-in-code.html).

## Reading a nested struct field

**There are two ways to reach a `STRUCT` column, and this section is about the first of them.**
Addressing a struct's *leaves* by their dot-separated paths, described below, reaches any depth of
nesting and hands back ordinary flat columns. Reading the struct as **one object** — with its field
set, each field's own nullness and the struct instance's own nullness kept separate — is
`parquet_struct_column`, on [its own page](struct-columns.html); that is also the only one of the
two that can be **written**. The two are complementary and neither replaces the other: pick the
dotted path when you want a column, and `parquet_struct_column` when you want a struct.

A Parquet `STRUCT` column's individual fields — at any nesting depth — can be read directly by
passing a dot-separated path as the `name` argument to `parquet_read_column`,
`parquet_read_array_row_mode`/`parquet_read_array_element_mode`, `parquet_read_column_chunk`,
`parquet_get_col_size`/`parquet_get_column_total_elements`, `qc:` bounds in a MAML schema, and
`parquet_filter` rules — every one of these dispatches on `name` the same way, so a struct-nested
column is used identically to a top-level one everywhere. That holds for a **field** path, the
kind described in this section; the **descent** paths of the next section are deliberately narrower
and `qc:`, `parquet_filter` and `sort_by=` all refuse them. Given a file with

```
main : STRUCT
├── id    : int32
└── inner : STRUCT
    ├── name : string
    └── age  : int32
```

`parquet_read_column(reader, "main.id", ...)` and
`parquet_read_column(reader, "main.inner.age", ...)` read those two leaves directly; nesting depth
is unlimited (`"main.inner.deeper.value"` works the same way). An exact top-level field name always
wins over path-splitting first, so an existing column literally named with a `.` in it is
unaffected. A dotted path may also resolve to a vector (`FIXED_SIZE_LIST`) leaf nested inside a
struct — reading it behaves exactly like any other vector column (`col_size`, row/element modes,
`(col_size, nrows)` shape, all apply unchanged).

A dotted path may equally resolve to a variable-length `LIST` leaf, which reads into a
[`parquet_list_column`](list-columns.html#reading-a-list-column-from-a-file) exactly as a top-level
one does — including into a 2-D array when its rows happen to be uniform. There is no
dotted-path-specific behaviour in either direction.

A dotted path must resolve all the way down to a leaf column: naming an **intermediate struct**
directly (`"main.inner"`) is not readable this way and fails like any other unknown column
(`error stop "...: column not found in parquet file: ..."`). Reading it as a struct would mean
changing what `parquet_get_column_names` enumerates, which every caller that iterates a file's
columns would see, so it is declined on purpose. Note a **top-level** struct is different and does
read, through [`parquet_struct_column`](struct-columns.html) — it is simply not *listed*, so an
opened `parquet_table` never holds one.

A **`MAP`** terminal leaf, by contrast, does resolve (`"main.attrs"`) and reads through
[`parquet_map_column`](map-columns.html), on the same terms as a `LIST` leaf. Every dotted path
`parquet_get_column_names` lists therefore resolves; the listing and the reader agree.

### Descent paths: addressing a container's child

Beyond field names, a path may **descend into a container**: `[]` names a list's element array,
`{key}` and `{value}` a map's keys and values. They compose with each other and with field names to
any depth.

| path | names |
|---|---|
| `"struct_of_list.values"` | a struct field, as always |
| `"list_of_struct[].x"` | the `x` field of the struct **elements** of a list |
| `"map_of_struct{value}.x"` | the same through a map's values |
| `"deep_nested[].tags[]"` | composes freely |

The suffixes name a **role** rather than Arrow's own child field name, deliberately: Arrow writes a
list's child as `element` or `item` depending on which tool produced the file, so keying on the name
would make a path file-dependent. A field genuinely called `[]` is unreachable by path and says so
rather than silently yielding the container's child.

Three properties are worth stating plainly, because each is a deliberate choice rather than an
accident:

- A descent path reads as an ordinary column of **one value per element**, not per row — so
  `"list_of_struct[].x"` over a 3-row list holding 1+0+2 elements is a 3-element column, and it is
  the *elements* that number 3, not the rows.
- It **resolves but is not listed**. `parquet_get_column_names` still enumerates struct leaves only,
  so a deeply nested file does not turn into a combinatorial listing every caller has to filter.
- It is **refused by `qc:`, `parquet_filter` and `sort_by=`**, which apply to scalar leaves only,
  permanently. A filter answer with one entry per element would be silently misaligned with every
  other column, so the refusal is explicit rather than left to fail further down.

**Null handling** combines every level a path passes through: a leaf is reported/treated as Null
(via `null_value=`/`is_valid=`, [above](#null-values)) if the top-level struct itself is missing for
that row, *or* any intermediate struct field is missing, *or* the leaf itself is missing —
generalizing the same "missing if either the row or the specific slot is missing" principle vector
columns already use, from one level of list-nesting to arbitrary levels of struct-nesting.

**`qc:` and `parquet_filter` both work against a dotted path** exactly as they do against a
top-level column — a MAML field's `name:` can be a dotted path, and a `parquet_filter%add` rule's
column can be one too.

**`parquet_close_reader(..., print_stat=.true.)`**
([above](../io/reading.html#printing-reader-statistics-with-parquet_close_reader-print_stattrue))
shows one row per top-level *physical* struct column touched by any of its leaves being read, not
one row per leaf — its `nulls`/`min`/`max` reflect the whole struct's own top-level figures (which
degrade to blank/`-` for a struct, since those statistics aren't well-defined for a nested type),
and `output_type` shows whichever leaf under that struct was most recently read. This is a known,
accepted limitation, not a bug: reading two different leaves under one struct is still reported as a
single touched column.

**The dotted-path mechanism described in this section is read-only**: it reaches a leaf of a struct
another tool produced, and there is no dotted-path *write*. Writing a struct column goes through
`parquet_struct_column` instead — see [Struct columns with `parquet_struct_column`](struct-columns.html)
— which builds the whole struct as one object and writes it with `parquet_write_column`. What that
path supports is narrower in one respect and wider in another: a field must be one of the nine
scalar kinds, so it cannot produce the arbitrarily deep nesting the dotted-path reader can consume,
and it is the only way to produce a `STRUCT` column at all.

