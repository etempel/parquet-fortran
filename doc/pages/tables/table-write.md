---
title: Building a table and writing it out
---

A `parquet_table` does not have to come from a file: it can be built from scratch in memory and
written out through the same `parquet_schema` machinery the writer uses. The basics of working
with a table are in [Whole tables in memory: the basics](table.html); writing files without a
table is covered in [Writing parquet files](../io/writing.html).

## Building a table in memory and writing it out

`parquet_new_table` starts an empty table; the first `%add_column` fixes the row count and every
later one must match it.

```fortran
program build_and_write
    use parquet
    use iso_fortran_env, only: int64, real64
    implicit none

    type(parquet_table)  :: t
    type(parquet_schema) :: s
    integer(int64)   :: ids(3)
    real(real64)     :: masses(3)
    character(len=8) :: names(3)

    ids    = [1_int64, 2_int64, 3_int64]
    masses = [1.5_real64, 2.5_real64, 3.5_real64]
    names  = ["alpha   ", "beta    ", "gamma   "]

    call parquet_new_table(t)
    call t%add_column("id",   ids)                  ! fixes nrows
    call t%add_column("mass", masses, unit="Msun")
    call t%add_column("name", names)

    call s%init("catalogue")
    call s%add_field("id",   "int64")
    call s%add_field("mass", "float64", unit="Msun")
    call s%add_field("name", "string", array_size=32)
    call parquet_write_table(t, "out.parquet", s)   ! parses the schema itself if you have not

    print *, "wrote", t%nrows(), "rows,", t%ncols(), "columns"
end program build_and_write
```

The declared width of `names` and the schema's `array_size=32` need not agree: the array's own
`character(len=8)` decides what the values are, and `array_size:` declares the width the *file*
stores them at. Later examples on this page are fragments, taking the surrounding program for
granted.

`%add_column` refuses a name that already exists unless you pass `force=.true.`, which replaces
the column outright.

**If you are adding several columns and holding a `%col` pointer across them**, reserve the slots
first with `%reserve_columns` — while spare capacity remains, an `%add_column` under a new name
relocates nothing and leaves every outstanding pointer and handle valid. See [Making room for
columns](table-mutate.html#making-room-for-columns-and-the-one-guarantee-that-comes-with-it).

### Adding a column you built yourself

Besides a plain Fortran array (and a [`parquet_string_column`](../types/string-columns.html)),
`%add_column` accepts a whole **`parquet_column`**, taking its kind, width and row count from the
column rather than from the shape of an array:

```fortran
type(parquet_column) :: c

call c%init(PK_FLOAT64, 0_int64, unit="Msun")
do i = 1, n                          ! n need not be known before the loop
    call c%append_values([mass(i)])
end do
call c%set_null(3_int64)

call t%add_column("mass", c)         ! [, unit=] [, force=] as usual
```

This is the only form that covers every kind and width through one call, and the only way to hand
over a column that could not have been a plain array in the first place. Three cases it exists for:

- a column **grown a row at a time** with `%append_values`, when the final length is not known up
  front — every other form needs a complete array;
- a column **carrying nulls**, which no `%add_column` form taking a plain numeric or logical array
  can express — none of them has an `is_valid=` argument, so an array always arrives null-free;
- a column **derived from another** with `%gather`, `%delete_by_mask` or `%reindex`.

The second of those is worth stating the other way round, because it is easy to read the list as
being about vector columns only. **A plain array handed to `%add_column` produces a column with no
nulls at all**, whatever its kind or width. The other three routes to a null column are to add the
values first and mark them afterwards with `%set_null(name, i)` or
`%set(name, values, is_valid=mask)`, to hand over a
[`parquet_string_column`](../types/string-columns.html), which carries its own validity, or to hand
over a `date`/`time`/`timestamp` array, whose null state lives inside each element. A vector column
carrying per-**element** nulls is the case where only a `parquet_column` will do.

Three things to know:

- **The column is copied and you keep your own**, exactly as every other `%add_column` form leaves
  its values alone — so one built column can be added to several tables. A column built with
  `%adopt` to avoid a copy does pay for one here.
- **`unit=` overrides the column's own unit**; leave it out and the column's own is kept.
- **A column with no kind is refused**, rather than added as a slot nothing can read. Give it one
  with `%init`, `%adopt` or `%append_values` first. This is the one failure mode the array forms
  cannot have, since they take their kind from the type they are handed.

`parquet_column` and everything needed to build one come from the same `use parquet`; the
generated reference for `parquet_columns` lists every constructor and mutator.

The schema is optional — see [Writing without a schema](#writing-without-a-schema) for what a
`parquet_write_table(t, "out.parquet")` with no schema does.

**The schema decides the output.** `parquet_write_table` walks the schema's enabled fields, looks
each one up in the table **by its internal name**, and writes it under the schema's output name —
so a `col_map:` rename works exactly as it does for `parquet_open_writer`. A table column the
schema does not name is simply not written.

Three things can go wrong on that walk, and they are deliberately not the same case:

- **A schema field naming a column the table does not have is an error.** The schema is a claim
  about what is there.
- **A schema field naming a column that holds no values is a different error** — a column of a type
  this library cannot read (see [What a table can and cannot
  read](table-open.html#what-a-table-can-and-cannot-read)) is listed and queryable but has nothing
  to write, so naming it in an output schema cannot be honoured.
- **A field the schema itself has disabled is skipped, not an error.** `%set_column_unavailable`
  turns a field off, and a disabled field makes no claim about the table — so the table need not
  carry a column for it.

**You do not have to parse the schema yourself.** A schema built with `%init`/`%add_field` is
already parsed — those two keep its fields in step with its MAML text as they go — and one whose
`%maml` was populated directly is parsed here, on your behalf, when it has not been parsed already.
Two things follow: such a schema is *left* parsed afterwards (that is a visible side effect, and the
reason its dummy argument is `intent(inout)`), and a schema that was never built at all — no
`%init`, no fields, no MAML text — is still an error, since there is nothing there to parse.

**Writing a file-backed table reads what it has not read yet.** Every column the schema names is
materialized as it is written, so `parquet_write_table` on a freshly opened table reads exactly the
columns the schema asks for — and on a [detached](table-mutate.html#what-detaching-means) table, a
schema field whose column was never read is an error, since there is no longer a file to read it
from.

**...and gives it back again.** By default (`release=.true.`) the table ends the write in the
residency state it started in: a column the write had to read is released once it has been written,
so writing a freshly opened 16-column table costs one column's residency rather than sixteen. A
column *you* had already read is left alone — materializing it was not the write's doing, so undoing
it is not the write's business either. Pass `release=.false.` to keep everything the write read,
which is what you want when the next thing you do is read those same columns. Releasing frees
storage a `%col` pointer could alias, so it advances
[`%generation()`](table.html#two-ways-to-reach-a-column) — see the note there for why that is a
conservative signal rather than a real one.

**A write is a structural change, so a shared table refuses it.** Because `release=` evicts
columns as it goes, `parquet_write_table` counts as a change to the table rather than a read of it:
calling it on a table several threads share is a hard error, alongside `%add_column` and the rest of
the column- and row-changing calls. Write before the parallel region or after it — see
[Thread safety](../operating/thread-safety.html) for the full table of what is and is not allowed.

**It costs about what writing the columns yourself costs.** `parquet_write_table` writes straight
out of the table's store without copying, and a column that holds no nulls is written with no
validity mask at all — the same call a hand-written loop would make. `tools/benchmark_table.sh`'s
write mode measures the two against each other and reports them at parity.

To write only some rows without changing the table, pass a mask:

```fortran
call parquet_write_table(t, "subset.parquet", s, row_mask=keep)
```

## Writing without a schema

For a small or temporary table, the schema is optional:

```fortran
call parquet_write_table(t, "out.parquet")                    ! whatever is in memory
call parquet_write_table(t, "out.parquet", write_maml=.true.) ! ...and a .maml describing it
```

**It writes the columns that are currently resident, and reads nothing.** That is what makes it the
quick path: a column the program never touched is not in the output. Columns come out in slot order
— file order for a file-backed table, insertion order for one built with `%add_column` — under their
own internal names, since without a schema there is no `col_map:` to rename them with. Use
`%rename_column` if you want different names in the file. There is no `columns=` argument: to choose
columns, or to rename them, pass a schema.

A few consequences worth knowing:

- **A table with nothing resident writes a valid empty file** — zero columns, zero rows — rather
  than failing. Materialize something first if that is not what you meant.
- **Writer-side qc is off**, because there is no `qc:` block to enforce. Read-time qc, if the table
  was opened with `qc=` or a read-in MAML, has already run and is unaffected.
- **The automatic [`parquet_row_index`](table-open.html#which-row-of-the-file-is-this) column is
  never written**, even when you have materialized it. It records where a row came from rather than
  being part of your table; name it in a schema if you want it in the output.
- **`release=` has nothing to do**, since a schema-less write only ever writes columns that were
  already resident.
- **`row_mask=` works here too.** Both paths share one write loop, so the mask described
  [above](#building-a-table-in-memory-and-writing-it-out) applies to a schema-less write
  unchanged. The single exception is a table with nothing resident, which has no rows to mask.

**The sidecar `.maml` is what makes this round-trip.** With `write_maml=.true.` the write also emits
a MAML file next to the parquet output (`out.parquet` → `out.maml`), generated from the table's own
columns: each one's name, type, resolved `col_size:`/`array_size:`, and its `unit:` where it has one
(a column with no unit gets no `unit:` key). The `table:` name is the output file's stem. That file
is a valid read-in MAML for the file it describes, so

```fortran
call parquet_write_table(t, "tmp.parquet", write_maml=.true.)
...
call parquet_open_table(t2, "tmp.parquet", maml="tmp.maml")   ! units come back
```

gets you back what you wrote, units included — which a parquet file alone cannot carry. The one
thing that cannot be described this way is a zero-column table: `write_maml=.true.` on a table with
nothing resident is an error rather than a silently missing file, because MAML has no way to express
`fields:` with nothing in it.

**Temporal columns keep their own resolution.** A `timestamp[ns]` column read from a file is written
back as `timestamp[ns]`, not coerced to the writer's microsecond default — the table records each
time/timestamp column's stored unit when it opens the file, because a `parquet_timestamp` value
carries no unit of its own. A temporal column *built in memory* with `%add_column` has no stored
unit to record, so it takes the default microseconds; give it a schema with an explicit
`timestamp[ns]`-style token if it needs finer.

## Writer options

The full call form, with square brackets marking the optional arguments (the brackets are notation
here and elsewhere on this page, not something you type) — the arguments are introduced a group at a
time above and below, and this is the one place they appear together:

`call parquet_write_table(table, filename, [schema], [row_mask], [copy_metadata], [metadata_keys], [write_maml], [qc], [compression], [compression_level], [chunk_size], [use_threads], [overwrite], [release])`

Everything [`parquet_open_writer`](../io/writing.html#writer-options) can be told,
`parquet_write_table` can be told too, under the same names and with the same defaults:

```fortran
call parquet_write_table(t, "out.parquet", s, compression="snappy", chunk_size=500000, &
                         overwrite=.false., write_maml=.true.)
```

| argument | meaning | default |
|---|---|---|
| `write_maml` | also emit a sidecar `.maml` next to the output | `.false.` |
| `qc` | run the schema's `qc:` min/max/miss checks on write | on with a schema, off without |
| `compression` | `uncompressed`/`snappy`/`gzip`/`zstd`/`brotli`/`lz4` | `"zstd"` |
| `compression_level` | codec level (not every codec has one — `snappy` does not) | codec's own (3 when the codec is defaulted too) |
| `chunk_size` | rows per row group | auto-sized |
| `use_threads` | Arrow's multi-threaded writer | `.true.` |
| `overwrite` | allow truncating an existing file | `.true.` |
| `release` | give back columns this write materialized | `.true.` |

**They are pass-throughs, not reinterpretations.** Each one is handed to `parquet_open_writer`
untouched, and an argument you omit stays omitted all the way down — so a table write and the
equivalent hand-written open are the same call, with the same defaults, producing the same file.
`release=` is the one exception, because it is about the table rather than the file; it is
described [above](#building-a-table-in-memory-and-writing-it-out).

`chunk_size` is a plain default-kind `integer` with no `int64` form, unlike most row counts in
this library. That is deliberate rather than an oversight: a row group cannot hold more than
`huge(1_int32)` rows, so there is no larger value to pass.

## Carrying the source file's metadata to the output

A table opened from a file snapshots that file's key/value metadata when it opens, and
`parquet_write_table` can carry it into the output:

```fortran
call parquet_write_table(t, "out.parquet", s, copy_metadata=.true.)      ! every key
call parquet_write_table(t, "out.parquet", s, metadata_keys=["origin"])  ! only these
```

Five rules:

- **`copy_metadata=.true.` and `metadata_keys=` are mutually exclusive.** One means every key and
  the other means exactly the listed ones, so asking for both is an error rather than a guess.
  Passing `copy_metadata=.false.` alongside `metadata_keys=` is fine, and carries the listed keys —
  the two are only in conflict when both are asking for something.
- **A key the schema declares itself wins.** The schema is your explicit statement about the
  output, so a carried key of the same name is skipped rather than overwriting it.
- **A key `metadata_keys=` names but the source file does not have is an error**, checked before
  the output file is opened. Naming a key is a claim that it is there.
- **Either form on a table that was never opened from a file is an error**, since there is no
  source file to copy from. A table built with `parquet_new_table` has no metadata snapshot at
  all; give the output schema its own entries with `%add_metadata` instead.
- **It works after the table has [detached](table-mutate.html#what-detaching-means)** — which is the
  point, since read, change rows, write is exactly the shape that detaches. It also adds nothing to
  your own schema, so writing a second table with the same schema does not inherit the first one's
  provenance.

`call t%get_file_metadata(key, value, [found])` reads one key of that snapshot at any time, and
survives detaching for the same reason. To see everything a file carries, before or without a
table, use
[`parquet_get_metadata_items`](../io/reading.html#listing-every-metadata-entry).
