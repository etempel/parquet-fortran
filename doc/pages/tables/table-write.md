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
- a column **derived from another** with `%gather`, `%gather_from`, `%delete_by_mask` or `%reindex`.

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
validity mask at all — the same call a hand-written loop would make. `bench/benchmark_table.sh`'s
write mode measures the two against each other and reports them at parity.

To write only some rows without changing the table, pass a mask:

```fortran
call parquet_write_table(t, "subset.parquet", s, row_mask=keep)
```

### Writing a container column

A table column can be a list, a map or a struct — see
[Container columns in a table](table.html#container-columns-in-a-table) for how one gets there.
Writing one needs nothing special: `parquet_write_table` emits it like any other column, and the
only thing to know is which schema token it corresponds to.

| the column holds | the schema field's `data_type` |
|---|---|
| `parquet_list_column` | `list[<element>]` — e.g. `list[float64]` |
| `parquet_map_column` | `map[<value>]` — e.g. `map[int32]`; a map's keys are always strings |
| `parquet_struct_column` | bare `struct` |

See [the MAML format](../schema/maml-format.html) for the tokens themselves. Four things follow:

- **A schema-less write needs no token from you.** It derives each one from the column, so
  `parquet_write_table(t, "out.parquet")` on a table holding a list and a map writes them without
  a schema being written first.
- **The sidecar does the same.** With `write_maml=.true.` the emitted `.maml` carries
  `data_type: list[float64]` and its siblings, and carries **no** `col_size:` or `array_size:` key
  for such a column — neither applies to a container, whose row length is data rather than a
  declared width.
- **A payload's temporal resolution is carried across**, so a `list[timestamp[ms]]` column
  round-trips as milliseconds rather than being coerced to the writer's microsecond default. The
  table records each container column's payload unit when it opens the file, for the same reason
  it records a scalar timestamp column's.
- **A nested container cannot be written.** A table can hold one — `parquet_read_column` reads a
  `list<struct>` into a `parquet_list_column`, and `%add_column` accepts it — but there is no
  `data_type` token for it, so the write is refused with a message naming the field and the
  token it could not accept (`invalid data_type 'list[struct]'`). Such a column can be inspected
  and not written back.

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

Six rules:

- **`copy_metadata=.true.` and `metadata_keys=` are mutually exclusive.** One means every key and
  the other means exactly the listed ones, so asking for both is an error rather than a guess.
  Passing `copy_metadata=.false.` alongside `metadata_keys=` is fine, and carries the listed keys —
  the two are only in conflict when both are asking for something.
- **A key the schema declares itself wins.** The schema is your explicit statement about the
  output, so a carried key of the same name is skipped rather than overwriting it. A carried
  `<name>.datatype` companion is skipped too when the schema declared `<name>` itself, so the
  output never ends up with two companions describing one key.
- **A key `metadata_keys=` names but the source file does not have is an error**, checked before
  the output file is opened. Naming a key is a claim that it is there.
- **A key the writer generates itself is never carried, and naming one is an error.** Those are
  `DATE`, `name`, `IVOA.VOTable-Parquet.content`, `IVOA.VOTable-Parquet.version` and every
  `column.<name>.<attr>` entry — the writer emits them from the output schema, so copying the
  source's would put two entries of one name in the file. `copy_metadata=.true.` skips them
  silently, since it means "every key" rather than a claim about any particular one; naming one
  through `metadata_keys=` aborts before the output is opened, because skipping it there would
  make an explicit request a no-op. Your column metadata comes from the schema you are writing
  with, which is where a change to it belongs.
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

## Writing a table one row group at a time

`parquet_write_table` holds the whole output in memory as one table. For a result larger than
memory — a catalogue transformed row group by row group — the same write is available one row group
at a time: open a writer whose columns are the table's, hand it one table per row group, close it.
Three calls make that a loop with no schema written by hand:

```fortran
type(parquet_writer) :: w
integer(int64), allocatable :: b(:,:)
integer :: rg

call parquet_table_row_group_bounds("in.parquet", b)
do rg = 1, int(size(b, 2))
    block
        type(parquet_table) :: t
        logical, allocatable :: keep(:)
        call parquet_open_table(t, "in.parquet", b(1, rg), b(2, rg))
        call derive_columns(t)                       ! your own work on this row group
        allocate(keep(t%nrows()))
        call t%row_mask("mag <= 21.5", keep)
        if (rg == 1) call parquet_open_writer_like(w, "out.parquet", t)
        call parquet_write_table_chunk(w, t, row_mask=keep)
    end block
end do
call parquet_close_writer(w)
```

The writer is opened from the first prepared chunk, because that is the first moment the derived
columns exist; every later chunk has the same columns; and `row_mask=` is passed on every iteration
or on none. The output's columns are the ones **resident** at the open — `parquet_derive_schema`
reads nothing — so an input column the work never touched is in the file only if `t%materialize`
read it before the open. From then on, a chunk's unread column is read by the chunk write itself,
because the schema names it.

### The schema a schema-less write builds: `parquet_derive_schema`

A schema-less write [builds a schema internally](#writing-without-a-schema) from the resident
columns. `parquet_derive_schema` hands that schema back, so it can be adjusted and used with the
writer directly — for a file written one row group at a time, or one that needs `protected_cols:`,
provenance metadata or a dropped column without writing the MAML out by hand:

```fortran
type(parquet_schema) :: s
type(parquet_writer) :: w

call parquet_derive_schema(t, s)          ! one field per resident column, in slot order
call s%set_protected("flux")              ! this column never holds a Null
call s%add_metadata("pipeline", "v3.2")
call parquet_open_writer(w, "out.parquet", s, write_maml=.true.)
...
call parquet_close_writer(w)
```

The schema is exactly the one the schema-less write would have used: one field per **resident**
column, in slot order, under the column's own internal name; `col_size:`/`array_size:` declared
`auto`, so the writer resolves them from the data; the `unit:` key present only where the column has
one; the automatic `parquet_row_index` never included. It comes back parsed. Nothing is read to
build it, and nothing is measured from the values — in particular, whether a column currently holds
a Null says nothing about the file, and the schema does not pretend otherwise: a column that must be
written non-nullable is declared so with `%set_protected` (or `protected_cols:`), in advance.

The optional third argument is the MAML `table:` name: `parquet_derive_schema(t, s, name="dr4")`.
Omitted, it is the stem of the file the table was opened from, or `table` for one built in memory.
A table with no resident column is an error rather than an empty schema, because MAML has no way to
declare zero fields; materialize something first.

### Opening the writer from a table: `parquet_open_writer_like`

`parquet_open_writer_like` does the derive and the open in one call, and takes every writer option
`parquet_write_table` takes, under the same name and with the same default (the brackets mark the
optional arguments, as [above](#writer-options)):

`call parquet_open_writer_like(writer, filename, table, [schema], [copy_metadata], [metadata_keys], [write_maml], [qc], [compression], [compression_level], [chunk_size], [use_threads], [overwrite])`

```fortran
call parquet_open_writer_like(w, "out.parquet", t, chunk_size=200000, write_maml=.true.)
call parquet_write_column(w, "id", ids)      ! ...or a parquet_new_row_group loop
call parquet_close_writer(w)
```

- **`schema=` means "use this one instead of deriving"**, so a hand-written or MAML-loaded schema
  goes through the same call; the writer then declares that schema's fields, whatever the table
  holds. As with `parquet_write_table`, a schema built with `%init`/`%add_field` is parsed here if
  it has not been.
- **`copy_metadata=`/`metadata_keys=`** carry the table's source-file metadata into the output
  under the rules [above](#carrying-the-source-files-metadata-to-the-output), onto a private copy
  of the schema — the next open with the same schema carries nothing unless asked again.
- **The derived schema's `table:` name is the output file's stem**, as a schema-less
  `parquet_write_table` names it, and `write_maml=.true.` writes the sidecar at close, once every
  `auto` size has been resolved.
- **Without `schema=`, a table with no resident column is refused** — an open writer with no
  column is nothing you can use — where a schema-less `parquet_write_table` writes an empty file.

Once the writer is open, `parquet_get_column_names(writer, names)` and
`parquet_is_column_enabled(writer, name)` report what it declares — see [Writer
options](../io/writing.html#writer-options) in the I/O guide.

### One table, one row group: `parquet_write_table_chunk`

`call parquet_write_table_chunk(writer, table, [row_mask])`

writes `table`'s rows as **one complete row group** of an open writer: one
[`parquet_new_row_group`](../io/writing.html#streamingchunked-writes), one
`parquet_write_column_chunk` per column the writer declares, one `parquet_finish_row_group`. Call
it once per row group, as many times as there are row groups, on a writer with no row group open;
close the writer as usual.

- **The columns are the writer's, under `parquet_write_table`'s rules.** Every enabled field of
  the writer's schema is written, in schema order; a field disabled with
  `%set_column_unavailable` is skipped and the table need not carry it; the lookup key is the
  internal name, so a `col_map:` rename is applied by the writer on the way out. A field naming a
  column the table does not have is an error. A column the table has but has not read is read by
  the call — the schema naming it is the request — and stays resident; there is no `release=`,
  since the next row group is another table (`%evict_column` if you want it gone).
- **A schema-less writer works too**, with the schema-less write's rule: every resident column in
  slot order, `parquet_row_index` never, the file's columns fixed by the first row group. Pass an
  explicit `chunk_size=` to such a writer, as the I/O guide advises.
- **The first row group fixes the file.** Every column that will ever appear must be in the first
  row group, and every later chunk must have the same columns. A column that becomes resident only
  later is refused by the writer, naming the column.
- **A validity mask is passed for every column that takes one, whether or not the column holds a
  Null.** This is the one place a table write deliberately does what a hand-written loop need not.
  A streamed column's nullability is fixed by its first row group from whether a mask was passed,
  and every later row group must match — so a mask passed only when a Null happens to be present
  would make a later Null abort rows away from anything you did wrong. The always-present mask
  costs measurably more than an unmasked write (one mask per column per row group, plus the null
  bitmap the writer builds from it; `bench/benchmark_table.sh`'s streamed-write mode measures the
  pair). Declaring a column `protected_cols:` — `s%set_protected(name)` on the derived schema —
  removes the cost and the nullability question together: the writer drops a protected column's
  all-`.true.` mask, stores the field non-nullable, and a Null in it is an error naming the column,
  never a corrupt file.
- **`row_mask=`** (`size(row_mask) == table%nrows()`) drops every `.false.` row from the output
  entirely, per chunk, under
  [`parquet_write_chunk_row_mask`](../io/writing.html#row-groups)'s rules: used on a writer's
  first row group, it must be used on every one, and it is unavailable once a column was written
  whole with `parquet_write_column`. A Null still occupies a row; a dropped row leaves no trace.
- **A table with no rows is an error**, because a Parquet row group holds at least one row; skip
  the call for an empty result. A row group already open is refused by the writer, naming
  `parquet_finish_row_group` as the call that has to come first.
- **One thread, in order.** The call inherits the writer's rule: one writer's row groups are
  written from one thread, in row-group order. Parallelize the work that produces each chunk, not
  the write.

## An output file that stays open: `parquet_table_writer`

The loop in the recipe above knows its own row-group boundaries. A loop that does not — one that
produces a smallish table per iteration, of whatever size the analysis yields, and wants them all in
one file — hands each result to an output that stays open and decides the row groups itself:

```fortran
type(parquet_table)        :: t, seed
type(parquet_table_writer) :: out
integer :: i

call build_one_chunk(seed, 1)                       ! your own work: the first result
call parquet_open_table_writer(out, "merged.parquet", seed, write_maml=.true.)
call out%append(seed)                               ! the first result is data too
do i = 2, nchunks
    call build_one_chunk(t, i)
    call out%append(t)                              ! a row group is written whenever enough rows have arrived
end do
call parquet_close_table_writer(out)                ! the last row group, then the footer
```

Peak memory is one row group's worth of buffer plus whatever `build_one_chunk` holds. The loop body
has no lifecycle logic, no `opened` flag and no knowledge of row groups. When the shape of the
output is known up front, the template is a table built and left empty — `%add_column` with
zero-length arrays — and the first iteration stops being special.

`call parquet_open_table_writer(out, filename, template, [schema], [copy_metadata], [metadata_keys], [write_maml], [qc], [compression], [compression_level], [chunk_size], [use_threads], [overwrite])`

The arguments are `parquet_write_table`'s, minus `row_mask=` and `release=` (neither has a meaning
for a file that stays open), and the writer options are the same pass-throughs with the same
defaults; the page does not repeat them.

- **The template decides the columns, once, at open.** Its **resident** columns fix the output's
  columns, kinds, widths and units, with the schema a schema-less write would build, named after the
  output file's stem; its rows are not written (append it afterwards if they belong in the file). A
  slice, a filtered table and a zero-row table all serve as templates: only their descriptors are
  read. With `schema=`, the schema's enabled fields are the column set — each is checked against the
  template before the file is created, and `col_map:`, `qc:` and `protected_cols:` apply as for
  `parquet_write_table`. A template with nothing resident and no `schema=` is refused: there would be
  nothing to write.
- **`%append(table)` accepts any number of rows, including zero**, under `%append`'s own rules:
  matching kinds, widths and units, or an error; a column the output has and the table lacks is
  **null-filled** for those rows. Two things are decided here rather than inherited. A column the
  output declares that the appended table has but has not read is **read** for the copy — the output
  naming it is the request, exactly as `parquet_write_table` reads what its schema names — and given
  back afterwards, so the table is left in the residency state it arrived in. A resident column the
  output does not declare is **refused**, naming the file, when the columns came from the template
  (silently dropping data is the worse answer), and **ignored** when they came from `schema=` (the
  schema is the selection, as for `parquet_write_table`). `parquet_row_index` is dropped either way.
- **`%append(row)` takes a `parquet_table_row` handle**, for a loop that builds rows rather than
  tables: the same buffer and the same flush rule. A declared column the row's table has not read is
  read once and **kept** resident there, so a loop over one table's rows does not read it once per
  row; `%evict_column` gives the memory back afterwards.
- **`chunk_size` is the flush threshold.** An append that leaves at least `chunk_size` rows buffered
  writes the whole buffer as one row group, so a row group holds between `chunk_size` rows and
  `chunk_size - 1` plus the rows of the one append that crossed it — and the buffer never holds
  more. Omitted, it is the library's estimate from the schema
  ([`parquet_get_chunk_size`](../io/writing.html#writer-options)), with every vector column's width
  resolved from the template first. That estimate counts a string column as one byte per row, so a
  string-heavy output resolves to its ceiling of ten million rows: **pass `chunk_size=` for a
  string-heavy output.**
- **`%flush()` writes the pending rows as a row group now**, and does nothing when none are pending.
  It is for a row-group boundary at a place you choose — one row group per input file, say — without
  giving up the automatic flushing elsewhere. It puts rows on disk; it does not make the file
  readable.
- **You must close it.** `parquet_close_table_writer` writes the pending rows as the final row group
  and then the footer, which is what makes a Parquet file readable. A run that ends without closing
  leaves **no readable file at all** — not a truncated one — however many row groups were flushed,
  because the footer describing them was never written. A sink that accepted no rows closes to a
  valid file carrying every column and zero rows, with the writer's warning.
- **Six queries**: `%nrows()` (rows accepted: written plus pending), `%rows_pending()`,
  `%row_groups()`, `%chunk_size()` (the resolved threshold), `%filename(name)` and `%is_open()`, the
  one that never aborts. `%nrows()`, `%rows_pending()` and `%row_groups()` are `integer(int64)`;
  `chunk_size` and `%chunk_size()` are a plain `integer`, for the reason `parquet_write_table`'s is.
- **The buffer is reserved to `chunk_size` rows at open and again after every flush**, so a run of
  `%append(row)` calls between two flushes reallocates nothing (a string column's characters still
  grow on their own, since no row count can size them). A validity mask is written for every column
  that takes one, so a Null may arrive in any row group; `protected_cols:` opts a column out, as in
  the recipe above.
- **A sink belongs to one thread.** `%append`, `%flush` and the close refuse a sink another thread
  may share, on the ownership test a shared `parquet_table` refuses a structural change with — see
  [Thread safety](../operating/thread-safety.html#rules-at-a-glance) for the supported per-thread
  shape. Assignment is refused (two sinks would share and double-close one file); open a second
  output instead. Closing twice, or using a closed or never-opened sink, is an error naming the call
  and the file. There is no finalizer of its own: an abandoned sink loses its pending rows and leaves
  an unreadable file, exactly as an abandoned `parquet_writer` does.

Which of the two shapes to use: the sink, for most loops — it costs one copy of every row through
the buffer and gives automatic re-chunking, null-filling and the always-present mask for free, and
measures at parity with the same buffer loop written by hand in `bench/benchmark_table.sh`'s
streamed-write mode. The recipe above, when the input already comes in row groups and that copy is
worth avoiding.
