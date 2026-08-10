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
type(parquet_table) :: t
type(parquet_schema) :: s

call parquet_new_table(t)
call t%add_column("id",   ids)                  ! fixes nrows
call t%add_column("mass", masses, unit="Msun")
call t%add_column("name", names)

call s%init("catalogue")
call s%add_field("id",   "int64")
call s%add_field("mass", "float64", unit="Msun")
call s%add_field("name", "string", array_size=32)
call parquet_write_table(t, "out.parquet", s)   ! parses the schema itself if you have not
```

`%add_column` refuses a name that already exists unless you pass `force=.true.`, which replaces
the column outright.

The schema is optional — see [Writing without a schema](#writing-without-a-schema) for what a
`parquet_write_table(t, "out.parquet")` with no schema does.

**The schema decides the output.** `parquet_write_table` walks the schema's enabled fields, looks
each one up in the table **by its internal name**, and writes it under the schema's output name —
so a `col_map:` rename works exactly as it does for `parquet_open_writer`. A schema field with no
matching table column is an error; a table column the schema does not name is simply not written.

**You do not have to parse the schema yourself.** A schema built with `%init`/`%add_field` carries
only MAML text until `parquet_parse_maml` turns it into fields; `parquet_write_table` makes that
call for you when it has not been made. Two things follow: the schema is *left* parsed afterwards
(that is a visible side effect, and the reason its dummy argument is `intent(inout)`), and a
schema that was never built at all — no `%init`, no fields — is still an error, since there is
nothing there to parse. `parquet_open_writer` still requires a parsed schema of its own.

**Writing a file-backed table reads what it has not read yet.** Every column the schema names is
materialized as it is written, so `parquet_write_table` on a freshly opened table reads exactly
the columns the schema asks for — and on a [detached](table-mutate.html#what-detaching-means) table, a schema field
whose column was never read is an error, since there is no longer a file to read it from.

**...and gives it back again.** By default (`release=.true.`) the table ends the write in the
residency state it started in: a column the write had to read is released once it has been
written, so writing a freshly opened 16-column table costs one column's residency rather than
sixteen. A column *you* had already read is left alone — materializing it was not the write's
doing, so undoing it is not the write's business either. Pass `release=.false.` to keep everything
the write read, which is what you want when the next thing you do is read those same columns.
Releasing frees storage a `%col` pointer could alias, so it advances
[`%generation()`](table.html#two-ways-to-reach-a-column) — see the note there for why that is a conservative
signal rather than a real one.

**It costs about what writing the columns yourself costs.** `parquet_write_table` writes straight
out of the table's store without copying, and a column that holds no nulls is written with no
validity mask at all — the same call a hand-written loop would make. `tools/benchmark_table.sh`'s
write mode measures the two against each other and reports them at parity.

To write only some rows without changing the table, pass a mask:

```fortran
call parquet_write_table(t, "subset.parquet", s, row_mask=keep)
```

### Writing without a schema

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
- **The automatic [`parquet_row_index`](table-open.html#which-row-of-the-file-is-this) column is never written**,
  even when you have materialized it. It records where a row came from rather than being part of
  your table; name it in a schema if you want it in the output.
- **`release=` has nothing to do**, since a schema-less write only ever writes columns that were
  already resident.

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
carries no unit of its own. A temporal column *built in memory* with `%add_column` has no stored unit
to record, so it takes the default microseconds; give it a schema with an explicit
`timestamp[ns]`-style token if it needs finer.

### Writer options

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
| `compression_level` | codec level (not every codec has one — `snappy` does not) | codec's own |
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

### Carrying the source file's metadata to the output

A table opened from a file snapshots that file's key/value metadata when it opens, and
`parquet_write_table` can carry it into the output:

```fortran
call parquet_write_table(t, "out.parquet", s, copy_metadata=.true.)      ! every key
call parquet_write_table(t, "out.parquet", s, metadata_keys=["origin"])  ! only these
```

Four rules:

- **The two forms are mutually exclusive.** `copy_metadata=.true.` means every key,
  `metadata_keys=` means exactly the listed ones; giving both is an error rather than a guess.
- **A key the schema declares itself wins.** The schema is your explicit statement about the
  output, so a carried key of the same name is skipped rather than overwriting it.
- **A key `metadata_keys=` names but the source file does not have is an error**, checked before
  the output file is opened. Naming a key is a claim that it is there.
- **It works after the table has [detached](table-mutate.html#what-detaching-means)** — which is the point, since
  read, change rows, write is exactly the shape that detaches. It also adds nothing to your own
  schema, so writing a second table with the same schema does not inherit the first one's
  provenance.

`call t%get_file_metadata(key, value, [found])` reads one key of that snapshot at any time, and
survives detaching for the same reason. To see everything a file carries, before or without a
table, use
[`parquet_get_metadata_items`](../io/reading.html#listing-every-metadata-entry).
