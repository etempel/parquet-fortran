---
title: Whole tables in memory with parquet_table
---

`parquet_tables` is a high-level layer over the reader and writer: it loads a whole parquet file
into memory as one `parquet_table`, hands its columns back as ordinary Fortran arrays, and writes
a table back out through the same `parquet_schema` you would use for `parquet_open_writer`. It
does not replace `parquet_read_column`/`parquet_write_column` — it drives them.

Use it when you want the whole file available at once and would rather not manage a reader, a
row count and one array per column by hand. Keep using the reader directly when you want a single
column, a row-group at a time, or precise control over what is resident.

```fortran
program mean_mass
    use parquet_tables
    use iso_fortran_env, only: real64
    implicit none
    type(parquet_table) :: t
    real(real64), allocatable :: mass(:)

    call parquet_open_table(t, "catalogue.parquet")
    call t%get("mass", mass)
    print *, "rows:", t%nrows(), " mean mass:", sum(mass) / size(mass)
end program mean_mass
```

## Two ways to reach a column

**`%get` is the one to reach for first.** It copies the column into an allocatable array of
whatever kind you declared, **widening** an `int32` column into an `integer(int64)` array and a
`float32` column into a `real(real64)` array on the way. So you can ask for the type you want to
work in without knowing what the file happened to store:

```fortran
real(real64), allocatable :: v(:)
call t%get("flux", v)        ! works whether the file stored float32 or float64
```

**`%col` is the zero-copy alternative.** It points a Fortran `pointer` straight at the table's own
storage: no copy, and writes through it change the table.

```fortran
real(real64), pointer :: p(:)
call t%col("flux", p)
p = p * 2.0_real64           ! modifies the column in place
```

Two things to know about `%col`:

- **The pointer kind must match the stored kind exactly.** There is no widening on this path — it
  aliases raw storage, so there is nothing to convert through. Ask `%kind` first if you do not
  already know the type (see below). A mismatch is a hard error, not a silent conversion.
- **Do not keep the pointer across a structural change.** Adding or replacing a column may
  relocate the store. Re-fetch the pointer after any such call.

You do **not** need to declare the table `target` to use `%col`. The pointer refers to the table's
internal heap storage, not to the table variable itself.

### Working with a column whose type you do not know

`%kind` returns a `PK_*` constant, and `parquet_kind_name` turns one into text for a message:

```fortran
select case (t%kind(name))
case (PK_FLOAT64); call t%col(name, p64); print *, sum(p64)
case (PK_INT32);   call t%col(name, p32); print *, sum(p32)
case default
    call parquet_kind_name(t%kind(name), kname)
    print *, "unhandled kind: ", kname
end select
```

If all you want is the numbers, `call t%get(name, arr_f64)` is shorter and widens for you — the
dispatch above is only worth it when you need the zero-copy path.

## What the table tells you about itself

| call | answer |
|---|---|
| `t%nrows()` | rows every column holds |
| `t%ncols()` | number of columns |
| `call t%column_names(names)` | every column name, in file order |
| `t%has_column(name)` | whether a column of that name exists |
| `t%kind(name)` | its `PK_*` kind (`PK_NONE` if unreadable) |
| `t%width(name)` | values per row: 1 for a scalar column, the element count for a vector one |
| `call t%unit(name, u)` | its unit string, or `""` |
| `t%residency(name)` | `RES_FULL` once loaded, `RES_EMPTY` for a column that could not be read |
| `t%is_supported(name)` | whether its physical type is one this library can read |
| `t%is_null(name, i)` | whether row `i` of that column is null |
| `call t%get_file_metadata(key, value, found)` | one key from the source file's metadata |

Every lookup above takes an optional **`found=`**: without it a missing column is a hard error;
with it, the miss is reported through `found` and the call returns quietly.

```fortran
call t%get("maybe_absent", v, found=ok)
if (.not. ok) print *, "column not in this file"
```

## Columns this library cannot read

A parquet file may contain a column whose physical type this library does not support — a
decimal or unsigned integer column written by another tool, or a `LIST`/`MAP` column. **Such a
column does not stop the file from opening.** It gets a slot, so it still appears in
`%column_names` and `%has_column`, but it holds no values: `%is_supported` reports `.false.`,
`%kind` reports `PK_NONE`, and any attempt to read its values is an error naming the column.

That way one exotic column never makes an otherwise-usable file unopenable.

## Nested struct columns

A struct field's leaves are addressable by their dotted paths, exactly as in
[`parquet_read_column`](reading.html):

```fortran
call t%get("main.inner.deep.value", v)
```

The bare struct name (`main`) is *not* a column — it is not readable on its own, so it never
appears in `%column_names`.

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
call parquet_parse_maml(s)                      ! REQUIRED before writing
call parquet_write_table(t, "out.parquet", s)
```

`%add_column` refuses a name that already exists unless you pass `force=.true.`, which replaces
the column outright.

**The schema decides the output.** `parquet_write_table` walks the schema's enabled fields, looks
each one up in the table **by its internal name**, and writes it under the schema's output name —
so a `col_map:` rename works exactly as it does for `parquet_open_writer`. A schema field with no
matching table column is an error; a table column the schema does not name is simply not written.

**A schema built with `%init`/`%add_field` must be passed through `parquet_parse_maml` first**, as
it must for `parquet_open_writer`. Forgetting it is a clear error rather than a mysterious
failure.

To write only some rows without changing the table, pass a mask:

```fortran
call parquet_write_table(t, "subset.parquet", s, row_mask=keep)
```

## Replacing values

`%set` replaces a column's values from an array of the **same length** — it changes values, never
the row set:

```fortran
call t%get("mass", m)
m = m * 2.0_real64
call t%set("mass", m)
```

Pass `modify_nulls=.false.` to leave null rows untouched.

## Current limitations

This is the first, deliberately narrow version of the table layer.

- **Everything is read when the file is opened.** There is no lazy per-column loading yet, so
  opening a wide file loads all of it. Use the reader directly if you need one column from a file
  too large to hold.
- **The whole file, or nothing.** There is no way to open a row range yet.
- **No filtering or sorting** at open time, and no in-memory row filtering, sorting or row
  addition.
- **Copying a table is not supported.** `b = a` is a hard error rather than a silent alias — the
  column store lives behind a pointer, and a shallow copy would leave two tables sharing (and
  double-freeing) one store.
- **A table is not thread-safe.** Reading an already-open table from several threads is fine;
  opening, adding columns and writing are single-threaded operations.
- **Units are not read from the file.** `%unit` reports what `%add_column(unit=)` stored, and `""`
  for a column read from a file.
- **Vector string columns are trimmed.** A rank-2 string column has no compact read path, so it
  goes through the fixed-width reader, where trailing blanks cannot be told from padding.

## Memory

The table keeps its own Fortran copy of every column and releases the reader's decoded Arrow
buffers as it goes, so a fully loaded table holds roughly one copy of the data rather than two.
`tools/benchmark_table.sh` measures this directly (see CONTRIBUTING.md); note that resident set
size does **not** show it, because Arrow's memory pool keeps freed pages rather than returning
them to the operating system.
