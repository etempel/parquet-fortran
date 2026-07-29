---
title: Whole tables in memory with parquet_table
---

`parquet_tables` is a high-level layer over the reader and writer: it presents a whole parquet
file as one `parquet_table`, hands its columns back as ordinary Fortran arrays, and writes a
table back out through the same `parquet_schema` you would use for `parquet_open_writer`. It does
not replace `parquet_read_column`/`parquet_write_column` — it drives them.

Use it when you want a file's columns available by name and would rather not manage a reader, a
row count and one array per column by hand. Keep using the reader directly when you want precise
control over exactly which reads happen and when.

**Opening a table reads no column data.** It reads the file's schema, so every column's name,
kind and width is known immediately, and each column's values are read the first time something
asks for them. A program that opens a 40-column file and uses two of them pays for two. See
[Laziness](#laziness-and-what-it-costs) below for what follows from that.

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

Three things to know about `%col`:

- **The pointer kind must match the stored kind exactly.** There is no widening on this path — it
  aliases raw storage, so there is nothing to convert through. Ask `%kind` first if you do not
  already know the type (see below). A mismatch is a hard error, not a silent conversion.
- **Do not keep the pointer across a structural change.** Adding or replacing a column may
  relocate the store. Re-fetch the pointer after any such call.
- **Arithmetic through the pointer is a little slower than over an array you own.** Measured with
  `tools/benchmark_table.sh`'s access mode, `z = xp + yp` through two `%col` pointers runs at
  roughly 1.1x the time of the same expression over two allocatables when the columns are large
  enough to be memory-bandwidth-bound, and around 1.7x once they are small enough to sit in cache.
  Declaring your own pointer `contiguous` does *not* recover it, so this is not `%col`'s missing
  stride guarantee.

You do **not** need to declare the table `target` to use `%col`. The pointer refers to the table's
internal heap storage, not to the table variable itself.

### Choosing between `%get` and `%col`

`%col` avoids a full copy of the column; computing through it costs a little more per pass. So the
choice is simply how many times you sweep the same column:

- **A few passes — use `%col`.** The copy `%get` would make costs far more than the arithmetic
  difference. On a 15.6M-row float64 column this break-even sits near sixty passes.
- **Many passes, or a hot inner loop — `%get` once and compute on your own array.** You pay the
  copy a single time and every pass afterwards runs at plain-array speed.
- **In-place modification — `%col`, always.** Writes through the pointer change the column; `%get`
  hands you a detached copy, and you would need `%set` to put it back.

`tools/benchmark_table.sh`'s access mode prints the break-even point for your own machine and
column size.

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
| `t%residency(name)` | `RES_FULL` once read, `RES_EMPTY` before that (and for an unreadable column) |
| `t%is_supported(name)` | whether its physical type is one this library can read |
| `t%is_null(name, i)` | whether row `i` of that column is null |
| `call t%get_file_metadata(key, value, found)` | one key from the source file's metadata |

Every lookup above takes an optional **`found=`**: without it a missing column is a hard error;
with it, the miss is reported through `found` and the call returns quietly.

```fortran
call t%get("maybe_absent", v, found=ok)
if (.not. ok) print *, "column not in this file"
```

## Laziness, and what it costs

A column is read on its first *value* access — `%get`, `%col`, `%set`, `%is_null`, a row handle's
`%get`, or `%get_slice`. Everything else (`%nrows`, `%ncols`, `%column_names`, `%kind`, `%width`,
`%has_column`) is answered from the schema and reads nothing.

```fortran
call parquet_open_table(t, "catalogue.parquet")
print *, t%kind("mass"), t%residency("mass")   ! PK_FLOAT64, RES_EMPTY -- nothing read yet
call t%get("mass", mass)                       ! now it is read
print *, t%residency("mass")                   ! RES_FULL
```

Three calls control it explicitly:

| call | does |
|---|---|
| `call t%prefetch(name)` / `call t%prefetch(names)` | read those columns now |
| `call t%materialize_all()` | read every column not yet read |
| `call t%reload(name)` | re-read one column from the file, discarding local `%set` edits |

`%prefetch`'s array form is not just a loop: it reads the named columns in one pass, which
matters for **struct leaves**. The reader decodes a struct as one array shared by all its leaves,
and the table frees that array as soon as the column it was asked for is stored — so touching
`main.a`, then `main.b`, then `main.c` one at a time decodes the struct three times, while
`call t%prefetch(["main.a", "main.b", "main.c"])` decodes it once. Reading them separately is
never *wrong*, only slower.

`%reload` only applies to a column that came from a file: reloading one built with `%add_column`
is an error, since there is nothing to reload it from.

> `t%prefetch` and the reader-level `parquet_prefetch_columns` are different things. The table's
> reads a column into the table's own store; the reader's warms Arrow's side of the read.

### Reading a table from several threads

Reading a column that is **already resident** takes no lock and runs fully in parallel. A *first
touch* inside an OpenMP parallel region is a hard error, because it allocates and publishes state
other threads may be reading at that moment. So a table shared across a parallel region must be
prefetched before it:

```fortran
call parquet_open_table(t, "catalogue.parquet")
call t%prefetch(["mass    ", "redshift"])      ! REQUIRED before the region
call t%col("mass", mass)
call t%col("redshift", z)
!$omp parallel do
do i = 1, t%nrows()
    lum(i) = luminosity(mass(i), z(i))         ! resident reads, no lock
end do
!$omp end parallel do
```

A table a thread **opens for itself inside** the region is a different case: it cannot be shared,
so its lazy reads are allowed — that is what the [slice regime](#reading-part-of-a-file-the-slice-regime)
below is for.

## Reading part of a file: the slice regime

`parquet_open_table(t, file, row_lo, row_hi)` gives the table a contiguous row range, and only the
row groups covering it are ever read. Row indices everywhere else — `%row(i)`, `%get_slice`,
`%is_null(name, i)` — then count from the slice's own first row, not the file's.

```fortran
call parquet_open_table(t, "big.parquet", 1000001_int64, 2000000_int64)
print *, t%nrows()          ! 1000000
```

`parquet_table_row_group_bounds(file, bounds)` reports where the natural boundaries are, without
opening a table at all — `bounds(1, rg)` and `bounds(2, rg)` are row group `rg`'s first and last
rows. It is the planning call: a thread cannot open its slice until it knows which rows to ask
for. `call t%row_group_bounds(bounds)` answers the same for an open table, always in the file's
own row numbering.

Putting the two together gives the per-thread pattern, where each thread reads only its own
share of the file:

```fortran
call parquet_table_row_group_bounds("big.parquet", bounds)
!$omp parallel do
do rg = 1, size(bounds, 2)
    block
        type(parquet_table) :: mine        ! see the note below -- NOT private(mine)
        real(real64), allocatable :: x(:)
        call parquet_open_table(mine, "big.parquet", bounds(1, rg), bounds(2, rg))
        call mine%get("mass", x)
        call process(x)
    end block
end do
!$omp end parallel do
```

> **Declare a per-thread table in a `block`, not with `private(t)`.** An OpenMP `private` copy of
> a finalizable derived type is not reliably default-initialized by gfortran, so opening into one
> runs the finalizer over an undefined pointer and the program dies in the allocator. A
> block-local is initialized on entry and finalized at exit, which is what a per-thread table
> wants anyway.

A slice need not line up with row-group boundaries — the covering groups are read and trimmed.
It cannot be cheaper than one row group, though, since a row group is the smallest unit the
format lets a reader decode.

## One row at a time

`t%row(i)` returns a lightweight, non-owning handle on a single row — the natural thing to hand
to a procedure that works on one row:

```fortran
type(parquet_table_row) :: r
real(real64) :: m
r = t%row(42)
call r%get("mass", m)               ! scalar kind -> scalar
call r%get("flux", spectrum)        ! vector kind -> allocatable rank-1 array
print *, r%index(), r%is_null("mass")
```

`%get` on a handle widens exactly as the table's own does, and triggers the same lazy first
touch, so a handle can reach a column nothing has read yet. The handle resolves the column by
name and the row by index on every access, so it survives anything that merely reallocates a
column; it is invalidated by a change to the row set or by the table going out of scope. The
table it came from does **not** need the `target` attribute.

## Picking rows out of a column

`%get_slice` copies a selection of rows rather than the whole column. Build the selection with
`parquet_slice_range` (`start:stop:step`) or `parquet_slice_list` (an explicit gather):

```fortran
type(parquet_slice) :: s
s = parquet_slice_range(1, 10, 2)          ! rows 1, 3, 5, 7, 9
call t%get_slice("mass", s, m)
s = parquet_slice_range(1000)              ! row 1000 to the end
s = parquet_slice_range(10, 6, -2)         ! counting down
s = parquet_slice_list([9, 2, 2, 15])      ! in that order, repeats allowed
```

`stop` defaults to the table's last row and is resolved when the slice is *used*, so one slice
object works on tables of different lengths. `step` defaults to 1 and must not be zero; every
selected row must exist. `%get_slice` widens like `%get`, and a vector column keeps its
`(element, row)` shape. There is no pointer form: a strided or gathered selection is not
contiguous, so `%col` (the whole column) remains the zero-copy path.

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

**It costs about what writing the columns yourself costs.** `parquet_write_table` writes straight
out of the table's store without copying, and a column that holds no nulls is written with no
validity mask at all — the same call a hand-written loop would make. `tools/benchmark_table.sh`'s
write mode measures the two against each other and reports them at parity.

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

This is still a deliberately narrow version of the table layer.

- **A column is all-or-nothing.** Reading one row of a column reads the whole column (across the
  table's row scope). Use the [slice regime](#reading-part-of-a-file-the-slice-regime) to bound
  how much that is.
- **No filtering or sorting** at open time, and no in-memory row filtering, sorting or row
  addition.
- **Nothing is ever dropped.** A column stays resident once read; there is no per-column
  eviction. Again, the slice regime is what bounds total memory.
- **Copying a table is not supported.** `b = a` is a hard error rather than a silent alias — the
  column store lives behind a pointer, and a shallow copy would leave two tables sharing (and
  double-freeing) one store.
- **A table is not thread-safe to mutate.** Reading resident columns from several threads is
  fine, and each thread may open and read its own table; adding columns and writing are
  single-threaded operations.
- **Units are not read from the file.** `%unit` reports what `%add_column(unit=)` stored, and `""`
  for a column read from a file.
- **Vector string columns are trimmed.** A rank-2 string column has no compact read path, so it
  goes through the fixed-width reader, where trailing blanks cannot be told from padding.

## Memory

The table keeps its own Fortran copy of each column it reads and releases the reader's decoded
Arrow buffers as it goes, so a fully loaded table holds roughly one copy of the data rather than
two — and a table whose columns were never touched holds nothing at all.
`tools/benchmark_table.sh` measures this directly (see CONTRIBUTING.md); note that resident set
size does **not** show it, because Arrow's memory pool keeps freed pages rather than returning
them to the operating system.
