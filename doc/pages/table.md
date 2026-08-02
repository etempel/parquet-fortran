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

**Reading a column costs one pass, not two.** The table takes over the array it just read rather than
copying into storage of its own, and it asks the file's own statistics whether the column contains any
Null before building a validity mask — for a Null-free column, which is most columns of most files, no
mask is built at all. `parquet_column_has_nulls` exposes that same question if you want it directly.

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


### A variable-length `LIST` column's width is discovered, not declared

One column type needs a caveat here. Parquet's `LIST` encoding allows a **different number of
elements in every row**, so unlike the `fixed_size_list` this library always writes, such a column
carries no width in the file's schema — whether a single width covers every row is a property of the
data. Only a file from another tool can contain one (any Arrow-based writer preserves
`fixed_size_list`).

For those columns, and only those, **`%kind` and `%width` count as a read**:

```fortran
call parquet_open_table(t, "from_another_tool.parquet")
print *, t%width("spec")     ! may read data, for a variable-length LIST column
```

Opening still reads nothing at all — the column is left unclassified until something asks. The cost
is kept down in two tiers: first the file footer alone is checked (per row group, the mean elements
per row must be a whole number and must agree between row groups), which settles most non-uniform
columns for free; only a column that survives that is scanned, one row group at a time, stopping at
the first row that disagrees. The whole column is never held in memory.

Two practical consequences:

- **Order matters if you are going to read the column anyway.** `%prefetch` resolves the width as a
  side effect of the read it was doing regardless, so `%prefetch` then `%kind` is one pass over the
  data. `%kind` then `%prefetch` can be two.
- **Inside an OpenMP region, `%kind`/`%width` on such a column follow the first-touch rule** like any
  other read — resolve them before the region on a shared table. Every other column, scalar or
  `fixed_size_list`, is classified at open and stays safe to query from anywhere.

A slice measures over **its own** row groups, so a file that is ragged overall can present a uniform
width within one slice — and two tables over the same file can legitimately report different widths
for the same column. That is deliberate: a slice table holds only its own rows, and measuring the
whole file would defeat the point of opening a slice.

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

Beyond that floor, a slice costs about its share: each column is allocated once at the slice's
own row count and each covering row group is written straight into its place, so reading a
quarter of a file costs roughly a quarter of reading all of it, however many row groups the
slice spans.

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

## Renaming a file's columns with a read-in MAML

`parquet_open_table` takes an optional `maml=` argument naming a **read-in MAML file** that
describes the parquet file being opened. Its `extra: remap:` block gives the file's columns
table-facing names of your own, so program code works in one stable vocabulary regardless of what
a particular file happens to call things:

```yaml
# catalogue.maml
table: input_table
extra:
  remap:
  - mass: MASS_KG
  - ra: RA_J2000
```

```fortran
call parquet_open_table(t, "catalogue.parquet", maml="catalogue.maml")
call t%get("mass", m)      ! reads the file's MASS_KG column
```

Only the columns you list are affected; everything else keeps its own name. Nothing is read at
open, exactly as without a MAML — remapping is a relabelling of the schema, not a read.

Three rules are worth knowing, and they are what make this more than a rename:

* **The lookup name is the internal one, always.** `%get`, `%col`, `%kind`, `%residency` and every
  other table API take the internal name. The file column is what is actually read, so `%reload`
  goes back to it, and `%rename_column` changes only the lookup name — remap and rename compose
  freely and in either order.
* **An internal name may equal one of the file's own column names, and does not then mean
  "itself".** With `- ra: dec`, internal `ra` reads the file's `dec` column, and the file's own
  `ra` becomes unreachable unless another entry points at it. This shadowing is deliberate: a file
  often carries columns a program does not want, and one of them happening to share a name must
  not make that name unusable.
* **Two internal names may read the same file column.** They become two ordinary, independent
  columns — identical when first read, and free to diverge afterwards, because each holds its own
  copy. Writing to one never affects the other.

A remap naming a column the file does not have, or declaring one internal name twice, is an error
at open rather than a surprise later. See
[the MAML format](maml-format.html#renaming-columns-for-reading-with-extra-remap) for the grammar
and its write-side counterpart `col_map:`.

## Filtering, sorting and checking rows as the file is opened

`parquet_open_table` takes the same read-time transform `parquet_open_reader` does, applied once as the file is opened, so the table simply *is* the filtered/sorted/sampled result — there is no separate step and nothing downstream has to know a transform was applied:

```fortran
type(parquet_filter)  :: filt
type(parquet_sortkey) :: srt
type(parquet_read_qc) :: qc

call filt%add("mass > 1.0e12 and quality is_not_null")
call srt%add("-mass")
call qc%add("mass, >0")

call parquet_open_table(t, "catalogue.parquet", filter=filt, sort=srt, qc=qc)
print *, t%nrows()          ! rows that SURVIVED the filter
```

Also accepted: `sample_fraction=`/`sample_seed=` for a random subset, `qc_soft=` to warn instead of aborting on a qc violation, and `use_threads=`. All of them mean exactly what they mean on `parquet_open_reader`.

Three consequences worth stating plainly:

- **Every column inherits it, including ones read much later.** A column touched for the first time long after the open still comes back with the same rows in the same order — the transform lives on the table's reader, not on any one column.
- **`%nrows()` is the post-filter count.** With a filter or a sample active it is *not* the file's row count, and there is no way to ask for the file's original count through the table (open it a second time without the filter if you need both).
- **`%clone` keeps it.** A clone reopens the file for its own lazy reads, and reattaches the identical transform — otherwise a column the source never touched would come back with rows the source had filtered away.

### Column names: yours, not the file's

**`filter=`, `sort=` and `qc=` name columns the way `%col`/`%get` do — in the table's own internal names.** For a column renamed by a read-in MAML's [`extra: remap:`](#renaming-a-files-columns-with-a-read-in-maml), that is *not* what the file calls it, and the translation is done for you:

```fortran
! the MAML says:  remap:  - mass: MASS_KG
call filt%add("mass > 1.0e12")          ! your name, not MASS_KG
call parquet_open_table(t, "catalogue.parquet", maml="read.maml", filter=filt)
```

A **read-in MAML's own** `extra: filter:`, `extra: sort:` and `fields: qc:` are the other way round: they name the **file's** columns, because a read-in MAML describes the physical file and travels with it. See [the MAML format](maml-format.html#filtering-and-sorting-on-read-with-extra-filter-and-extra-sort) for that grammar.

When both sources are given, they compose:

| | rule |
|---|---|
| **filter** | AND — the same way two `%add` calls on one filter already combine. Order does not matter. |
| **sort** | the MAML's keys first (so they are the primary ones), then yours as tie-breakers. Order *does* matter here. |
| **qc** | per **column**: if the MAML declares `qc:` for a column at all, the MAML wins in full for that column and your entry for it is dropped. See [Quality control](quality-control.html#the-merge-rule-per-column-not-per-bound). |

### qc is checked on first touch, not at open

Opening a table reads no column data, so a `qc=` bound is enforced when the column is actually read — on its first `%get`/`%col`, or at `%prefetch`/`%materialize_all`. This is the lazy table's normal behaviour applied to qc, not a weaker guarantee, but it has one consequence to be aware of: **a violated bound on a column the program never touches is never reported.** Call `%materialize_all` if you want every declared bound checked up front.

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

To write a single cell, use `%set_element`:

```fortran
call t%set_element("mass", 42, 1.75_real64)   ! row 42 of the mass column
```

The value's kind must match the column's exactly, as it does for `%set`, and writing a value
**clears that row's null** — a cell cannot be both a value and missing.

## Null values, and changing them

`%is_null(name, i)`, `%set_null(name, i)` and `%clear_null(name, i)` all work a **row** at a
time, and that is true even for a vector column: `%set_null` on a `float64` column of width 3
nulls the whole row, all three elements together. A single element of a vector row cannot be null
on its own — the file format this library reads can express it, but the table cannot represent it,
so a per-element null read from a file is widened to the whole row.

A column that holds no null at all carries no null bitmap, which is why `%compact_validity(name)`
exists: it drops the bitmap of a column that once had nulls and no longer does. A whole-column
`%set` already does this on its own, so `%compact_validity` is only for a column edited cell by
cell.

## Changing a table

Mutation falls into three classes, and the difference between them matters more than any
individual procedure:

| class | what it does | detaches? |
|---|---|---|
| **cell** — `%set_element`, `%set_null`, `%clear_null` | changes values in place | no |
| **column** — `%add_column`, `%drop_column`, `%rename_column`, `%cast_column` | changes which columns exist | no |
| **row** — `%filter_rows`, `%sort_by`, `%delete_rows`, `%truncate`, `%append`, `%append_null_rows` | changes which rows exist | **yes** |

```fortran
call t%materialize_all()                 ! read everything you want to keep, first
call t%filter_rows(mass > 1.0e10_real64) ! keep the rows a mask selects
call t%sort_by(["mass"], descending=[.true.])
call t%drop_column("scratch")
```

### What "detaching" means

A table opened from a file reads its columns lazily. Once a **row**-changing operation runs, the
rows in memory no longer line up with the rows in the file, so any column that had not been read
by then can never be read at all. The table records this — `%is_detached()` reports it — and every
later attempt to read from the file says so rather than returning misaligned data.

**So read what you need before changing the row set**, with `%prefetch` or `%materialize_all`.
Columns you deliberately do not want are simply left behind, which is what keeps a lazy table
from having to read a whole file before it can drop a single row.

Detaching does not freeze a table: it can still be read, edited and written out, and mutated
further. What it loses is the file behind it.

### Sorting

`%sort_by` takes one or more key columns, primary first, with optional per-key `descending=` and
`nulls_first=` arrays:

```fortran
call t%sort_by(["group", "mass "], descending=[.false., .true.])
```

Every key column must already be resident — sorting will not read one implicitly, because that
would make the memory a sort costs depend on which columns happened to have been read. Nulls and
NaNs are placed absolutely and are never flipped by `descending`; ascending order gives values,
then NaNs, then nulls. Ties keep their existing order.

This is the same sort engine `parquet_open_reader(..., sort_by=...)` uses, so sorting a table in
memory and reading the same file sorted give the identical row order.

### Adding rows

The way to add many rows is a batch that structurally cannot have the wrong columns:

```fortran
call t%clone_structure(batch)     ! same columns, zero rows, no file
call batch%append_null_rows(n)    ! give it n rows to fill
call batch%set("id", ids)         ! ... and fill them
call batch%set("mass", masses)
call t%append(batch)
```

`%append` requires the appended table's columns to be a subset of this table's, with matching
kinds, widths and units. A column this table has and the batch does not is **null-filled**; a
column the batch has and this table does not is an error rather than being silently dropped; a
kind mismatch is an error too, and `%cast_column` is the way round it. There is no unit
conversion, so appending "km/h" rows to an "m/s" column is refused.

`%append(row)` adds one row from a `parquet_table_row` handle. It is convenient but slow in bulk —
it costs a whole table's machinery per row — so prefer the batch form above for anything large.

### Copying, and going back

Mutation happens in place and there is no undo, so the way to keep a version to return to is to
copy first:

```fortran
call t%clone(before)     ! independent deep copy
call t%filter_rows(keep)
```

A clone copies the columns already read and leaves the unread ones unread, opening its own reader
on the same file so it stays lazy. `before` must be declared as the same concrete table type as
`t`.

> **A pointer from `%col` does not survive a row-changing operation.** `%filter_rows`, `%sort_by`,
> `%append` and the rest reallocate each column's storage, so a pointer taken before one of them
> points at freed memory afterwards. Fortran cannot detect this. Take the pointer again after the
> mutation.

## Current limitations

This is still a deliberately narrow version of the table layer.

- **A column is all-or-nothing.** Reading one row of a column reads the whole column (across the
  table's row scope). Use the [slice regime](#reading-part-of-a-file-the-slice-regime) to bound
  how much that is.
- **No filtering or sorting at open time.** In-memory `%filter_rows` and `%sort_by` are
  available (see [Changing a table](#changing-a-table)), but a table cannot yet be opened with a
  read-time `filter=`/`sort_by=` the way a reader can, so the whole scope is read and then
  reduced.
- **Nothing is ever evicted.** A column stays resident once read; there is no per-column
  eviction, and `%drop_column` is the only way to give a column's memory back. The slice regime
  is what bounds total memory.
- **`b = a` is a hard error**, not a silent alias — the column store lives behind a pointer, and
  a shallow copy would leave two tables sharing (and double-freeing) one store. Use `%clone`.
- **A table is not thread-safe to mutate.** Reading resident columns from several threads is
  fine, and each thread may open and read its own table; every mutation is a single-threaded
  operation.
- **`%cast_column` converts only between the numeric scalar kinds**, and refuses any value that
  would not survive the round trip rather than truncating it.
- **`%append` null-fills a missing column; a per-column default value is not available.** Fill
  the batch explicitly if you want something other than nulls.
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
