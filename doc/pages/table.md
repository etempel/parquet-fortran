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

### What to `use`

**`use parquet` is enough for everything on this page.** It re-exports the whole library, so a
program that mixes a table with a kind constant, a schema, a filter and a timestamp needs exactly
one `use` statement:

```fortran
use parquet          ! parquet_table, PK_FLOAT64, parquet_schema, parquet_filter, ... all in scope
```

The example above says `use parquet_tables` instead, and that still works: each layer remains its
own module and can be named individually when you want a narrower import (it compiles marginally
faster, and it documents which layers a source file actually depends on). The table below says
which module each name lives in, for when you want that.

| you write | its own module |
|---|---|
| `parquet_table`, `parquet_open_table`, `parquet_new_table`, `parquet_write_table`, `parquet_table_row`, `parquet_slice`/`parquet_slice_range`/`parquet_slice_list`, `parquet_table_row_group_bounds`, `RES_EMPTY`/`RES_FULL` | `parquet_tables` |
| the `PK_*` kind constants, `parquet_kind_name` | `parquet_columns` |
| `parquet_schema`, `parquet_parse_maml`, `parquet_filter`, `parquet_sortkey`, `parquet_read_qc` | `parquet_core` (internal — reach these through `use parquet`) |
| `parquet_string_column` | `parquet_strings` |
| `parquet_date`, `parquet_time`, `parquet_timestamp` | `parquet_temporal` |

`parquet_core` is the one entry there you should not name directly: it is an internal
implementation module that may be renamed or restructured in any release, and `use parquet` is
what the library's [API stability](../index.html#important-behavior) promise covers.

## Two ways to reach a column

**`%get` is the one to reach for first.** It copies the column into an allocatable array of
whatever kind you declared, **widening** an `int32` column into an `integer(int64)` array and a
`float32` column into a `real(real64)` array on the way. So you can ask for the type you want to
work in without knowing what the file happened to store:

```fortran
real(real64), allocatable :: v(:)
call t%get("flux", v)        ! works whether the file stored float32 or float64
```

**`%get` does not change what the table holds.** The widening happens on the way into *your*
variable; the column keeps the kind the file stored, and asking for it again in another kind
converts again. The call that changes the stored kind is [`%cast`](#changing-a-columns-type), and
the two together are the way to read a file column into a type you choose:

```fortran
call t%cast("flux", PK_FLOAT64)   ! the table now holds float64, whatever the file stored
call t%get("flux", v)             ! ... and this is a copy, not a conversion
```

On a column nothing has read yet, `%cast` costs nothing extra: the read decodes straight into the
kind you asked for, in one pass.

**`%col` is the zero-copy alternative.** It points a Fortran `pointer` straight at the table's own
storage: no copy, and writes through it change the table.

```fortran
real(real64), pointer :: p(:)
call t%col("flux", p)
p = p * 2.0_real64           ! modifies the column in place
```

Four things to know about `%col`:

- **The pointer kind must match the stored kind exactly.** There is no widening on this path — it
  aliases raw storage, so there is nothing to convert through. Ask `%kind` first if you do not
  already know the type (see below). A mismatch is a hard error, not a silent conversion. To point
  at a column in the kind your code wants rather than the kind the file happened to store, convert
  the column first with [`%cast`](#changing-a-columns-type) — a column already of that kind is
  left alone, so the call is safe to make unconditionally.
- **Do not keep the pointer across a structural change.** Adding or replacing a column may
  relocate the store, and every row-changing operation does. Re-fetch the pointer after any such
  call. Fortran cannot detect a stale pointer and neither can this library, but `%generation()`
  can tell you whether anything structural happened:

  ```fortran
  g = t%generation()
  call some_procedure(t)          ! might mutate it
  if (t%generation() /= g) call t%col("flux", p)   ! re-fetch; the old p may be stale
  ```

  The counter is deliberately conservative — every column- and row-structural call bumps it,
  whether or not it actually moved anything — so a change in it means "re-fetch", not "definitely
  invalidated". A call that changes nothing does not bump it.

  One bump is worth knowing about because the call does not look structural:
  [`parquet_write_table`](#writer-options) advances the counter when it gives back a column it had
  to read (its default). Nothing you can hold a pointer to is ever given back — taking a pointer
  materializes the column, and only columns the write itself materialized are released — so after
  a write the counter may have moved while every pointer you hold is still good. Re-fetch or
  ignore it; do not read the advance as evidence of damage.
- **A string column has no *array* pointer form, but it does have one.** `PK_STRING`/
  `PK_STRING_VEC` are stored as a packed variable-length buffer, so there are no fixed row slots
  for a Fortran array to alias — but `%col` will hand back a `type(parquet_string_column),
  pointer` to the store itself. Read it and edit its values in place; do not change its length or
  element count through the pointer, since the column's own row count is kept separately. See
  [String columns in a table](#string-columns-in-a-table).
- **Arithmetic through the pointer costs what arithmetic over your own array costs.** Measured
  with `tools/benchmark_table.sh`'s access mode, `z = xp + yp` through two `%col` pointers runs
  within 1% of the same expression over two allocatables. There is no pointer penalty to trade
  against the copy `%get` would have made.

You do **not** need to declare the table `target` to use `%col`. The pointer refers to the table's
internal heap storage, not to the table variable itself.

### Choosing between `%get` and `%col`

**Use `%col` unless you want a copy.** Computing through the pointer is within 1% of computing
over an array you own, so there is no break-even to work out and no number of passes at which
`%get` starts winning: `%col` avoids a full copy of the column and gives up nothing measurable for
it.

`%get` is the right call when a *copy* is what you are after:

- **You want to change your values without changing the table.** `%get` hands back a detached
  array; writes through a `%col` pointer go straight into the column.
- **You want a different kind from the stored one.** `%get` widens on the way out. (If the column
  itself should change kind, [`%cast`](#changing-a-columns-type) it and keep using `%col`.)
- **You want an array that outlives the table, or survives a row-changing mutation.** A `%col`
  pointer does neither.

`tools/benchmark_table.sh`'s access mode measures both on your own machine and column size.

### Reading and writing a column's nulls alongside its values

`%get`, `%col`, `%get_slice` and `%set` all take an optional `is_valid=`, `.true.` where a value
is present:

```fortran
call t%get("flux", v, is_valid=ok)      ! values and their validity in one call
call t%set("flux", v, is_valid=ok)      ! ... and back again: .false. entries become null
```

**The mask has the same shape as the values it describes.** A scalar column's is `ok(:)`, one entry
per row; a **vector** column's is `ok(:,:)`, shaped `(width, nrows)` exactly like the values, with
one entry per *element*:

```fortran
real(real64), allocatable :: v(:,:)
logical, allocatable :: ok(:,:)

call t%get("spectrum", v, is_valid=ok)  ! ok is (width, nrows)
if (.not. ok(3, 7)) print *, "element 3 of row 7 is missing"
```

There is no one-entry-per-row form for a vector column: a vector column's nulls are per element,
and a mask that could not say which element is missing would be answering a different question.

Four things to know:

- **On `%set` it goes the other way.** A plain `%set` drops the column's nulls outright; passing
  `is_valid=` writes the values and then marks the `.false.` entries null, which is the only way to
  replace a column and its nulls in one call. A mask of the wrong shape is an error naming both.
- **From `%col` it is a snapshot, not an alias.** Validity is a packed bitmap, so there is no
  `logical` array in the column for a pointer to refer to. Writing through the value pointer
  afterwards does not update the mask you were given.
- **It is the same data as `%get_valid_mask(name, mask)`**, which is the way to ask for validity
  on its own without copying the values too — and which accepts either shape (see below).
- **A wide column's mask is not free.** `logical` is four bytes, so a `(width, nrows)` mask costs
  `4 * width * nrows` bytes — for a width-100 column of a million rows, 400 MB. Ask for it when you
  want the whole thing; use `%is_null(name, i, e)` for a few elements.

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

The third option removes the question instead of answering it: [`%cast`](#changing-a-columns-type)
converts the column to the kind your code is written for, so one branch covers every numeric
column the file might have held.

```fortran
call t%cast(name, PK_FLOAT64)   ! no-op if it is float64 already
call t%col(name, p64)           ! one path, whatever the file stored
```

`%cast` converts only between the numeric kinds, so `%kind` is still the way to find out whether a
column is numeric at all.

## What the table tells you about itself

| call | answer |
|---|---|
| `t%nrows()` | rows every column holds |
| `t%nrows_unfiltered()` | rows before `filter=`/`sample_fraction=` — the slice's length, or the file's |
| `t%row_group_extent()` | rows in the row groups this table covers, i.e. what reading it decodes |
| `t%ncols([resident_only])` | number of columns; `resident_only=.true.` counts only the ones already read |
| `call t%column_names(names [, resident_only])` | every column name, in file order; `resident_only=.true.` lists only the ones already read |
| `t%has_column(name)` | whether a column of that name exists |
| `t%kind(name)` | its `PK_*` kind (`PK_NONE` if unreadable) |
| `t%width(name)` | values per row: 1 for a scalar column, the element count for a vector one |
| `call t%unit(name, u)` | its unit string, or `""` |
| `t%residency(name)` | `RES_FULL` once read, `RES_EMPTY` before that (and for an unreadable column) |
| `t%is_supported(name)` | whether its physical type is one this library can read |
| `t%is_null(name, i)` | whether row `i` of that column is null |
| `t%has_nulls(name [, found])` | whether the column holds any null — from the file's footer if it has not been read |
| `call t%get_valid_mask(name, mask [, found])` | its per-row validity as a `logical` array, `.true.` where the row holds a value |
| `t%generation()` | a counter bumped by every structural change (see [pointers](#two-ways-to-reach-a-column)) |
| `t%is_detached()` | whether a row-changing operation has cut the table loose from its file |
| `call t%filename(f)` | the file this table was opened from, or `""` for one built in memory |
| `call t%get_file_metadata(key, value, [found])` | one key from the source file's metadata — an error on a table built in memory, which has no file to ask |
| `call t%unit(name, u)` | a column's unit, from the read-in MAML's `unit:` key |

(An argument in **square brackets** is optional — `[found]` above means `found` may be omitted.
The brackets are notation for this documentation, never something you type.)

**`found=` is available on the calls that look a column up by name**, and it turns a missing
column from a hard error into a quiet report: `%kind`, `%width`, `%unit`, `%residency`,
`%is_supported`, `%get`, `%col`, `%prefetch` and `%reload`.

```fortran
call t%get("maybe_absent", v, found=ok)
if (.not. ok) print *, "column not in this file"
```

On a reported miss `%get` leaves a zero-length array and `%col` a null pointer, so a program that
ignores `found` gets an empty result rather than stale data.

**Every procedure that takes a column name now accepts it**, mutators included — `%is_null`,
`%set`, `%set_element`, `%set_null`, `%clear_null`, `%get_slice`, `%set_slice`, `%get_element`,
`%has_nulls`, `%get_valid_mask`, `%compact_validity`, `%drop_column`, `%rename_column`,
`%copy_column` and `%cast`. On a mutating call `found=.false.` means **nothing was changed**: the
column is looked up before anything is written. `%row(i)` is the exception, since it names no
column, and `%get_file_metadata`'s `found` reports a missing **key** rather than a missing column.

On `%prefetch`'s array form, `found` is the *conjunction*: it comes back `.false.` if any name
was missing, and the names that do exist are still read. A column whose type this library cannot
read is an error for `%prefetch` and `%reload` whether or not `found=` is present — asking to read
it is a mistake rather than a miss.

## Laziness, and what it costs

A column is read on its first *value* access. That is anything that has to see the values:
`%get`, `%col`, `%set`, `%is_null`, `%get_slice`, a row handle's `%get`/`%is_null`, the cell
mutators `%set_element`/`%set_null`/`%clear_null`/`%compact_validity`, and `%copy_column`.
`parquet_write_table` counts too — it reads whatever its schema names and the table has not read
yet. `%cast` is the one that can go either way: on a column nothing has read it only records the
kind to decode into, and the read still happens later (see
[Changing a column's type](#changing-a-columns-type)). Everything else (`%nrows`, `%ncols`,
`%column_names`, `%kind`, `%width`, `%unit`, `%has_column`, `%residency`, `%is_supported`,
`%filename`) is answered from the schema and reads nothing.

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
| `call t%evict_column(name)` | release a column's values, keeping the column |

`%prefetch`'s array form is not just a loop: it reads the named columns in one pass, which
matters for **struct leaves**. The reader decodes a struct as one array shared by all its leaves,
and the table frees that array as soon as the column it was asked for is stored — so touching
`main.a`, then `main.b`, then `main.c` one at a time decodes the struct three times, while
`call t%prefetch(["main.a", "main.b", "main.c"])` decodes it once. Reading them separately is
never *wrong*, only slower.

**Naming the struct itself does the same for all of its leaves**, which saves listing them:

```fortran
call t%prefetch("main")                            ! every leaf under main., in one pass
call t%prefetch(["main.a", "main.b"])              ! ... or just these two
```

A real column of that exact name always wins over the prefix reading, so a file with a column
literally called `main` is unaffected. A name matching neither is a missing column, reported the
usual way.

**Calling either twice is well defined, and they differ.** `%prefetch` on a column that is already
resident does nothing — so prefetching a list of columns repeatedly, or prefetching one another
call has already read, costs a name lookup and no I/O. `%reload` re-reads *every* time by design:
it discards what is in the store and goes back to the file, which is the whole point of it, so two
`%reload` calls are two reads. On a column nothing has read yet the two have the same outcome, and
only then.

**`%evict_column` is `%prefetch`'s counterpart** and the way to give a column's memory back
without losing the column. An evicted column still appears in `%column_names`, still answers
`%kind`/`%width`/`%unit`, and is read again on the next touch — where `%drop_column` removes it
for good. Evicting is only allowed where the values can be read back: a column built with
`%add_column`, and any column of a [detached](#what-detaching-means) table, is an error rather
than silent data loss. Evicting a column that is not resident is a no-op.

Eviction is **user-driven only**. Nothing in this library evicts on its own — there is no LRU and
no memory budget — so what a table holds stays predictable from the calls you wrote.

`%reload` only applies to a column that came from a file: reloading one built with `%add_column`
is an error, since there is nothing to reload it from, as is reloading anything once the table has
[detached](#what-detaching-means). It re-reads into the kind the column currently has, so a
`%cast` is *not* undone by a reload — only local value edits are.

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
below is for. That extends to changing it: a thread-private table can be filtered, sorted, renamed
and dropped from inside the region, because no other thread can see it.

`%prefetch` and `%materialize_all` **read their columns on several threads by themselves**, so a
wide file is faster to bring into memory without you writing any OpenMP at all:

```fortran
call parquet_open_table(t, "wide.parquet")
call t%materialize_all()      ! reads the columns in parallel, internally
```

Each thread drives its own reader, so nothing is shared and nothing needs a lock. This happens only
when it is both safe and worth it: at least two columns to read, more than one thread available, no
read-time transform (a `filter=`/`sort=`/`qc=`/`sample_fraction=` table falls back to the ordinary
serial read, since a second reader would have to redo that work — and for an unseeded sample would
select different rows). Measured at **3.9x** on a 24-column, 900k-row file with 8 threads.

### Growing one table from several threads

`%append` into a **shared** table is safe and needs no `!$omp critical` of your own — the table
serialises it internally:

```fortran
call parquet_new_table(out)
call out%add_column("mass", empty)          ! give it its columns BEFORE the region

!$omp parallel do default(shared)
do g = 1, nchunks
    block
        type(parquet_table) :: mine, batch  ! in a block, never in private() -- see below
        call parquet_open_table(mine, "in.parquet", lo(g), hi(g))
        call mine%materialize_all()         ! this thread opened it: allowed
        ...
        call out%append(batch)              ! serialised for you
    end block
end do
!$omp end parallel do

call out%sort_by(["id"])                    ! arrival order is not deterministic
```

Three things to know:

- **The region is append-only.** No thread may *read* the shared table while any thread is appending
  to it — an append reallocates every column's storage. This is checked and aborts, though a read
  starting at the exact instant an append does may slip past, so treat it as a rule rather than a
  net.
- **Every pointer into the shared table dies at an append.** Re-fetch `%col`/`%ref` afterwards.
- **Prepare it first.** Adding a column is a structural change and is refused inside the region, so
  the destination needs its columns before the region starts.

Declare per-thread tables inside a `block`, **never** in an OpenMP `private()` clause: `parquet_table`
is finalizable, and a `private` copy of such a type is not reliably initialised — the first
finalization then frees an undefined pointer.

The full per-operation table is in
[What a `parquet_table` allows concurrently](thread-safety.html#what-a-parquet_table-allows-concurrently).

### Nulling elements from several threads

Validity storage is allocated lazily — a null-free column carries no bitmap at all, which is what
keeps it cheap. That means the *first* null on a column allocates, and two threads doing that at once
would race. The library refuses it rather than racing, and tells you the fix:

```fortran
call t%ensure_validity("flags")     ! or t%ensure_validity() for every resident column
!$omp parallel do
do i = 1, n
    if (bad(i)) call t%set_null("flags", i)
end do
!$omp end parallel do
```

`%ensure_validity` changes no value and no null state — it only decides *when* the allocation
happens. It belongs **before** the region: it allocates, so calling it from inside one on a shared
table is refused for the same reason `%set_null` is. A `date`/`time`/`timestamp` column never needs it (its null state lives in the element), and
a string column cannot be written from several threads at all.


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

When you need the width the *whole file* agrees on, open a whole-file table and ask it, before
opening any slice. That measurement scans row groups one at a time and holds no column, so it
costs a pass over the file's `LIST` column rather than the memory of it:

```fortran
call parquet_open_table(full, "from_another_tool.parquet")
w = full%width("spec")            ! measured over every row group; nothing stays resident
call parquet_open_table(part, "from_another_tool.parquet", lo, hi)
```

## Reading part of a file: the slice regime

`parquet_open_table(t, file, row_lo, row_hi)` gives the table a contiguous row range, and only the
row groups covering it are ever read. Row indices everywhere else — `%row(i)`, `%get_slice`,
`%is_null(name, i)` — then count from the slice's own first row, not the file's.

```fortran
call parquet_open_table(t, "big.parquet", 1000001_int64, 2000000_int64)
print *, t%nrows()          ! 1000000

call parquet_open_table(t, "big.parquet", lo, hi)   ! plain default INTEGERs work too
```

`row_lo`/`row_hi` take either integer kind — both `integer(int32)` or both `integer(int64)`, not
one of each — so ordinary `INTEGER` variables and literals need no `_int64` suffix.

`parquet_table_row_group_bounds(file, bounds)` reports where the natural boundaries are, without
opening a table at all — `bounds(1, rg)` and `bounds(2, rg)` are row group `rg`'s first and last
rows. It is the planning call: a thread cannot open its slice until it knows which rows to ask
for. `call t%row_group_bounds(bounds, physical=.true.)` answers the same for an open table. Without
`physical=` it answers in *that table's* own row numbering instead, which for a whole-file
unfiltered table is the same thing — see
[Which rows a row group holds](#which-rows-a-row-group-holds) once a filter or a slice is in play.

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

### A slice with a filter, a sample or qc

A slice takes the same read-time transform the whole-file form does — `filter=`, `qc=`,
`qc_soft=`, `sample_fraction=`, `sample_seed=` and `maml=` — and applies it *within* the slice.
What each argument means, and how a `maml=` file's own `extra:` lists compose with them, is
described once for both forms in
[Filtering, sorting and checking rows as the file is opened](#filtering-sorting-and-checking-rows-as-the-file-is-opened);
this section covers only what is different about a slice:

```fortran
call filt%add("mass > 1.0e10")
call parquet_open_table(t, "big.parquet", 1000001_int64, 2000000_int64, filter=filt)
print *, t%nrows()          ! however many of those million rows match -- NOT 1000000
```

Two consequences worth stating plainly:

- **The row count is no longer the slice's length.** `%nrows()` is the number of rows of
  `[row_lo, row_hi]` that survive, and every row index the table takes or reports counts those
  survivors: row 1 is the first surviving row of the slice, not file row `row_lo`. Rows outside
  the slice never appear, however well they match.
- **`%row_group_bounds(physical=.true.)` keeps answering in the file's own row numbering**, which
  is what makes it the planning call — it is how the *next* slice gets chosen, so it has to be
  available in the coordinates a slice is expressed in. Without `physical=` it answers in the
  table's own rows; see [Which rows a row group holds](#which-rows-a-row-group-holds).

Without a filter and without `sample_fraction=` nothing changes at all: the slice is trimmed out
of the covering row groups in memory, exactly as it always was, and `%nrows()` is
`row_hi - row_lo + 1`.

**There is no `sort` argument on the slice forms.** A sort reorders rows across the whole file, so
"rows 1000001 to 2000000" would no longer name the rows the caller chose — supplying one is a
compile error rather than something to discover at runtime. A `maml=` whose `extra: sort:` list is
non-empty is the same rejection, necessarily as an `error stop`. Sort the whole file instead, or
sort the slice in memory afterwards with `%sort_by`.

### Which rows a row group holds

`%row_group_bounds` answers in **this table's** row numbering by default, and in the **file's** with
`physical=.true.`:

```fortran
call t%row_group_bounds(mine)                    ! rows OF THIS TABLE, per row group
call t%row_group_bounds(theirs, physical=.true.) ! the file's own rows, per row group
```

The two differ only when the table does not hold every row of the file — a slice, a filter, a
sample. For a whole-file table with no transform they are the same array, and both match
`parquet_table_row_group_bounds`.

Which one to reach for follows from what the answer is *for*: the default relates a row index you
already have back to the row group it was read from (`mine(1, rg) .. mine(2, rg)` are rows of `%get`'s
output); `physical=.true.` is the planning form, in the coordinates `parquet_open_table`'s own
`row_lo`/`row_hi` are expressed in.

Both have **one entry per physical row group and are index-aligned**, so they can be read side by
side — row group `rg` holds file rows `theirs(:, rg)`, which are this table's rows `mine(:, rg)`. A
row group contributing no rows at all (outside the slice, or filtered away entirely) is reported as
an **empty range**, `mine(1, rg) > mine(2, rg)`, rather than dropped; dropping it would break the
alignment that makes the pairing possible.

Some tables cannot answer in their own row numbering, and say so rather than inventing an answer:
a table built in memory (it has no row groups) and a [detached](#what-detaching-means) one (its
rows no longer come from any row group). **A table opened with `sort=` is the third** — a sorted
row can come from any row group, so no range of its rows belongs to one, and the default form is an
error naming the table.

**`physical=.true.` answers for every file-backed table**, whatever transform it carries: a sort
does not move the file's own row groups, and neither does a filter or a sample. So the planning
form keeps working on a sorted table even though the default form cannot, and which transforms
happen to be present never decides whether the question can be answered.

`parquet_table_row_group_bounds(file, bounds)` is the same answer without a table at all: it opens
its own footer-only reader and is unaffected by anything a table did.

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

A handle has three procedures, and that is all:

| call | answer |
|---|---|
| `call r%get(name, value)` | one column's value in this row — scalar for a scalar column, allocatable rank-1 array for a vector one |
| `call r%set(name, value)` | writes that value into the table — a handle is a view of it, not a copy |
| `call r%ref(name, p)` | a pointer to this row's storage: zero copy, writable, exact kind |
| `r%is_null(name)` | whether this row of that column is null |
| `r%index()` | which row this is, in the table's own numbering |

`%set` is exact-kind (a write never widens) and clears that row's null, exactly as
`%set_element` does. `%ref` gives a scalar pointer for a scalar column and a pointer to the whole
vector for a vector one; the two string kinds have no `%ref`, since a packed variable-length store
has no fixed slot to point at. A `%ref` pointer carries the same lifetime rule as `%col`'s — see
[Two ways to reach a column](#two-ways-to-reach-a-column).

`%get` on a handle widens exactly as the table's own does, and triggers the same lazy first
touch, so a handle can reach a column nothing has read yet. The handle resolves the column by
name and the row by index on every access, so it survives anything that merely reallocates a
column; it is invalidated by a change to the row set, by dropping a column it reads, and by the
table going out of scope. None of those is detectable from the handle, so treat it as
short-lived: make it, use it, let it go. The index must be a row the table has, and the table it
came from does **not** need the `target` attribute.

**The handle has to be a variable — `call t%row(42)%get("mass", m)` does not compile.** That is a
constraint of Fortran itself, not a gap in this library: the leftmost part of a data reference
cannot be a function reference, so neither the call above nor `t%row(42)%index()` is legal, and no
library change can make them so. Write the two steps:

```fortran
type(parquet_table_row) :: r
r = t%row(42)
call r%get("mass", m)
```

For a single cell there is no need for a handle at all — `call t%get_element("mass", 42, m)` is
the one-call form (see [Replacing values](#replacing-values)).

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

**`%set_slice` writes a selection back**, taking the same selection object in the same order:

```fortran
call t%get_slice("mass", s, m)
m = m * 2.0_real64
call t%set_slice("mass", s, m)      ! the same rows, in the same order
```

The array must have exactly one value (or one vector) per selected row, and unlike `%get_slice` it
does **not** widen — a copy *into* the table is exact-kind, as `%set` is. It also takes
`is_valid=` (one entry per selected row) and `modify_nulls=`.

## String columns in a table

A `PK_STRING` column is reached by copy, in either of two shapes, and which one you declare picks
the specific:

```fortran
character(len=:), allocatable  :: names(:)   ! fixed width, sized to the longest value present
type(parquet_string_column)    :: packed     ! compact: offsets + data + validity

call t%get("name", names)      ! a null row comes back blank -- gate on %is_null to tell them apart
call t%get("name", packed)     ! keeps each value's own length, and its nulls with it
```

Four things follow from strings having no fixed-width storage:

- **`%col` gives a pointer to the packed store, not to an array.** `call t%col("name", sp)` with
  `type(parquet_string_column), pointer :: sp` aliases a `PK_STRING` column's own store: reading
  and in-place value edits go straight to the table. Changing how many elements it holds does not
  — the column's row count is kept separately and would stop matching.
- **A `parquet_string_column` is a first-class value in both directions.** `%get`, `%set` and
  `%add_column` all take one (`%set` and `%add_column` copy it in, so the caller's own column and
  the table's do not share storage afterwards). The `character(len=*)` forms are still there for
  when a fixed-width array is what you have.
- **A character ARRAY is trimmed on the way in; a character SCALAR is not.** Every element of a
  `character(len=*)` array shares one declared length, so a shorter value is blank-padded by
  Fortran and those blanks carry nothing you could have meant — `%add_column`, `%set` and
  `%set_slice` therefore store `"ab"` from a `character(len=32)` array as two characters, not
  thirty-two. `%set_element` and a row handle's `%set` take a *scalar*, whose length is exactly
  what you wrote, so they store it verbatim. This is why `%get` into a
  `character(len=:), allocatable` comes back sized to the longest real value rather than to the
  width you put in:

  ```fortran
  character(len=32) :: src(3) = ["ab", "cde", "f"]
  character(len=:), allocatable :: back(:)

  call t%add_column("tag", src)
  call t%get("tag", back)        ! len(back) == 3, not 32
  ```
- **`%get_slice` offers all three** — `parquet_string_column`, `character(:)` and the
  `(element, row)` rank-2 form — and a row handle's `%get` hands back a
  `character(len=:), allocatable` scalar for a `PK_STRING` column, or a rank-1 array of them for a
  `PK_STRING_VEC` one.
- **A vector string column is trimmed on read.** It has no compact read path, so it goes through
  the fixed-width reader, where trailing blanks cannot be told from padding. A value written as
  `"ok  "` comes back as `"ok"` blank-padded to the array's width, which is indistinguishable from
  the value `"ok"`:

  ```fortran
  character(len=:), allocatable :: tags(:,:)    ! (element, row)
  call t%get("tags", tags)                      ! width 3, say
  print *, "[", tags(1, 1), "]"                 ! [ok      ] -- padded to the widest value
  print *, "[", trim(tags(1, 1)), "]"           ! [ok]       -- any trailing blanks are gone
  ```

  Scalar string columns do not have this problem when read into a `parquet_string_column`, which
  keeps each value's own length; there is no such path for a vector one.

See [Compact string columns with `parquet_string_column`](string-columns.html) for the type itself.

## Columns this library cannot read

A table holds the same column types the rest of the library reads — `int32`, `int64`, `float32`,
`float64`, `logical`, `string`, `date`, `time` and `timestamp`, each as a scalar column or as a
fixed-width vector one (18 kinds in all, the `PK_*` constants `%kind` reports) — see
[Supported data types](supported-data-types.html).

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

### Units

A parquet file records no unit for a column, so a read-in MAML is where a table's units come from.
A `fields:` entry's `unit:` key gives that column its unit, and `%unit` reports it:

```yaml
# catalogue.maml
table: input_table
fields:
  - MASS_KG:
      data_type: float64
      unit: kg
```

```fortran
call parquet_open_table(t, "catalogue.parquet", maml="catalogue.maml")
call t%unit("MASS_KG", u)      ! "kg" -- and nothing has been read yet
```

Three things follow:

- **It answers before the column is read.** The unit is part of what the table knows about a
  column from the schema, like `%kind` and `%width`, not something that arrives with the values.
- **The MAML names the file's columns.** A column renamed by `extra: remap:` takes its unit under
  the *file* name it is declared with, and you ask for it under your internal name.
- **The unit travels with the values.** Once read, it is on the column itself, so `%append`'s unit
  check sees it, `%clone` keeps it, and `parquet_write_table` writes it out.

There is no unit *conversion* in this library: appending a `"km/h"` column to an `"m/s"` one is an
error rather than a silent reinterpretation.

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

Also accepted: `sample_fraction=`/`sample_seed=` for a random subset, `qc_soft=` to warn instead of aborting on a qc violation, and `use_threads=` (default `.true.`, exactly as on `parquet_open_reader` — pass `.false.` to keep one thread's reads on one thread, typically when you are already parallelizing at a coarser level; see [Thread safety](thread-safety.html#thread-pool-tuning)). All of them mean exactly what they mean on `parquet_open_reader`.

Three consequences worth stating plainly:

- **Every column inherits it, including ones read much later.** A column touched for the first time long after the open still comes back with the same rows in the same order — the transform lives on the table's reader, not on any one column.
- **`%nrows()` is the post-filter count.** With a filter or a sample active it is *not* the file's row count, and nothing on the table reports that count directly — `call t%row_group_bounds(b, physical=.true.)` gives the file's own row groups, whose last entry ends at the file's last row, and `parquet_table_row_group_bounds(file, b)` answers the same without a table at all.
- **`%clone` keeps it.** A clone reopens the file for its own lazy reads, and reattaches the identical transform — otherwise a column the source never touched would come back with rows the source had filtered away.

A **slice** takes all of this too, applied within its own row range, with `sort=` the one exception — see [A slice with a filter, a sample or qc](#a-slice-with-a-filter-a-sample-or-qc).

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
the columns the schema asks for — and on a [detached](#what-detaching-means) table, a schema field
whose column was never read is an error, since there is no longer a file to read it from.

**...and gives it back again.** By default (`release=.true.`) the table ends the write in the
residency state it started in: a column the write had to read is released once it has been
written, so writing a freshly opened 16-column table costs one column's residency rather than
sixteen. A column *you* had already read is left alone — materializing it was not the write's
doing, so undoing it is not the write's business either. Pass `release=.false.` to keep everything
the write read, which is what you want when the next thing you do is read those same columns.
Releasing frees storage a `%col` pointer could alias, so it advances
[`%generation()`](#two-ways-to-reach-a-column) — see the note there for why that is a conservative
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
- **The automatic [`parquet_row_index`](#which-row-of-the-file-is-this) column is never written**,
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

Everything [`parquet_open_writer`](writing.html#writer-options) can be told,
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
- **It works after the table has [detached](#what-detaching-means)** — which is the point, since
  read, change rows, write is exactly the shape that detaches. It also adds nothing to your own
  schema, so writing a second table with the same schema does not inherit the first one's
  provenance.

`call t%get_file_metadata(key, value, [found])` reads one key of that snapshot at any time, and
survives detaching for the same reason. To see everything a file carries, before or without a
table, use
[`parquet_get_metadata_items`](reading.html#listing-every-metadata-entry).

## Replacing values

`%set` replaces a column's values from an array of the **same length** — it changes values, never
the row set:

```fortran
call t%get("mass", m)
m = m * 2.0_real64
call t%set("mass", m)
```

By default every row is written and the column's null bitmap is dropped outright — after a plain
`%set` the column holds no nulls at all. Pass `modify_nulls=.false.` to leave the null rows, and
their bitmap, exactly as they were.

To write a single cell, use `%set_element`, and to read one, `%get_element`:

```fortran
call t%set_element("mass", 42, 1.75_real64)   ! row 42 of the mass column
call t%get_element("mass", 42, m)             ! ... and back out again
```

`%get_element` widens into your variable exactly as `%get` does, takes either integer kind for
the row index, and is the one-call form of the two-step row handle (`r = t%row(42)` then
`call r%get("mass", m)` — see [One row at a time](#one-row-at-a-time)).

The value's kind must match the column's exactly, as it does for `%set`, and writing a value
**clears that row's null** — a cell cannot be both a value and missing.

## Null values, and changing them

`%is_null`, `%set_null` and `%clear_null` each work a **row** at a time or a single **element** at
a time, depending on whether you name an element:

```fortran
call t%set_null("spectrum", 7_int64)              ! the whole row: every element of row 7
call t%set_null("spectrum", 7_int64, 3_int64)     ! just element 3 of row 7
print *, t%is_null("spectrum", 7_int64, 3_int64)  ! .true.
print *, t%is_null("spectrum", 7_int64)           ! .true. -- "any element of row 7 is null"
```

**Each call acts at the granularity you named**, and the one place that is worth stating twice is
the difference between asking and setting:

- **Naming only a row and setting** marks the row missing — every element of it.
- **Naming only a row and asking** answers about the row as a whole: `%is_null(name, i)` is `.true.`
  when *any* element of it is null. So a row you nulled one element of reports itself null, which is
  usually what you want to know before using it.

Element indices run `1..width` and are checked; a scalar column has width 1, so `e` can only be 1
and the two forms agree. The row handle takes the same pair: `r%is_null("spectrum")` and
`r%is_null("spectrum", 3_int64)`.

`%set_null` also takes a **mask**, in either shape — one `logical` per row, or one per element,
matching whichever shape `%get_valid_mask` gave you:

```fortran
call t%get_valid_mask("flux", valid)          ! valid(:)   -- per row
valid = valid .and. (flux > 0.0_real64)
call t%set_null("flux", valid)                ! nulls every row the mask marks .false.

call t%get_valid_mask("spectrum", vok)        ! vok(:,:)   -- per element, (width, nrows)
call t%set_null("spectrum", vok)              ! nulls individual elements
```

On a vector column the rank-1 `%get_valid_mask` is the **row summary** (`.false.` where any element
of the row is null) and the rank-2 one is the true per-element state. Both are useful — the first
answers "which rows are complete?" — so both are available, and which you get is decided by how you
declared the array.

It only ever *adds* nulls: a `.true.` entry leaves the entry exactly as it was, so a mask describing
only part of what you know cannot clear a null you did not mention. `%has_nulls(name)` answers
whether there are any at all — and for a column that has not been read yet it answers from the
file's footer without reading it, so it is cheap enough to ask before deciding to.

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
| **column** — `%add_column`, `%drop_column`, `%rename_column`, `%copy_column`, `%cast` | changes which columns exist, or a column's kind | no |
| **row** — `%filter_rows`, `%sort_by`, `%top_n`, `%delete_rows`, `%truncate`, `%append`, `%append_null_rows` | changes which rows exist | **yes, when it changes one** |

```fortran
call t%materialize_all()                 ! read everything you want to keep, first
call t%filter_rows(mass > 1.0e10_real64) ! keep the rows a mask selects
call t%sort_by(["mass"], descending=[.true.])
call t%drop_column("scratch")
```

The two simplest column changes are worth a sentence each, because both are cheaper than they
look:

- **`%drop_column` never reads the column it drops.** Dropping one that was never touched is the
  memory-reclaiming case and costs nothing; the remaining columns keep their order, so
  `%column_names` still reads like the file. Keeping that order costs nothing either — the
  remaining columns' storage is handed over rather than copied.
- **`%rename_column` changes only the name you look the column up by.** A file-backed column that
  has not been read yet still reads from the same physical column afterwards, which is what lets a
  rename compose with a MAML [remap](#renaming-a-files-columns-with-a-read-in-maml) in either
  order. The new name must not be blank and must not already be taken.

### Changing a column's type

`%col` hands back a typed pointer and insists on the exact stored kind, which is awkward when the
code knows what type it wants to work in but not what type the file happens to hold. `%cast`
settles that: it converts the column itself, so the `%col` call afterwards is the one the code was
written for.

```fortran
call t%cast("mass", PK_FLOAT64)   ! whatever it was stored as, it is float64 now
call t%col("mass", mass)          ! ... so this pointer is the right kind by construction
```

A column already of the requested kind is left alone, so this is safe to call unconditionally.

**The conversions allowed are exactly those the reader and the writer already perform** between
numeric kinds — see [Reading a column into a different numeric
kind](supported-data-types.html#reading-a-column-into-a-different-numeric-kind). In short:
`int32` ↔ `int64`, `float32` ↔ `float64`, and either integer kind to or from either real kind,
scalar or vector. Anything else is refused: there is no conversion between a logical, string or
temporal column and a numeric one, and none between a scalar column and a vector one (that would
change the column's width, which is a reshape rather than a conversion).

Two kinds of loss are treated differently, again matching the read path:

- **An integer that does not fit, and a real with a fractional part converted to an integer kind,
  are errors** naming the row and the value. So is a finite `float64` too large for `float32`,
  which would otherwise quietly become infinity.
- **Losing digits is silent.** `float64` → `float32`, and a large integer into a real kind, round
  the way a plain Fortran conversion would. Pass `exact=.true.` to make those an error too.

`%cast` keeps the column's nulls, its unit and its row count. It does **not** keep a pointer:

> **A pointer from `%col` does not survive a `%cast` of that column.** The conversion replaces the
> column's storage, so take the pointer again afterwards. Fortran cannot detect this.

A cast asked for **before anything has read the column** is carried out by the read itself: the
column is decoded straight into the target kind in one pass, rather than being read and then
converted. This is invisible except for one thing worth knowing — the reader performs the
conversion in that case, and the reader does not check for the precision loss described above, so
`exact=.true.` reads the column first rather than deferring.

To keep the original column as well, copy instead of casting:

```fortran
call t%copy_column("mass", "mass_f32", PK_FLOAT32)   ! adds a column, leaves "mass" alone
call t%copy_column("mass", "mass_backup")           ! no target kind: a plain copy
```

`%copy_column` with no `to_kind` copies **any** column — string, temporal and vector columns
included — and with one, converts by `%cast`'s rules. It differs in one deliberate way: `exact`
defaults to `.true.` here, because a copy is usually taken in order to keep something, so a value
that would not survive the round trip is refused rather than truncated.

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

**A call that changes no row does not detach.** Detaching costs you every column you have not read
yet, permanently, so it is only paid for when the row set actually moved — a row mutation that
turns out to have nothing to do returns without touching a column, without invalidating a `%col`
pointer, and with the file still attached. Five of the six are decided by the arguments you passed:

| call | changes nothing when |
|---|---|
| `%truncate(n)` | `n >= %nrows()` |
| `%filter_rows(keep)` | every entry of `keep` is `.true.` |
| `%delete_rows(indices)` | `indices` is empty |
| `%append(other)` | `other` has no rows (it is still checked for compatibility first) |
| `%append_null_rows(n)` | `n == 0` |
| `%sort_by(keys)` | the rows were already in that order |

The last is the odd one out: whether a sort moves anything depends on the **data**, not on the
call, so `%sort_by` on an already-ordered column leaves the table attached and the same call after
an edit may not. Treat "did it detach?" as something to ask (`%is_detached()`) rather than
predict, and never rely on staying attached across a mutation you expect to be a no-op.

### Removing rows

`%filter_rows(keep)` is the general form and takes a mask of exactly `%nrows()` entries.
`%delete_rows(indices)` and `%truncate(n)` are conveniences over it, in either integer kind:

```fortran
call t%delete_rows([3, 7, 7, 11])   ! a row named twice is still removed once
call t%truncate(1000)               ! keep the first 1000 rows
```

A row index outside `1..nrows` is an error, and so is a negative `n`; both are checked before
anything is changed, so a rejected call leaves the table as it was. `%truncate(0)` empties the
table. A `%truncate` asking to keep more rows than there are, a `%filter_rows` whose mask keeps
everything and a `%delete_rows` with no indices all return without touching anything and without
detaching — see [What "detaching" means](#what-detaching-means).

To keep rows by *rank* rather than by position or by mask — the best hundred by some column — use
[`%top_n`](#keeping-only-the-best-rows), which is cheaper than sorting and cutting.

### Sorting

`%sort_by` takes one or more key columns, primary first, with optional per-key `descending=` and
`nulls_first=` arrays:

```fortran
call t%sort_by(["group", "mass "], descending=[.false., .true.])
```

A key column that has not been read yet is read for you, by the same lazy first touch `%get` and
`%col` use — so sorting a table you have just opened needs no `%prefetch` first. Only the key
columns are read; the rest stay exactly as they were. Nulls and NaNs are placed absolutely and are
never flipped by `descending`; ascending order gives values, then NaNs, then nulls. Ties keep their
existing order.

**Any scalar column can be a key** — numeric, logical, string, date, time or timestamp. A vector
column cannot: there is no defined order on a whole vector row, so naming one is an error, as are
an empty key list and a `descending=`/`nulls_first=` array whose length does not match the keys.
Every key is validated before a single column is touched, so a rejected `%sort_by` leaves the
table exactly as it was — and a sort that finds the rows already in the order asked for moves
nothing and leaves the table attached (see [What "detaching" means](#what-detaching-means)).

This is the same sort engine `parquet_open_reader(..., sort_by=...)` uses, so sorting a table in
memory and reading the same file sorted give the identical row order.

### Keeping only the best rows

When you want the brightest hundred rows and not the other hundred million, `%sort_by` is the wrong
tool: it orders every row and reallocates every column, to keep 100 of them. `%top_n` selects
instead. Optional arguments are shown in square brackets; they are not part of the call syntax:

```fortran
call t%top_n(["flux"], 100, descending=[.true.])   ! the 100 brightest, brightest first
```

`call t%top_n(keys, n, [descending], [nulls_first])` takes the same keys, the same engine and the
same refusals as `%sort_by`, including reading a key column that has not been read yet. Afterwards
the table holds exactly those `n` rows, in key order — it is `%sort_by` followed by `%truncate(n)`,
at O(rows) instead of O(rows log rows), and gathering each column straight to `n` rows instead of
reindexing it in full and then cutting it down.

**"The last `n`" is `descending=`**, not a separate call — with the default ascending order,
`t%top_n(["flux"], 100)` keeps the hundred *faintest* rows.

**`n` is clamped, not checked.** Asking for more rows than the table has keeps all of them, so an
`n` derived from a fraction or a config value needs no `min()` of its own; a negative `n` is still
an error. At or above the row count this *is* a whole sort, and behaves as one — including leaving a
table whose rows are already in that order completely alone.

Like every row-changing operation it **detaches** the table (see
[What "detaching" means](#what-detaching-means)), so a column you still want must be read before the
call, not after:

```fortran
call t%prefetch("name")                  ! or %materialize_all()
call t%top_n(["flux"], 100, descending=[.true.])
```

That includes [`parquet_row_index`](#which-row-of-the-file-is-this), which is how to find out *which*
file rows survived — ask for it before the call and it is gathered along with everything else.

There is deliberately no `%take(indices)` taking a permutation you built yourself. Applying an
arbitrary row order to a table is the one thing this type refuses: applied to some columns and not
others, or applied from an array that has since gone stale, it breaks the row correspondence that
makes a table a table, and nothing detects it. `%top_n` is safe because it builds the indices itself,
from this table's own columns, and applies them to every column together. If you want to *read* rows
in an order without changing anything, that is `%argsort_by` and `%get_slice`, next.

### Ordering rows without reordering them

`%sort_by` **detaches** the table, which is the right thing when you meant to reorder it — but it
also means there is no way to get a sorted *view* of a table and keep the file behind it. That is
what `%argsort_by` is for: it answers with the row order and moves nothing.

```fortran
integer(int64), allocatable :: perm(:)
real(real64), allocatable :: mass(:)

call t%argsort_by(["mass"], perm)                     ! the order, unapplied
call t%get_slice("mass", parquet_slice_list(perm), mass)   ! read in that order
```

The table is untouched and still attached, so lazy columns still read, `%reload` still works, and
nothing has been reallocated. Beyond a sorted read this is also how to apply one table's order to
another table or to a plain array, which nothing else in the library offers.

Same keys, same engine and same refusals as `%sort_by`, including per-key `descending=` and
`nulls_first=`. Optional arguments are shown in square brackets below; they are not part of the
call syntax:

| call | what it gives |
|---|---|
| `call t%argsort_by(keys, perm, [descending], [nulls_first], [group_offsets], [group_nkeys])` | the row order the keys imply |
| `call t%argsort_partial(keys, perm, n, [descending], [nulls_first])` | just the `n` best rows, by selection |
| `call t%top_n(keys, n, [descending], [nulls_first])` | *(mutating)* keeps only those `n` rows — see [above](#keeping-only-the-best-rows) |
| `ok = t%is_sorted_by(keys, [descending], [nulls_first])` | whether the rows are already in that order |

`perm` may be declared `integer(int32)` or `integer(int64)`; `group_offsets` follows whichever you
chose, since its entries are positions in `perm`.

**`%argsort_partial` is the one with an asymptotic argument.** Asking a 100-million-row table for
its brightest 100 rows through `%sort_by` sorts everything and reallocates every column;
`%argsort_partial` selects instead, and reorders nothing at all. `n` is *clamped* to the row count
rather than checked, so a derived `n` needs no `min()` of its own, and "the last `n`" is
`descending=`, not a separate call.

**`%is_sorted_by` costs O(rows) with an early exit**, where asking `%sort_by` the same question
costs a full sort:

```fortran
if (.not. t%is_sorted_by(["ra"])) call t%sort_by(["ra"])
```

#### Grouping

`group_offsets=` reports where each run of rows equal under the keys begins, in the same pass as
the sort:

```fortran
integer(int64), allocatable :: perm(:), go(:)

call t%argsort_by(["field_id"], perm, group_offsets=go)
do g = 1, size(go) - 1
    call t%get_slice("mag", parquet_slice_list(perm(go(g):go(g+1) - 1)), mags)
end do
```

The array has length `ngroups + 1` and its last entry is the sentinel `nrows + 1`, so every group
slices the same way and the last one needs no special case. All nulls form one group and all NaNs
form one group.

`group_nkeys=` groups on the first few keys **without changing the sort**, which is what gives
"grouped by field, ordered by magnitude within each group":

```fortran
call t%argsort_by(["field_id", "mag     "], perm, group_offsets=go, group_nkeys=1)
```

It counts key *names*, must be between 1 and the number of keys, and requires `group_offsets`.

> **A permutation goes stale silently.** It describes the table *as it was*. Any row-structural
> change — `%sort_by`, `%top_n`, `%filter_rows`, `%delete_rows`, `%truncate`, `%append` — invalidates it, and
> nothing reports that: against a table that has since shrunk, the indices stay in range and name
> the wrong rows. This is the same hazard as a saved `%col` pointer, except that a stale pointer
> usually crashes while a stale permutation just answers wrongly. `%generation()` is bumped by every
> such change — record it beside a permutation you intend to keep, and compare before reusing it.

### Adding rows

The way to add many rows is a batch that structurally cannot have the wrong columns:

```fortran
call t%clone_structure(batch)     ! same columns, zero rows, no file
call batch%append_null_rows(n)    ! give it n rows to fill
call batch%set("id", ids)         ! ... and fill them
call batch%set("mass", masses)
call t%append(batch)
```

`%clone_structure` reads nothing: every column's kind and width are known from the file's schema,
so a batch can be cloned from a freshly opened table without touching a column. Pass
`resident_only=.true.` to clone only the columns that have been read.

`%append` requires the appended table's columns to be a subset of this table's, with matching
kinds, widths and units. A column this table has and the batch does not is **null-filled**; a
column the batch has and this table does not is an error rather than being silently dropped; a
kind mismatch is an error too, and `%cast` is the way round it. There is no unit
conversion, so appending "km/h" rows to an "m/s" column is refused.

`%append(row)` adds one row from a `parquet_table_row` handle. It is convenient but slow in bulk —
it costs a whole table's machinery per row — so prefer the batch form above for anything large.

Appending a **zero-row** table is checked for compatibility exactly as any other append, and then
does nothing at all — including not detaching. `%append_null_rows(0)` is the same.

### Copying, and going back

Mutation happens in place and there is no undo, so the way to keep a version to return to is to
copy first:

```fortran
call t%clone(before)     ! independent deep copy
call t%filter_rows(keep)
```

A clone copies the columns already read and leaves the unread ones unread, opening its own reader
on the same file so it stays lazy — with the same read-time transform reattached, so a column the
source never touched still comes back with the source's rows. Cloning a **detached** table copies
the values it holds and leaves the copy detached too: there is no file left to reopen, for either
of them. `before` must be declared as the same concrete table type as `t`.

> **A pointer from `%col` does not survive a row-changing operation.** `%filter_rows`, `%sort_by`,
> `%append` and the rest reallocate each column's storage, so a pointer taken before one of them
> points at freed memory afterwards. Fortran cannot detect this. Take the pointer again after the
> mutation.

## Looking a value up in a sorted table

There is no table-level binary search, and it is not missing: `%col` hands back a plain pointer to a
column's storage, and [`parquet_sorting`](sorting.html)'s searches work on that directly.

```fortran
real(real64), pointer :: ra(:)
integer(int64) :: lo, hi
logical :: ok

call t%sort_by(["ra"])
call t%col("ra", ra)

call pf_is_sorted(ra, ok)                 ! O(n), once
do i = 1, size(targets)
    call pf_equal_range(ra, targets(i), lo, hi, assume_sorted=ok)
    ! rows lo .. hi-1 hold targets(i)
end do
```

**Check once, outside the loop.** `assume_sorted` defaults to `.false.`, which is the safe default —
searching unsorted input returns a plausible index with no symptom — but it makes each search O(n)
in front of an O(log n) operation. Hoisting one `pf_is_sorted` out of the loop turns *m* searches
from O(m·n) into O(n + m log n).

**Re-take the pointer after any row-changing operation**, per the warning above, and re-check
sortedness after anything that could disturb the order. A table does *not* remember that it is
sorted, deliberately: a stale "still sorted" flag would return wrong rows silently.

## Ranking rows by a column

Likewise there is no `%rank` — adding a rank or percentile column is three lines through `%col`:

```fortran
real(real64), pointer :: flux(:)
integer(int64), allocatable :: ranks(:)

call t%col("flux", flux)
call pf_rank(flux, ranks, descending=.true.)   ! 1 = brightest
call t%add_column("flux_rank", ranks)
```

`method=` picks how ties are handled — `"competition"` (1,2,2,4, the default), `"dense"` (1,2,2,3)
or `"ordinal"` (1,2,3,4) — and **a null gets rank 0**, since a missing value has no rank rather than
the last one. Dense ranks over a sorted key column are also the cheapest way to label groups: every
row of a group gets the same number.

## Which row of the file is this?

A file-backed table has one column it did not read from the file: **`parquet_row_index`**, holding
each row's 1-based physical row number in the source parquet file.

```fortran
use parquet_tables, only: PARQUET_ROW_INDEX
integer(int64), allocatable :: src(:)

call parquet_open_table(t, "big.parquet", filter=filt)
call t%get(PARQUET_ROW_INDEX, src)     ! which file rows survived the filter, in order
```

It answers for every regime, and each is a different question:

| the table is | `parquet_row_index` holds |
|---|---|
| the whole file, untransformed | `1, 2, 3, ...` |
| an unfiltered slice | the slice's own file rows — `row_lo` onwards, not `1..%nrows()` |
| filtered or sampled | the file rows that survived, in file order |
| sorted | the file rows, in the sorted order |

The last two are the reason it exists: which rows survived and what order they ended up in lives
inside the reader and is not otherwise visible.

Four things to know:

- **It is virtual until you ask for it.** It costs 8 bytes a row — 8 GB at a billion rows — so it
  is not built at open. `%has_column(PARQUET_ROW_INDEX)` answers `.true.` before the first use,
  while `%ncols`/`%column_names` do not list it; asking for it once gives it a real slot, listed
  last. That is the one place those two queries disagree, and deliberately: `%has_column` answers
  "can I use this name?", `%column_names` lists what the table is holding. Any of `%get`, `%col`
  or `%prefetch(PARQUET_ROW_INDEX)` counts as asking. `%materialize_all` does **not** — it reads
  the columns the table has, so a table that never asked for the row index does not acquire one
  from a bulk read.
- **Materialize it before changing the row set.** It names rows of a file, so a
  [detached](#what-detaching-means) table cannot produce it any more — asking then is an error
  saying so. Ask for it *first* and it becomes an ordinary column: `%sort_by` reorders it with
  everything else, and it still says where each row came from.
- **A file column of that name is unreachable.** The reserved name belongs to the automatic
  column, so a file that has its own `parquet_row_index` gets a warning at open and that column is
  dropped from the table. A read-in MAML's [`extra: remap:`](#renaming-a-files-columns-with-a-read-in-maml)
  is how to reach it, by giving it another internal name.
- **A table built in memory has no such column** — it was not read from anywhere.

`parquet_get_physical_row_indices(reader, rows)` is the same answer at the reader level, if you
are not using a table.

## Seeing what a table holds

`call t%print_stat()` prints one line per **materialized** column to standard output — its kind
and unit, width, null count and min/max — under a header saying how many of the table's columns
those are:

```
parquet_table: catalogue.parquet
  rows: 6   columns: 18 (3 materialized)
  column  kind                width   nulls       min                     max
  s_i32   PK_INT32            1       0           1                       6
  s_f64   PK_FLOAT64 [Msun]   1       1           1.75000                 10.5000
  s_str   PK_STRING           1       0           a                       p
```

`all=.true.` lists every column, with `-` where one that has not been read has nothing to report.

Four things worth knowing:

- **Printing never reads anything.** A deferred plain-`LIST` column prints as `pending` rather
  than being measured — a diagnostic that changes what it is diagnosing is worse than one that
  admits it does not know.
- **A `logical` column reports `T:<n>`/`F:<n>`** instead of an ordering, and a **vector** column's
  statistic is over all of its elements, flattened. An all-null column has no min or max and
  prints `-`.
- **These are statistics of what is in memory**, computed by a plain scan here — not the file's
  own footer statistics, and the only ones available for a column built with `%add_column`, which
  has no footer at all.
- **It costs a pass over every column it prints**, so it is a diagnostic, not something to put in
  a loop.

## Checking qc without keeping the columns

qc declared with `qc=` or by a read-in MAML is enforced when a column is *read*, so on a lazy
table a bound is only checked once something asks for that column. `call t%validate_qc()` checks
them all without leaving them in memory:

```fortran
call parquet_open_table(t, "catalogue.parquet", qc=qc)
call t%validate_qc()      ! reads each qc-declaring column, checks it, releases it again
```

What it leaves behind is the point: residency is recorded before anything is read, and only the
columns *this call* made resident are released afterwards — a column the program had already read
stays exactly as it was. A violation is reported the way it would be on an ordinary read (an
abort, or a warning under `qc_soft=`), and a table with no qc declared is a no-op.

## Extending `parquet_table` with your own type

`parquet_table` is designed to be extended, and two public entry points exist for that. Most
programs meet them through a [generated table type](generated-tables.html), which is the intended
way in — but both are ordinary API and work just as well on a type you write by hand.

**`clone_extra` is the hook `%clone` calls for an extension's own components.** `%clone` and
`%clone_structure` copy everything `parquet_table` itself holds, and cannot know about components
an extending type added; overriding this hook is how those come across. Both call it as their last
action, dispatching on the source's dynamic type, so an override runs wherever either is used:

```fortran
type, extends(parquet_table) :: my_table
    real(real64) :: zeropoint = 0.0_real64
contains
    procedure :: clone_extra => my_clone_extra
end type my_table
...
subroutine my_clone_extra(self, out, structure_only)
    class(my_table), intent(in) :: self
    class(parquet_table), intent(inout) :: out
    logical, intent(in) :: structure_only   ! .true. when called from %clone_structure
    select type (out)
    class is (my_table)                     ! `class is`, so a further extension still gets this
        out%zeropoint = self%zeropoint
    end select
end subroutine my_clone_extra
```

`%clone` has already checked that source and destination have the same dynamic type, so the
guarded branch always matches. **Without an override, an added component arrives
default-initialized and nothing reports it** — which is why the generator writes these assignments
for you.

A concrete-typed override of `%clone` itself is not possible: an overriding procedure has to keep
every dummy argument's characteristics, so `out` cannot be narrowed from `class(parquet_table)`.
This hook is the supported substitute, and it keeps one name for one operation.

**`%bind_predefined` is what a generated type's `%init` calls.** It takes a column's declared name,
kind, width and whether it comes from the file, then checks each one against the file, converts it
to the declared kind, reads them all in one pass, and marks the slot *predefined* — which is what
makes `%drop_column` refuse it without `force=.true.`. It is public because a generated module is
a different module and `parquet_table`'s components are private; hand-written code that opened a
table with `parquet_open_table` already reaches every column by name and rarely needs it. See
[Generated table types](generated-tables.html) for the full contract.

**Opening an extension** goes through the parent component:
`call parquet_open_table(t%parquet_table, filename)`. `parquet_open_table`'s dummy is
non-polymorphic on purpose, so an extension that needs a binding step cannot be opened without it.
`parquet_write_table`, by contrast, takes `class(parquet_table)` and accepts an extending type
directly.

## Current limitations

This is still a deliberately narrow version of the table layer.

- **A column is all-or-nothing.** Reading one row of a column reads the whole column (across the
  table's row scope). Use the [slice regime](#reading-part-of-a-file-the-slice-regime) to bound
  how much that is.
- **A slice cannot be opened with `sort=`.** Every other read-time transform applies to a slice as
  it does to a whole file (see [A slice with a filter, a sample or
  qc](#a-slice-with-a-filter-a-sample-or-qc)); a sort reorders rows across the whole file, so it is
  refused there and has to be done in memory afterwards with `%sort_by`.
- **`b = a` is a hard error**, not a silent alias — the column store lives behind a pointer, and
  a shallow copy would leave two tables sharing (and double-freeing) one store. Use `%clone`.
- **A table is not thread-safe to mutate.** Reading resident columns from several threads is
  fine, and each thread may open and read its own table; every mutation is a single-threaded
  operation.
- **`%cast` and `%copy_column` convert only between the numeric kinds** (`%copy_column` with no
  target kind copies any kind at all). Logical, string and temporal columns have no conversion,
  and a conversion never changes a column's width.
- **`%append` null-fills a missing column; a per-column default value is not available.** Fill
  the batch explicitly if you want something other than nulls.
- **A parquet file records no unit for a column.** `%unit` reports what a read-in MAML's `unit:`
  key declared (see [Units](#units)) or what `%add_column(unit=)` stored, and `""` otherwise.
  There is no unit *conversion* anywhere in this library.
- **Vector string columns are trimmed.** A rank-2 string column has no compact read path, so it
  goes through the fixed-width reader, where trailing blanks cannot be told from padding.
- **String columns are copy-only.** There is no `%col` pointer form for them, and `%set`/
  `%add_column` take a character array rather than a `parquet_string_column` — see [String columns
  in a table](#string-columns-in-a-table).

## Memory

The table keeps its own Fortran copy of each column it reads and releases the reader's decoded
Arrow buffers as it goes, so a fully loaded table holds roughly one copy of the data rather than
two — and a table whose columns were never touched holds nothing at all.
`tools/benchmark_table.sh` measures this directly (see CONTRIBUTING.md); note that resident set
size does **not** show it, because Arrow's memory pool keeps freed pages rather than returning
them to the operating system.
