---
title: Whole tables in memory: the basics
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
what the library's [API stability](../../index.html#important-behavior) promise covers.

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
  [`parquet_write_table`](table-write.html#writer-options) advances the counter when it gives back a column it had
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
| `t%column_index(name [, found])` | its 1-based position among the table's columns, or 0 when absent |
| `call t%column_name(j, nm [, found])` | the name of the column at 1-based position `j` |
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

### Asking about a column by position instead of by name

**`%kind`, `%width`, `%unit`, `%residency`, `%is_supported`, `%has_nulls` and `%is_null` also
accept a 1-based column position** wherever they accept a name, so a sweep over every column does
not have to copy a name out just to ask about it:

```fortran
do j = 1, t%ncols()
    call t%column_name(j, nm)
    print *, nm, parquet_kind_name(t%kind(j)), t%width(j)
end do
```

`%column_index(name)` and `%column_name(j, nm)` are inverses, and a position is only meaningful
against the table's **current** width: `%drop_column`, `%add_column` and the other column-set
changes renumber the slots, so re-derive positions after one rather than holding them across it.
An out-of-range position is an error, and reports through `found=` exactly as a missing name does.

These are metadata queries — they do **not** read a column's values, with the one documented
exception `%kind`/`%width` already carry (a plain `LIST` column from a non-Arrow writer, whose
width is a property of the data). Positions address the table's own columns, so they are
unrelated to the element index within a vector column.

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
`%add_column`, and any column of a [detached](table-mutate.html#what-detaching-means) table, is an error rather
than silent data loss. Evicting a column that is not resident is a no-op.

Eviction is **user-driven only**. Nothing in this library evicts on its own — there is no LRU and
no memory budget — so what a table holds stays predictable from the calls you wrote.

The one place a table holds slightly more than its rows need is after appending: storage grows
geometrically, so an appended-to table can carry up to 1.5x its rows' worth until
[`%compact()`](table-mutate.html#adding-rows) releases it. A table read from a file, or one that has only been
filtered or sorted, is allocated exactly to size.

`%reload` only applies to a column that came from a file: reloading one built with `%add_column`
is an error, since there is nothing to reload it from, as is reloading anything once the table has
[detached](table-mutate.html#what-detaching-means). It re-reads into the kind the column currently has, so a
`%cast` is *not* undone by a reload — only local value edits are.

> `t%prefetch` and the reader-level `parquet_prefetch_columns` are different things. The table's
> reads a column into the table's own store; the reader's warms Arrow's side of the read.

Reading a not-yet-resident column, growing a table, and nulling elements **from several
threads** each have their own rules — see
[What a `parquet_table` allows concurrently](../operating/thread-safety.html#what-a-parquet_table-allows-concurrently)
for the enforcement table and the worked patterns for all three.

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

What a row handle can do:

| call | answer |
|---|---|
| `call r%get(name, value)` | one column's value in this row — scalar for a scalar column, allocatable rank-1 array for a vector one |
| `call r%set(name, value)` | writes that value into the table — a handle is a view of it, not a copy |
| `call r%get(c, value)` / `call r%set(c, value)` | the same, with the column named by a [column handle](#a-column-handle) instead of a string — no name lookup |
| `call r%ref(name, p)` | a pointer to this row's storage: zero copy, writable, exact kind |
| `r%is_null(name [, e])` | whether this row of that column is null, or element `e` of it |
| `r%index()` | which row this is, in the table's own numbering |
| `r%is_valid()` | whether the handle is still usable — see below |

`%set` is exact-kind (a write never widens) and clears that row's null, exactly as
`%set_element` does. `%ref` gives a scalar pointer for a scalar column and a pointer to the whole
vector for a vector one; the two string kinds have no `%ref`, since a packed variable-length store
has no fixed slot to point at. A `%ref` pointer carries the same lifetime rule as `%col`'s — see
[Two ways to reach a column](#two-ways-to-reach-a-column).

`%get` on a handle widens exactly as the table's own does, and triggers the same lazy first
touch, so a handle can reach a column nothing has read yet. The index must be a row the table has,
and the table it came from does **not** need the `target` attribute.

**A row handle does not survive a structural change, and says so.** It stamps the table's
`%generation()` when it is made and refuses once the two differ, so a handle left over from before
a `%sort_by`, a `%filter_rows`, an `%append`, a dropped column or a `%compact` aborts with a
message naming the remedy rather than quietly reading whatever now sits at that index. `%is_valid()`
is the non-aborting way to ask:

```fortran
r = t%row(42)
call t%sort_by("mass")
if (.not. r%is_valid()) r = t%row(42)   ! re-fetching costs one call
```

The rule is deliberately conservative — some mutations a particular handle could have survived are
refused anyway — because one total rule is easier to rely on than a list of exceptions. Handles are
cheap, so the shape to reach for is re-fetching inside the loop rather than working out which
changes are safe.

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
the one-call form (see [Replacing values](table-mutate.html#replacing-values)).

## A column handle

`t%column("mass", c)` gives a handle on one **column** — the mirror image of `t%row(i)`. A row
handle fixes the row and names the column on every access; a column handle fixes the column and
names the row:

```fortran
type(parquet_table_col) :: c
real(real64) :: m
call t%column("mass", c)
do i = 1, t%nrows()
    call c%get(i, m)
    ...
end do
```

**The point is what it removes.** `%get_element(name, i, v)` looks the column up by name on every
call, and that lookup is **52–77% of what the call costs** — measured on four toolchains across
three machines. A handle resolves it once. How much the loop gains depends on the compiler: a
resolve-once loop measured between **1.5x and 4x** the name form's throughput on those same four
toolchains.

`t%column(j, c)` takes a 1-based position instead of a name, which is what makes a
`do j = 1, t%ncols()` loop work. Both forms take `[found]`, and report a missing name or an
out-of-range position the same way every other lookup does.

What a column handle can do — brackets mark optional arguments:

| call | answer |
|---|---|
| `call c%get(i, value)` | row `i`'s value, widening exactly as `%get_element` does |
| `call c%set(i, value)` | writes it; exact kind, and the write clears that row's null |
| `call c%get(i, e, value)` / `call c%set(i, e, value)` | **one element** of row `i` of a vector column, with no array allocated |
| `c%is_null(i [, e])` | whether that row — or element `e` of it — is null |
| `call c%set_null(i [, e])` / `call c%clear_null(i [, e])` | mark or unmark it |
| `call c%ref(p [, is_valid])` | the same pointer `%col` gives, without the lookup |
| `c%index()`, `c%kind()`, `c%width()`, `c%residency()` | this column's position, kind, values per row, residency |
| `call c%name(nm)`, `call c%unit(u)` | its name and unit string |
| `c%is_valid()` | whether the handle is still usable |

**`c%get(i, e, value)` is new capability, not a faster spelling.** There has never been a way to
read one element of a vector row without materialising the whole row — `%get_element` on a vector
column allocates a width-long array on every call. This reads the one element.

A handle is a **view**: `%set` through it changes the table, and the change is visible through the
name form immediately. `%ref` hands back exactly the pointer `%col` does, with exactly the same
rules and the same warning — see [Two ways to reach a column](#two-ways-to-reach-a-column).

**A column handle does not survive a structural change**, on the same terms the row handle does:
it stamps `%generation()` when it is made, refuses once they differ, and `%is_valid()` asks without
aborting. Dropping a column renumbers the slots above it, so a handle taken beforehand would
otherwise name a *different* column — valid values, wrong answer, no diagnostic. That is the case
the stamp exists for.

### Choosing between the three ways to read a cell

| you want | use | why |
|---|---|---|
| the fastest possible loop; kind known at compile time, no widening | `%col` — and hoist the pointer out of the loop | a plain array read, no call at all |
| a guarded per-cell loop: kind decided at run time, a string column, widening, or validity-aware access | a column handle | one resolve for the whole loop, and every guard still runs |
| one cell, or a few | `%get_element(name, i, v)` | nothing to hoist, nothing to keep valid |

Two traps, both of which only appear at scale:

- **Hoist the handle out of the loop; never re-fetch it inside one.** Making a handle costs about
  what one `c%get` costs, so re-fetching per cell doubles the loop and gives back everything the
  handle won. If the loop *changes* the table's structure, do not use a handle at all — use the
  name form, which resolves afresh each time.
- **`t%column(j, c)` READS column `j`.** Making a handle resolves the column, which triggers the
  same lazy first touch any value access does. A `do j = 1, t%ncols()` loop that builds a handle
  just to print `%name()` and `%kind()` therefore reads the whole file. For a metadata sweep use
  the by-position queries instead — `t%kind(j)`, `t%width(j)`, `t%column_name(j, nm)` — which
  answer from the descriptor and read nothing. See
  [Asking about a column by position](#asking-about-a-column-by-position-instead-of-by-name).

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

See [Compact string columns with `parquet_string_column`](../types/string-columns.html) for the type itself.

## Changing a column's type

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
kind](../types/supported-data-types.html#reading-a-column-into-a-different-numeric-kind). In short:
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

## Current limitations

This is still a deliberately narrow version of the table layer.

- **A column is all-or-nothing.** Reading one row of a column reads the whole column (across the
  table's row scope). Use the [slice regime](table-open.html#reading-part-of-a-file-the-slice-regime) to bound
  how much that is.
- **A slice cannot be opened with `sort=`.** Every other read-time transform applies to a slice as
  it does to a whole file (see [A slice with a filter, a sample or
  qc](table-open.html#a-slice-with-a-filter-a-sample-or-qc)); a sort reorders rows across the whole file, so it is
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
  key declared (see [Units](table-open.html#units)) or what `%add_column(unit=)` stored, and `""` otherwise.
  There is no unit *conversion* anywhere in this library.
- **Vector string columns are trimmed.** A rank-2 string column has no compact read path, so it
  goes through the fixed-width reader, where trailing blanks cannot be told from padding.
- **String columns are copy-only.** There is no `%col` pointer form for them, and `%set`/
  `%add_column` take a character array rather than a `parquet_string_column` — see [String columns
  in a table](#string-columns-in-a-table).

One topic a reader may expect on this page lives with the generator instead:
[Extending `parquet_table` with your own type](../utilities/generated-tables.html#extending-parquet_table-with-your-own-type)
— a generated table type is the automated version of exactly that.

## Memory

The table keeps its own Fortran copy of each column it reads and releases the reader's decoded
Arrow buffers as it goes, so a fully loaded table holds roughly one copy of the data rather than
two — and a table whose columns were never touched holds nothing at all.
`tools/benchmark_table.sh` measures this directly (see CONTRIBUTING.md); note that resident set
size does **not** show it, because Arrow's memory pool keeps freed pages rather than returning
them to the operating system.
