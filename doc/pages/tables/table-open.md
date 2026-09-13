---
title: Opening a table: slices, filters and renaming
---

`parquet_open_table(t, filename)` has options beyond the plain open: reading only part of the
file (the slice regime), filtering, sorting and checking rows as they are read, reading a filtered
file larger than memory one row group at a time, and renaming the file's columns through a read-in
MAML. This page collects them, together with what the table can and cannot read. Working with the
open table itself is covered in [Whole tables in memory: the basics](table.html).

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

**The range must be inside the file, and it is not clamped.** `row_lo` below 1, `row_hi` past the
file's last row, or `row_lo` above `row_hi` is an `error stop` naming the range asked for and the
row count the file actually has. A loop cutting its own chunks therefore has to bound the last
one (`hi = min(k*chunk, n)`) rather than let it overrun.

`parquet_table_row_group_bounds(file, bounds)` reports where the natural boundaries are, without
opening a table at all — `bounds(1, rg)` and `bounds(2, rg)` are row group `rg`'s first and last
rows. It is the planning call: a thread cannot open its slice until it knows which rows to ask
for. `call t%row_group_bounds(bounds, physical=.true.)` answers the same for an open table. Without
`physical=` it answers in *that table's* own row numbering instead, which for a whole-file
unfiltered table is the same thing — see
[Which rows a row group holds](#which-rows-a-row-group-holds) once a filter or a slice is in play.

Putting the two together gives the per-thread pattern, where each thread reads only its own
share of the file — a complete program, since this is the one on the page most worth copying
exactly:

```fortran
program per_thread_slices
    use parquet
    use iso_fortran_env, only: int64, real64
    implicit none

    integer(int64), allocatable :: bounds(:,:)
    real(real64) :: total
    integer :: rg

    call parquet_table_row_group_bounds("big.parquet", bounds)

    total = 0.0_real64
    !$omp parallel do reduction(+:total)
    do rg = 1, size(bounds, 2)
        block
            type(parquet_table) :: mine        ! see the note below -- NOT private(mine)
            real(real64), allocatable :: x(:)
            call parquet_open_table(mine, "big.parquet", bounds(1, rg), bounds(2, rg))
            call mine%get("mass", x)
            total = total + sum(x)
        end block
    end do
    !$omp end parallel do

    print *, "total mass:", total
end program per_thread_slices
```

Every row of the file is covered exactly once, because the row groups partition `1..nrows` — which
is what makes the slices safe to hand out without any further bookkeeping.

> **Declare a per-thread table in a `block`, not with `private(t)`.** An OpenMP `private` copy of
> a finalizable derived type is not reliably default-initialized by gfortran, so opening into one
> runs the finalizer over an undefined pointer and the program dies in the allocator. A
> block-local is initialized on entry and finalized at exit, which is what a per-thread table
> wants anyway.

A slice need not line up with row-group boundaries — the covering groups are read and trimmed.
It cannot be cheaper than one row group, though, since a row group is the smallest unit the
format lets a reader decode. `%row_group_extent()` is that cost in rows: how many rows the
covering groups hold, against `%nrows()`'s how many the table keeps.

Beyond that floor, a slice costs about its share: each column is allocated once at the slice's
own row count and each covering row group is written straight into its place, so reading a
quarter of a file costs roughly a quarter of reading all of it, however many row groups the
slice spans.

### A slice with a filter, a sample or qc

A slice takes the same read-time transform the whole-file form does — `filter=`, `qc=`,
`qc_soft=`, `sample_fraction=`, `sample_seed=` and `maml=`, plus `use_threads=` — and applies it
*within* the slice.
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
of the covering row groups in memory, and `%nrows()` is `row_hi - row_lo + 1`.

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
a table built in memory (it has no row groups) and a
[detached](table-mutate.html#what-detaching-means) one (its rows no longer come from any row
group). Neither can answer with `physical=.true.` either — one never had a file and the other has
left its own behind, so there is nothing to answer *about*. **A table opened with `sort=` is the
one that differs between the two forms** — a sorted row can come from any row group, so no range
of its rows belongs to one, and the default form is an error naming the table.

**`physical=.true.` answers for every table that still has its file**, whatever transform it
carries: a sort does not move the file's own row groups, and neither does a filter or a sample.
So the planning form keeps working on a sorted table even though the default form cannot, and
which transforms happen to be present never decides whether the question can be answered.

`parquet_table_row_group_bounds(file, bounds)` is the same answer without a table at all: it opens
its own footer-only reader and is unaffected by anything a table did.

## Filtering, sorting and checking rows as the file is opened

`parquet_open_table` takes the same read-time transform `parquet_open_reader` does, applied once
as the file is opened, so the table simply *is* the filtered/sorted/sampled result — there is no
separate step and nothing downstream has to know a transform was applied:

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

Also accepted: `sample_fraction=`/`sample_seed=` for a random subset, `qc_soft=` to warn instead
of aborting on a qc violation, and `use_threads=` (default `.true.`, exactly as on
`parquet_open_reader` — pass `.false.` to keep one thread's reads on one thread, typically when
you are already parallelizing at a coarser level; see
[Thread safety](../operating/thread-safety.html#thread-pool-tuning)). All of them mean exactly
what they mean on `parquet_open_reader`.

Three consequences worth stating plainly:

- **Every column inherits it, including ones read much later.** A column touched for the first
  time long after the open still comes back with the same rows in the same order — the transform
  lives on the table's reader, not on any one column.
- **`%nrows()` is the post-filter count.** With a filter or a sample active it is *not* the file's
  row count. `%nrows_unfiltered()` is the count before the transform — the file's rows for a
  whole-file table, the slice's own length for a slice — and it is captured at open, so it keeps
  answering after a detach.
- **`%clone` keeps it.** A clone reopens the file for its own lazy reads, and reattaches the
  identical transform — otherwise a column the source never touched would come back with rows the
  source had filtered away. This holds for `sample_fraction=` **with no `sample_seed=`** as well:
  the table settles a seed once, when it is opened, so a clone samples the same rows rather than
  drawing its own. Two separate `parquet_open_table` calls with no seed still draw independently
  of each other, exactly as `parquet_open_reader` does — what a table guarantees is that *it* has
  one sample, not that an unseeded sample is reproducible across runs.

A **slice** takes all of this too, applied within its own row range, with `sort=` the one
exception — see [A slice with a filter, a sample or qc](#a-slice-with-a-filter-a-sample-or-qc).

### Reading a file larger than memory: `bounded=`

A filter can cut a huge file down to a handful of rows, but by default getting there is not cheap
in memory: installing the filter decodes every column the filter names, over the whole file, in
one pass; and each column you then read is decoded whole and filtered afterwards. Both are
proportional to the **file**, however few rows survive. On a file that fits in memory that is the
right trade, and it is what `parquet_open_table` keeps doing.

`bounded=.true.` asks for the other one:

```fortran
type(parquet_filter) :: filt
type(parquet_table)  :: t

call filt%add("quality > 0.9")
call parquet_open_table(t, "huge.parquet", filter=filt, bounded=.true.)
call t%materialize_all()
```

The filter is evaluated one row group at a time, and every column is assembled from per-row-group
chunks, so the peak is one row group's worth of one column instead of one whole column.

**It is opt-in, and it is not free.** Nothing is cached and the filter is evaluated per row group,
so a filter column you go on to read is read a second time, and on a file that fits in memory it is
never faster than the default and is often slower. The library cannot know how much memory your
program has, so it never chooses this for you.

**What is bounded, and what is not.** The parts that scale with the file rather than with the
answer are worth knowing before you rely on this:

- **Bounded:** the filter install, and each column's assembly — one row group at a time. The
  internally-parallel read gives each thread its own row group, so the peak there is that many at
  once;
  [`parquet_set_prefetch_threads(n)`](../operating/settings.html#threads-for-reading-a-table)
  caps it.
- **Not bounded:** the row mask itself. One bit per row of every row group the row-group
  statistics could not rule out, held for as long as the table's reader is open — around a
  hundred megabytes at a billion rows, and roughly eight times that transiently while the filter is
  being installed. That is the floor of the whole approach, and it grows with the file's row count
  rather than with the number of rows that survive.
- **Not bounded:** one byte per row of the file for each `in`/`not_in` clause, while the filter is
  being installed — the same shape and size as the mask `sample_fraction=` builds, and released
  once the filter is in place. The set's own distinct keys are held for as long as the filter is.
- **Not bounded:** a variable-length `LIST` column from a foreign writer whose width has to be
  measured from the data. Under a filter that measurement still reads the column whole. Open with
  `list_columns="container"` to skip it — see
  [`list_columns=`: keep a `LIST` column as a list](#list_columns-keep-a-list-column-as-a-list).


#### Restricting one file by another file's results

The one thing a `bounded=` read cannot express with the verbs is *"keep the rows whose ID appears
in this other table"*. Every in-memory route — `pf_in` then `%filter_rows`, or a semi `%join` —
needs the key column resident in full and then **detaches** the table, which is exactly the memory
cost `bounded=` exists to avoid. It has to be a *filter*, so that the cut happens while the second
file is read:

```fortran
type(parquet_table)  :: t1, t2
type(parquet_filter) :: f1, f2
integer(int64), allocatable :: ids(:)
logical, allocatable :: ok(:)

! File 1: an aggressive cut, one row group at a time.
call f1%add("snr > 10 and mag_g < 23.5 and quality == 0")
call parquet_open_table(t1, "survey_a.parquet", filter=f1, bounded=.true.)
call t1%get("ID", ids, is_valid=ok)          ! the survivors' IDs, assembled per row group

! File 2: only the rows whose ID is in that set, also one row group at a time.
call f2%add_in("ID", ids, is_valid=ok)       ! bound as an ARRAY, never as text
call f2%add("flag == 0")                     ! composes with ordinary rules as usual
call parquet_open_table(t2, "survey_b.parquet", filter=f2, bounded=.true.)

call t2%get("mag_r", mag_r)                  ! every column now holds just the matching rows
```

Both files stay bounded. The membership test is evaluated before any of the second file's data
columns are read, so the row-group screen knows exactly which row groups hold a member — and on a
file clustered or sorted by `ID`, most of it is skipped without being read at all. See
[Membership in a set](../io/filter-sort-sample.html#membership-in-a-set-in-and-not_in) for the
null and NaN rules, the element families accepted, and what the clause costs.

**`sort=` is refused with it.** A sort reorders rows across the whole file, so a sorted row
belongs to no row group and no column can be assembled one row group at a time. Open without it,
then sort the table you have:

```fortran
call parquet_open_table(t, "huge.parquet", filter=filt, bounded=.true.)
call t%materialize_all()
call t%sort_by(["mass"], descending=[.true.])
```

**Everything else is unaffected.** `sample_fraction=`, `qc=`, `list_columns=`, a
read-in MAML's remapping, `parquet_row_index`, `%clone` (which stays bounded), `%reload` and the
row-structural mutations all mean exactly what they mean without it. One difference is worth
naming: a `qc=` bound declared on a column the filter *names* is checked when that column is
read, not at the open. That is the same rule every other column already follows — see
[qc is checked on first touch, not at open](#qc-is-checked-on-first-touch-not-at-open) — but under
the default engine such a column happens to be checked at the open as a side effect of being
cached, so a bound violated only there is reported later under `bounded=` than without it. Call
`%validate_qc()` if you want every declared bound checked up front; it is chunked too.

On the **slice** forms `bounded=` is accepted and does nothing: a slice already reads only the row
groups it covers, and assembles each column from them one at a time. Passing it is harmless, so
one caller can hand the same arguments to either form.

If your filter happens to match rows that sit together — a column the file is already ordered by,
say — the default engine may already be cheap, because whole row groups are ruled out from the
file's own statistics before anything is read. `bounded=` is for the case where the survivors are
scattered and nothing can be ruled out.

### Column names: yours, not the file's

**`filter=`, `sort=` and `qc=` name columns the way `%col`/`%get` do — in the table's own internal
names.** For a column renamed by a read-in MAML's
[`extra: remap:`](#renaming-a-files-columns-with-a-read-in-maml), that is *not* what the file
calls it, and the translation is done for you:

```fortran
! the MAML says:  remap:  - mass: MASS_KG
call filt%add("mass > 1.0e12")          ! your name, not MASS_KG
call parquet_open_table(t, "catalogue.parquet", maml="read.maml", filter=filt)
```

A **read-in MAML's own** `extra: filter:`, `extra: sort:` and `fields: qc:` are the other way
round: they name the **file's** columns, because a read-in MAML describes the physical file and
travels with it. See
[the MAML format](../schema/maml-format.html#filtering-and-sorting-on-read-with-extra-filter-and-extra-sort)
for that grammar.

When both sources are given, they compose:

| | rule |
|---|---|
| **filter** | AND — the same way two `%add` calls on one filter already combine. Order does not matter. |
| **sort** | the MAML's keys first (so they are the primary ones), then yours as tie-breakers. Order *does* matter here. |
| **qc** | per **column**: if the MAML declares `qc:` for a column at all, the MAML wins in full for that column and your entry for it is dropped. See [Quality control](../schema/quality-control.html#the-merge-rule-per-column-not-per-bound). |

### qc is checked on first touch, not at open

Opening a table reads no column data, so a `qc=` bound is enforced when the column is actually
read — on its first `%get`/`%col`, or at `%prefetch`/`%materialize_all`. This is the lazy table's
normal behaviour applied to qc, not a weaker guarantee, but it has one consequence to be aware of:
**a violated bound on a column the program never touches is never reported.** Call
`%materialize_all` if you want every declared bound checked up front, or `%validate_qc` — next —
if you want them checked without keeping the columns.

### Checking qc without keeping the columns

`call t%validate_qc()` checks every declared bound without leaving the columns in memory:

```fortran
call parquet_open_table(t, "catalogue.parquet", qc=qc)
call t%validate_qc()      ! reads each qc-declaring column, checks it, releases it again
```

What it leaves behind is the point: residency is recorded before anything is read, and only the
columns *this call* made resident are released afterwards — a column the program had already read
stays exactly as it was. A violation is reported the way it would be on an ordinary read (an
abort, or a warning under `qc_soft=`).

It is a no-op rather than an error whenever there is nothing to check: a table with no qc
declared, one built in memory, and one that has been detached from its file. So it can be called
unconditionally on any table.

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
[the MAML format](../schema/maml-format.html#renaming-columns-for-reading-with-extra-remap) for the grammar
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

Six things to know:

- **It is virtual until you ask for it.** It costs 8 bytes a row — 8 GB at a billion rows — so it
  is not built at open. `%has_column(PARQUET_ROW_INDEX)` answers `.true.` before the first use,
  while `%ncols`/`%column_names` do not list it; asking for it once gives it a real slot, listed
  last. That is the one place those two queries disagree, and deliberately: `%has_column` answers
  "can I use this name?", `%column_names` lists what the table is holding. Any of `%get`, `%col`
  or `%prefetch(PARQUET_ROW_INDEX)` counts as asking. `%materialize_all` does **not** — it reads
  the columns the table has, so a table that never asked for the row index does not acquire one
  from a bulk read. Building it is **one-shot**: `%drop_column(PARQUET_ROW_INDEX)` removes it for
  good, the next `%get` does not rebuild it, and `%has_column` stops answering `.true.` — exactly
  as for any other dropped column. Reopen the file to get it back.
- **Materialize it before changing the row set.** It names rows of a file, so a
  [detached](table-mutate.html#what-detaching-means) table cannot produce it any more — asking
  then is an error saying so. Ask for it *first* and it becomes an ordinary column: `%sort_by`
  reorders it with everything else, and it still says where each row came from.
- **A file column of that name is unreachable.** The reserved name belongs to the automatic
  column, so a file that has its own `parquet_row_index` gets a warning at open and that column is
  dropped from the table. A read-in MAML's [`extra: remap:`](#renaming-a-files-columns-with-a-read-in-maml)
  is how to reach it, by giving it another internal name.
- **Ask for it before a parallel region, on a table you share.** The first request builds the
  column, which adds a slot and moves every other column's descriptor — the one thing a shared
  table refuses inside a region, exactly as `%add_column` is refused. Asking once beforehand
  (`%get`, `%col` or `%prefetch`) makes it an ordinary resident column, readable from any number
  of threads thereafter. A table *this* thread opened inside the region is thread-private and is
  not affected.
- **A table built in memory has no such column** — it was not read from anywhere.
- **To put it in an output file, use `row_index_name=`** rather than writing this column:
  `parquet_write_table(t, "out.parquet", row_index_name="src_row")` writes the same row numbers
  under a name of your choosing, whether or not you materialized them
  ([details](table-write.html#writing-the-source-files-row-numbers-row_index_name)). Writing them
  under the reserved name itself is accepted but warned about, since a file column called
  `parquet_row_index` is dropped when this library reopens it, as above.

`parquet_get_physical_row_indices(reader, rows)` is the same answer at the reader level, if you
are not using a table.

## What a table can and cannot read

Three answers a reader needs before trusting `%column_names`: which column types a
table cannot read at all, the one type whose readability depends on its data rather
than its schema, and how a struct's fields are addressed.

### Columns this library cannot read

A table holds the same column types the rest of the library reads — `int32`, `int64`, `float32`,
`float64`, `logical`, `string`, `date`, `time` and `timestamp`, each as a scalar column or as a
fixed-width vector one (18 kinds, to which the container kinds below add `PK_LIST`, `PK_MAP` and
`PK_STRUCT` — together, the 21 `PK_*` constants a column this library can read reports; `PK_NONE`
below is the twenty-second) — see
[Supported data types](../types/supported-data-types.html).

A parquet file may contain a column whose physical type this library cannot read at all — an
`INTERVAL`/duration or a binary column, or a `LIST` whose own elements are a list, a map or a
struct. **Such a column does not stop the file from opening.** It gets a slot, so it still appears
in `%column_names` and `%has_column`, but it holds no values: `%is_supported` reports `.false.`,
`%kind` reports `PK_NONE`, and any attempt to read its values is an error naming the column.

That way one exotic column never makes an otherwise-usable file unopenable.

**A foreign numeric type is not in that group.** A column stored as a decimal, as an unsigned or
narrow (`int8`/`int16`) integer, or as a `half_float` reads normally: the table classifies it as
the narrowest Fortran kind that holds its values, exactly as `parquet_read_column` does, so a
`uint32` column arrives as `int64` and a decimal one as `float64`. The full mapping is
[What a column is read as](../io/reading.html#what-a-column-is-read-as). Nor is a plain `LIST`
column, whose elements are of a type the library reads — see
[A variable-length `LIST` column's width is discovered, not declared](#a-variable-length-list-columns-width-is-discovered-not-declared)
next; that one is readable exactly when its rows agree on a width.

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
  other read: on a table the region shares, they are refused with an `error stop` telling you to
  resolve the width first, rather than racing. Do it before the region — `%kind`, `%width`,
  `%prefetch` or `%materialize_all` all settle it. Every other column, scalar or
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

### `list_columns=`: keep a `LIST` column as a list

The measurement above is what an unqualified open does, and it is the right default for a file
whose `LIST` columns really are uniform — the column arrives as an ordinary 2-D one and every
existing accessor works on it. When that is not what you want, `list_columns=` says so at the open:

```fortran
call parquet_open_table(t, "from_another_tool.parquet", list_columns="container")
```

- **`"auto"`** — the default. A `LIST` column whose rows all hold the same number of elements
  becomes a vector column of that width; a ragged one is read one row at a time. The width is
  measured, so it depends on the data and on which rows the table covers.
- **`"container"`** — a `LIST` column whose elements are one of the nine element types becomes a
  [`parquet_list_column`](../types/list-columns.html), decided from the schema alone. Nothing is
  measured, nothing is data-dependent, and no `%kind` or `%width` call reads anything. A `LIST` of
  lists, maps or structs is not a table column under either token — see
  [Columns this library cannot read](#columns-this-library-cannot-read).

**Choose `"container"` when the column is genuinely a list to your program**, or when you need two
tables over one file to agree: under `"auto"` a slice covering a uniform stretch of a ragged file
reports a vector column while the whole file reports a scalar one, and both are answering honestly
about the rows they hold. It also removes the "`%kind` counts as a read" caveat above entirely.

It governs the plain-`LIST` case and nothing else. A `fixed_size_list` column — which is what every
column this library writes is — carries its width in the schema and is a vector column under both
tokens, so turning `"container"` on cannot reshape your own files.

### Map columns

A `MAP` column becomes a [`parquet_map_column`](../types/map-columns.html), with no argument needed
and nothing measured: a map is a container in the file's schema, so there is no width to discover
and no policy to choose. Its keys must be strings and its values one of the
[nine element types](../types/supported-data-types.html); a map that fails either test is reported
as an unreadable column, exactly like any other type this library cannot read, and does not stop the
file opening.

```fortran
call parquet_open_table(t, "with_maps.parquet")
if (t%kind("tags") == PK_MAP) then
    call t%col("tags", m)                      ! m is a type(parquet_map_column), pointer
    print *, m%length(1_int64), " entries in row 1"
end if
```

A `STRUCT` column is different again, and the difference is in the file's own addressing rather
than here: a table lists a struct's LEAVES under their dotted paths
(`nested.vals`, and see below), never the struct itself, so a table opened from a file never holds
a `parquet_struct_column`. Build one with
[`%add_column`](table.html#container-columns-in-a-table) when you want a struct column in a table.

### Nested struct columns

A struct field's leaves are addressable by their dotted paths, exactly as in
[`parquet_read_column`](../io/reading.html):

```fortran
call t%get("main.inner.deep.value", v)
```

The bare struct name (`main`) is *not* a column — it is not readable on its own, so it never
appears in `%column_names`.
