---
title: Changing a table
---

An open table can be changed in memory: values replaced, nulls edited, columns added, dropped
or renamed, rows filtered, sorted, ranked or appended. This page is the reference for those
operations and the rules they share. The basics of working with a table are in
[Whole tables in memory: the basics](table.html).

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

`%set` takes two more optional arguments, both shared with the rest of the table API rather than
special to it: `is_valid=` carries validity alongside the values, shaped like them — per row for a
scalar column, `(width, nrows)` for a vector one — and is applied *after* the values, so it survives
the bitmap drop above; and `found=` reports a missing column instead of aborting. Both are described
once, for every call that takes them, in [Reading and writing a column's nulls alongside its
values](table.html#reading-and-writing-a-columns-nulls-alongside-its-values).

To write a single cell, use `%set_element`, and to read one, `%get_element`:

```fortran
call t%set_element("mass", 42, 1.75_real64)   ! row 42 of the mass column
call t%get_element("mass", 42, m)             ! ... and back out again
```

`%get_element` widens into your variable exactly as `%get` does, takes either integer kind for
the row index, and is the one-call form of the two-step row handle (`r = t%row(42)` then
`call r%get("mass", m)` — see [One row at a time](table.html#one-row-at-a-time)).

The value's kind must match the column's exactly, as it does for `%set`, and writing a value
**clears that row's null** — a cell cannot be both a value and missing.

## Null values, and changing them

### Filling in what is missing

```fortran
call t%fillna(names, value)      ! every null in those columns takes `value`
call t%ffill(names, [limit])     ! ... or the previous non-null row's value
call t%bfill(names, [limit])     ! ... or the next one's
```

Optional arguments are shown in square brackets here and everywhere below; they are not part of
the call syntax.

A null is two things at once: a value nobody wrote, and a flag saying so. **Filling one means
writing the value *and* clearing the flag**, and doing only the first leaves a row that reads back
as your sentinel and as Null at the same time — and is written to a Parquet file as Null, sentinel
and all. That is why these three verbs exist rather than a loop of `%set_element`: `%clear_null`
takes one row at a time and cannot be given a mask, so there was no bulk way to say "this column
has no missing values any more".

`%fillna` takes one value for a list of columns, and converts it per column: an integer widens into
a wider integer column or into a real one, and a real is **refused** for an integer column, naming
that column so a mixed list is debuggable. The logical, character and three temporal values each
fill their own family and nothing else. On a vector column every null **element** takes the value.
A [container column](table.html#container-columns-in-a-table) is refused — there is no meaning to
replacing a missing list with a scalar.

```fortran
call t%fillna("flux_g, flux_r, flux_i", -999.0_real64)   ! a name list, one sentinel
call t%fillna(["survey", "note"], "")                    ! strings get an empty one
print *, t%has_nulls("flux_g")                           ! .false.
```

`%ffill` and `%bfill` carry a neighbouring row's value instead, which needs no conversion and works
for every kind including string and temporal. A **leading** run of nulls has nothing before it and
stays null under `%ffill`; a **trailing** run stays null under `%bfill`. `limit` caps how many
consecutive nulls one value may fill, so in a run of five with `limit=2` three of them survive.
On a vector column each element position is its own series.

**None of the three changes the row set**, so none detaches the table and none invalidates a
pointer taken with `%col` — the values are written where they already were, and a pointer taken
before the call sees them afterwards. A column with no nulls is left exactly as it was. A named
column that is not resident is read first, the same lazy touch `%get` performs.

Every named column is checked before any is written, so a call that names one incompatible column
fills nothing rather than stopping half way.

### Setting and clearing nulls by hand

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
only part of what you know cannot clear a null you did not mention.

`%has_nulls(name)` answers whether a column holds any, as cheaply as its state allows — a column
already read answers from its own validity, and one that has not been read answers from the **file's
footer statistics** without reading a byte, which is what makes it worth asking before deciding to
read. **The footer answer is one-sided**: `.false.` is a guarantee, `.true.` means "may hold nulls",
because a file written without statistics cannot say. So `.true.` is a reason to read the column and
look, not a fact to branch on — and reading it may well turn the answer into `.false.`. Once the
column is resident the answer is exact. `%has_nulls` also takes a 1-based column position in place
of a name.

A column that holds no null at all carries no null bitmap, which is why `%compact_validity(name)`
exists: it drops the bitmap of a column that once had nulls and no longer does. A whole-column
`%set` already does this on its own, so `%compact_validity` is only for a column edited cell by
cell.

`%ensure_validity([name])` is the other side of that laziness. Because the bitmap is allocated on
demand, the **first** null written to a null-free column allocates — which is fine on one thread and
a race on several, so a shared table refuses it. Calling `%ensure_validity` first makes the
allocation happen up front and lets the concurrent nulling proceed; with no `name` it prepares every
resident column. A temporal column never needs it, since its null state lives inside each element
and nulling one allocates nothing. A [container column](table.html#container-columns-in-a-table)
**does** need it — its nullness lives inside the container, but the container allocates lazily like
everything else — so do not read the temporal exception as covering it. See [Thread
safety](../operating/thread-safety.html) for where this matters.

**On a container column every one of these acts on the whole ROW**, because a list, map or struct
row is a variable-length object and the table's own descriptor gives it a width of 1. So
`%is_null(name, i)`, `%set_null(name, i)`, `%clear_null(name, i)` and the rank-1 mask form all mean
what they say; the element spellings — `%is_null(name, i, e)`, `%set_null(name, i, e)`,
`%clear_null(name, i, e)`, the rank-2 `(width, nrows)` mask and both column-handle forms — are
accepted and mean exactly the same thing, since `e` can only be 1. To ask about one element *inside*
a row, reach the container itself through `%col`.

## Changing a table

Mutation falls into three classes, and the difference between them matters more than any
individual procedure:

| class | what it does | detaches? |
|---|---|---|
| **cell** — `%set_element`, `%set_null`, `%clear_null` | changes values in place | no |
| **column** — `%add_column`, `%drop_column`, `%drop_columns`, `%keep_columns`, `%rename_column`, `%copy_column`, `%cast` | changes which columns exist, or a column's kind | no |
| **cell** — `%fillna`, `%ffill`, `%bfill` | writes over the nulls of whole columns, in place | no |
| **cell** — `%set_matrix` | writes several whole columns from one `(column, row)` array, in place | no |
| **column** — `%parse_column`, `%format_column` | replaces a string column's values with numbers, or a column's values with text | no |
| **row** — `%filter_rows`, `%sort_by`, `%top_n`, `%delete_rows`, `%truncate`, `%append`, `%append_null_rows`, `%dropna` | changes which rows exist | **yes, when it changes one** |
| **read** — `%row_mask` | reports which rows a rule selects, changing nothing | no |
| **read** — `%get_matrix` | copies several whole columns out as one `(column, row)` array | no |
| **row** — `%join` | matches another table's rows against this one's and brings its columns over | **yes, unless every row survives once and in place** |

```fortran
call t%materialize("mass,age,zphot")     ! read what you want to keep, first
call t%filter_rows(mass > 1.0e10_real64) ! keep the rows a mask selects
call t%filter_rows("mass > 1.0e10")      ! or say it as a rule, the reader's own grammar
call t%sort_by("-mass")                  ! or sort_by(["mass"], descending=[.true.])
call t%drop_column("scratch")
```

`%join` has a page of its own — see [Joining two tables](table-join.html) — because it is the one
row-changing operation that takes a second table, and most of what there is to say about it is
about which of *that* table's columns come across. It is also the one that may not change the row
set at all: a join in which every row of this table survives exactly once, in place, has only added
columns, so it keeps its file and everything that follows from having one — see [When the join
detaches](table-join.html#when-the-join-detaches-and-when-it-does-not).

Four of them are explained on the neighbouring page rather than here, because all four are really
about a column's *type* rather than about mutating a table: `%cast` converts a column to another
numeric kind in place, `%copy_column` adds a converted (or plain) copy beside the original, and
`%parse_column`/`%format_column` are the string ↔ number pair `%cast` refuses — see [Changing a
column's type](table.html#changing-a-columns-type). The rest are here.

**A [container column](table.html#container-columns-in-a-table) goes through all of this like any
other**, with two exceptions: it may not be a sort key (see [Sorting](#sorting) below), and `%cast`
refuses it, since there is no conversion between a list, a map or a struct and anything else.
`%copy_column` with no target kind copies one, `%clone` carries one, and every row-changing
operation carries it alongside every other column so the rows stay aligned.

The two simplest of those are worth a sentence each, because both are cheaper than they look:

- **`%drop_column` never reads the column it drops.** Dropping one that was never touched is the
  memory-reclaiming case and costs nothing; the remaining columns keep their order, so
  `%column_names` still reads like the file. Keeping that order costs nothing either — the
  remaining columns' storage is handed over rather than copied.
- **`%rename_column` changes only the name you look the column up by.** A file-backed column that
  has not been read yet still reads from the same physical column afterwards, which is what lets a
  rename compose with a MAML [remap](table-open.html#renaming-a-files-columns-with-a-read-in-maml)
  in either order. The new name must not be blank and must not already be taken.

### Dropping and keeping several columns

`%drop_column` removes one column by name. Two list forms sit beside it, and both are as cheap as
the singular one — a column that was never read costs nothing to drop, so cutting a 300-column
lazy table down to the four you want reads nothing at all. Optional arguments are shown in square
brackets; they are not part of the call syntax:

```fortran
call t%drop_columns(names, [force], [ignore_missing])   ! remove the ones named
call t%keep_columns(names, [force])                     ! remove everything BUT the ones named
```

`names` is an array of names or one comma/semicolon-separated string, as everywhere else in this
API. The columns that survive keep the **table's** order, not the order they happen to be named
in, so `%column_names` still reads like the file.

```fortran
call t%drop_columns("scratch, tmp_a, tmp_b")   ! three at once
call t%keep_columns("ra, dec, zphot, mass")    ! the projection df[["ra", "dec", ...]]
```

**Every name is checked before anything is dropped**, and the default aborts naming *every* name
that is missing rather than the first — the same rule `%require_columns` follows, so a list with
two typos is fixed in one pass rather than two. `ignore_missing=.true.` on `%drop_columns` skips
the names that are not there. `%keep_columns` deliberately has no such option: a projection that
silently keeps fewer columns than it was asked for is one nothing downstream can check, and the
same list handed to `%drop_columns` says what it is discarding instead.

`force=.true.` is needed to drop a **predefined** column — one bound by a
[generated table type](../utilities/generated-tables.html)'s `%init` — exactly as it is for `%drop_column`, and a
program using those accessors expects them to be there. `%keep_columns` reaches the same rule from
the other side, by *not* naming one, which is the case `%drop_column` cannot express at all.

Neither verb detaches: every remaining column keeps its length, so a table that has not read a
column yet can still read it afterwards. What both do invalidate is every outstanding `%col`
pointer and column handle, because a column that shifted down is a different column at that
position — `%generation()` advances to say so, and only when something was actually dropped. A
call that removes nothing leaves it untouched.

### What "detaching" means

A table opened from a file reads its columns lazily. Once a **row**-changing operation runs, the
rows in memory no longer line up with the rows in the file, so any column that had not been read
by then can never be read at all. The table records this — `%is_detached()` reports it — and every
later attempt to read from the file says so rather than returning misaligned data.

**So read what you need before changing the row set**, with `%materialize(names)` (or its
synonym `%prefetch(names)`) — naming the columns, not `%materialize_all()`, unless you really do
want the whole file. Columns you deliberately do not want are simply left behind, which is what
keeps a lazy table from having to read a whole file before it can drop a single row.

Detaching does not freeze a table: it can still be read, edited and written out, and mutated
further. What it loses is the file behind it.

**A call that changes no row does not detach.** Detaching costs you every column you have not read
yet, permanently, so it is only paid for when the row set actually moved — a row mutation that
turns out to have nothing to do returns without touching a column, without invalidating a `%col`
pointer, and with the file still attached. Five of the seven are decided by the arguments you
passed:

| call | changes nothing when |
|---|---|
| `%truncate(n)` | `n >= %nrows()` |
| `%filter_rows(keep)` | every entry of `keep` is `.true.` |
| `%delete_rows(indices)` | `indices` is empty |
| `%dropna([names], [min_valid], [how])` | no row falls short in the named columns |
| `%append(other)` | `other` has no rows (it is still checked for compatibility first) |
| `%append_null_rows(n)` | `n == 0` |
| `%sort_by(keys)` | the rows were already in that order |
| [`%join(other, on)`](table-join.html) | every row of this table matched exactly once, and in place |

`%dropna`, `%sort_by` and `%join` are the odd ones out: whether a drop finds anything to drop,
whether a sort moves anything, and whether a join leaves every row where it was all depend on the
**data** rather than on the call — so `%sort_by` on an already-ordered column leaves the table
attached and the same call after an edit may not, and a join that kept its file yesterday may
detach today on a wider `other`. Treat "did it detach?" as something to ask (`%is_detached()`)
rather than predict, and never rely on staying attached across a mutation you expect to be a
no-op.

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

### Removing rows by a filter expression

`%filter_rows` also takes a **rule**, in the same grammar a read-time
[`filter=`](../io/filter-sort-sample.html) uses, and applies it to the rows already in memory:

```fortran
call t%filter_rows("n_obs >= 8 and score > 3")
call t%filter_rows('survey == "wide" or mag_r < 19.5')
call t%filter_rows("field_id in (3, 5, 9)")
```

A rule selects the same rows here as it would at the reader. That is not a coincidence: both run
the same parser and the same per-clause rules, so the null rule (a Null row is excluded unless the
clause is `is_null`), the NaN rule (a NaN is a value, so it survives `/=` but no ordering
comparison), the string ordering and every message a bad rule raises are one implementation rather
than two. Anything the read-time grammar accepts is accepted here.

**A column the rule names is READ if it is not resident yet**, by the same lazy first touch a
`%get` performs — so a rule may name a column nothing has looked at. That also means this reads
the table's *data*: on a table that has already detached, a rule naming an unread column fails
with the detach message rather than quietly selecting nothing.

A `@name` clause needs the set bound to it, so pass the `parquet_filter` itself for that:

```fortran
type(parquet_filter) :: filt
call filt%bind("wanted", target_ids)
call filt%add("field_id in @wanted")
call t%filter_rows(filt)
```

### Removing the rows that are missing something

`%dropna` is the row-set half of the fill family: it drops the rows that are null in the columns
you name.

```fortran
call t%dropna(["flux", "err"])              ! drop a row null in EITHER (the default, "any")
call t%dropna("flux, err", how="all")       ! ... only when null in BOTH
call t%dropna("g, r, i, z", min_valid=3)    ! ... keep a row with at least three values
call t%dropna()                             ! every RESIDENT column
```

`how="any"` is the default. `min_valid` is pandas' `thresh` under a name that says what it counts,
and it **replaces** `how` rather than refining it, so passing both is refused; it must lie between
0 and the number of columns named, since a threshold no row could meet would empty the table and is
a mistake rather than a request. On a vector column a row counts as null when any of its elements
is; on a container column, when the row itself is. Naming a column READS it, the same lazy touch
`%get` performs.

**Pass `how` and `min_valid` by keyword.** A bare string in first position is a name list
everywhere in this API, so `t%dropna("all")` drops the rows that are null in the column called
`all` — which is what it says, and not what someone reaching for `how="all"` meant.

**With no names it looks at the RESIDENT columns**, not at every column. On a lazy table "every
column" would mean reading the whole file merely to decide which rows to drop, which is not what
`t%dropna()` asks for; naming a column is the explicit way to read one. A table nothing has read
yet therefore drops nothing — call `%prefetch` or `%materialize_all` first, or name the columns.
That is the same rule [`%join`](table-join.html) follows.

`%dropna` is row-structural, so it [detaches](#what-detaching-means) — but only when it actually
drops a row, since it goes through the same path `%filter_rows` does. **If the rows you want gone
were never worth reading**, say so at the reader instead: `parquet_open_reader(..., filter="flux
is_not_null")` never reads them at all, and the table stays attached.

### Asking which rows a rule selects, without removing any

`%row_mask` is the non-mutating half. It fills a mask of exactly `%nrows()` entries and changes
nothing — no row is dropped, no pointer is invalidated and the table keeps its file:

```fortran
logical, allocatable :: keep(:)
allocate(keep(t%nrows()))
call t%row_mask("n_obs >= 8 and score > 3", keep)
print *, count(keep), "rows would survive"
```

That is what to reach for before deciding whether to filter at all, and it is how a selection the
grammar cannot express is combined with one it can:

```fortran
call t%row_mask("n_obs >= 8", keep)
keep = keep .and. my_own_test(...)      ! anything the grammar has no words for
call t%filter_rows(keep)                ! the mask form, from further up this section
```

`%row_mask` takes a `parquet_filter` too, on the same terms as `%filter_rows` above.

### Sorting

`%sort_by` takes one or more key columns, primary first, with optional per-key `descending=` and
`nulls_first=` arrays:

```fortran
call t%sort_by(["group", "mass "], descending=[.false., .true.])
```

**Or write the keys as one string**, which is usually what you want — commas and/or semicolons
separate them, and a key may carry its own direction in exactly the grammar a read-time
`parquet_sortkey` key uses (`asc`/`desc`, case-insensitive, with a leading `-` as shorthand for
descending):

```fortran
call t%sort_by("group,-mass")            ! group ascending, mass descending
call t%sort_by("group asc; mass desc")   ! the same thing, longhand
```

The array form needs every key padded to one declared length — note the `"mass "` above — and
guessing that length too short **silently truncates** a key name rather than failing. The string
form has neither problem.

Every key-taking binding accepts both spellings: `%sort_by`, `%top_n`, `%argsort_by`,
`%argsort_partial` and `%is_sorted_by`. A direction token and a `descending=` argument say the
same thing twice, so giving **both** is an error for the whole call — use one or the other.
`nulls_first=` is unaffected and works with either spelling. A column whose own name contains a
comma, a semicolon, a leading `-` or a trailing ` asc`/` desc` is reachable through the array form
only.

A key column that has not been read yet is read for you, by the same lazy first touch `%get` and
`%col` use — so sorting a table you have just opened needs no `%prefetch` first. Only the key
columns are read; the rest stay exactly as they were. Nulls and NaNs are placed absolutely and are
never flipped by `descending`; ascending order gives values, then NaNs, then nulls. Ties keep their
existing order.

**Any scalar column can be a key** — numeric, logical, string, date, time or timestamp. A vector
column cannot: there is no defined order on a whole vector row, so naming one is an error, as are
an empty key list and a `descending=`/`nulls_first=` array whose length does not match the keys.
A [container column](table.html#container-columns-in-a-table) cannot either, and for the same
reason — sort by a scalar column and the list, map or struct is carried along with it.
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
call t%materialize("name")               ! or %materialize_all()
call t%top_n("-flux", 100)
```

That includes [`parquet_row_index`](table-open.html#which-row-of-the-file-is-this), which is how to
find out *which* file rows survived — ask for it before the call and it is gathered along with
everything else.

There is deliberately no `%take(indices)` taking a permutation you built yourself. Applying an
arbitrary row order to a table is the one thing this type refuses: applied to some columns and not
others, or applied from an array that has since gone stale, it breaks the row correspondence that
makes a table a table, and nothing detects it. `%top_n` is safe because it builds the indices
itself, from this table's own columns, and applies them to every column together. If you want to
*read* rows in an order without changing anything, that is `%argsort_by` and `%get_slice`, next.

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

`perm` may be declared `integer(int32)` or `integer(int64)` on **both** `%argsort_by` and
`%argsort_partial`, and `group_offsets` follows whichever you chose, since its entries are positions
in `perm`.

**`n` is the exception to this library's usual two-kind rule**, on `%top_n` and `%argsort_partial`
alike: it is a plain default-kind `integer` with no `int64` form, deliberately, because an `n` that
large is not a top-N at all but a whole sort — which is what both delegate to. Row counts and row
indices elsewhere on this page (`%truncate`, `%delete_rows`) do take either kind.

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
slices the same way and the last one needs no special case. It is **always allocated when asked
for** — an empty table gives `[1]`, so the loop above runs zero times rather than touching an
unallocated array. All nulls form one group and all NaNs form one group.

`group_nkeys=` groups on the first few keys **without changing the sort**, which is what gives
"grouped by field, ordered by magnitude within each group":

```fortran
call t%argsort_by(["field_id", "mag     "], perm, group_offsets=go, group_nkeys=1)
```

It counts key *names*, must be between 1 and the number of keys, and requires `group_offsets`.

> **A permutation goes stale silently.** It describes the table *as it was*. Any row-structural
> change — `%sort_by`, `%top_n`, `%filter_rows`, `%delete_rows`, `%truncate`, `%append` —
> invalidates it, and nothing reports that: against a table that has since shrunk, the indices stay
> in range and name the wrong rows. This is the same hazard as a saved `%col` pointer, except that a
> stale pointer usually crashes while a stale permutation just answers wrongly. `%generation()` is
> bumped by every such change — record it beside a permutation you intend to keep, and compare
> before reusing it.

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

`%append(row)` adds one row from a `parquet_table_row` handle, copying just that row out of the
handle's table. The batch form above is still cheaper per row — a row append takes the table's lock,
advances the generation counter and detaches once per call — but the cost is linear in the rows
added, not quadratic, so building a table a row at a time is a perfectly reasonable thing to do.

Growing a table row by row costs nothing extra in allocation either: a column's storage grows
**geometrically**, so a run of appends reallocates a handful of times rather than once per row. The
price is that an appended-to table can hold up to 1.5x the storage its rows need. Two calls control
that, and neither changes the row set, so neither detaches:

```fortran
call t%reserve(1000000)      ! one allocation up front, if the final size is known
do i = 1, 1000000
    call t%append(rows%row(i))
end do
call t%compact()             ! give the slack back
```

A row-structural mutation rewrites every column, and may do so on several threads;
[`parquet_set_table_threads(n)`](../operating/settings.html#threads-for-mutating-a-table) caps how
many it uses.

**Both invalidate every `%col` pointer and row handle into the table**, because both reallocate
storage without changing the row set — as do `%cast`, `%evict_column` and `%reload`, which replace
or release one column's storage while the rows stay as they were. `%generation()` advances only
when something actually moved, so the usual take-it-before / compare-it-after / re-fetch pattern
re-fetches only when a pointer really did die.

`%compact()` is a **no-op on a table that has not been appended to**: reading a column from a file
allocates exactly what it holds, and so does every rebuild (`%filter_rows`, `%sort_by`, `%top_n`,
`%delete_rows`, `%truncate`), which hand their memory back on their own. So it is safe to call
unconditionally. It is *not* needed before `parquet_write_table` — the writer never sees the slack.
`%reserve(n)` takes the total row count to make room for, not an increment, and reserving less than
the table already holds does nothing.

Appending a **zero-row** table is checked for compatibility exactly as any other append, and then
does nothing at all — including not detaching. `%append_null_rows(0)` is the same.

### Making room for columns, and the one guarantee that comes with it

`%reserve_columns(n)` is the column-side counterpart of `%reserve(n)`, and unlike everything else
on this page it exists for **safety** rather than speed:

> **While spare column capacity remains, adding a column under a NEW name relocates no existing
> column's storage, moves no existing column's slot position, and does not advance
> `%generation()`.** A pointer taken from `%col`, a `parquet_table_col` handle and a
> `parquet_table_row` handle all stay valid across such a call.

That is what makes the commonest derived-column idiom safe rather than merely lucky:

```fortran
call t%reserve_columns(t%ncols() + 2)   ! two derived columns coming

call t%col("mag_g", g)                  ! pointers taken up front...
call t%col("mag_r", r)
call t%add_column("g_minus_r", g - r)   ! ...and still valid here, by contract
call t%add_column("is_blue", g - r < 0.5_real64)
```

Without the reservation, an `%add_column` that happens to fill the slot array reallocates it, and
Fortran leaves a pointer's association status **undefined** across the `MOVE_ALLOC` that does it —
code that usually works and is not permitted to.

Three things the guarantee deliberately does not cover:

- **`force=.true.`** — replacing an existing column frees that column's storage, which no amount
  of spare capacity prevents. The guarantee is about *adding*.
- **Row-changing operations** — unchanged; see the note at the end of this page.
- **Growing the reservation itself.** `%reserve_columns` *is* a relocation when it actually grows,
  so it invalidates everything and advances `%generation()`, exactly as `%reserve` (rows) does.
  **Reserve first, take pointers second.**

`n` is the total capacity to make room for, not an increment, so reserving less than is already
allocated does nothing. `%column_capacity()` reports how many slots exist and
`%column_capacity(free=.true.)` how many are spare — capacity only ever grows, and a reservation
survives `%clone` and `%clone_structure`.

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
column's storage, and [`parquet_sorting`](../utilities/sorting.html)'s searches work on that
directly.

```fortran
real(real64), pointer :: ra(:)
integer(int64) :: first(size(targets)), last(size(targets))

call t%sort_by(["ra"])
call t%col("ra", ra)

call pf_equal_range(ra, targets, first, last)   ! every target, one pass over the column
! rows first(i) .. last(i) hold targets(i) -- INCLUSIVE
```

**`last` is inclusive, and an absent target comes back as `last == first - 1`** — so the count is
`last(i) - first(i) + 1` and the test for "did I find anything" is `last(i) >= first(i)`, not
`last(i) > first(i)`. Reading `ra(first(i))` without checking that count first reads a row that does
not match.

**Pass every target in one call rather than looping.** Each single-target search rebuilds the whole
column's internal sort key before it can search, so a loop of them is `O(m·n)` — `assume_sorted=`
skips the sortedness scan and not that rebuild. The array form extracts once and is `O(n + m log n)`;
see [Searching a sorted array](../utilities/sorting.html#many-targets-pass-them-all-at-once).

**Re-take the pointer after any row-changing operation**, per the warning above, and re-check
sortedness after anything that could disturb the order. A table does *not* remember that it is
sorted, deliberately: a stale "still sorted" flag would return wrong rows silently.

## Ranking rows by a column

Likewise there is no `%rank` — adding a rank column is four calls through `%col`, shown here as a
complete program since it is the one place on this page where getting the nulls right matters:

```fortran
program rank_by_flux
    use parquet
    use iso_fortran_env, only: int64, real64
    implicit none

    type(parquet_table) :: t
    real(real64) :: flux(5)
    real(real64), pointer :: fp(:)
    integer(int64), allocatable :: ranks(:)
    logical, allocatable :: valid(:)
    integer :: i

    flux = [3.0_real64, 1.0_real64, 4.0_real64, 1.0_real64, 5.0_real64]
    call parquet_new_table(t)
    call t%add_column("flux", flux)
    call t%set_null("flux", 2_int64)          ! two rows with no measurement
    call t%set_null("flux", 4_int64)

    call t%col("flux", fp)                    ! the values...
    call t%get_valid_mask("flux", valid)      ! ...and which of them are real
    call pf_rank(fp, ranks, descending=.true., is_valid=valid)   ! 1 = brightest
    call t%add_column("flux_rank", ranks)

    do i = 1, int(t%nrows())
        print '(a,i0,a,i0)', "row ", i, " rank ", ranks(i)
    end do
end program rank_by_flux
```

which prints ranks `3 0 2 0 1` — the two nulled rows ranked 0, and the three real values ranked
brightest-first.

`method=` picks how ties are handled — `"competition"` (1,2,2,4, the default), `"dense"` (1,2,2,3)
or `"ordinal"` (1,2,3,4) — and **a null gets rank 0**, since a missing value has no rank rather than
the last one. Dense ranks over a sorted key column are also the cheapest way to label groups: every
row of a group gets the same number.

**`is_valid=` is what makes that rank-0 rule reachable, and leaving it out is a silent wrong
answer.** `%col` hands back the *values* array and nothing else — a `parquet_column` keeps its nulls
in a separate bitmap — so a `pf_rank` given only the pointer has no idea which rows are missing and
ranks them as ordinary values. Worse, a null row's value bytes are unspecified, so what it ranks
them *by* is undefined. On a five-row column with rows 2 and 4 nulled, the call without the mask
gives `3 4 2 4 1` where the call with it gives `3 0 2 0 1` — plausible ranks either way, and only
one of them right. Pass the mask, or be sure the column holds no nulls.

The same applies to every `pf_*` operation reached through a `%col` pointer, including the searches
[above](#looking-a-value-up-in-a-sorted-table): each takes its own optional `is_valid=`, and without
it a null row participates as whatever its value slot happens to hold.
