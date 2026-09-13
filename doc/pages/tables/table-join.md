---
title: Joining two tables
---

`%join` matches another table's rows against this one's on one or more key columns and brings that
table's columns over. It is the cross-match at the heart of most catalogue work: a survey against a
reference catalogue, a 100-million-row table against a small lookup table, two observations of the
same objects.

```fortran
call parquet_open_table(cat, "waves_full.parquet")   ! reads nothing yet
call parquet_open_table(lut, "todd.parquet")
call lut%materialize_all()                           ! the lookup table is small: bring it in

call cat%join(lut, on = "uberID", how = "left", require = "m:1")
!  cat has read exactly one of its own columns -- the key -- and now carries every column
!  `lut` held, matched row for row. require="m:1" is what makes "row for row" true rather
!  than hoped for: it refuses the join if uberID is repeated in lut.
```

Everything on this page is about `parquet_table`. The rules it depends on — laziness, residency,
detaching — are the ones [Changing a table](table-mutate.html) sets out, and it is worth reading
[What "detaching" means](table-mutate.html#what-detaching-means) first.

## The call

```fortran
call t%join(other, on, [other_on], [how], [columns], [other_suffix], &
                       [require], [order], [max_rows], [matched], &
                       [pairs], [other_pairs], [threads])
```

Square brackets mark an optional argument, and the comma sits outside the bracket; every bracketed
signature on this page reads that way. `on` is either a separated string (`"id"`, `"ra,dec"`) or an
array of names — the two forms behave identically, and `other_on` takes whichever form `on` did.

| argument | what it does |
|---|---|
| `on` | the key columns of **this** table, primary first |
| `other_on` | the key columns of `other`, when their names differ. One name per `on` name |
| `how` | `"inner"` (the default), `"left"`, `"right"`, `"outer"`, `"semi"` or `"anti"`, case-insensitive like `require=` and `order=`; see [Which rows come out](#which-rows-come-out) |
| `columns` | which of `other`'s non-key columns to bring over; see [What the join carries](#what-the-join-carries) |
| `other_suffix` | the suffix an incoming name takes when it clashes. Default `"_2"`; a blank one is refused |
| `require` | the cardinality you expect: `"m:m"` (the default), `"1:1"`, `"1:m"` or `"m:1"`; see [Saying what you expect](#saying-what-you-expect) |
| `order` | `"left"` (the default) or `"key"`; see [Which rows come out](#which-rows-come-out) |
| `max_rows` | refuse the join rather than build a result larger than this |
| `matched` | out: one entry per row of **this** table as it was before the join |
| `pairs` | out: one entry per output row — the row of **this** table it came from, or 0; see [The match itself](#the-match-itself) |
| `other_pairs` | out: the same for `other`'s rows |
| `threads` | the team size for the engine that builds the match, and for nothing else; see below and [How the match is built](#how-the-match-is-built) |

That last row is narrow on purpose, and a join has **two** thread controls that are not
interchangeable. `threads=` sizes the engine that builds the match — everything up to the pair list
and its conversion into row indices — and nothing else. The column
work a join does — this table's own columns rewritten, `other`'s copied in beside them — belongs to
the table layer and is capped by
[`parquet_set_table_threads(n)`](../operating/settings.html#threads-for-mutating-a-table), as every
other row-structural mutation is.

So a join that is slow because the *match* is large is one to pass `threads=` to, and a join that is
slow because the *columns* are many or wide answers to `parquet_set_table_threads`. Neither argument
changes the result: both are performance controls, and the rows that come out, and their order, are
the same at every team size.

**`%join` changes this table in place**, and detaches it unless every one of its rows survives
exactly once and in place — see [When the join detaches](#when-the-join-detaches-and-when-it-does-not).
If you want the original as well, `call t%clone(out)` first and join the
copy — a clone copies only the columns that are resident, so on a lazy table it is nearly free.
There is no separate non-mutating entry point, and that is deliberate: one would have to copy every
column of the left table into a new one, which forces a 300-column table to materialize all 300 to
gain four.

## Which rows come out

`how=` picks which rows survive, and there are six:

| `how` | keeps |
|---|---|
| `"inner"` | the default. Only the rows that found a counterpart |
| `"left"` | every row of this table; the incoming columns are null where there was none |
| `"right"` | every row of `other`; **this table's own** columns are null where there was none |
| `"outer"` | both, so either half of a row may be null |
| `"semi"` | the rows of this table that matched, and no column from `other` at all |
| `"anti"` | the rows of this table that did **not** match, and no column from `other` |

```fortran
call a%join(b, "id")                  ! inner: only the matches
call a%join(b, "id", how = "left")    ! every row of `a`, nulls where `b` had nothing
call a%join(b, "id", how = "outer")   ! both sides, nulls on whichever half is missing
call a%join(b, "id", how = "semi")    ! `a` filtered down to the rows `b` knows about
```

**`"semi"` and `"anti"` are row selections, not joins in the usual sense**: they answer "did this
row find a counterpart?" and keep or drop it accordingly, bringing nothing across. Nothing is
duplicated by them either, however many counterparts a row has. `columns=` is refused with both,
rather than ignored — there would be no column for it to name.

A key value that appears twice on one side and three times on the other produces six rows, which is
what "join" means and is also the way a join gets unexpectedly large. Two keys that are unique on
both sides produce one row each.

**Rows come out in this table's original order**, and within each of them, that row's matches in
`other`'s original order. So an array you computed against this table before the join still lines
up row for row whenever the join was one-to-at-most-one. A row this table contributed nothing to
has no place in a walk over its rows, so under `"right"` and `"outer"` those rows follow at the end,
in `other`'s own order.

`order="key"` asks for the engine's own order instead: the rows grouped by key value, which is what
astropy's join produces. The same pairs come out either way and only the sequence differs, so the
default is the one to keep unless the grouping itself is what you want — an array computed against
this table before the join no longer lines up under `"key"`.

## What counts as a match

**A null key matches nothing** — not a value, and not another null. A row whose key is null is
simply an unmatched row: dropped by `how="inner"`, kept with null incoming columns by `how="left"`.
This is SQL's rule, STILTS' and polars', and it is the only one that makes sense for a key that
means "unknown". pandas is the odd one out: its `merge` matches NA to NA, and in a float key it
cannot tell a NULL from a NaN — so a pandas join over a table with null keys comes out larger than
this library's, by pandas' documented choice rather than by any disagreement about the rows.

**A NaN key is an ordinary value and matches every other NaN.** That is worth knowing before it
surprises you: a column where the missing values were written as NaN rather than as nulls will
match every such row against every other one, which for a few thousand of them is a few million
output rows.

**Key kinds must match exactly.** An `int32` key and an `int64` key are refused rather than
promoted. Nothing is silently widened, because a 64-bit
catalogue identifier above 2⁵³ does not survive being promoted to a real — cast one side with
`%cast` first if you really do mean to compare them.

"Equal" here means exactly what it means in `%sort_by` and in a read-time `sort_by=`: whichever
engine builds the match (see [How the match is built](#how-the-match-is-built)) applies the sort
comparator's equality, so there is only ever one answer to what equality is.

**A join key is a column name and carries no direction.** `on="-id"` or `on="id desc"` is a *sort*
key, and it is refused with a message saying so — a join is an equality test, and the order the
keys are compared in is the library's business rather than yours.

Every scalar column that can be a sort key can be a join key: the nine orderable kinds. A vector
column and a container column cannot, for the same reason they cannot be sorted by.

A container column cannot be **carried across** a join either, on any `how`, nor held on this side
of a `"right"`/`"outer"` one — see [When a row has no counterpart
here](#when-a-row-has-no-counterpart-here). Every other column kind, vectors included, is carried
and filled normally.

## How the match is built

The pair list — which row here meets which row there — is built by one of two engines, and the
choice is the library's, made from the key kinds and `order=` alone:

- **The hash engine**, for every join whose key columns are integer, real, date, time or timestamp
  (any number of them), or a single string column, under the default `order="left"`. It builds a
  [`pf_index_multimap`](../utilities/index-maps.html#a-key-that-repeats-pf_index_multimap) over
  `other`'s keys and probes it once per row of this table. A real key is matched by the rule above
  (every NaN one value, `-0.0` equal to `+0.0`), a timestamp by its instant whatever unit either
  file stored it in, a string by its exact bytes.
- **The sort engine**, for the rest: `order="key"` (the grouping it asks for *is* the sort's own
  order, which the hash engine has nothing to offer in place of), a logical key, and a string key
  beside another key. It sorts the two key columns concatenated and reads the matches off the runs
  of equal keys — the engine `%sort_by` runs.

The rows that come out, and their order, are identical under both. The engines differ in how fast,
never in what they answer, and nothing about the data — its values, its row counts — takes part in
the choice, so the same call runs on the same engine on every input and at every thread count. The
hash engine is several times faster on every shape that has been measured, most of all on a
lookup-table join; `bench/benchmark_join.sh` times a join under each engine side by side.

`threads=` sizes whichever engine builds the match: the multimap's build and probe, under
[`parquet_set_index_threads`](../operating/settings.html#threads-for-an-index-build-or-a-bulk-lookup),
or the sort, under [`parquet_set_sort_threads`](../operating/settings.html#threads-for-sorting) —
and, with the sort, the passes that read its runs of equal keys (the classification, the `require=`
check, the counting and the emission), which run on the sort's team. On either engine the two passes
that turn the pair list into row indices run on that team too.
The column work that follows still answers to `parquet_set_table_threads`, as above. That team
divides across the columns when there are at least as many as threads, and goes inside each column
in turn otherwise, so a join carrying one column uses it too; each incoming column is built in one
pass from `other`'s rows, holding one transient copy of it rather than two — see
[Threads for mutating a table](../operating/settings.html#threads-for-mutating-a-table).

## What the join carries

**`columns=` absent brings over every column of `other` that is already resident — not every column
it has.** This is the rule most likely to catch you out, and it follows from `other` being lazy in
exactly the way this table is: a freshly opened table has read nothing, so there is nothing to
carry.

```fortran
call parquet_open_table(lut, "lookup.parquet")
call a%join(lut, "id", how = "left")      ! carries NOTHING: lut has read no column yet

call parquet_open_table(lut, "lookup.parquet")
call lut%materialize_all()                ! "all of it, please"
call a%join(lut, "id", how = "left")      ! carries every column lut has

call parquet_open_table(lut, "lookup.parquet")
call a%join(lut, "id", how = "left", columns = "mag_r,mag_r_err")   ! reads and carries these two
```

Naming a column in `columns=` **does** read it, by the ordinary lazy first touch — a caller who
names a column has said what they want read. Naming one `other` does not have is an error rather
than a silent omission, because a join keeps no link to `other`'s file: a column that did not come
across is gone from the result, and asking about it later says only that there is no such column.

**The same rule governs this table on a join that
[detaches](#when-the-join-detaches-and-when-it-does-not), and there it is an obligation on you.** A
column of this table that has not been read is skipped rather than read, and the table then detaches
— so that column is lost. That failure is loud (the next read of it says the table has detached),
but the fix is to read what you want first:

```fortran
call cat%materialize("uberID,ra,dec,mag_r")   ! the columns you want to keep
call cat%join(lut, "uberID", how = "left")    ! everything else is left behind
```

One sentence covers both sides: **a join carries what is resident, plus whatever you named.** A join
that keeps every row where it was reads and skips nothing on this side, and leaves every unread
column exactly as readable as it was — which is why the m:1 left join is the one shape a
300-column lazy table can be joined without reading anything but its key.

A container column — a list, a map or a struct — cannot be carried across a join at all. An
unmatched row has to be filled with nulls, and a container's own row gather has no settled
behaviour for that, so the refusal is by kind rather than by whether this particular join happens
to have an unmatched row. Name the columns you do want with `columns=`.

**That refusal is reached by `%materialize_all()` on the right table as well as by naming the
column**, since it selects everything resident and a container column is resident like any other.
So a lookup table holding one list column cannot be brought over wholesale, however little the
join wanted it — `columns=` is the way to say which of its columns you meant.

## When a row has no counterpart here

`"right"` and `"outer"` are the two that can emit a row this table contributed nothing to, and two
things follow that the other four never have to deal with.

**This table's own columns are null at such a row**, exactly as the incoming ones are under
`"left"`. So every column here has to be *fillable with nulls* — which a container column (list,
map, struct) is not, and one is refused under those two `how` values with a message saying so. The
refusal is by kind and by side rather than by whether this particular join happens to have an
unmatched row, so that a join does not start failing the day its input gains one.

**The merged key takes `other`'s value there.** Where `on` and `other_on` name the same column the
result holds one key column, and at a row with no counterpart here it is `other`'s key that fills
it — this table has none to give. So the key column of a `"right"` or `"outer"` join is never null
on account of the join itself, and remains the column you can group or match on afterwards. When
the two key names *differ* nothing is merged, both columns are kept, and this table's own key is
null at such a row like the rest of its row.

```fortran
call a%join(b, "id", how = "outer")
!  a row only `b` had: `a`'s own columns are null, and `id` holds `b`'s value
```

## Saying what you expect

A join's worst failure is not an error. It is a result of the wrong size that nothing complains
about — a duplicated row in a lookup table, and every matching row of a 100-million-row catalogue
silently doubled. Three arguments exist for that, and all three are settled from the counting pass,
before a single value column is touched, so a join that will not do what you meant is refused rather
than half-built.

**`require=` is the assertion, and it reads left-side-first.**

| token | asserts | fails when |
|---|---|---|
| `"m:1"` | many rows here may share a key, and each finds at most one row in `other` | `other` repeats a key this table also has |
| `"1:m"` | each key that matches appears at most once in **this** table | this table repeats a key `other` also has |
| `"1:m"` and `"m:1"` together, as `"1:1"` | neither side repeats a key the other has | either side does |
| `"m:m"` | nothing. The default | never |

`require="m:1"` is the annotation for every lookup-table join, and it is worth writing by reflex —
it is what turns a duplicated row in the lookup table into a named error instead of a result many
times the size you expected.

```fortran
call cat%join(lut, "uberID", how = "left", require = "m:1")
!  a repeated uberID in lut is now an error at this call, naming the two rows that share it
```

The message names the two offending rows and how many share their key, on the side that broke the
assertion — a row index is something you can look up, where a key may be several columns of several
types. Rows whose key is **null** are not counted: a null key matches nothing, so several of them
are several rows whose key is *unknown* rather than several copies of one key, and counting them
would refuse a join that was about to be correct. A repeat whose key finds **no counterpart on the
other side** is not counted either: it contributes no output row, so it cannot multiply the
result — and a result of the wrong size is what these assertions exist to catch. So `require=` is
an assertion about the rows that took part in the match, not a uniqueness check on either table by
itself.

**`max_rows=` is the weaker form**, for when you know the scale but not the cardinality. It refuses
the join when the counted output would be larger, and the message names the count, both row counts
and the largest single key group — because "the join wanted four billion rows" is not actionable
without "one key value contributes 64 000 of them". There is no default limit, and none is planned:
a limit that fires on a legitimate join is worse than no limit at all.

```fortran
call a%join(b, "id", max_rows = 10000000)          ! a plain integer, or 10000000_int64
```

**`matched=` is the diagnostic**, and it is one entry per row of this table **as it was on entry** —
the only coordinate system in which "did my object find a counterpart?" still has an answer once the
join has changed the rows. `count(matched)` is the line a cross-match script prints.

```fortran
logical, allocatable :: matched(:)

call a%join(b, "object_id", how = "inner", matched = matched)
write (*, "(a,i0,a,i0)") "matched ", count(matched), " of ", size(matched)
```

None of the three changes what the join produces when it is satisfied: `require=` and `max_rows=`
either abort or do nothing at all, and `matched=` is read-only.

## The match itself

`pairs=` and `other_pairs=` hand back what the join worked from: one entry each per row of the
result, naming the row of this table and the row of `other` that produced it, with **0 for "no
counterpart on that side"**. Both are `integer(int64), allocatable`, both are the length of the
joined table, and either can be asked for without the other.

Both number the rows **as they were on entry**, like `matched=`. That is the point of them: by the
time you read them the join has already rewritten this table, so an index into the rows it has *now*
would answer a question nobody asked.

This is what applies the same match to something the table does not hold — an array of a derived
type, a second table keyed the same way, a file you are about to write beside this one:

```fortran
real(real64), allocatable :: exptime(:), joined(:)   ! parallel to `a`'s rows, but not columns of it
integer(int64), allocatable :: il(:)
integer(int64) :: k

call a%join(b, "object_id", how = "left", pairs = il)
allocate(joined(a%nrows()))
joined = -1.0_real64
do k = 1, a%nrows()
    if (il(k) /= 0) joined(k) = exptime(il(k))        ! il(k) numbers the rows `a` had on entry
end do
```

Under `how="inner"` and `how="left"` every output row has a left row, so `pairs` is never 0 there
and the guard above is pure defence; it earns its place under `"right"` and `"outer"`, which can
emit a row this table contributed nothing to. `other_pairs` is never 0 under `"inner"` and
`"right"`, and is 0 on every row of a `"semi"` or `"anti"` join, which carry nothing from `other`
at all.

If what you have is two arrays rather than two tables, the same question is answered one layer
down by [`pf_match`, `pf_match_all` and `pf_in`](../utilities/sorting.html#matching-two-arrays),
which use the same convention: an index into the other array, or 0.

The same two arrays are the shape [`pf_spatial_index%pairs_within`](../utilities/spatial.html)
hands a positional match back in, so a pair list reads the same way whichever produced it. It is a
shape rather than an interface: `%pairs_within` matches one dataset against itself and reports each
pair once as `i < j`, never 0, where these two index two different tables and use 0 for a row one of
them did not contribute to. Asking for neither costs nothing; asking for either costs nothing
either, since the join hands its own arrays over rather than copying them.

## When the join detaches, and when it does not

A join that leaves every one of this table's rows exactly once, in its original position, has not
changed which rows the table has — it has only added columns. On that path the table keeps its file:
nothing is rewritten, no column that has not been read becomes unreadable, a slice still covers the
rows it covered, and an outstanding `%col` pointer can survive the whole operation. Anything else is
a change to the row set, and the table detaches from its file exactly as `%sort_by` and
`%filter_rows` do.

**Which one you get is a property of the data, not of `how`.** A left join keeps every left row —
but keeps it *once* only when each key it matches appears once in `other`. Duplicate that key and
the row is duplicated too, and the join detaches:

```fortran
call cat%join(lut, "uberID", how = "left")
!  uberID unique in lut  ->  cat keeps its file, and every column it never read
!  uberID repeated in lut ->  those rows are duplicated, so cat detaches
```

The same rule covers the other four with no clause of its own. A `"semi"` join whose key matches
every row here is a filter that removes nothing, so it keeps its file; one that removes a row
detaches, as `%filter_rows` does. A `"right"` or `"outer"` join detaches as soon as a row of
`other` has no counterpart here, because that row is one this table did not have — and a `"right"`
join detaches on the reverse too, since it drops a row of this table that found nothing.

So the rule to plan against is the one on this page's [What the join
carries](#what-the-join-carries): read the columns you want to keep *before* a join, unless you know
the key is unique on the other side. `%is_detached()` answers afterwards.

**`%generation()` moves only if the incoming columns had to grow the slot array.** Reserve room
first and it does not move at all, which is what lets a pointer taken before the join still be
valid after it:

```fortran
call cat%reserve_columns(cat%ncols() + 4)   ! four columns coming from lut
call cat%col("ra", ra)                      ! a live pointer into cat's own storage
call cat%join(lut, "uberID", how = "left")  ! m:1, so no row moves
!  cat%generation() is unchanged, and `ra` still points at cat's ra column
```

Check `%generation()` rather than assuming: a join that did move rows will have bumped it, and that
is the signal to fetch the pointer again.

## What the result is called

**The key column appears once**, taken from this table, when `on` and `other_on` name the same
thing. When they differ both are kept, because the right-hand key really is a different column:

```fortran
call a%join(b, on = "id", other_on = "object_id")   ! `a` gains `object_id` as well
```

**`columns=` does not govern either case.** A right-hand key whose name differs comes across
whichever columns you named, because it is the only place its values appear in the result at all;
and naming the merged key in `columns=` adds nothing, since that column is already here. `columns=`
lists what arrives *in addition to* the keys.

Any other incoming column whose name clashes takes `other_suffix` (default `"_2"`), and **only the
incoming column is renamed** — your own `mag_r` stays `mag_r` and the arriving one becomes
`mag_r_2`. A mutating call has no business renaming a column you already had. If the suffixed name
clashes too, that is an error naming both rather than a second suffix.

An incoming column keeps its own unit. `%get_file_metadata` still answers about this table's own
source file; `other`'s metadata is not merged, because there is no defensible rule for what to do
with a key both files define.

## Two worked examples

**A cross-match between two surveys**, keeping only the objects both have:

```fortran
call parquet_open_table(a, "survey_a.parquet")
call parquet_open_table(b, "survey_b.parquet")
call a%materialize_all()          ! how="inner" drops rows, so `a` detaches: read what you want
call b%materialize("mag_r_ref,mag_r_err_ref")

call a%join(b, on = "object_id", how = "inner", require = "1:1", matched = matched)
write (*, "(a,i0,a,i0)") "objects in both surveys: ", count(matched), " of ", size(matched)
```

`require="1:1"` is the right assertion here rather than `"m:1"`: an object identifier is expected to
be unique in *both* surveys, and a repeat in either one means the input is not what it was taken to
be.

**A lookup-table enrichment**, the shape most catalogue work has:

```fortran
call parquet_open_table(cat, "waves_full.parquet")
call parquet_open_table(lut, "todd.parquet")
call cat%materialize("uberID,ra,dec")   ! the columns to keep through the detach
call lut%materialize_all()              ! the lookup table is small

call cat%join(lut, on = "uberID", how = "left", require = "m:1")
!  m:1, so no row of `cat` moves: it keeps its file, and `ra`/`dec` stay where they were.
```

## Limitations

- A container column cannot be carried across a join, cannot be a key, and cannot be held on this
  side of a `"right"` or `"outer"` one.
- `columns=` is refused with `how="semi"` and `how="anti"`, which carry no columns at all.
- A table cannot be joined to itself. `call t%clone(other)` first and join the clone — Fortran
  forbids one variable reaching a procedure as two arguments when either is written to, and no
  compiler diagnoses it, so `%join` refuses it explicitly.
