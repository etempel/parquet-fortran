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

call cat%join(lut, on = "uberID", how = "left")
!  cat has read exactly one of its own columns -- the key -- and now carries every column
!  `lut` held, matched row for row.
```

Everything on this page is about `parquet_table`. The rules it depends on — laziness, residency,
detaching — are the ones [Changing a table](table-mutate.html) sets out, and it is worth reading
[What "detaching" means](table-mutate.html#what-detaching-means) first.

## The call

```fortran
call t%join(other, on [, other_on] [, how] [, columns] [, other_suffix] [, threads])
```

Square brackets mark an optional argument, and the comma sits outside the bracket; every bracketed
signature on this page reads that way. `on` is either a separated string (`"id"`, `"ra,dec"`) or an
array of names — the two forms behave identically, and `other_on` takes whichever form `on` did.

| argument | what it does |
|---|---|
| `on` | the key columns of **this** table, primary first |
| `other_on` | the key columns of `other`, when their names differ. One name per `on` name |
| `how` | `"inner"` (the default) or `"left"`; see [Which rows come out](#which-rows-come-out) |
| `columns` | which of `other`'s non-key columns to bring over; see [What the join carries](#what-the-join-carries) |
| `other_suffix` | the suffix an incoming name takes when it clashes. Default `"_2"` |
| `threads` | forwarded to the sort the join runs. Absent resolves automatically |

**`%join` changes this table in place**, and detaches it unless every one of its rows survives
exactly once and in place — see [When the join detaches](#when-the-join-detaches-and-when-it-does-not).
If you want the original as well, `call t%clone(out)` first and join the
copy — a clone copies only the columns that are resident, so on a lazy table it is nearly free.
There is no separate non-mutating entry point, and that is deliberate: one would have to copy every
column of the left table into a new one, which forces a 300-column table to materialize all 300 to
gain four.

## Which rows come out

`how="inner"` keeps only the rows that found a counterpart. `how="left"` keeps every row of this
table and fills the incoming columns with nulls where there was no counterpart.

```fortran
call a%join(b, "id")                 ! inner: only the matches
call a%join(b, "id", how = "left")   ! every row of `a`, nulls where `b` had nothing
```

A key value that appears twice on one side and three times on the other produces six rows, which is
what "join" means and is also the way a join gets unexpectedly large. Two keys that are unique on
both sides produce one row each.

**Rows come out in this table's original order**, and within each of them, that row's matches in
`other`'s original order. So an array you computed against this table before the join still lines
up row for row whenever the join was one-to-at-most-one.

`"right"`, `"outer"`, `"semi"` and `"anti"` are recognised names and are refused for now: they need
a rewrite that can null-fill *this* table's own columns, or remove its rows, which `%join` does not
do yet. Asking for one says so rather than doing something else.

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

## What counts as a match

**A null key matches nothing** — not a value, and not another null. A row whose key is null is
simply an unmatched row: dropped by `how="inner"`, kept with null incoming columns by `how="left"`.
This is SQL's rule and STILTS', and it is the only one that makes sense for a key that means
"unknown".

**A NaN key is an ordinary value and matches every other NaN.** That is worth knowing before it
surprises you: a column where the missing values were written as NaN rather than as nulls will
match every such row against every other one, which for a few thousand of them is a few million
output rows.

**Key kinds must match exactly.** An `int32` key and an `int64` key are refused rather than
promoted, and so are two keys of different widths. Nothing is silently widened, because a 64-bit
catalogue identifier above 2⁵³ does not survive being promoted to a real — cast one side with
`%cast` first if you really do mean to compare them.

"Equal" here means exactly what it means in `%sort_by` and in a read-time `sort_by=`: the join runs
the same engine over the same keys, so there is only ever one answer to what equality is.

**A join key is a column name and carries no direction.** `on="-id"` or `on="id desc"` is a *sort*
key, and it is refused with a message saying so — a join is an equality test, and the order the
keys are compared in is the library's business rather than yours.

Every scalar column that can be a sort key can be a join key: the nine orderable kinds. A vector
column and a container column cannot, for the same reason they cannot be sorted by.

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

## What the result is called

**The key column appears once**, taken from this table, when `on` and `other_on` name the same
thing. When they differ both are kept, because the right-hand key really is a different column:

```fortran
call a%join(b, on = "id", other_on = "object_id")   ! `a` gains `object_id` as well
```

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

call a%join(b, on = "object_id", how = "inner")
write (*, "(a,i0)") "objects in both surveys: ", a%nrows()
```

**A lookup-table enrichment**, the shape most catalogue work has:

```fortran
call parquet_open_table(cat, "waves_full.parquet")
call parquet_open_table(lut, "todd.parquet")
call cat%materialize("uberID,ra,dec")   ! the columns to keep through the detach
call lut%materialize_all()              ! the lookup table is small

call cat%join(lut, on = "uberID", how = "left")
```

## Limitations

- `how=` accepts `"inner"` and `"left"`. `"right"`, `"outer"`, `"semi"` and `"anti"` are recognised
  and refused for now.
- A container column cannot be carried across a join, and cannot be a key.
- A table cannot be joined to itself. `call t%clone(other)` first and join the clone — Fortran
  forbids one variable reaching a procedure as two arguments when either is written to, and no
  compiler diagnoses it, so `%join` refuses it explicitly.
- `%join` always detaches this table, including the case where it keeps every row in its original
  order.
