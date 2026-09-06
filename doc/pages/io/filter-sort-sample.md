---
title: Filtering, sorting and sampling rows
---

A reader can be restricted to a subset of a file's rows — by value (`filter=`) or by random draw
(`sample_fraction=`) — and can return them in a chosen order (`sort_by=`). Everything documented in
[Reading parquet files](reading.html) then behaves as if the file only ever contained the surviving
rows, in the requested order. This page is the reference for those three options and how they
compose.

## Row filtering with `parquet_filter`

`parquet_open_reader(reader, filename, filter=filt)` restricts a reader to only the rows matching
a `type(parquet_filter)`. Once a filter is set, it is completely transparent to everything else:
`parquet_get_nrows`, `parquet_read_column`, `parquet_prefetch_columns`, and
`parquet_close_reader(..., print_stat=.true.)` all behave exactly as if the file only ever
contained the matching rows — there is no separate "filtered count" to track yourself.

```fortran
type(parquet_filter) :: filt
type(parquet_reader) :: reader
integer(int32) :: nrows
integer(int32), allocatable :: ra(:)

call filt%add("(ra > 180 and ra <= 360) or ra is_null")
call filt%add("id is_not_null")

call parquet_open_reader(reader, "data.parquet", filter=filt)
call parquet_get_nrows(reader, nrows)      ! already the filtered row count
allocate(ra(nrows))
call parquet_read_column(reader, "ra", ra) ! already just the matching rows
call parquet_close_reader(reader)
```

### The rule grammar

Each `filt%add(rule)` call contributes one boolean **expression** over the file's columns. An
expression is either a single clause or several clauses combined with `and`, `or`, `not` and
parentheses:

```
   expr     := or_expr
   or_expr  := and_expr { or and_expr }
   and_expr := not_expr { and not_expr }
   not_expr := [ not ] not_expr | primary
   primary  := '(' expr ')' | clause
   clause   := <column> <op> [ <value> ]
   value    := ... | '@' NAME          ! for in / not_in: a set attached with %bind
             | '(' item { ',' item } ')'   ! for in / not_in: a list written out here
```

| | |
|---|---|
| **Precedence** | `not` binds tightest, then `and`, then `or` — so `a or b and c` means `a or (b and c)`, and `not a and b` means `(not a) and b`. Parentheses override. |
| **Keywords** | `and` / `or` / `not`, in any case (`AND`, `And`, `and`). Only whole tokens are keywords, so a column named `android` or `nothing` is unaffected. Fortran-style `.and.` and C-style `&&` are *not* accepted. |
| **Operators** | `>`, `>=`, `<`, `<=`, `==`, `/=`, `in`, `not_in`, `is_null`, `is_not_null`, `is_nan`, `is_not_nan`, `is_finite`, `is_not_finite`. A clause's operator must be surrounded by spaces (`"v > 3"`, not `"v>3"`); parentheses need no surrounding spaces. The four value-class operators — `is_nan`/`is_not_nan`/`is_finite`/`is_not_finite` — are accepted only for a floating-point column (`float32`/`float64`, and a `half_float` column written by some other tool); any other column type is rejected, since no value of it could ever be a NaN or an infinity. |
| **Values** | A bare number for a numeric column (`ra > 180`), `true`/`false` for a boolean column (`flag == true`), a **double-quoted** string for a `string` column (`name == "abell_1"`), or a **double-quoted ISO-8601 literal** for a `date`/`time`/`timestamp` column (see below). The four value-class operators take no value, and `in`/`not_in` take a **set** rather than a single literal — either the name of one attached with `%bind` (`ID in @wanted`) or a list written out in the rule (`ID in (3, 5, 9)`); see [Membership in a set](#membership-in-a-set-in-and-not_in) below. A quoted value may contain spaces, parentheses, and the keywords themselves — it is read as one token. An `inf`/`-inf` value is accepted as an ordinary bound (`v < inf`); a bare `nan` is **rejected**, because every comparison against a NaN is false and every `/=` against it is true, so such a clause could only ever match nothing or everything — say `is_nan`/`is_not_nan` instead. |
| **Column names** | May be a dotted struct-leaf path (`main.inner.age > 35`). A column name cannot contain spaces. |
| **Several `%add` calls** | **AND-combined**: two calls mean `(expr1) and (expr2)`. So a filter written as one clause per call means the conjunction of its clauses; write `or` inside a single rule when you want alternatives. |
| **Limits** | 32 levels of nesting, 1024 expression terms per filter, 64 bound sets per filter, 8192 characters per rule, and, within one clause, 64 characters per column name and 512 per value — each reported as a clean error rather than a crash. The first three are published constants you can check a rule against beforehand; see [Read-only limits](../operating/settings.html#read-only-limits). |

`between` and wildcard/`like` matching are not supported: the first is shorthand for what the
grammar already expresses (`x between 1 and 9` is `x >= 1 and x <= 9`), and pattern matching is
genuinely different work. `in` and `not_in` **are** supported, over a set you attach as an array and
over a list written out in the rule; a list is not merely shorthand for an `==` chain, because the
whole set is answered in one pass over the column rather than one pass per member. That is the next
section.

### Membership in a set: `in` and `not_in`

`in` keeps the rows whose value appears in a set you attach to the filter as an **array**:

```fortran
type(parquet_filter) :: filt
integer(int64), allocatable :: ids(:)

call filt%add_in("ID", ids)                  ! keep rows whose ID is one of these
call parquet_open_reader(reader, "survey.parquet", filter=filt)
```

`%add_in(column, values, [is_valid], [negate])` is the one-set-one-column form. The general form
attaches the array under a **name** and refers to it from ordinary rule text with a leading `@`,
which is what lets a set combine with `or`, `not` and parentheses like any other clause:

```fortran
call filt%bind("wanted", ids)                ! attach the array under a name
call filt%add("ID in @wanted or flag == 7")  ! ... and use it in an expression
call filt%add("src not_in @bad or src is_null")
```

The spelling is pandas' `query("ID in @ids")`. `%bind` accepts `integer(int32)`, `integer(int64)`,
`real(real32)`, `real(real64)`, a `character` array, or a `parquet_string_column`, each with an
optional `is_valid=` mask marking the elements to leave out of the set.

**A short set can be written out in the rule instead**, as a parenthesised, comma-separated list:

```fortran
call filt%add("field_id in (3, 5, 9)")
call filt%add('survey not_in ("test", "commissioning") and z_spec is_finite')
```

A list means exactly what the same members bound with `%bind` mean, and is answered the same way:
one pass over the column, one leaf, one row-group screen — not one pass per member, which is what an
`==` chain would cost. Everything the rest of this section says about a bound set applies to it
unchanged. Four points are specific to the written-out form:

- **What the elements are is decided by the column, not by the text.** `x in (1, 2, 3)` is an
  integer list against an integer column and a floating-point one against a `float32`/`float64`
  column, exactly as the bare literal in `x == 1` is. A member for a `string` column must be
  double-quoted, and one for a numeric column must not be; either mistake is reported naming the
  element's position.
- **A quoted member may contain a comma, a parenthesis or a keyword** — `name in ("a,b", "c)d")` is
  two members — because a quoted run is read as one token before the list is split.
- **`inf` and `-inf` are accepted; `nan` is not**, for the reason a bare `nan` literal is not: it
  could never match. Say `is_nan` instead.
- **The element grammar is deliberately narrow.** An integer element is an optional sign then
  digits; a real element is an optional sign, then digits with an optional fraction and an optional
  `e` exponent, or an infinity. Two spellings a Fortran programmer might reach for are refused:
  Fortran's own `d` exponent (`1d3`, which a bare literal does not accept either), and a C-style
  hexadecimal float (`0x1p3`, which a bare literal does accept). Anything the grammar refuses is a
  clean error naming the element, never a different value. The whole list has to fit the
  512-character value limit, so a long identifier list belongs in `%bind`, which has no length limit
  at all.

**What the set is compared against.** An integer set matches any integer column; a real set matches
a `float32`/`float64` column; a string set matches a `string` column. Anything else is refused
naming the column, exactly as a mistyped literal is. A `boolean` column is refused because a set of
booleans is `==` with extra steps, and a vector, `list`, `map` or `struct` column is refused for the
reason every filter column is — there is no single value per row to compare.

**The set is copied, and deduplicated, at `%bind`.** Your array may change or go out of scope
afterwards. Repeats are collapsed rather than rejected, which matters because an identifier list
produced by a join or a group-by very often carries some. A `character` array's elements are
trimmed on the way in; a `parquet_string_column`'s are taken verbatim, and a Null element of one is
simply not in the set.

**Nulls and NaNs follow the filter's own rules**, not those of the in-memory `pf_in`:

| the row's value | `x in @s` | `x not_in @s` |
|---|---|---|
| a member of the set | true | false |
| not a member | false | true |
| Null | unknown — dropped, as under every comparison | unknown — dropped |
| NaN | false (it matches nothing) | **true** |

So `is_null` remains the only way a Null row enters a result, and a NaN row survives `not_in` for
the same reason it survives `/=`: IEEE says it equals nothing. A NaN **inside** a bound set is
rejected at `%bind`, because it could never match — the same reason a bare `nan` literal is
rejected.

**An empty set matches nothing**, so `in` keeps no rows and `not_in` keeps every non-null row. That
is expressed by binding a zero-length array; an empty list `()` in rule text is refused instead,
since as a written expression it is almost always a mistake.

**A set clause prunes row groups better than any other clause can.** The membership test is
evaluated before the file's data columns are read — one row group of the key column at a time, by
the library's own key index — so by the time the row-group screen runs it knows, exactly, which row
groups hold a member. That means it prunes where a min/max rule cannot: on a **string** column, on a
file written with **no statistics at all**, and on a scattered set whose values fall in the gaps
between row groups' ranges. On a file clustered or sorted by the key, most of it is skipped without
being read.

**What it costs.** The distinct keys, once per copy of the filter (about 8 bytes each for a numeric
set), plus one byte per row of the file per `in` clause while the filter is being installed — the
same shape and size as the mask `sample_fraction=` already builds. Under `bounded=.true.` that array
is, like the filter's own row mask, one of the things that still scale with the file's row count.
`bench/benchmark_filter_set.sh` measures the evaluation against the row-group decode it precedes.

**This is the one filter clause a `bounded=.true.` read can use to restrict a second file by a first
one's results** — see [Opening a table](../tables/table-open.html#reading-a-file-larger-than-memory-bounded).
The in-memory alternatives (`pf_in` then `%filter_rows`, or a semi `%join`) both need the key column
resident in full and both detach the table, which is exactly the cost `bounded=` exists to avoid.

### Null values follow SQL's three-valued logic

A comparison against a Null is neither true nor false but **unknown**, and only rows that come out
*true* survive:

| expression | a row whose `x` is Null |
|---|---|
| `x > 5` | excluded (unknown) |
| `not x > 5` | **still excluded** — negating unknown is unknown, not true |
| `x > 5 or y > 5` | survives only if `y > 5` is true |
| `x is_null` | survives |
| `not x is_null` | excluded; identical to `x is_not_null` |

`is_null`/`is_not_null` are the only operators that answer true/false for a Null row, so they are
the only way to select one. This matches SQL's `WHERE` clause and Arrow's own kernels — in
particular, `not` does **not** let Null rows in through the back door.

Two consequences worth stating outright, because filters are usually written assuming them without
checking:

- A row whose filter column is Null can never be brought in by a *comparison* on that column —
  only by an explicit `is_null` on it, or by some other column's clause being true on the other
  side of an `or`.
- So a filter that mentions one column and never says `is_null` returns no rows with a Null in
  that column, whatever operators and negations it uses.

### NaN is a value, not a Null

A NaN in a floating-point column is an ordinary **value** that happens to compare false against
everything, so it follows IEEE rules rather than the three-valued rules above — and behaves as
almost the *opposite* of a Null:

| expression | a row whose `x` is NaN | a row whose `x` is Null |
|---|---|---|
| `x > 5`, `x >= 5`, `x < 5`, `x <= 5`, `x == 5` | excluded (every IEEE comparison against NaN is false) | excluded (unknown) |
| `x /= 5` | **survives** — `NaN /= 5` is true | excluded (unknown) |
| `not x > 5` | **survives** — false negates to true | excluded (unknown negates to unknown) |
| `x is_not_null` | survives — a NaN is not missing | excluded |
| `x is_nan` | survives | excluded (unknown) |
| `x is_not_nan` | excluded | excluded (unknown) |
| `x is_finite` | excluded — a NaN is not finite | excluded (unknown) |
| `x is_not_finite` | survives | excluded (unknown) |

Four operators say this directly, on a floating-point column:

```fortran
call filt%add("flux is_not_nan")             ! only rows whose flux is not a NaN
call filt%add("flux is_finite")              ! ... and not an infinity either
call filt%add("flux is_nan or flux is_null") ! only the rows with no usable value
```

`is_finite` is the stricter of the two positives: `x is_not_nan` admits `+inf` and `-inf`, while
`x is_finite` keeps only the values you can do arithmetic with. It is exactly `x > -inf and x < inf`
— a NaN satisfies neither comparison, so it is excluded by both spellings — and `is_not_finite`
is its negation over the non-null rows.

All four are Kleene-honest about nullness — a Null row is *unknown* for every one of them, exactly
as it is for a comparison. So `x is_not_nan` means "`x` is a real number", not "`x` is anything
other than a NaN", and `x is_not_finite` means "`x` is a NaN or an infinity", not "`x` is anything
other than a finite number": write `x is_not_finite or x is_null` when a missing value should count
too. Nullness stays governed solely by `is_null`/`is_not_null`.

None of the four adds expressive power the grammar lacked — under Kleene logic `not (x >= 0 or
x < 0)` already meant exactly `x is_nan`, since every non-NaN real satisfies precisely one of the
two disjuncts — but writing that out is easy to get wrong and hard to read, which is what they
are for.

### Filtering `date`, `time` and `timestamp` columns

A temporal column is compared against a double-quoted ISO-8601 literal, which is converted into
the column's own stored unit:

```fortran
call filt%add('obs_date >= "2024-01-31"')
call filt%add('obs_time < "12:30:00"')
call filt%add('obs_ts == "2024-01-31T12:30:00"')
```

- A **less** precise literal is fine: a date-only literal against a `timestamp` column means
  midnight of that date.
- A **more** precise literal is an error, not a silent truncation: `"2024-01-31T12:30:00.123456"`
  against a `timestamp[ms]` column is rejected, because that column cannot represent the value
  being asked about. So is any literal with a time part against a `date` column.
- An **unquoted** value against a temporal column is rejected too — a bare number would mean days
  for one column and microseconds since the epoch for another, with nothing in the rule to say
  which.
- **Timezones are not interpreted.** A `parquet_timestamp` holds the stored epoch offset verbatim
  (see [Dates and times](../types/date-time.html)), so a literal is read as a civil date/time and
  compared against the same stored instants a read returns.

### Validation and cost

Only plain scalar columns can be filtered — naming a **vector or variable-length `list`** column in
a rule fails immediately with `error stop` when `parquet_open_reader` is called. (Both are refused
for the same reason, that a row holds no single value to compare, so do not read this as a
`col_size > 1` test: a ragged `list` column measures `col_size` `1` and is refused just the same.)
So does naming a column
that doesn't exist in the file, or a rule with invalid syntax (an unbalanced parenthesis, a
dangling `and`, an unknown operator, an unquoted string value, a non-numeric value against a
numeric column, ...) — every rule is fully parsed and validated (column existence,
type-compatibility, and value parsing) right there in `parquet_open_reader`, before any of your
own code runs.

The benefit is mostly downstream: `parquet_get_nrows` and every column you read only ever reflect
the matching rows, so your own code loops over, allocates for, and processes far fewer rows when
the filter is selective — at the cost of a small transient memory bump while a column's decoded
array and its filtered result briefly coexist, before the unfiltered one is discarded. (The
`nrows=` shortcut mentioned [above](reading.html#the-nrows-shortcut) works here too — it reflects
the post-filter row count.)

### Row groups a filter cannot match are never read

A filtered reader consults each row group's own **footer statistics** (the per-column-chunk min,
max and null count Parquet records) before reading anything, and skips the row groups those
statistics prove cannot contain a matching row. The row groups that survive are then read and
evaluated exactly, since min/max can only ever prove impossibility, never a match.

This is automatic, has no option to turn it on, and **changes nothing about the result** — the
same rows come back in the same order, with the same nulls. The only difference is how much of the
file was read to produce them, and it applies to the whole read, not just the filter's own
columns: a payload column you read afterwards skips the same row groups.

```fortran
type(parquet_filter) :: filt
call filt%add("id == 8123456")
call parquet_open_reader(reader, "big.parquet", filter=filt)  ! 400 row groups
call parquet_read_column(reader, "flux", flux)                ! may read 1 of them, not 400
```

How much this saves depends entirely on how the file is laid out. A column whose values are
**clustered by row group** (written in sorted order, or naturally grouped like an observation
date) prunes well; one whose values are scattered uniformly gives every row group the same wide
min/max, and nothing can be ruled out. On a well-clustered column a highly selective read can be
several times faster — roughly 5x on a measured 4M-row, 9-column file in 40 row groups — while a
filter matching every row is unchanged.

`parquet_close_reader(reader, print_stat=.true.)` reports a `screened:` line when it skipped
anything, which is the way to tell whether it engaged on your data.

Some cases can never prune, and fall back to reading everything — correctly, just without the
saving:

- a file written **without statistics** (or a column chunk missing them);
- a column whose min/max this library declines to interpret: an **unsigned** integer, `decimal`,
  `half_float`, or legacy `int96`, and likewise any column whose declared Parquet sort order is not
  the one the screen reads for its type — signed for numbers and temporals, unsigned-byte for
  strings. (These last two arise only from files written by other tools; this library's own writer
  never produces them.);
- `is_nan`/`is_not_nan`/`is_finite`/`is_not_finite`, and a **floating-point** column under `not` or
  `/=` — Parquet excludes NaN from min/max and records no NaN count, so for a float column the
  statistics can never prove a comparison is false everywhere, nor say anything about finiteness
  (see [NaN is a value, not a Null](#nan-is-a-value-not-a-null));
- a column whose values are longer than Parquet's statistics size limit (4096 bytes by default),
  for which no bounds are recorded at all.

Those two entries are about the *bounds* being unusable, not about pruning being switched off: a row
group in which the filter column has no non-null values at all is still skipped, because then every
row is unknown for those operators whatever the values would have been.

Two things pruning deliberately does **not** change: `parquet_get_num_row_groups` still reports
every row group in the file, and a chunked read still visits every one of them (a skipped row
group reads as an empty chunk, exactly as a row group the filter emptied already did). Pruning is
an I/O optimization, not a view of the file.

One caveat worth stating: this trusts the file's own footer. A file whose statistics are *wrong* —
written by a tool that recorded bounds not matching its data — will give a wrong answer, in the
same way `parquet_column_has_nulls` already trusts the recorded null count.

### Applying a filter after the reader is open

`parquet_reader_set_filter(reader, filt)` applies a filter to an already-open reader, with exactly
the same result as having passed `filter=` to `parquet_open_reader`. It exists for callers that do
not own the `parquet_open_reader` call, and for filters that can only be built once the file's
schema or metadata has been inspected:

```fortran
call parquet_open_reader(reader, "data.parquet")
if (parquet_column_exists(reader, "quality")) call filt%add("quality > 0.9")
call parquet_reader_set_filter(reader, filt)
```

It refuses, with `error stop`, in three situations: when the reader **already has a filter**
(combine the clauses into one `parquet_filter` instead — several `%add` calls are AND-combined);
when **any column has already been read** on that reader, whether as a whole column or through
[`parquet_read_column_chunk`](reading.html#streamingchunked-reads), since data already handed back
covers the unfiltered rows and could not be lined up with anything read afterwards; and when the
reader **already has a sort**, because a filter must be applied before one (see below). A reader
opened with `sample_fraction=` is fine — the filter applies on top of the sample, exactly as
passing both to `parquet_open_reader` does.

**Naming row groups chooses a different engine.** The call above takes two arguments, but it also
accepts an inclusive 1-based row-group range, and optionally a physical row range inside it:

```fortran
call parquet_reader_set_filter(reader, filt)                              ! whole file
call parquet_reader_set_filter(reader, filt, row_group_lo, row_group_hi)  ! those row groups
call parquet_reader_set_filter(reader, filt, row_group_lo, row_group_hi, row_lo, row_hi)
```

**Whether you name row groups at all — not which ones — chooses how the filter is evaluated.**
Without them, every filter column is read whole-file in one batched pass and left decoded, so
reading one afterwards is free; that is the fastest option and the right default, at the cost of
one full copy of those columns in memory. With them, the expression is evaluated one row group at a
time and only the mask is kept, which is what makes a filtered read possible on a file larger than
memory. `row_group_lo = 0` selects that bounded-memory engine over the *whole* file. Both integer
kinds are accepted throughout.

A `parquet_table` reaches the same engine with one argument:
[`parquet_open_table(..., bounded=.true.)`](../tables/table-open.html#reading-a-file-larger-than-memory-bounded)
installs its filter this way and assembles every column from per-row-group chunks.

A scoped filter scopes the whole reader, not just a loop over those row groups — rows outside the
range have no mask bits, so nothing later can return them. See
[Memory-bounded filtering with a row-group scope](reading.html#memory-bounded-filtering-with-a-row-group-scope)
for the full treatment, including the row-range form and pairing it with a chunked loop.

### The same rules against rows already in memory

Everything on this page describes filtering *while reading*. The identical grammar also applies to
a `parquet_table` whose rows are already in memory:

```fortran
call t%filter_rows("n_obs >= 8 and score > 3")   ! drop the rows that do not match
call t%row_mask("n_obs >= 8", keep)              ! or just ask which they are
```

Same parser, same clause rules, same messages — a rule selects the same rows either way, and both
forms take a `parquet_filter` so a bound set works too. See
[Removing rows by a filter expression](../tables/table-mutate.html#removing-rows-by-a-filter-expression).
Which one to reach for is a question about *when*: a read-time `filter=` never brings the rejected
rows into memory at all and can skip whole row groups, so it is the cheaper option whenever the
selection is known before the file is opened; `%filter_rows` is for a selection that only becomes
known afterwards, or one applied to a table that was built rather than read.

## Reading rows in sorted order with `parquet_sortkey`

`parquet_open_reader(reader, filename, sort_by=srt)` returns the file's rows ordered by one or
more of its columns, instead of in physical file order. Every column read afterwards comes back in
that order, so there is no separate "sorted index" to carry around:

```fortran
type(parquet_reader)  :: reader
type(parquet_sortkey) :: srt
integer(int64) :: nrows
real(real64), allocatable :: ra(:)

call srt%add("ra asc")
call srt%add("dec desc")
call parquet_open_reader(reader, "cat.parquet", sort_by=srt)
call parquet_get_nrows(reader, nrows)      ! unchanged: sorting reorders rows, never adds or removes
allocate(ra(nrows))
call parquet_read_column(reader, "ra", ra) ! already in (ra asc, dec desc) order
call parquet_close_reader(reader)
```

### Sort keys

One key per `%add` call, applied in the order added — the first key is the primary one, later keys
break its ties:

| key text | meaning |
|---|---|
| `"ra"` | order by `ra`, ascending (the default) |
| `"ra asc"` / `"ra ascending"` | the same, spelled out |
| `"dec desc"` / `"dec descending"` | order by `dec`, descending |
| `"-dec"` | shorthand for `"dec desc"` |
| `"main.inner.age"` | a dotted struct-leaf path is a valid key |

Direction words are case-insensitive. Combining the `-` shorthand with an explicit direction
(`"-dec desc"`) is rejected rather than silently resolved, since it reads equally as agreement or
as cancellation.

### Null and NaN placement

Nulls sort **last** by default. Pass `nulls_first=.true.` on `%add` to move that one key's nulls
to the front instead — it is per key, not per sort:

```fortran
call srt%add("quality desc", nulls_first=.true.)
```

`NaN` sits between the real values and the nulls, so an ascending float key gives *values, then
NaNs, then nulls*.

**Placement is absolute: ordering a key descending reverses its values, it does not move its nulls
or NaNs.** So `"v desc"` yields the largest value first and still ends with the nulls. This
reproduces Arrow's own sort ordering exactly, which means a result cross-checked against `pyarrow`
matches row for row.

Rows that tie on **every** key keep their original file order (the sort is stable).

### What can be sorted, and what it costs

Any scalar column can be a sort key: `int32`/`int64`, `float32`/`float64`, `boolean`, `string`,
and `date`/`time`/`timestamp`. A **vector** or a **variable-length `list`** column has no single
value per row to order by and is rejected with `error stop` — from the schema, before any data is
read. (As with filtering, that is not a `col_size > 1` test: a ragged `list` column measures
`col_size` `1` and is refused just the same.) So is a key naming a column the file doesn't have,
or one whose text doesn't parse.

A sort has its own limits, each reported as a clean `error stop` rather than a crash: **16 keys**
per `parquet_sortkey`, **320 characters** per key, and **64 characters** for the column name inside
one. The first two are published constants (`parquet_max_sort_keys`, `parquet_max_sort_key_len`) —
see [Read-only limits](../operating/settings.html#read-only-limits).

If several threads each open their own reader over the same filtered or sorted file, they need not
each rebuild that work: `parquet_reader_adopt_transform` hands one reader's mask and permutation to
another for the cost of two atomic refcount increments — see
[Thread safety](../operating/thread-safety.html).

A sort key column is always read **whole**: a global order needs every row, so there is no
row-group-scoped equivalent the way there is for filtering. That is the one place sorting costs
memory that filtering does not — but only while the permutation is being built. Once it exists the
key column is **released**, so sorting by a column you never read leaves nothing of it resident and
costs nothing to reorder. Reading the key back afterwards is therefore an ordinary read: it decodes
the column again and hands it back in sorted order like any other. Sorting itself never skips I/O —
the benefit is that your own code receives the rows already ordered. (Composed with `filter=`, the
key column is read over the surviving row groups only, since the filter's pruning applies to every
column read after it.)

`prefetch=.true.` is the one case that keeps the key resident, because there you have asked for
every column to be in memory anyway and releasing it would only make the prefetch decode it twice.

### Sorting composes with filtering and sampling

Give both and the filter runs **first**, then the sort orders the surviving rows:

```fortran
call filt%add("mag < 20")
call srt%add("mag asc")
call parquet_open_reader(reader, "cat.parquet", filter=filt, sort_by=srt)
```

`sample_fraction=` behaves the same way. `parquet_close_reader(..., print_stat=.true.)` prints the
keys as applied, on their own `sort:` line.

### Finding out which rows you got

Everything above is phrased as "the reader behaves as if the file only ever contained the surviving
rows", which leaves one question open: *which* rows are they?
`parquet_get_physical_row_indices(reader, rows)` answers it. It fills an
`integer(int64), allocatable` array with the 1-based **physical file row number** of every row the
reader currently returns, in the order it returns them — one entry per row, so `size(rows)`
matches `parquet_get_nrows`:

```fortran
integer(int64), allocatable :: rows(:)

call parquet_open_reader(reader, "cat.parquet", filter=filt, sort_by=srt)
call parquet_get_physical_row_indices(reader, rows)
! rows(1) is the file row that sorted first among the filter's survivors
```

This is the only way to recover that mapping: it lives in the reader's own mask and permutation and
is not otherwise visible. Without a `filter=`, `sample_fraction=` or `sort_by=` it is simply
`1, 2, 3, ...`. A `parquet_table` exposes the same information as an automatic `parquet_row_index`
column — see [Row provenance](../tables/table-open.html) — which is built on this procedure.

### What a sort disallows

A sort permutation destroys row-group locality — sorted row 5 may come from row group 47 and row 6
from row group 3 — so anything row-group-scoped fails with `error stop` while a sort is active:

- [`parquet_read_column_chunk`](reading.html#streamingchunked-reads) and `parquet_get_chunk_size`.

This is **inherent to sorting**, not a limitation to be lifted later: there is no coherent "row
group N of the sorted output" to hand back. It is also the one respect in which sorting differs
from filtering, which supports both — a filter only ever *removes* rows, so row groups stay
contiguous, while a sort *reorders* them.

`parquet_read_array_row_mode` and `parquet_read_array_element_mode` still work, but they fall back
to reading the whole column, since the row they are asked for no longer belongs to any one row
group. `row_index`/`elem_index` then address the **sorted** result, as everything else does.

### Applying a sort after the reader is open

`parquet_reader_set_sort(reader, srt)` sorts an already-open reader, with the same result as
passing `sort_by=` to `parquet_open_reader` — the counterpart of
[`parquet_reader_set_filter`](#applying-a-filter-after-the-reader-is-open), and for the same
reasons. Any column already decoded (by `prefetch=`, say) is reordered too.

It refuses, with `error stop`, when the reader **already has a sort** (add every key to one
`parquet_sortkey` instead) and when **any column has already been read** on that reader —
including through [`parquet_read_column_chunk`](reading.html#streamingchunked-reads), whose rows
were handed back in physical row-group order and cannot be reconciled with a reordering applied
afterwards.

**Apply a filter before a sort, never after.** A filter only removes rows and a sort then orders the
survivors, so a sort permutation is sized to the post-filter row count. Giving both to
`parquet_open_reader` gets this right automatically, as does `set_filter` followed by `set_sort`;
calling `parquet_reader_set_filter` on a reader that is already sorted is refused rather than
silently composing the two the wrong way round.

## What cannot be filtered or sorted: container columns and descent paths

Filtering, sorting and `qc:` all evaluate **one value per row**. Two kinds of column name have no
such value, and both are refused up front — at `parquet_open_reader`, from the schema, before any
data is read.

**A container column cannot be a filter or sort target.** A vector, a variable-length `list`, a
`map` and a `struct` taken as a whole all fail with `error stop`. The vector and list cases name
the shape:

```
filter column 'flux' is a variable-length list column; filtering only supports scalar columns
sort key 'flux' is a variable-length list column; sorting only supports scalar columns
```

A `map` or a `struct` is refused a little further along, by the type check rather than the shape
check, so its message names the type instead (`column 'attrs' has a type that filtering does not
support`). The distinction is not something you need to act on — both are refusals of the same
thing — but it explains why the wording differs.

To filter or sort on data inside a container, address a **scalar leaf** of it. A struct's fields
are reachable by [dotted path](../types/supported-data-types.html#reading-a-nested-struct-field)
and behave like any other column: `filt%add("pos.ra > 10")` is fine, and so is a sort key naming
one.

**A [descent path](../types/supported-data-types.html#descent-paths-addressing-a-containers-child)
is refused too, and permanently.** `events[].id`, `attrs{value}` and `attrs{key}` each resolve to a
perfectly good column — but one with **one entry per element**, not one per row, so there is no row
for its values to belong to. All three of `qc:`, `parquet_filter` and `sort_by=` reject one:

| where | message |
|---|---|
| `filter=` | `a descent path is not filterable: events[].id (qc: and parquet_filter apply to scalar leaves only)` |
| `sort_by=` | `a descent path cannot be a sort key: events[].id (it has one entry per element, not per row)` |
| `qc:` in a MAML | `a descent path is not a qc target: events[].id (qc: and parquet_filter apply to scalar leaves only)` |

This is a **settled restriction, not a limitation waiting to be lifted**: a per-element predicate
and a per-row result are different shapes of answer, and silently producing one where the other was
asked for would misalign that column against every other one in the same read. The `qc:` case is a
hard error rather than a skipped rule for the same reason it is elsewhere — an ignored qc rule is a
check you believe is running.

If what you want is to select rows by something inside a container, read the container column and
do it in your own code; there is no read-time equivalent.

## Renaming the columns a filter or sort refers to

`filt%remap_column_names(from, to)` and `srt%remap_column_names(from, to)` rewrite, in place, the
columns a filter's rules or a sort's keys refer to: every reference to `from(k)` becomes `to(k)`.
The two arrays are parallel and must be the same size.

```fortran
type(parquet_filter) :: filt
call filt%add("mass > 1.0e12 and (redshift < 0.5 or flag is_null)")
call filt%remap_column_names(["mass    ", "redshift"], ["m_200c  ", "z       "])
! the filter now reads: (m_200c > 1.0e12 and (z < 0.5 or flag is_null))
```

This exists for callers that build a filter in one column-name vocabulary and must apply it in
another. The main user is [`parquet_table`](../tables/table.html), which lets a program write
filters in the table's own internal names and translates them into the file's physical names — see
[Renaming a file's columns for
reading](../schema/maml-format.html#renaming-columns-for-reading-with-extra-remap) — but any
caller with the same split can use it directly.

Three properties are worth knowing:

- **It renames column references only.** The substitution happens on the *parsed* expression,
  after the parser has already separated column names from operators, keywords, parentheses and
  quoted literals — so a string value that happens to spell a column name (`name == "mass"`) is
  never touched. Sort keys keep their direction and their `nulls_first` setting.
- **It never fails on a name it does not recognize.** A `from` entry that no rule mentions is a
  no-op, and a rule naming a column absent from `from` is left alone rather than rejected: only
  the caller knows which names are supposed to exist. A rule that does not parse is also left
  untouched, so the reader that applies it still reports the parse error, with the file named.
- **All the renames in one call happen at once.** Passing `from = ["a", "b"]`, `to = ["b", "a"]`
  swaps the two columns; it does not rename `a` to `b` and then everything named `b` back to `a`.

It does `error stop` in three cases: `from` and `to` differing in size, a replacement name longer
than the 64-character column-name limit, and — filters only — a rule that fitted
`parquet_max_filter_rule_len` in its original names but no longer does once renamed.

## Random downsampling with `sample_fraction`

`parquet_open_reader(reader, filename, sample_fraction=0.1_real64)` keeps each row independently
with probability `sample_fraction` (Bernoulli sampling) — like `filter=`, it narrows what every
subsequent call sees (`parquet_get_nrows`, `parquet_read_column`, `parquet_close_reader(...,
print_stat=.true.)`, ...), with no separate "sampled count" to track:

```fortran
type(parquet_reader) :: reader
integer(int64) :: nrows
integer(int32), allocatable :: id(:)

call parquet_open_reader(reader, "data.parquet", sample_fraction=0.1_real64, sample_seed=42_int64)
call parquet_get_nrows(reader, nrows)      ! already the post-sample row count
allocate(id(nrows))
call parquet_read_column(reader, "id", id) ! already just the sampled rows
call parquet_close_reader(reader)
```

- `sample_fraction` (`real(real64)`, optional): omitted, or `>= 1.0`, reads every row — the
  current/default behavior. Must not be negative or `NaN` — either aborts immediately with `error
  stop`. Exactly `0.0` deterministically yields zero rows (not just with overwhelming
  probability).
- `sample_seed` (`integer(int64)`, optional — write the literal as `42_int64`): omitted, or
  `<= 0`, draws a fresh seed from entropy — a different sample each time you open the file. A
  positive value makes the draw reproducible:
  the same `sample_fraction`/`sample_seed` pair always selects the exact same rows. Whichever seed
  actually gets used (caller-supplied or entropy-drawn) is reported by
  `parquet_close_reader(..., print_stat=.true.)` (a `sample: fraction=... seed=...` line) — read
  it back from there to reproduce a run you didn't originally seed yourself. (A `parquet_table` is
  slightly stronger: it settles one seed when it is opened and reuses it for every reader it opens
  afterwards, so an unseeded sample stays fixed across a `%clone` — see [Filtering, sorting and
  checking rows as the file is
  opened](../tables/table-open.html#filtering-sorting-and-checking-rows-as-the-file-is-opened).
  Two separate opens still draw independently either way.) The one exception is
  `sample_fraction = 0.0`, which performs no draw at all and so reports `seed=0` whatever you
  passed — there are no rows to reproduce in that case.
- Bernoulli sampling means the matched row count fluctuates around `sample_fraction * nrows`
  rather than equaling it exactly (most noticeable on small files) — there is no "select exactly N
  rows" mode.
- Sampling and `filter=` share the same underlying mechanism: give both, and the filter is applied
  on top of the downsample (a row must pass both to be kept). Consequently, `sample_fraction <
  1.0` behaves exactly as `filter=` does throughout, even with no `filter=` given at all —
  `parquet_read_column_chunk`, `parquet_read_array_row_mode` and `parquet_read_array_element_mode`
  all stay row-group-scoped, with row indices and chunk sizes referring to the surviving rows.
- Sampling never skips I/O: every column is fully read and decoded from disk regardless of
  `sample_fraction`, and unlike `filter=` there are no statistics for a random draw to consult.
  The benefit is purely to your own code processing fewer rows afterward. (Combining
  `sample_fraction=` with `filter=` does get the filter's own row-group pruning — the two compose,
  and a sample only ever removes rows the filter already kept.)

### Which rows a seed selects

The draw is this library's own counter-based generator, and the rule is published rather than
internal — `parquet_sample_algorithm` names it, and it changes only when the rule does:

```fortran
character(len=:), allocatable :: rule
rule = parquet_sample_algorithm        ! "sample:bernoulli-u<p/philox/v1"
```

For physical row `r` (1-based — the row's position in the file as written), the rule is:

```fortran
key     = pf_random_key(seed, parquet_sample_label)
u       = pf_random_at(key, 0_int64, r)
keep    = u < sample_fraction
```

You can run exactly that yourself to work out which rows a seed will select, without opening the
file. Three consequences are worth knowing:

- **A row's keep/drop depends on nothing but the seed and the row number.** Not on how the file is
  read, not on which row groups a filter's statistics screen pruned, not on chunking. Two programs
  reading the same file with the same seed see the same rows even if one of them filters first.
- **`parquet_sample_label` keeps your own draws separate.** Passing `sample_seed=42_int64` does not
  consume or collide with the values `pf_random_at(42_int64, ...)` gives you — the sample derives
  its own stream through `pf_random_key`, so one seed can safely drive both.
- **`sample_fraction = 0.0` keeps nothing** because `u` is in `[0, 1)` and no value is below zero,
  not because of a special case.

