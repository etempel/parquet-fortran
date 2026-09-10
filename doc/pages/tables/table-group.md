---
title: Grouping rows and aggregating per group
---

`%group_by` partitions a table's rows by the values of one or more key columns and keeps the
partition as a `parquet_grouping`: an object that answers per group — the rows of a group, one row
per group, the counts, the key values as a table of their own, one statistic of one column through
`%agg` — and calls a procedure of yours once per group through `%apply`. The table itself is not
reordered, keeps its file attached and reads a column only if nothing has read it yet. It is the
per-field summary of a survey catalogue, the properties of every galaxy group in a group catalogue,
one output per observing frame.

```fortran
type(parquet_table) :: t, summary
type(parquet_grouping) :: grp
integer(int64), allocatable :: n(:)
real(real64), allocatable :: med_mag(:), blue(:), snr_p90(:)

call parquet_open_table(t, "sources.parquet")
call t%group_by("field_id,class", grp)             ! one sort of the key columns; t is left as it is
call grp%size(n)                                   ! rows per group, in group order
call grp%agg("mag_g", "median", med_mag)           ! one statistic of one column per group
call grp%agg("is_blue", "mean", blue)              ! the fraction: the mean of a logical column
call grp%agg("snr", "quantile", snr_p90, q=0.9_real64)
call grp%key_table(summary)                        ! one row per group: the field_id and class columns
call summary%add_column("n_sources", n)
call summary%add_column("median_mag_g", med_mag)
call summary%add_column("blue_fraction", blue)
call summary%add_column("snr_p90", snr_p90)
call parquet_write_table(summary, "field_summary.parquet")
```

Every procedure on this page is a **read** of the table: none reorders it, touches a row, detaches
it from its file or advances `%generation()`.

## The call

```fortran
call t%group_by(keys, grp, [dropna], [threads])   ! keys: a character array, primary first
call t%group_by("field_id,class", grp, [dropna])  ! or one separated string
```

Square brackets mark optional arguments; the comma stays outside the bracket. `keys` are column
**names**, not sort keys, primary first when there are several:

- **Any column `%argsort_by` accepts as a key is accepted here**: the numeric kinds, strings, the
  temporal elements, logicals. A vector, list, map or struct column is refused, naming it and its
  kind, and so is a name that carries a direction (`"-id"`, `"id desc"`): the group order is a
  contract (below), not an option, so there is no `descending=` and nothing to spell.
- **`dropna`** (default `.true.`): a row whose value is Null in *any* key column belongs to no
  group. With `dropna=.false.` a Null is one more key value: the rows null in a key form one group
  for that key, placed last. **A NaN is a value**, so it forms one group under either setting.
- **`threads`** goes to the sort, exactly as `%argsort_by`'s does; absent means the sort's
  automatic policy.
- `grp` is replaced by the call: building into an object that already holds a grouping does not
  need a `%clear()` first. An empty table gives a grouping with zero groups, not an error.

**The order is a contract.** Groups come in ascending key order — the sort's, so for each key in
turn: the values ascending, then the NaN group, then (with `dropna=.false.`) the Null group — and
within a group the rows are in ascending row order. The first is what makes a grouped summary
"sorted by the keys" for free; the second is what a procedure can rely on (`rows(1)` is a group's
lowest row). Every group has at least one row.

## What a grouping answers

```fortran
n  = grp%ngroups()               ! integer(int64); 0 before a build, or over an empty table
n  = grp%nrows()                 ! rows that belong to some group: fewer than t%nrows() under dropna
n  = grp%max_size()              ! rows in the largest group
n  = grp%nkeys()                 ! how many key columns
call grp%key_names(names)        ! the key column names, in key order
call grp%size(counts)            ! counts(ngroups): rows per group; int32 or int64
call grp%rows(g, rows)           ! the rows of group g, ascending; g int64, rows int32 or int64
call grp%csr(offsets, rows)      ! the whole partition, int64: group g is rows(offsets(g):offsets(g+1)-1)
call grp%first_rows(rows)        ! one row per group: its LOWEST row index; int32 or int64
call grp%last_rows(rows)         ! one row per group: its HIGHEST row index
call grp%group_ids(codes)        ! codes(t%nrows()): each table row's group, 0 for none; int32 or int64
ok = grp%is_current()            ! .false. once the table's %generation() has moved; never aborts
call grp%clear()                 ! back to the never-built state
```

- **`%csr` is the hot-loop form**: one copy, then you walk it. `offsets` is 1-based, `ngroups + 1`
  long, its last entry `nrows + 1` — `[1]` for an empty grouping, so a loop over the groups runs
  zero times. `%rows(g, rows)` allocates per call and is the readable form for a few groups;
  `%gather` (below) is the same loop over a column's values.
- **`%first_rows` and `%last_rows` are how `first` and `last` work for every column kind**: the
  row indices, then `call t%get_slice(name, parquet_slice_list(rows), vals)`. They are the
  minimum and maximum of each group's rows, computed, never read off the ends of the sort.
- **`%group_ids`** is the join-back key for anything computed per group: `codes(i)` is the group
  row `i` belongs to, so a per-group array indexed by it is a per-row array.
- Every count or index that can exceed `int32` comes in both kinds; the group number `g` is
  `int64` only.

## The key table, and the summary pattern

```fortran
call grp%key_table(out, [size_name])   ! one row per group: the key columns, in group order
call grp%count(name, out)              ! non-null rows of `name` per group; int32 or int64
```

`%key_table` is one row per group carrying each key column's value, kind, width and unit — a
gathered **copy**, so the source table is untouched — and, with `size_name=`, an `int64` column of
rows per group. It is an ordinary table: sort it, filter it, write it, and above all `%add_column`
the per-group answers onto it, which is the summary pattern the opening example shows. The
per-group answers come from `%size`, `%count` and `%apply` below.

`%count` counts a column's non-null rows per group, for a column of any kind, and reads the
column from the file if nothing has yet.

```fortran
call grp%nunique(name, out, [dropna])  ! distinct values of `name` per group; int32 or int64
```

`%nunique` counts a column's distinct values per group, for any column `%argsort_by` can order:
it is one more sort, over the keys and then the column, so that distinctness is the sort's own
equality — every NaN is one value, and a null is not counted unless `dropna=.false.`, when it is
one more.

## One statistic per group: `%agg`

`%agg` computes one statistic of one column for every group, with the whole vocabulary of
[array statistics](../utilities/statistics.html), its null and NaN rules and its weights:

```fortran
call grp%agg(name, stat, out, [weights], [weight_column], [q], [ddof], [method], [scale], [threads])
call grp%agg(name, stat, out, [threads])            ! out integer(int64): the exact family
call grp%agg(name, func, out, [weights], [weight_column], [threads])   ! a procedure of yours, below
```

`out` is allocated by the call, one entry per group in group order, zero-length for an empty
grouping. Each token is the named `pf_*` procedure applied to the group's values, so a group's
answer is that procedure's answer over the same rows, bit for bit, with the same options:

| token | what it is | behind it |
|---|---|---|
| `"size"` | rows in the group | — |
| `"count"` | non-null rows (and, with weights, rows of positive weight) | `pf_count_valid` |
| `"sum"`, `"mean"` | the sum and the mean | `pf_sum`, `pf_mean` |
| `"var"`, `"std"`, `"sem"` | the variance, standard deviation and standard error, `ddof=` charged (default 1) | `pf_variance`, `pf_stddev`, `pf_sem` |
| `"min"`, `"max"`, `"range"` | the smallest and largest value, and their difference | `pf_moments` |
| `"median"`, `"quantile"`, `"iqr"` | the order statistics; `"quantile"` needs `q=` on a 0–1 scale; `method=` as `pf_quantile`'s | `pf_median`, `pf_quantile`, `pf_iqr` |
| `"mad"` | the median absolute deviation, scaled unless `scale="raw"` | `pf_mad` |
| `"first"`, `"last"` | the first and last non-null value in row order | — |
| `"nunique"` | the distinct values, as `%nunique` counts them | — |

- **The column** is a scalar numeric or logical one; a logical column counts as 0 and 1, so its
  `"mean"` is its fraction of `.true.` — the `blue_fraction` of the opening example. Any other
  kind is refused naming it: for the first or last value of any kind use `%first_rows` or
  `%last_rows` with `%get_slice`; for the counts, `%count` and `%nunique` take every kind.
- **Nulls leave the population**, as everywhere in the statistics module, and a NaN is skipped
  by that module's default. An undefined statistic is a NaN, never an abort: a group of one has a
  NaN `"std"`, an all-null group a NaN `"mean"`.
- **`weights=`** holds one `real64` weight per *table* row, gathered per group beside the
  values; **`weight_column=`** names a scalar numeric column of the table instead, widened to
  `real64`, with a null weight read as zero — a missing membership probability is no membership.
  A zero weight removes its row from the population; a negative, NaN or infinite weight aborts
  naming the row, before any group is computed. Giving both is refused, and so are weights on a
  statistic they cannot affect (`"size"`, `"nunique"`, `"first"`, `"last"`).
- **An option a token does not take is refused rather than ignored**: `q=` belongs to
  `"quantile"`, `method=` to `"median"`, `"quantile"` and `"iqr"`, `ddof=` to `"var"`, `"std"` and
  `"sem"`, `scale=` to `"mad"`.
- **`threads=` absent is automatic** here, unlike `%apply`: the loop is library code, so it runs
  on the automatic team under the table tier's cap (`parquet_set_table_threads`), serially inside a
  parallel region, and the answer is the same at every thread count. The groups are dealt out
  dynamically; each group's statistic is computed by one thread on one buffer.

### The exact family

Declaring `out` as `integer(int64)` selects the statistics that are exact on an integer column,
and keeps them exact: `"size"`, `"count"`, `"nunique"`, `"sum"`, `"min"`, `"max"`, `"first"` and
`"last"`, on an integer or logical column, nulls left out and nothing passed through a real64
buffer — an `int64` identifier above 2**53 comes back as it went in, where the real64 form would
round it. The exact `"sum"` **aborts on overflow**, naming the group; it never wraps, because a
wrapped sum is an in-range number nothing downstream can question. `"min"`, `"max"`, `"first"`
and `"last"` of a group with no non-null value abort too, since no exact answer exists (the real64
form gives NaN there). A real column is refused: its statistics are real.

```fortran
integer(int64), allocatable :: n(:), total(:)
call grp%agg("n_visits", "sum", total)         ! exact, aborts rather than wraps
call grp%agg("object_id", "nunique", n)
```

### A procedure over one column's values

The third form hands a procedure of yours **one group's values of one column**, widened to
`real64` exactly as the statistics module widens every kind, and takes back one value. Its
interface is published as `parquet_group_column_reduce_i`:

```fortran
function func(values, is_valid, weights) result(r)
    real(real64), intent(in) :: values(:)             ! the group's values, in row order
    logical, intent(in), optional :: is_valid(:)      ! present only when the group holds a null
    real(real64), intent(in), optional :: weights(:)  ! present only when %agg was given weights
    real(real64) :: r
end function func
```

`is_valid` and `weights` arrive exactly when they mean something — test `present()` and nothing
else — so a null-free group takes your fast path. A robust scatter estimate is then two lines,
with the statistics module doing the ordering:

```fortran
function robust_mad(values, is_valid, weights) result(r)
    real(real64), intent(in) :: values(:)
    logical, intent(in), optional :: is_valid(:)
    real(real64), intent(in), optional :: weights(:)
    real(real64) :: r, med
    call pf_median(values, med, is_valid=is_valid, weights=weights)
    call pf_median(abs(values - med), r, is_valid=is_valid, weights=weights)
end function robust_mad

call grp%agg("mag_g", robust_mad, scatter)
```

A procedure that needs the exact integer values, a string, or several columns at once uses
`%apply` below and reads the columns through its own pointers. The threading contract is
`%apply`'s: `threads=` absent means serial, and giving it is your statement that the procedure may
be called from several threads at once.

## One answer per group: `%apply`

`%apply` calls a procedure of yours once per group, handing it **the group number and the group's
rows, and nothing else**. The library gathers, widens and copies nothing and does not know which
columns the procedure reads: it reads them through `%col` pointers or `%get` arrays it holds, and
indexes them with `rows` directly — `sum(lum(rows))`. Nothing in an `%apply` moves a row, so a
`%col` pointer taken before the loop stays valid throughout it. Several passes over the group, a
sorted copy of one column, an iterative solver, a bootstrap: all ordinary Fortran inside the
procedure.

The procedure comes in two shapes, and can be an object instead of a procedure:

```fortran
call grp%apply(func, out, [threads])          ! one real64 per group: out(ngroups), allocated here
call grp%apply(func, nout, out, [threads])    ! nout real64 per group: out(nout, ngroups), allocated here
call grp%apply(reducer, out, [threads])       ! the same two shapes with an object of yours
call grp%apply(reducer, nout, out, [threads]) ! extending parquet_group_reducer (below)
```

- **`out` is allocated by the call**, sized `ngroups` or `(nout, ngroups)`, even for zero groups
  — a loop over `size(out)` runs zero times rather than touching an unallocated array. With zero
  groups the procedure is never called.
- **`out(nout, ngroups)` is column-major on purpose**: one group's results are contiguous, so
  `out(k, :)` is the k-th result over all groups and goes straight into `%add_column`.
- **`g` is passed** so a procedure can index a per-group input of its own — a per-group
  parameter, a seed stream `seed + g` — and so a procedure with no context at all is a plain
  module procedure.
- **`size(rows) >= 1` always**: every group has at least one row, and `rows` is ascending.
- The procedure is called **once per group, in group order when the loop is serial**; on a team
  the order is unspecified and each result lands at its own `g`, so the answer does not depend
  on the schedule.
- `nout` below 1 is refused; so is `threads` below 1.

### The procedure form

The two interfaces, published by `parquet_tables` (and by `parquet`) as
`parquet_group_reduce_i` and `parquet_group_apply_i`:

```fortran
function func(g, rows) result(r)             ! the one-value form: parquet_group_reduce_i
    integer(int64), intent(in) :: g          ! the group number, 1 .. ngroups
    integer(int64), intent(in) :: rows(:)    ! the group's table rows, ascending; size(rows) >= 1
    real(real64) :: r
end function func

subroutine func(g, rows, out)                ! the nout-value form: parquet_group_apply_i
    integer(int64), intent(in) :: g
    integer(int64), intent(in) :: rows(:)
    real(real64), intent(out) :: out(:)      ! size(out) == nout on every call
end subroutine func
```

The result is `real64` in both: widen an integer count; for an exact integer answer index the
column yourself over `%csr`. Whatever context the procedure needs — the columns, a cosmology, a
tolerance — is yours to hold, in module variables as the opening example does.

**Write the procedure as a module procedure, not an internal one.** An internal procedure
(one after a `contains` inside a program or another procedure) is the convenient spelling under
some compilers and crashes under others before it is even called: flang cannot pass one as a
callback at all on some platforms. A module procedure with its context in module variables runs
everywhere. When the context is more than a couple of variables, or two different contexts are
needed in one program, the object form below is the better spelling in any case.

### The object form: `parquet_group_reducer`

The second way to give `%apply` a procedure with context is an **object**: extend the abstract type
`parquet_group_reducer`, put the context — `%col` pointers, tunables, a seed — into components,
implement the one deferred binding `reduce`, and pass the object where the procedure would go.

```fortran
module dispersion_mod
    use iso_fortran_env, only: int64, real64
    use parquet_tables, only: parquet_group_reducer
    implicit none

    ! Luminosity-weighted velocity dispersion per group. The columns are pointer components,
    ! which is what %col hands back, and the tunable is a plain component.
    type, extends(parquet_group_reducer) :: dispersion_reducer
        real(real64), pointer :: vel(:) => null()
        real(real64), pointer :: lum(:) => null()
        real(real64) :: scale = 1.0_real64
    contains
        procedure :: reduce => dispersion_reduce
    end type dispersion_reducer
contains
    subroutine dispersion_reduce(self, g, rows, out)
        class(dispersion_reducer), intent(in) :: self
        integer(int64), intent(in) :: g, rows(:)
        real(real64), intent(out) :: out(:)
        real(real64) :: wsum, vmean
        wsum  = sum(self%lum(rows))
        vmean = sum(self%vel(rows) * self%lum(rows)) / wsum
        out(1) = self%scale * sum(self%lum(rows) * (self%vel(rows) - vmean)**2) / wsum
        if (size(out) > 1) out(2) = real(size(rows), real64)
    end subroutine dispersion_reduce
end module dispersion_mod

program example
    use parquet
    use dispersion_mod
    implicit none
    type(parquet_table) :: t
    type(parquet_grouping) :: grp
    type(dispersion_reducer) :: red
    real(real64), allocatable :: res(:, :)

    call parquet_open_table(t, "galaxies.parquet")
    call t%group_by("id_group", grp)
    call t%col("vel", red%vel)                   ! live pointers into the table's storage
    call t%col("lum", red%lum)
    red%scale = 2.5_real64
    call grp%apply(red, 2, res, threads=8)       ! res(2, ngroups): the object form, on a team
end program example
```

- **One `reduce` serves both output shapes.** The one-value form calls it with the one-element
  section `out(g:g)`, so a reducer that tests `size(out)` before writing a second entry, as the one
  above does, works in both.
- **`self` is `intent(in)`, on purpose.** A `reduce` that assigns a component does not compile,
  so a reducer cannot accumulate across groups and cannot race when `threads=` opens a team. A
  reducer that needs scratch declares locals; one that must write outside itself does so through a
  pointer component's target, visibly.
- **Two contexts in one program are two objects** — two cosmologies, two tunings — which module
  variables cannot express. This, rather than taste, is what decides between the forms.
- **It runs under every supported compiler**, because the context lives in components rather than
  in a host: it is the form to lead with for any procedure that needs context.
- The type carries no components and no finalizer, so an extension may be a module variable, a
  local or an array element.

### Running the loop on a team

`threads=` **absent means serial** on `%apply`, a deliberate exception to the library's automatic
threading, because the library cannot know whether your procedure can be called from several
threads at once. Giving `threads=` is your statement that it can — locals only, shared state
read-only, no `save` variable, nothing written outside its own result — and the loop then runs on
a team of that size, clamped to the processors this process may use. The groups are dealt out
dynamically, since they are unequal; each result lands at its own `g`, so the answer is the same at
every thread count. A `%col` pointer is live storage that nothing here moves, so reading the table
through pointers from every thread is exactly the safe case.

Two consequences worth knowing:

- **A threaded procedure that cannot answer returns a NaN** (or a sentinel of its own) and the
  caller inspects `out` afterwards. It should not `error stop`: an abort raised from inside a team
  leaves the exit status nondeterministic under some compilers, and the library cannot wrap it. A
  serial `%apply` may abort freely.
- **Your procedure may open its own team** — a bootstrap over replicates, say — but that is then a
  nested region, and OpenMP runs a nested region on one thread unless nesting is enabled and the
  active-level limit raised (`omp_set_max_active_levels(2)` alongside `omp_set_nested(.true.)`,
  before any region opens). Threading the groups is usually the better place to spend the team.

### A bootstrap error per group

The matrix form, `pf_random_resample` and the group number as the replicate stream give a per-group
statistic with its bootstrap error in a dozen lines, reproducible from the seed alone:

```fortran
subroutine mass_with_error(g, rows, out)
    integer(int64), intent(in) :: g, rows(:)
    real(real64), intent(out) :: out(:)
    integer(int64), allocatable :: idx(:)
    real(real64) :: m(NBOOT)
    integer(int64) :: b, n
    n = size(rows, kind=int64)
    out(1) = mass_of(rows)                                  ! the statistic on the group itself
    allocate(idx(n))
    do b = 1_int64, NBOOT
        call pf_random_resample(idx, n, SEED, g * NBOOT + b)    ! n draws from 1..n, replicate b of group g
        m(b) = mass_of(rows(idx))                           ! the same kernel on a resample
    end do
    out(2) = sqrt(sum((m - sum(m) / NBOOT)**2) / NBOOT)
end subroutine mass_with_error
```

`rows(idx)` is the resample as a row list, so `mass_of` reads the columns through the same
pointers either way. See [Drawing WITH replacement](../utilities/random.html#drawing-with-replacement-pf_random_resample)
for the resample's contract.

### Many-to-many membership: `%explode` first

A grouping is one group per row. A catalogue whose rows list *several* groups each — a galaxy with
a `group_ids` vector column — is grouped after `%explode` has made one row per (galaxy, group)
pair with the group id a scalar column; then `%group_by` on that column is the membership, and
`%apply` sees each galaxy once per group it belongs to. See
[Repeating rows: `%explode`](table-mutate.html#repeating-rows-explode).

## Back onto the rows, and into your buffer: `%broadcast` and `%gather`

```fortran
call grp%broadcast(per_group, per_row, [fill], [threads])  ! per_group(ngroups) -> per_row(t%nrows()); real64 or int64
call grp%gather(name, g, buf, n, [is_valid])               ! group g's values of `name` into buf(1:n); allocates nothing
```

`%broadcast` is pandas' `transform`: `per_row(r) = per_group(g)` for every row `r` of group `g`,
and `fill` — absent: a quiet NaN for `real64`, 0 for `int64` — on a row that belongs to no group.
It is how a per-group answer reaches the rows: the velocity dispersion of the group beside every
member, the radius of each galaxy's own group, or "subtract each object's own group median", which
is two calls:

```fortran
call grp%agg("mag", "median", med)      ! one per group
call grp%broadcast(med, med_of_row)     ! one per row; a row in no group gets the NaN fill
call t%get("mag", mag)
mag = mag - med_of_row
```

`per_group` holds exactly one value per group, in group order — what `%size`, `%agg` and `%apply`
give — and any other length is refused naming both counts. The loop threads automatically
(`threads=` as on `%agg`); every grouped row is written once, by its own group, so the answer is
the same at every thread count.

`%gather` is the values form of the `%csr` loop: group `g`'s values of a column into the first `n`
entries of a buffer **you own**, in row order, for a buffer of `int32`, `int64`, `real32`, `real64`
or `logical`, widening exactly as `%get` does — an `int32` column into an `int64` buffer, a
`float32` column into a `real64` one — and nothing else. Size the buffer once by `%max_size()`: the
call allocates nothing, leaves the entries past `n` as they were, and **aborts rather than
truncates** when the buffer is shorter than the group, naming both sizes, because a statistic over
the first `size(buf)` rows of a group is a plausible wrong answer. `is_valid`, a logical buffer under
the same rule, receives for each entry whether the value is non-null; without it a null row's entry
is whatever the column stores there.

```fortran
real(real64), allocatable :: v(:)
logical, allocatable :: ok(:)
integer(int64) :: g, n

allocate(v(grp%max_size()), ok(grp%max_size()))
do g = 1_int64, grp%ngroups()
    call grp%gather("vel", g, v, n, is_valid=ok)
    ! v(1:n) are the group's velocities in row order, ok(1:n) which of them are values
end do
```

It replaces `%get_slice(name, parquet_slice_list(rows), vals)` per group, which allocates `vals` on
every call, and is what a loop over `%rows` that wants values rather than indices reaches for. A
string, temporal or vector column keeps `%get_slice`.

## When a grouping goes stale

A grouping describes the table *as it was built*. It stamps the table's `%generation()` when it is
built and compares it on every per-group query, so after any row-structural change — `%filter_rows`,
`%sort_by`, `%top_n`, `%delete_rows`, `%truncate`, `%append`, `%explode`, `%drop_duplicates` — a
query **aborts**, naming the table and both generations, instead of handing out in-range row
numbers that name the wrong rows (and, through `%apply`, a plausible mass from the wrong galaxies).
`%is_current()` is the question without the abort; `%group_by` again is the remedy.

A change that moves no row leaves the grouping usable: `%set`, `%set_null`, `%fillna`, and an
`%add_column` that fits inside the room `%reserve_columns` made. An `%add_column` that has to
relocate the column slots advances the generation, and so stales it, like a row change. The five
questions about the object itself (`%ngroups`, `%nrows`, `%max_size`, `%nkeys`, `%key_names`)
answer without the check.

The raw permutation from `%argsort_by(group_offsets=)` is the same partition without the object,
and without the check: its [stale-permutation warning](table-mutate.html#grouping) is what this
object exists to make loud.

## What it costs

Grouping costs one sort of the key columns, plus, under `dropna`, one pass over the groups that asks
each key column whether the group's representative row is null. Each per-group query is one pass
over the partition, and `%key_table` one gather per key column. Each `%agg` statistic costs one
pass over the column into a per-thread buffer the size of the largest group, allocated once, and
one call of the statistic per group; `%nunique` costs one more sort. `%apply` costs whatever your
procedure costs, times the number of groups, divided by the team you asked for. `%broadcast` costs
one pass over the rows and `%gather` one copy of the group's rows into your buffer; neither
allocates per group. The object holds two `int64` arrays the length of the grouped rows and the
group count — nothing per group, and nothing of the table's columns.
