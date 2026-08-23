---
title: Compact string columns with parquet_string_column
---

`parquet_strings` is a small, self-contained module for storing large collections of
variable-length strings compactly, in the same Arrow-like layout Parquet uses for
`BYTE_ARRAY`/`STRING` columns. It is designed for tens to hundreds of millions of rows and
multi-gigabyte payloads, with a minimal allocation count and good cache locality.

It is an **independent module** at its core — a program whose only import is `use parquet_strings`
compiles two Fortran files and never reaches this library's C++ bindings (the *package* still links
Arrow; see [Choosing a module](../operating/choosing-a-module.html)). Beyond `iso_fortran_env` and
`iso_c_binding` it reaches
only `parquet_settings_base` (a leaf holding the two settings it honours: the `verbosity` its print
procedures obey, and the thread cap described under
[Threading inside one column](#threading-inside-one-column)) and `omp_lib` under `-fopenmp`. Its
`parquet_string_column` type is also wired directly into
`parquet_read_column`/`parquet_write_column`/`parquet_read_column_chunk`/
`parquet_write_column_chunk` as an alternative to a padded `character(len=...)` array; see
[Reading and writing compact string columns](#reading-and-writing-compact-string-columns) below.

To use it, add parquet-fortran as an FPM dependency (see
[Minimal setup to depend on this library](../../index.html#minimal-setup-to-depend-on-this-library) in
the README for the `fpm.toml` snippet) and `use parquet` — the compact read/write entry points and
both types (`parquet_string_column` and `parquet_string`) are re-exported from the main module, so
every feature below (`find`/`contains`, the interop hooks, everything) is reachable through the
types themselves once you have them in scope. A separate `use parquet_strings` is only needed if
you want this module *without* the rest of the library — e.g. a project that wants compact string
storage but not the Arrow/Parquet C++ dependency `parquet` (via `parquet_bindings`) pulls in. That
build really does link without Arrow, and a lint check keeps it that way.

## Why not an array of allocatable strings?

The obvious representation of a string column is one allocatable string per row:

```fortran
type :: row_string
    character(len=:), allocatable :: value
end type
type(row_string), allocatable :: names(:)   ! one heap allocation PER row
```

That costs one allocation and one pointer indirection per element, scatters the strings across
the heap (poor cache locality), and scales badly to hundreds of millions of rows.

`parquet_string_column` instead stores every string end-to-end in **one contiguous byte
buffer**, with an **offsets array** marking the boundaries (and a lazily allocated, bit-packed
**validity bitmap** for nulls):

```
offsets = [0, 3, 8, 9]          data = "abchellox"
                                        ^  ^    ^^
row 1 = data(1:3)  = "abc"      offsets(i)+1 : offsets(i+1)
row 2 = data(4:8)  = "hello"
row 3 = data(9:9)  = "x"
```

This is exactly Arrow's `LargeUtf8` layout (int64 offsets), so it is memory-efficient,
cache-friendly, and cheap to hand to a Parquet writer without rebuilding each string.

## The two types

- **`parquet_string_column`** — the owning container. Holds all the bytes; you append to it,
  index into it, and hand it around. Every row/character index is `integer(int64)` internally,
  and the public index arguments accept both `integer(int32)` and `integer(int64)`.
- **`parquet_string`** — a lightweight, **non-owning handle** to one element of a column (a
  column reference plus an index). It resolves lazily, so it stays valid across appends to the
  column. Use it to embed a "string field" in your own row type without copying bytes.

## Quick start

```fortran
program strings_quickstart
    use parquet_strings
    use iso_fortran_env, only: int64
    implicit none

    type(parquet_string_column) :: names
    character(len=:), allocatable :: s
    integer(int64) :: i

    call names%append_string("Alice")
    call names%append_string("Bob")
    call names%append_null()             ! a missing value (not the same as "")
    call names%append_string("")         ! an empty string

    print *, "rows:", names%size()               ! 4
    print *, "chars:", names%character_size()     ! 8
    print *, "nulls:", names%null_count()         ! 1

    do i = 1, names%size()
        if (names%is_null(i)) then
            print *, i, "<null>"
        else
            call names%get(i, s)
            print *, i, '"' // s // '"'
        end if
    end do
end program strings_quickstart
```

`get(i, s)` writes the string into `s`, a fresh `character(len=:), allocatable`. For read-only
access without allocating a copy per row, use a handle (next section).

## Mapping a row type: the array-of-structs pattern

A common pattern is a row type with an allocatable string field:

```fortran
type :: my_type
    integer :: id
    real    :: age
    character(len=:), allocatable :: name
end type
```

To store the `name` field compactly across many rows, keep the names in one column and let each
row hold a **handle** into it. The column must be declared with the `target` attribute and must
outlive the handles:

```fortran
use parquet_strings
use iso_fortran_env, only: int64

type(parquet_string_column), target :: col_name   ! owns ALL names (declare it a target!)

type :: my_row
    integer              :: id
    real                 :: age
    type(parquet_string) :: name    ! a handle referring into col_name
end type

type(my_row), allocatable :: table(:)
integer(int64) :: i, n
```

**Row → column ingestion** (e.g. one Parquet row group at a time):

```fortran
! reserve once up front, then append allocation-free
call col_name%reserve(n_rows_estimate, n_chars_estimate)
do i = 1, n
    if (have_name(i)) then
        call col_name%append_string(raw_name(i))
    else
        call col_name%append_null()
    end if
end do

! build the row array; each row's name is a lightweight handle (no copy)
allocate(table(n))
do i = 1, n
    table(i)%id   = raw_id(i)
    table(i)%age  = raw_age(i)
    table(i)%name = col_name%view(i)     ! O(1); stays valid across later appends
end do
```

The `table(i)%name = col_name%view(i)` loop above is the general form (useful when the row
array isn't populated all at once, or the column and row array grow independently). When the
column is already fully populated and `table` already has the right size, `view_all` does the
same mapping in one call:

```fortran
call col_name%view_all(table(:)%name)   ! table(i)%name = col_name%view(i), for every i
```

It aborts if `size(table)` doesn't match `col_name%size()`, rather than silently filling only
the shorter length. `col_name` must still be a `target`, exactly as for `view`.

`view_slice` is `view_all` restricted to a contiguous row range `[first, last]` (1-based,
inclusive) — useful for mapping only part of an already-populated column into handles:

```fortran
type(parquet_string) :: page(10)
call col_name%view_slice(21_int64, 30_int64, page)   ! page(1) = col_name%view(21), ..., page(10) = col_name%view(30)
```

It aborts if `size(page)` doesn't match `last - first + 1`, same convention as `view_all`, and
if `[first, last]` isn't a valid non-empty range within the column (`first < 1`, `last >
col_name%size()`, or `first > last`).

**Column → row usage / materialization:**

```fortran
character(len=:), allocatable :: nm

do i = 1, size(table)
    if (table(i)%name%is_null()) then
        call use_missing(table(i)%id)
    else
        call table(i)%name%to_string(nm)      ! one allocation, the string
        call use_name(table(i)%id, nm)
    end if
end do

! zero-copy inspection with no per-row allocation:
do i = 1, size(table)
    if (.not. table(i)%name%is_null() .and. table(i)%name%startswith("Dr ")) then
        call flag_doctor(table(i)%id)
    end if
end do
```

Handles are cheap (24 bytes each — a column pointer plus an index) and, because they resolve
lazily, they survive the column
growing/reallocating during the build loop. See
[Handle lifetime rules](#handle-lifetime-rules) for exactly when a handle becomes invalid.

## Null values versus empty strings

A **null** (missing value) and an **empty string** `""` are kept strictly distinct:

| Predicate on element `i` | null element | empty string `""` |
|---|---|---|
| `is_null(i)` | `.true.` | `.false.` |
| `is_empty(i)` | `.true.` (by default) | `.true.` |
| `length(i)` | `0` (by default) | `0` |
| `get(i, s)` | **error stops** by default | `s` allocated, length 0 |
| `equals(i, "")` | `.false.` | `.true.` |

Reading the content of a null is treated as a programmer error and **error stops by default**,
so a missing value is never silently confused with data. You choose how to handle nulls
explicitly:

```fortran
call col%get(i, s)                        ! error stops if element i is null
call col%get(i, s, null_value="<NA>")     ! returns "<NA>" for a null
call col%get(i, s, allow_null=.true.)     ! suppresses the abort, returns "" for a null

if (col%is_null(i)) ...                   ! the always-safe guard
```

The lenient predicates (`length`, `is_empty`, and the comparison functions) return a sensible
default for a null (`0`, `.true.`, and `.false.` respectively). Pass `check_null=.true.` to make
a null an error instead:

```fortran
n = col%length(i)                     ! 0 for a null
n = col%length(i, check_null=.true.)  ! error stops on a null
```

> Note: `call col%get(i, s, allow_null=.true.)` returns `s` as an **empty string**, not an
> unallocated one. `s`'s `intent(out)` status would technically allow leaving it unallocated to
> signal a null, and that is deliberately not done: an unallocated result is indistinguishable
> from one the callee never reached. Detect nulls with `is_null()`.

## Trimming on append

By default, `append_string` (and `set`) store the string **verbatim**, preserving every space.
Two optional flags opt into trimming:

```fortran
call col%append_string("  hi  ")                 ! stored as "  hi  " (verbatim, default)
call col%append_string("  hi  ", strip=.true.)   ! stored as "hi"   (both ends)
call col%append_string("  hi  ", trim=.true.)    ! stored as "  hi" (trailing only)
```

`strip` removes leading **and** trailing blanks; `trim` removes trailing blanks only; when both
are given, `strip` wins.

**This type's verbatim default is deliberate, and differs from `parquet_column`/`parquet_table`.**
Here you hand over one string at a time and its length is exactly what you wrote, so there is
nothing to guess. Those two take whole `character(len=*)` **arrays**, whose elements share one
declared length and are blank-padded by Fortran — so they trim, because the padding cannot have
been meant. See [table.md](../tables/table.html#string-columns-in-a-table).

**The two bulk forms follow the array rule, not the scalar one**, for exactly that reason:

```fortran
call col%build_from(values)                  ! replaces the column; every element trimmed
call col%append_values(more)                 ! extends it; same rule
call col%build_from(values, is_null=mask)    ! mask entries become zero-width nulls
```

`values` is a `character(len=*)` array, so each element's trailing blanks are dropped — matching
`parquet_column%set_all`/`%append_values` on a string column, which reach these same two bulk
forms internally. Prefer these over a loop of `append_string` whenever the whole array is in
hand: filling a million-row column this way measured **about six times faster**, because the
per-element form re-derives the trim, re-checks capacity, and copies the payload through a
temporary each time.
`build_from` replaces the column's whole contents; `append_values` leaves what is already there
alone. Both take an optional `is_null` mask of the same length as `values`. To trim a whole column that is already built, use `strip_all()` (both
ends) or `trim_all()` (trailing only) — both operate in place and skip null elements:

```fortran
call col%strip_all()   ! strip every non-null element in place
```

## Searching and comparison

Comparisons operate directly on the stored bytes without allocating:

```fortran
if (col%equals(i, "Alice"))        ...   ! exact by default
if (col%equals(i, "Alice ", exact=.false.)) ...  ! trailing-trim both sides, Fortran == semantics
if (col%startswith(i, "Al"))       ...
if (col%endswith(i, "ce"))         ...
if (col%contains(i, "lic"))        ...
```

`find` returns the 1-based index of the first matching element (byte-exact by default), or `0`
if none. It never matches a null. Pass `exact=.false.` to trailing-trim both sides, or
`reverse=.true.` to scan from the last row backward (returning the last match):

```fortran
j = col%find("Bob")                     ! first exact match, or 0
j = col%find("Bob", reverse=.true.)     ! last match
j = col%find("Bob ", exact=.false.)     ! trailing-blanks ignored on both sides
```

The same operations are available on a handle: `h%equals(str)`, `h%startswith(prefix)`,
`h%endswith(suffix)`, `h%contains(str)`.

`copy_to(i, dest)` copies an element into a **fixed-length** slot you already have — a
`character(len=N)` array being filled row by row, or a scratch buffer reused across a loop — instead
of allocating a string sized to the element the way `get` must:

```fortran
character(len=32) :: slot
call col%copy_to(i, slot)                      ! blank-padded to len(slot)
call col%copy_to(i, slot, allow_null=.true.)   ! a null yields blanks
```

It follows Fortran's own assignment semantics exactly, so it is a drop-in for
`call col%get(i, s); slot = s`: a short value is blank-padded and one longer than the slot is
truncated. Size the slot from `length(i)` or from `statistics(max_len=...)` first if you must not
truncate — neither of those allocates either.

`append_from(src, i)` is the matching primitive on the writing side: it appends element `i` of
another column, **null state included**, without materialising the value.

```fortran
do k = 1, n                       ! copy the rows you want into a new column
    call picked%append_from(col, keep(k))
end do
```

`src` must not be the same object as the destination — Fortran forbids passing one object as both
an `intent(inout)` and an `intent(in)` argument of the same call, and no compiler diagnoses it.
Appending a column to itself is `append_column`'s job, on a copy.

`compare(i, j)` orders two elements of the same column against each other, returning `-1` when `i`
sorts first, `+1` when `j` does, and `0` when they are equal. It is exactly Fortran's own `<` on the
two values — the shorter one is compared as though padded with blanks, so `"ab"` equals `"ab  "` and
sorts before `"abc"` — so it can replace a `%get`-both-then-compare without changing any answer:

```fortran
if (col%compare(i, j) < 0) ...          ! same as: get both, then a < b
```

The point of it is that **it allocates nothing**. A scan for the smallest and largest value that
fetches every element through `%get` pays one heap round-trip per row; tracking the two winning
*indices* through `compare` and fetching only those two at the end pays none — which is how
`parquet_table%print_stat` reports a string column's min and max. Nulls are not special-cased: a
null is zero-width, so it compares as an empty string and sorts first. Test `%is_null` yourself if
you need them ordered otherwise, exactly as you would around `%get`.

## Modifying a column

```fortran
call col%set(i, "new value")   ! replace element i (clears its null status)
call col%set_null(i)           ! set element i to null, discarding its content
call col%erase(i)              ! remove element i, shifting later elements down
call col%append_column(other)  ! append every element (and null) from another column
```

`set` is O(length) when the replacement is the same length, otherwise O(N) (it shifts the
payload tail). `set_null` is `set`'s null counterpart: it shrinks element `i`'s payload to zero
width and marks it invalid, same O(N) tail-shift cost as a `set` that shortens an element.
Calling it on an already-null element is a harmless no-op past the first call (`null_count()`
isn't double-counted). `erase` compacts immediately and preserves order (O(N)). `append_column`
bulk-copies the payload and merges the validity bitmaps; it never re-trims.

`set_null` is also available on a `parquet_string` handle, as a **write-through**: it mutates the
*referenced column*, not just the handle, exactly like every other handle accessor resolves live
against the column rather than holding independent state:

```fortran
type(parquet_string) :: h
h = col%view(i)
call h%set_null()             ! col%is_null(i) is now .true. -- observable through col directly too
```

This is the intended way to neutralize a handle before it goes stale (e.g. before an `erase`
shifts indices, or before gathering it with `build_from` below) — see
[Gathering handles into a column: build_from](#gathering-handles-into-a-column-build_from).

## Bulk row-set operations

`set`/`set_null`/`erase` above act on one element. Four operations act on the whole row set at
once, each rebuilding the column in a single pass rather than repeating a per-element shift:

```fortran
call col%reindex(perm)          ! reorder: result element k is the old element perm(k)
call col%delete_by_mask(keep)   ! keep only elements whose mask entry is .true., in order
call col%gather(idx)            ! keep the listed elements, in the listed order; repeats allowed
call col%append_nulls(n)        ! append n null elements, growing capacity once
```

**Prefer these to a loop.** `delete_by_mask` is the bulk counterpart of `erase`: deleting `m`
elements one at a time costs O(m × nchars), this costs O(nchars) once. `append_nulls(n)` is
`append_null` n times with one capacity growth instead of n.

`reindex` validates `perm` completely before touching a buffer — length, range and no duplicates —
so a bad permutation aborts with the column unchanged. `gather` checks only the range, deliberately:
refusing repeats would need a seen-set sized by the column on every call, which is the cost this
primitive exists to avoid. So `idx` may name an element more than once, and the result may be
shorter than, as long as, or longer than the column it replaces. `append_nulls(0)` is a no-op; a
negative count aborts.

**All three of `reindex`, `delete_by_mask` and `gather` invalidate every outstanding handle** —
see [Handle lifetime rules](#handle-lifetime-rules). All three are threaded on a large enough
column, along with `build_from`, `strip_all`/`trim_all`, `to_character` and `statistics` — see
[Threading inside one column](#threading-inside-one-column).

`%empty()` is the obvious companion query: `.true.` when the column holds no rows.

## Ownership: clone, move, swap

```fortran
b = a%clone()          ! independent deep copy; mutating one never affects the other
call b%move_from(a)    ! transfer a's buffers into b in O(1); a becomes a valid empty column
call a%swap(b)         ! exchange contents in O(1); a%swap(b) == b%swap(a)
```

`move_from` and `swap` move buffers rather than copying them, so transferring gigabytes of
string data is O(1). `clone` is the only one that copies the payload.

## Extracting a row range: slice

`slice` materializes an independent, owning copy of a contiguous row range `[first, last]`
(1-based, inclusive) into a destination column — useful for splitting one large, already-populated
column into row-group-sized pieces for a chunked write:

```fortran
type(parquet_string_column) :: chunk
call col%slice(21_int64, 30_int64, chunk)   ! chunk = independent copy of rows 21..30
```

`dest` (`chunk` above) is cleared first, then filled; `col` itself is left unchanged. Like `clone`,
`slice` copies the payload bytes (one bulk copy) and rebases the offsets — it is not a zero-copy
subview, because a subview's offsets would still need rebasing to be usable by anything downstream
(e.g. the Parquet writer via `raw_buffers`), which costs the same as also copying the bytes, at the
cost of a dangling-reference hazard if the source column is mutated afterward.

`[first, last]` must be a valid, non-empty range: `first >= 1`, `last <= col%size()`, and
`first <= last` — there is no "empty slice" call shape, same as `append_string`/`append_null` have
no no-op mode either.

A `slice` result must be assigned into a local variable before being passed on — `slice` is a
subroutine, not a function, so there is no expression form to chain inline. It fills a destination
you own, which is what lets one buffer serve a whole chunking loop instead of a fresh column being
built per row group; `%clone`, which has no such loop to serve, is a function. (No procedure in
this module returns `character(len=:), allocatable` — that shape is forbidden project-wide, for a
reason unrelated to either: see
[Thread safety](../operating/thread-safety.html#a-note-on-functions-returning-characterlen-allocatable).)

```fortran
type(parquet_string_column) :: chunk
integer(int64) :: chunk_start

chunk_start = 1_int64
do while (chunk_start <= col%size())
    call col%slice(chunk_start, min(chunk_start + group_size - 1_int64, col%size()), chunk)
    call parquet_write_column_chunk(writer, "name", chunk)
    chunk_start = chunk_start + group_size
end do
```

Use `view_slice` (see [Mapping a row type](#mapping-a-row-type-the-array-of-structs-pattern)
above) instead when you only need to read/iterate a range without copying; use `slice` when an
independent, self-contained buffer is actually needed (e.g. handing a range to the writer, as
above).

## Gathering handles into a column: build_from

`build_from` is the reverse of `view_all`/`view_slice`: instead of scattering a column's elements
into an array of handles, it **gathers** an array of independently-obtained `parquet_string`
handles — potentially referring into different, unrelated columns, or the same column at
non-contiguous indices — into a single column:

```fortran
type(parquet_string_column), target :: names
type(t_row) :: rows(100)
type(parquet_string_column) :: gathered

! ... rows(:)%name populated via col%view(i) at various points, from various columns ...

call gathered%build_from(rows(:)%name)   ! gathered%get(k) == rows(k)%name%to_string(), for every k
```

`self` (`gathered` above) is cleared first, then filled in array order; a null handle becomes a null
row. Every handle is validated **before** `self` is touched, and aborts on the first handle (in array
order) that is:

- **an alias of `self` itself** — e.g. `call col%build_from(some_array_of_col_handles)` where
  the handles were obtained from `col` (via `view`/`view_all`/`view_slice`) and `self` is that
  same `col`. This is caught deliberately: `build_from` clears `self` before gathering, which
  would otherwise silently destroy the very data the handles are about to read.
- **unassociated** — a `parquet_string` that was never bound to a column via `view`.
- **stale** — a handle whose index no longer exists in its source column (e.g. after an `erase`
  shifted things). `build_from` does not skip or null out a stale handle on your behalf; call
  `set_null()` on it explicitly beforehand (see [Modifying a column](#modifying-a-column) above)
  if you want it to end up as a null row instead of aborting the whole call.

That validation pass also **sizes** the result, so the destination is allocated once rather than
grown per element, and each element's bytes are then copied straight out of its own source column.
The whole call is one pass over the handles plus one over the gathered bytes, so it runs at
roughly memory-copy speed for the payload it gathers. An element gathered twice is copied
twice — this is a gather, not a permutation, so the result may be shorter than, as long as, or
longer than any of its sources.

## Capacity management

The column grows automatically (geometric growth) as you append, so appends are amortized
O(1). When you know the rough size ahead of time, `reserve` avoids repeated reallocation — this
is the key optimization for row-group-at-a-time ingestion:

```fortran
call col%reserve(n_rows, n_chars)   ! grow capacity once (never shrinks; pass 0 for "no requirement")
! ... many appends with no further reallocation ...
call col%shrink_to_fit()            ! release unused capacity (e.g. before a long-lived hold)
```

Introspection:

```fortran
character(len=:), allocatable :: smry

print *, col%size(), col%character_size()             ! logical sizes
print *, col%capacity(), col%character_capacity()     ! allocated capacity
print *, col%null_count(), col%memory_usage()
call col%summary(smry)
print *, smry                                          ! one-line overview string
```

## Converting to a plain Fortran array

`to_character` materializes the whole column into a conventional fixed-length character array,
each element blank-padded to the longest element's length. It is a convenience escape hatch for
handing data to code that expects plain Fortran strings — **the memory cost is the point to watch,
not the speed**: every row is widened to the longest element, so a column with one long outlier
becomes far larger than the packed buffer it came from. The copy itself reads straight out of that
buffer at roughly memory-copy speed. A null element error stops unless you
pass `null_value`:

```fortran
character(len=:), allocatable :: arr(:)
call col%to_character(arr, null_value="NA")
```

## Handle lifetime rules

A `parquet_string` handle borrows from its column; it owns nothing and frees nothing. The
referenced column must be a `target` and must outlive the handle. Because the handle resolves
lazily, most column operations do **not** invalidate outstanding handles:

| Column operation | Invalidates handles? |
|---|---|
| `append_string` / `append_null` / `append_column` | **No** (indices unchanged) |
| `reserve` / `shrink_to_fit` | **No** |
| `set(i, …)` / `set_null(i)` | No (content of `i` changes; the handle to `i` stays valid) |
| `get`, `view`, `view_slice`, comparisons, `find`, size queries, `print` | No (read-only) |
| `erase(j)` | Handles at index `>= j` (the element is gone/shifted) |
| `reindex(perm)` / `delete_by_mask(keep)` / `gather(idx)` | **Yes**, every handle — each rebuilds the whole column |
| `clear` | **Yes** (the column is now empty) |
| `move_from` / `swap` | **Yes** for handles into the emptied/swapped column |
| `slice(first, last, dest)` | **Yes** for handles into `dest` (cleared first); handles into the source column are unaffected |
| `build_from(handles)` (called as `self%build_from(...)`) | **Yes** for handles into `self` (cleared first) — and calling it with a handle that itself aliases `self` aborts rather than silently corrupting it, see [Gathering handles into a column: build_from](#gathering-handles-into-a-column-build_from) |
| the column going out of scope / being finalized | **Yes** (dangling) |

## Threading inside one column

Seven operations split their work across threads when the column is large enough to be worth it —
`%reindex`, `%gather`, `%delete_by_mask`, `%build_from`, `%strip_all`/`%trim_all`, `%to_character`
and `%statistics`. Nothing needs to be asked for. `parquet_string_threads()` reports the ceiling
your machine and settings allow — not what a given call will use, since an individual operation
narrows that further by the rules below — and `parquet_set_string_threads(n)` caps it (see
[Settings](../operating/settings.html#threads-inside-one-string-column)).

Five things decide how many threads an operation uses, and the first four all decline toward serial:

- **Inside your own OpenMP parallel region it stays serial**, deliberately — `T` threads each asking
  for `T` more is slower than not threading at all, and a nested region is your business, not the
  library's. This is the same rule sorting follows.
- **A small payload stays serial.** The floor is measured in bytes of payload rather than rows,
  because that is what has to be copied.
- **Below four threads it stays serial**, which is less obvious and worth knowing: making a rebuild
  splittable costs a restructure roughly half again as slow as the single pass it replaces, so two
  or three threads cannot pay for themselves. The library declines rather than running the slower
  shape.
- **At most one thread per eight rows.** The validity bitmap packs eight rows to a byte, and thread
  boundaries have to fall on byte boundaries so that no two threads ever read-modify-write the same
  one. This is a correctness constraint rather than a tuning choice — it is what stops a split from
  silently losing a null — and on a short column it is what makes the answer serial.

- **The automatic count is capped well below what OpenMP offers.** Past a certain thread count this
  work stops scaling and starts losing ground — on a machine with several hundred logical threads,
  taking the full count measured 18–40 % *worse* than the best available. If your machine wants
  more, `parquet_set_string_threads(n)` is honoured above the default, bounded by what OpenMP offers
  and by the CPU affinity the process actually has — see [Thread
  placement](../operating/performance.html#thread-placement-omp_places-and-omp_proc_bind) for that
  second bound, which applies to every thread count in the library and warns once when it bites.

**The result is byte-identical whatever the thread count** — same values, same offsets, same nulls.
Threading here is purely a wall-clock control, never a change of answer. For calibration: a modest
gain on a small machine, rising to several times on a large one, with larger columns gaining more
than smaller ones on the same hardware. `tools/benchmark_strings.sh` measures it on yours (see
CONTRIBUTING.md).

## Thread safety

The column follows a many-readers **xor** single-writer model with no internal locking. All
read-only operations (`get`, `view`, `length`, the comparisons, `find`, the size queries,
`print`/`summary`/`statistics`, `validate`) write nothing, so concurrent readers are safe as
long as no thread mutates the column concurrently. Any mutation must be externally
synchronized, and a handle must not be used across a mutation on any thread.

This is the only thread-safety rule specific to this type. A separate, library-wide compiler caveat
around functions returning `character(len=:), allocatable` applies here as it does everywhere else,
and is covered in the main
[Thread safety](../operating/thread-safety.html#a-note-on-functions-returning-characterlen-allocatable) guide.

## Complexity at a glance

| Operation | Complexity |
|---|---|
| `size`, `capacity`, `character_size`, `null_count`, `has_validity`, `is_null`, `is_empty`, `length`, `view` | O(1) |
| `reserve_validity` | O(rows/8) bytes, once |
| `view_all`, `view_slice` | O(N), O(range length) |
| `append_string` / `append_null` | amortized O(1) |
| `append_column(other)` | O(other rows + other chars) |
| `get(i)` | O(length) |
| `equals` / `contains` / `startswith` / `endswith` (one element) | O(length) |
| `compare(i, j)` | O(shorter length), no allocation |
| `copy_to(i, dest)` | O(length), no allocation |
| `append_from(src, i)` | amortized O(length), no allocation |
| `find` | O(rows × avg length) |
| `set` (different length), `set_null`, `erase`, `strip_all`, `trim_all`, `clone`, `to_character`, `shrink_to_fit` | O(N) |
| `reindex(perm)`, `delete_by_mask(keep)` | O(nchars), one pass |
| `gather(idx)` | O(selected chars), rebuilt into fresh buffers |
| `append_nulls(n)` | amortized O(n), one capacity growth |
| `empty` | O(1) |
| `slice(first, last, dest)` | O(range length + range chars) |
| `build_from(handles)` | O(sum of gathered elements' lengths) |
| `move_from`, `swap`, `clear` | O(1) |

## Reading and writing compact string columns

`parquet_write_column`/`parquet_read_column` and their chunked counterparts
(`parquet_write_column_chunk`/`parquet_read_column_chunk`) accept a `parquet_string_column`
directly, as an alternative to a padded `character(len=...)` array — see
[Reading a column](../io/reading.html)/[Writing a column](../io/writing.html) for the general read/write API;
this section only covers what's different for the compact path.

```fortran
use parquet
use iso_fortran_env, only: int64

type(parquet_writer) :: writer
type(parquet_reader) :: reader
type(parquet_string_column) :: names, names_back

call names%append_string("Alice")
call names%append_string("Bob")
call names%append_null()

call parquet_open_writer(writer, "data.parquet")
call parquet_write_column(writer, "name", names)
call parquet_close_writer(writer)

call parquet_open_reader(reader, "data.parquet")
call parquet_read_column(reader, "name", names_back)   ! cleared, then filled
call parquet_close_reader(reader)

if (names_back%is_null(3)) ...   ! nulls come back as real Nulls, not a sentinel string
```

Differences from the padded `character(len=...)` path:

- **No `is_valid`/`null_value` arguments.** `parquet_string_column` already tracks its own
  per-element null status (`%append_null()` on write, `%is_null(i)` on read) — every Null in the
  file becomes `%append_null()` in the column, and vice versa, with no separate mask to plumb
  through.
- **No pre-sizing.** On read, `values` is cleared and grown to fit — unlike a padded array, you
  never need to know the row count (or the longest string's length —
  [`parquet_get_string_length`](../io/reading.html#column-shape-and-size-queries) has no role here) ahead of time.
- **Scalar (1-D) columns only.** There is no vector/matrix `parquet_string_column` specific — a
  vector-of-strings column still needs the padded `character(len=...), dimension(:,:)` path.
- **File format is identical either way.** Parquet's `BYTE_ARRAY` physical type is always
  variable-length; the padded path's fixed width is a Fortran-side convenience that never reaches
  disk (trailing padding is trimmed before writing). A file written via one path reads back
  correctly via the other.
- **Chunked (streaming) write/read** work the same way, one `parquet_string_column` per row
  group: `values` on a chunk write must hold exactly the currently-open row group's rows (like
  every other `parquet_write_column_chunk` specific); on a chunk read, `values` is cleared and
  filled with just the requested row group's rows.

```fortran
! streaming write, one row group at a time -- names is a larger, already-populated column;
! slice (see "Extracting a row range: slice" above) carves out one row group at a time
type(parquet_string_column) :: chunk
integer(int64) :: chunk_start

chunk_start = 1_int64
do while (chunk_start <= names%size())
    block
        integer(int64) :: chunk_end
        chunk_end = min(chunk_start + group_size - 1_int64, names%size())
        call parquet_new_row_group(writer, chunk_end - chunk_start + 1_int64)
        call names%slice(chunk_start, chunk_end, chunk)   ! exactly this row group's rows
        call parquet_write_column_chunk(writer, "name", chunk)
        call parquet_finish_row_group(writer)
        chunk_start = chunk_end + 1_int64
    end block
end do

! streaming read, one row group at a time
call parquet_read_column_chunk(reader, "name", row_group, chunk)   ! cleared, then filled
```

## Interop hooks for the read/write path

Three advanced procedures expose the internal buffers that back the read/write integration above —
ordinary users of `parquet_write_column`/`parquet_read_column` never need to call them directly:

- `raw_buffers(offsets_ptr, data_ptr, validity_ptr, nrows, nchars, has_validity)` — exports
  `c_loc` pointers to the internal offsets/data/validity buffers plus counts, for a writer to
  consume directly. The pointers are valid only until the next mutation of the column.
- `append_buffers(nrows_in, nchars_in, offsets, data, validity, offsets_int32)` — bulk-appends
  one row group straight from C buffers (offsets/data/validity from a decoded Arrow array),
  handling both int32 (`STRING`) and int64 (`LargeString`) source offsets and merging the
  validity bitmap. `offsets` must already be rebased to this chunk (its first entry must be 0,
  with `data` pointing at the payload byte that entry refers to) — a source sliced out of a
  larger buffer (e.g. an Arrow array with a non-zero `offset()`) must be rebased by the caller
  first; an un-rebased `offsets` aborts immediately rather than silently misplacing every
  element's bytes. `validity`'s bit 0 has the same requirement and is not separately guarded
  (not detectable from a raw pointer), so a source bitmap with a non-byte-aligned logical start
  must likewise be repacked by the caller before calling this.
- `copy_buffers(offsets, data)` — the **safe** counterpart to `raw_buffers`: it copies the same
  offsets and packed payload into two caller-supplied arrays (sized `size()+1` and
  `character_size()`; anything shorter aborts) instead of handing out pointers. Two `memcpy`s, and
  the result outlives the next mutation. Prefer it whenever the consumer is Fortran — `raw_buffers`
  exists for the case where a C caller must read the buffers in place, and its pointers are only
  valid while the actual argument carries `TARGET` all the way up a call chain the column cannot
  inspect. This is how a string sort key is packed.

These reference only `iso_c_binding`, keeping the module independent of the rest of this
library — `parquet_write_string.f90`/`parquet_read_string.f90` (the string read/write integration
layer) are the only callers.

## Example: word-frequency-style ingestion

A compact end-to-end example — read tokens from somewhere, store them, then query:

```fortran
program token_column
    use parquet_strings
    use iso_fortran_env, only: int64
    implicit none

    type(parquet_string_column) :: tokens
    integer(int64) :: i, first_the, n_empty

    call tokens%reserve(1000, 8000)          ! rough estimate; avoids realloc churn
    call tokens%append_string("the")
    call tokens%append_string("quick")
    call tokens%append_string("the")
    call tokens%append_string("  fox  ", strip=.true.)   ! stored as "fox"

    first_the = tokens%find("the")           ! 1
    print *, "first 'the' at row", first_the
    print *, "last  'the' at row", tokens%find("the", reverse=.true.)   ! 3
    print *, "'fox' present:", tokens%find("fox") > 0                    ! T (trimmed on append)

    n_empty = 0
    do i = 1, tokens%size()
        if (.not. tokens%is_null(i) .and. tokens%is_empty(i)) n_empty = n_empty + 1
    end do
    print *, "empty tokens:", n_empty

    call tokens%print()                      ! human-readable dump
end program token_column
```
