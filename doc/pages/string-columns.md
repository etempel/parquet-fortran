---
title: Compact string columns with parquet_string_column
---

`parquet_strings` is a small, self-contained module for storing large collections of
variable-length strings compactly, in the same Arrow-like layout Parquet uses for
`BYTE_ARRAY`/`STRING` columns. It is designed for tens to hundreds of millions of rows and
multi-gigabyte payloads, with a minimal allocation count and good cache locality.

It is an **independent module** at its core — `use parquet_strings` depends only on
`iso_fortran_env` and `iso_c_binding`, nothing else in this library — but its `parquet_string_column`
type is also wired directly into [`parquet_read_column`/`parquet_write_column`/`parquet_read_column_chunk`/
`parquet_write_column_chunk`](reading.html#streamingchunked-reads) as an alternative to a padded
`character(len=...)` array; see
[Reading and writing compact string columns](#reading-and-writing-compact-string-columns) below.

To use it, add parquet-fortran as an FPM dependency (see
[Minimal setup to depend on this library](../index.html#minimal-setup-to-depend-on-this-library) in
the README for the `fpm.toml` snippet) and `use parquet` — the compact read/write entry points and
both types (`parquet_string_column` and `parquet_string`) are re-exported from the main module, so
every feature below (`find`/`contains`, the interop hooks, everything) is reachable through the
types themselves once you have them in scope. A separate `use parquet_strings` is only needed if
you want this module *without* the rest of the library — e.g. a project that wants compact string
storage but not the Arrow/Parquet C++ dependency `parquet` (via `parquet_bindings`) pulls in;
`parquet_strings` on its own depends on nothing but `iso_fortran_env`/`iso_c_binding`.

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

Handles are cheap (~16 bytes each) and, because they resolve lazily, they survive the column
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

> Note: `call col%get(i, s, allow_null=.true.)` returns `s` as an empty string, not an
> unallocated one, purely by choice (to keep this behavior unchanged from before `get` became
> a subroutine) — `s`'s `intent(out)` status would technically allow leaving it unallocated to
> signal a null. Detect nulls with `is_null()` either way.

## Trimming on append

By default, `append_string` (and `set`) store the string **verbatim**, preserving every space.
Two optional flags opt into trimming:

```fortran
call col%append_string("  hi  ")                 ! stored as "  hi  " (verbatim, default)
call col%append_string("  hi  ", strip=.true.)   ! stored as "hi"   (both ends)
call col%append_string("  hi  ", trim=.true.)    ! stored as "  hi" (trailing only)
```

`strip` removes leading **and** trailing blanks; `trim` removes trailing blanks only; when both
are given, `strip` wins. To trim a whole column that is already built, use `strip_all()` (both
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

## Modifying a column

```fortran
call col%set(i, "new value")   ! replace element i (clears its null status)
call col%erase(i)              ! remove element i, shifting later elements down
call col%append_column(other)  ! append every element (and null) from another column
```

`set` is O(length) when the replacement is the same length, otherwise O(N) (it shifts the
payload tail). `erase` compacts immediately and preserves order (O(N)). `append_column`
bulk-copies the payload and merges the validity bitmaps; it never re-trims.

## Ownership: clone, move, swap

```fortran
b = a%clone()          ! independent deep copy; mutating one never affects the other
call b%move_from(a)    ! transfer a's buffers into b in O(1); a becomes a valid empty column
call a%swap(b)         ! exchange contents in O(1); a%swap(b) == b%swap(a)
```

`move_from` and `swap` move buffers rather than copying them, so transferring gigabytes of
string data is O(1). `clone` is the only one that copies the payload.

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
handing data to code that expects plain Fortran strings — not the performance path (it pads, so
it uses more memory than the packed buffer). A null element error stops unless you pass
`null_value`:

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
| `set(i, …)` | No (content of `i` changes; the handle to `i` stays valid) |
| `get`, `view`, comparisons, `find`, size queries, `print` | No (read-only) |
| `erase(j)` | Handles at index `>= j` (the element is gone/shifted) |
| `clear` | **Yes** (the column is now empty) |
| `move_from` / `swap` | **Yes** for handles into the emptied/swapped column |
| the column going out of scope / being finalized | **Yes** (dangling) |

## Thread safety

The column follows a many-readers **xor** single-writer model with no internal locking. All
read-only operations (`get`, `view`, `length`, the comparisons, `find`, the size queries,
`print`/`summary`/`statistics`, `validate`) write nothing, so concurrent readers are safe as
long as no thread mutates the column concurrently. Any mutation must be externally
synchronized, and a handle must not be used across a mutation on any thread.

**Past gfortran/OpenMP runtime caveat — now worked around in this module's source.** The rule
above is about *sharing one column* across threads; for a while this project also had to warn
about something else: each thread having its own, fully independent `parquet_string_column`
could still silently corrupt memory under concurrent execution on some gfortran/OpenMP builds.
Root-caused to a specific, still-open gfortran bug
([PR113797](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=113797); related:
[PR97977](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=97977)): the compiler's codegen for
*receiving* a `character(len=:), allocatable` function result uses a hidden length-tracking
variable that isn't always properly thread-local. Confirmed with a minimal reproducer using no
code from this library — the hazard needed nothing more than a `character(len=:), allocatable`
function called concurrently on a type with two or more allocatable components (this type has
three: `offsets`, `data`, `validity`); it did not depend on construction/destruction, `class()`
dispatch, or sharing, and did not affect non-`character` allocatable results.

The fix: **every accessor that used to return `character(len=:), allocatable` as a function
result is now a subroutine** that writes into an `intent(out)`/`intent(inout)` allocatable
`character` argument instead — `get`, `summary`, and the `parquet_string` handle's `to_string`
(the three that were directly exposed), which sidesteps the defective codegen path entirely
rather than working around a symptom. This was independently verified: the identical reproducer
converted from a function to a subroutine reproduced **zero** failures across tens of thousands
of iterations, vs. hundreds of failures per 8000 for the function form. `to_character`, `print`,
and the handle's `print` were already subroutines and needed no signature change, only an
internal update to call the now-subroutine `get`. `find`, `contains`, `startswith`, `endswith`,
`equals` were never exposed in the first place — they compare bytes directly against the stored
buffer and never returned an allocatable `character` result.

No special precaution is needed for concurrent, independent `parquet_string_column` use as a
result — the "many-readers xor single-writer" rule at the top of this section remains the only
thread-safety rule that applies.

## Complexity at a glance

| Operation | Complexity |
|---|---|
| `size`, `capacity`, `character_size`, `null_count`, `is_null`, `is_empty`, `length`, `view` | O(1) |
| `append_string` / `append_null` | amortized O(1) |
| `append_column(other)` | O(other rows + other chars) |
| `get(i)` | O(length) |
| `equals` / `contains` / `startswith` / `endswith` (one element) | O(length) |
| `find` | O(rows × avg length) |
| `set` (different length), `erase`, `strip_all`, `trim_all`, `clone`, `to_character`, `shrink_to_fit` | O(N) |
| `move_from`, `swap`, `clear` | O(1) |

## Reading and writing compact string columns

`parquet_write_column`/`parquet_read_column` and their chunked counterparts
(`parquet_write_column_chunk`/`parquet_read_column_chunk`) accept a `parquet_string_column`
directly, as an alternative to a padded `character(len=...)` array — see
[Reading a column](reading.html)/[Writing a column](writing.html) for the general read/write API;
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
  [`parquet_get_string_length`](reading.html) has no role here) ahead of time.
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
! streaming write, one row group at a time
type(parquet_string_column) :: chunk

call parquet_new_row_group(writer, nrows)
call chunk%append_string(...)          ! exactly nrows rows for this row group
call parquet_write_column_chunk(writer, "name", chunk)
call parquet_finish_row_group(writer)

! streaming read, one row group at a time
call parquet_read_column_chunk(reader, "name", row_group, chunk)   ! cleared, then filled
```

## Interop hooks for the read/write path

Two advanced procedures expose the internal buffers that back the read/write integration above —
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

These reference only `iso_c_binding`, keeping the module independent of the rest of this
library — `parquet_write.f90`/`parquet_read.f90` (the read/write integration layer) are the only
callers.

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
