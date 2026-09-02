---
title: Error handling
---

This library reports failures through two distinct mechanisms, neither of which can be caught or
recovered from once it fires. The reader, writer and schema surface has no status or `ierr` return
code anywhere, so check inputs (file existence, column names, array bounds) before calling into it
if you need to avoid aborting. **The `parquet_table` layer is the exception**: every one of its
type-bound procedures that looks an existing column up by name takes an optional `found=`, and
passing it turns what would abort into a reported miss. That covers the readers (`%get`, `%col`,
`%get_slice`, `%get_element`), the mutators (`%set`, `%set_null`, `%drop_column`, `%cast` and the
rest) and the metadata queries (`%kind`, `%width`, `%unit`, `%has_nulls`) alike. See
[Asking about a column by position instead of by name](../tables/table.html#asking-about-a-column-by-position-instead-of-by-name)
for the full list and for the three name-taking calls deliberately outside the rule.

## The two failure classes

Both are genuine, immediate process terminations, and the practical advice is the same either way:
validate before calling, and do not rely on catching anything. The distinction is for **reading a
log** — it tells you which half of the library detected the problem, and the two look nothing alike.

### Fortran `error stop`

Prints your compiler's `ERROR STOP` line carrying the library's message, usually followed by a
backtrace. Both the exact prefix and whether you get a backtrace are properties of your compiler and
build flags rather than guarantees this library makes — gfortran writes `ERROR STOP <message>`, NAG
`ERROR STOP: <message>`, flang `Fortran ERROR STOP: <message>`. What is the same everywhere is the
message after it, which is the library's.

This is the class used for precondition and validation failures this library's own Fortran code
detects directly: a missing file, invalid MAML, an unknown column name, calling a reader/writer
procedure before it's open, a declared vector column's size not matching what was requested at the
schema level, asking to write a container column whose payload is itself a container (which this
library reads but does not write), and so on.

**Messages from the `parquet_table` layer carry a second prefix of their own**, so a real one reads
`parquet_table: <procedure>: <message>` after whatever your compiler puts in front. That is worth
recognising: it tells you the failure came from the table container rather than from the reader or
writer underneath it.

### A C++-level process exit

Prints a single line to stderr with no `ERROR STOP` prefix and no Fortran backtrace, always in the
form `parquet-fortran: <procedure>: <message>`:

```
parquet-fortran: parquet_read_column: type mismatch for column: d (expected int32/int64, got timestamp[us])
```

This is the class used where the failing check happens on the C++/Arrow side of the
`parquet_wrapper.cpp` boundary rather than in Fortran. It covers:

- a column's physical Parquet type not matching what you asked for (the example above — see
  [Limitations](../../index.html#limitations));
- reading a column containing a genuine Parquet Null without `null_value=`/`is_valid=` (see
  [Null values](../types/supported-data-types.html#null-values));
- exceeding the `col_size`/table-column-count/`chunk_size` int32 ceilings, and the matching ceiling
  on the entries of one `MAP` column, which Arrow has no wider offset buffer to hold (see
  [Limitations](../../index.html#limitations));
- the [concurrency guard](thread-safety.html#the-concurrency-guard) firing when a shared
  `reader`/`writer` is called from two threads at once.

### Telling them apart

Three things distinguish them, and the first is the one to reach for in a script:

| | Fortran `error stop` | C++-level exit |
|---|---|---|
| exit status | nonzero, and **never 134** | always exactly **134** |
| first line | your compiler's `ERROR STOP` prefix, then `<message>` | `parquet-fortran: <procedure>: <message>` |
| backtrace | usually | never |

**Test for 134, not for the other one.** The C++ status is exact because that path ends in an
explicit `_Exit(134)`; the Fortran one is whatever your compiler chose for `ERROR STOP` — 1 with
gfortran and flang, 2 with NAG — and the Fortran standard leaves it processor-dependent, so a script
that keys on a particular value is testing its compiler rather than this library.

**134 is also what a shell reports for a process killed by `SIGABRT`**, so a wrapper that inspects
only the exit code cannot tell a deliberate C++-side failure from a genuine crash. Read the stderr
line to be sure: a deliberate one always begins `parquet-fortran: `.


If you're contributing to this library and need to add or test one of these failure paths, see
[CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md) for
how that's done out-of-process, and for this project's own conventions around C++-level error
reporting.

## Warnings are not errors

Not everything the library disapproves of is fatal. A third outcome exists and is easy to mistake
for one of the two above when you meet it in a log: a line beginning `WARNING: `, and an exit status
of **0**. Closing a writer that declared columns and wrote none is one — every declared column is
written with 0 rows and the program carries on:

```
WARNING: parquet_close_writer: no column was written; writing every declared column with 0 rows
```

Warnings behave differently from errors in both directions that matter. They are **suppressed** at
`verbosity="errors_only"` — note that `"silent"` is *not* enough, since it quiets informational and
solicited output while leaving warnings alone — and they **follow** `message_stream`, so a program
can move them to stderr. Nothing that terminates the process can be silenced or moved either way.
See [Settings](settings.html) for the two knobs themselves.

## Contextual error messages

Once a reader/writer/schema is far enough along to know it (i.e.
`parquet_open_reader`/`parquet_open_writer` has already recorded the filename, or
`schema%init`/`parquet_schema(...)` has already named the schema), most read/write/schema-building
`error stop` messages append that context — e.g. `... (file: output.parquet, maml: my_schema.maml)`
— so a failure is identifiable even when a program has several readers/writers/schemas in play at
once. This wording isn't a fixed contract (exact phrasing may change between releases); only the
presence of file/schema context, where available, is intended to be relied on.

**Some context arrives on a second stream instead, and this catches people out.** Where naming the
file would make the abort message unreasonably long, the library prints it as its own line *before*
aborting — and those lines go to **standard output**, while the abort message itself goes to
standard error. Closing a writer with a declared column left unwritten is the case you are most likely to
meet:

```
stdout:  parquet_close_writer: output file: catalogue.parquet
stdout:  parquet_close_writer: schema: internal:demo
stderr:  ERROR STOP parquet_close_writer: missing write for enabled column: col_c
```

(the `ERROR STOP` prefix there is gfortran's; see [Telling them apart](#telling-them-apart))

**These lines are never suppressed and never redirected**, deliberately: not even
`verbosity="errors_only"`, the strictest setting, removes them, and `message_stream` does not move
them, because silencing them would leave an abort that names no file at all. The practical consequence is the one to remember — **a program that
captures only stderr loses the filename**. Capture both streams when you want a diagnosable failure.

## Calling before open, or after close

Every procedure that takes a `parquet_reader` — all twenty-five of them, from `parquet_read_column`
and `parquet_prefetch_columns` through the metadata queries to `parquet_reader_set_filter` and
`parquet_release_column` — checks that the reader is open before doing anything else, and every
procedure taking a `parquet_writer` does the same. Calling one before
`parquet_open_reader`/`parquet_open_writer` fails with `error stop`, naming both the procedure you
called and the open call you missed:
`ERROR STOP parquet_read_column: reader has not been opened (call parquet_open_reader first)`. The
same applies to `parquet_close_reader`/`parquet_close_writer` themselves: closing a reader/writer
that was never opened, or that was already closed, also fails with `error stop` rather than silently
doing nothing.

## Asking instead of aborting: `found=`

The reader, writer and schema surface has no way to say "tell me rather than stopping". The
`parquet_table` layer does: pass `found=` to any call that looks an existing column up by name, as
described at the top of this page, and a miss is reported instead of aborting. It applies to a
missing column name and to an out-of-range column position alike.

```fortran
program found_or_abort
    use parquet
    use iso_fortran_env, only: real64
    implicit none
    type(parquet_table) :: t
    real(real64) :: mass(3)
    real(real64), allocatable :: got(:)
    logical :: ok

    mass = [1.5_real64, 2.5_real64, 3.5_real64]
    call parquet_new_table(t)
    call t%add_column("mass", mass)

    call t%get("flux", got, found=ok)          ! there is no "flux" column
    print *, "flux found? ", ok                ! F -- and the program is still running

    call t%get("mass", got, found=ok)
    print *, "mass found? ", ok, " rows:", size(got)
end program found_or_abort
```

Omit `found=` from that first call and the same missing name is an immediate `error stop`. That is
the whole of the difference: the argument does not change what counts as a miss, only what happens
when there is one.

## Re-opening and implicit finalization

Re-opening an already-open `reader`/`writer` variable (calling
`parquet_open_reader`/`parquet_open_writer` again on one that's already in use, without closing it
first) does **not** error and does **not** leak the old handle: Fortran automatically finalizes the
previous handle first, since `reader`/`writer` are `intent(out)` arguments of a finalizable type.
This finalization is *not* the same as an explicit `parquet_close_writer`/`parquet_close_reader`
call, though: it skips the completeness checks those perform (e.g. "every enabled schema column was
written"), so the abandoned file is not guaranteed to be a complete or valid parquet file. Always
call `parquet_close_writer`/`parquet_close_reader` explicitly when you actually want the file that
was being written/read to end up valid; rely on the implicit finalizer only as a safety net for a
variable you're discarding or reusing anyway.

**`parquet_table` inverts this advice, because there is no `parquet_close_table` to call.** A table
is finalized when it goes out of scope or is re-opened, and that is the normal path rather than a
safety net — there is nothing you were supposed to have called instead. This is safe because a
table's finalizer frees its store without validating anything, which is the rule every finalizer in
this library follows: an implicit finalizer runs at points a caller cannot see or handle, so it must
always succeed silently rather than being able to fail.
