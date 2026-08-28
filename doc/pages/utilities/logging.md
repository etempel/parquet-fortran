---
title: Logging with parquet_logging
---

`parquet_logging` is a general-purpose logging module for **your** program. Severity levels,
several destinations at once, a line layout you choose, colour, cheap level checks — and, unusually
for a Fortran logger, correct and readable output from inside an OpenMP parallel region.

It is a leaf: `use parquet_logging` compiles **one** of this library's Fortran files and never
crosses the C++ boundary. It is also available through `use parquet` like everything else.

**It is not this library's own messaging.** What parquet-fortran itself prints is governed by the
`verbosity` and `message_stream` [settings](../operating/settings.html); nothing in the library
calls this module, and turning a logger off does not silence the library or the other way round.
The two have different audiences: those settings are for what *this library* says to *you*, and
this module is for what *your program* says to *its* user.

Optional arguments are shown in square brackets: `%init([level], [console])` means both may be
omitted.

## Contents

- [Quick start](#quick-start)
- [Levels](#levels)
- [Sinks: where records go](#sinks-where-records-go)
- [The line layout](#the-line-layout)
- [Colour](#colour)
- [Building a message](#building-a-message)
- [Logging from an OpenMP parallel region](#logging-from-an-openmp-parallel-region)
- [Deduplicating a repeated record](#deduplicating-a-repeated-record)
- [Turning down a library's noise](#turning-down-a-librarys-noise)
- [Rank filtering, for MPI and worker pools](#rank-filtering-for-mpi-and-worker-pools)
- [Configuring from the environment](#configuring-from-the-environment)
- [Important behavior](#important-behavior)

## Quick start

```fortran
program simple
    use parquet_logging
    implicit none

    call pf_log_init(level = PF_LEVEL_INFO)
    call pf_log_add_file('run.log', level = PF_LEVEL_DEBUG)

    call pf_log_info("starting")
    call pf_log_debug("cache warm")                     ! file only: below the console threshold
    call pf_log_warning("no statistics in row group 4", once = .true.)
    call pf_log_flush()
end program simple
```

```
14:03:12.481 [INFO    ] starting
14:03:12.499 [WARNING ] no statistics in row group 4
```

The `pf_log_*` procedures drive one process-wide **default logger**, so code deep in a call tree can
log without a logger being threaded through every signature. Declare your own `type(pf_logger)` when
you want a second, independent destination set; every `pf_log_*` procedure has a matching binding.

## Levels

Levels **ascend** with severity and use the same numbers as Python's `logging`, so a level carried
in a shared configuration file means the same thing on both sides of a pipeline. A record is emitted
when `level >= threshold`.

| constant | value | |
|---|---|---|
| `PF_LEVEL_ALL` | 0 | a threshold admitting everything |
| `PF_LEVEL_TRACE` | 5 | finer than debug |
| `PF_LEVEL_DEBUG` | 10 | |
| `PF_LEVEL_INFO` | 20 | |
| `PF_LEVEL_WARNING` | 30 | |
| `PF_LEVEL_ERROR` | 40 | |
| `PF_LEVEL_CRITICAL` | 50 | |
| `PF_LEVEL_OFF` | 60 | a threshold admitting nothing |

An arbitrary integer is accepted too — `call lg%log(25, "...")` is legal and filters numerically,
rendering as `Level 25`. `pf_log_level_from_name` converts `"info"`, `"INFO"` or `"20"`, reporting
an unrecognised name through an optional `ok` instead of aborting, which is what reading a
configuration file needs.

**Check before building an expensive message.** `%enabled` is one integer comparison:

```fortran
if (pf_log_enabled(PF_LEVEL_DEBUG)) then
    call pf_log_debug("residuals " // trim(pf_str(sum(abs(r)))))
end if
```

## Sinks: where records go

A sink is one destination, with its own threshold, layout, colour policy, flush policy and rank
filter. A record is offered to every sink and each decides.

```fortran
type(pf_logger) :: lg
integer :: s_err

call lg%init(level = PF_LEVEL_DEBUG, name = "qspipe")   ! installs a stdout console
call lg%add_console(stream = PF_LOG_STDERR, level = PF_LEVEL_ERROR, sink = s_err)
call lg%add_file('pipeline.log', level = PF_LEVEL_DEBUG)
call lg%set_format("{level}: {message}", sink = s_err)
```

- **`%init([level], [name], [format], [console], [thread_mode])`** clears every sink and then
  installs one stdout console sink, unless `console = .false.` It is the "give me a working logger"
  call. Its `format` applies to the console sink it installs and does not become a default for
  sinks added later.
- **`%add_console([stream], [level], [format], [color], [only_rank], [sink])`**. A second console
  sink on the same stream would double every line, so it is refused.
- **`%add_file(path, [level], [format], [append], [flush], [only_rank], [sink])`**. `append`
  defaults to `.true.`, so a restarted program does not destroy the log of the run that just
  failed; pass `append = .false.` to truncate. A file that cannot be opened aborts, naming the path.
- **`%add_unit(unit, ...)`** attaches a unit you opened and still own — the logger never closes it.
  This is how a test captures output into a scratch file it can read back, and how a program that
  already manages its own output file adds logging to it.
- **`%set_level(level, [sink], [name])`** sets the logger threshold, one sink's, or a per-name
  override. Passing both `sink` and `name` is refused rather than guessed at.
- **`%set_format` / `%set_color`** with no `sink` apply to every sink *currently* attached, and do
  not become a default for later ones.
- **`%close`** closes units this logger opened and clears every sink, leaving it silent. It never
  aborts, and it is safe to call twice.

**A freshly declared `type(pf_logger)` owns no sinks and is silent.** That is what makes this module
usable *inside* a library: log into your own named logger and, until the application configures it,
you print nothing and pay one integer comparison per call site. The module's default logger is the
exception — it behaves as though it owned a stdout console at `PF_LEVEL_INFO` until you first
configure it, so a program that just calls `pf_log_info` sees output.

## The line layout

A layout is a template with named placeholders, parsed once when you set it:

```fortran
call lg%set_format("{stamp} [{level}]{name| }{context| }: {message}")
```

`{date}` `{time}` `{stamp}` `{elapsed}` `{level}` `{name}` `{thread}` `{rank}` `{context}`
`{message}`. Timestamps are ISO-8601 ordered (`2026-08-28 14:03:12.481`), so a log file sorts
chronologically as text; `{elapsed}` comes from a monotonic clock, not from wall-clock arithmetic.

**`{field|sep}` emits `sep` only when the field is non-empty.** That is what keeps an unnamed record
from leaving a stray `[]` or a doubled separator mid-line, and it is what lets an optional field sit
in the middle of a template rather than only at the end.

Two templates are provided: `PF_LOG_FMT_BRIEF` (`{time} [{level}] {message}`, the console default)
and `PF_LOG_FMT_FULL` (date, level, name, thread and context — the file default).

## Colour

Per sink: `PF_LOG_COLOR_AUTO` (the default), `_NEVER`, `_ALWAYS`. `AUTO` colours a console sink
only, and only when `NO_COLOR` is unset and `TERM` is set to something other than `dumb`. A file
sink under `AUTO` never colours, so there is nothing to strip out of a log file.

Colour is applied to the rendered `{level}` field. To colour message text yourself, use
`pf_log_color(text, code, out)` with one of the `PF_LOG_C_*` codes.

## Building a message

Fortran has no varargs, so a message is ordinary concatenation. `pf_str` renders any scalar:

```fortran
call pf_log_info("processed " // trim(pf_str(nrows)) // " rows in " // trim(pf_str(dt, '(f6.2)')) // " s")
```

It is generic over `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)` and `logical`,
each with an optional format descriptor. **The result is a fixed-length `character(len=PF_LOG_STR_LEN)`,
hence the `trim`** — a function returning a deferred-length allocatable character is not thread-safe
under gfortran, and this helper exists to be called from inside parallel regions.

## Logging from an OpenMP parallel region

**Share one logger; do not give each thread its own.** A `pf_logger` has no allocatable components
and no finalizer, which makes it safe shared, safe in `private()` and `firstprivate()`, and safe
declared inside a `block` in a region. Writes are serialised, so lines never interleave or corrupt.

```fortran
!$omp parallel do default(shared) schedule(dynamic)
do i = 1, ntiles
    call process_tile(i)
    call lg%info("tile done")
end do
!$omp end parallel do
```

**Tag each thread's records so the log is readable.** `{thread}` renders the thread number, and a
per-thread **context stack** labels records without any shared state to race on:

```fortran
!$omp parallel do default(shared) private(i)
do i = 1, ntiles
    call pf_log_push_context("tile=" // trim(pf_str(i)))
    call process_tile(i)                  ! anything it logs, at any depth, carries the tag
    call pf_log_clear_context()           ! boundary reset -- the next iteration starts clean
end do
```

**Contexts nest.** If `process_tile` pushes `"fibre=3"`, its records read `tile=17 fibre=3`; its
`pop` removes only that frame and leaves `tile=17` intact. A callee can neither see nor destroy its
caller's frame. `pf_log_pop_context` on an empty stack is a no-op, so a defensive pop costs nothing;
pass the `frame` token `pf_log_push_context` reports to have an unbalanced pair aborted at the site
instead of silently mislabelling later records.

`pf_log_set_context` sets a **shared base** tag rendered ahead of every thread's frames. It is
shared rather than per-thread because an OpenMP `threadprivate` copy is undefined in threads other
than the initial one at the start of a region — so a per-thread base set before a region would be
visible to one thread only, which reads as a bug.

**Buffered mode** keeps one thread's narrative together, at the cost of records appearing late and
of global time order across threads:

```fortran
call lg%set_thread_mode(PF_LOG_THREAD_BUFFERED)   ! BEFORE the region
!$omp parallel do
do i = 1, ntiles
    call lg%info("step 1")
    call lg%info("step 2")                        ! these two stay adjacent in the output
end do
!$omp end parallel do
call lg%flush()                                   ! AFTER the region, from one thread
```

That post-region `%flush()` is **required** — without it, whatever is still buffered is not written.
It recovers every thread's records, not just the calling thread's. `slot_bytes` sizes the per-thread
buffer, which is worth setting on a machine with hundreds of threads.

## Deduplicating a repeated record

```fortran
call lg%warning("no statistics in this row group", once = .true.)   ! emitted once per process
call lg%info("still working", every = 1000)                          ! every thousandth occurrence
```

The key is the logger name, the level and the message text — deliberately not the context, since
deduplicating across contexts is normally the point. The table is process-wide, so `once` means once
per process even from a parallel region. It is cleared only by `pf_log_reset_dedup()`; neither
`%init` nor `%close` touches it, because it is shared and one logger must not un-suppress another's
messages.

## Turning down a library's noise

Give a logger a name, and an application can silence it without knowing anything else about it —
the Fortran counterpart of `logging.getLogger('matplotlib').setLevel('INFO')`:

```fortran
call pf_log_set_level(PF_LEVEL_WARNING, name = "qfeet.io")   ! and every "qfeet.io.*" below it
```

The override applies to that name and to any name below it in dotted notation. `%enabled(level)`
cannot see a name, so it answers conservatively (it may say yes for a record an override will drop);
pass `%enabled(level, name = ...)` for the exact answer.

## Rank filtering, for MPI and worker pools

```fortran
call lg%set_rank(my_rank)
call lg%add_console(only_rank = 0)          ! console shows rank 0 only
call lg%add_file('rank.log')                ! every rank writes its own
```

The rank is whatever identity your program already has — an MPI rank, a worker index, a chunk id —
so **this adds no MPI dependency**. `PF_LOG_RANK_ANY` is the unset sentinel; adding a rank-filtered
sink before calling `%set_rank` is refused, because it would otherwise discard every record silently.

## Configuring from the environment

```fortran
call pf_log_configure_from_env()                       ! PF_LOG_LEVEL, PF_LOG_FILE, ...
call pf_log_configure_from_env("MYAPP_LOG_")           ! MYAPP_LOG_LEVEL, ...
```

| variable | effect |
|---|---|
| `<prefix>LEVEL` | the default logger's threshold, as a name or a number |
| `<prefix>FILE` | adds a file sink at that path |
| `<prefix>FORMAT` | sets the layout of every current sink |
| `<prefix>COLOR` | `auto`, `always` or `never` |

`prefix` is the whole literal prefix including its trailing separator, so a doubled separator cannot
arise. Reading is **additive and never destructive**: an unset variable changes nothing, a
set-but-empty one is ignored, and `<prefix>FILE` adds a sink beside whatever is already attached.
It is never applied implicitly — these are `PF_LOG_*` variables, not `PARQUET_FORTRAN_*` ones, and
they configure your program's logging rather than this library's behaviour. `NO_COLOR` is the one
variable honoured without an explicit call.

## Important behavior

- **Emission is thread-safe; configuration is not.** Configure a logger before entering a parallel
  region. There is no detection machinery for a violation, deliberately: a documented rule that is
  easy to follow beats a guard that cannot reliably fire.
- **Copying a logger is supported** — that is what `firstprivate` does — and every copy names the
  same units. `%close` is therefore idempotent across copies, but a write to a sink some copy has
  already closed **aborts**, naming the path, rather than going nowhere.
- **A failed write aborts for a file or unit sink and drops the sink for a console sink.** A full
  disk that silently swallowed a log leaves a run unexplainable; a closed standard output
  (`./prog | head`) must not kill the program.
- **A record is never silently lost.** A line longer than `PF_LOG_MAX_LINE` is emitted whole rather
  than truncated; a record too large for a buffered slot is written directly; a full `once=` table
  degrades to emitting every time. Every bound is a published `PF_LOG_MAX_*` parameter, and
  exceeding one is a clean error at configuration time — never a truncation at emission time.
- **The one exception is the context budget**, which saturates: past `PF_LOG_MAX_CONTEXT_DEPTH`
  frames or `PF_LOG_MAX_CONTEXT` characters, a frame's *text* is dropped while the *depth* stays
  exact, and one warning is issued. That is the safe direction — a missing frame degrades a
  diagnostic, whereas a shifted stack would tag records with the wrong context.
- **A build without OpenMP behaves identically for one thread**: `{thread}` renders `0`, buffered
  mode holds one slot, and the context stack is an ordinary saved variable.
