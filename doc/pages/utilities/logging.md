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
- [Which threshold decides](#which-threshold-decides)
- [The line layout](#the-line-layout)
- [Colour](#colour)
- [Emitting a record: the call signature](#emitting-a-record-the-call-signature)
- [Building a message](#building-a-message)
- [Logging from an OpenMP parallel region](#logging-from-an-openmp-parallel-region)
- [What every logger shares](#what-every-logger-shares)
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
when `level >= threshold` — and there is more than one threshold in play, so see
[Which threshold decides](#which-threshold-decides) for which one applies where.

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
filter. A record is offered to every sink and each decides — against **both** its own threshold and
the logger's, whichever is stricter. See [Which threshold decides](#which-threshold-decides).

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
  sinks added later. **`level =` sets the LOGGER's threshold, not that console's** — the console
  `%init` installs is left at `PF_LEVEL_ALL`, so the logger's level is what governs it. The
  distinction is invisible until a second sink exists; see
  [Which threshold decides](#which-threshold-decides).
- **`%add_console([stream], [level], [format], [color], [only_rank], [sink])`**. A second console
  sink on the same stream would double every line, so it is refused.
- **`%add_file(path, [level], [format], [append], [flush_level], [only_rank], [sink])`**. `append`
  defaults to `.true.`, so a restarted program does not destroy the log of the run that just
  failed; pass `append = .false.` to truncate. A file that cannot be opened aborts, naming the path.
  `flush_level` is described under [When records reach the disk](#when-records-reach-the-disk).
- **`%add_unit(unit, ...)`** attaches a unit you opened and still own — the logger never closes it.
  This is how a test captures output into a scratch file it can read back, and how a program that
  already manages its own output file adds logging to it.
- **`%set_level(level, [sink], [name])`** sets the logger threshold, one sink's, or a per-name
  override. Passing both `sink` and `name` is refused rather than guessed at.
- **`%set_name(name)` / `%get_name(name)`** set and read the logger's own base name, and
  **`%get_full_name(name)`** reads it composed with this thread's pushed frames — the text `{name}`
  actually renders. `pf_log_push_name`/`pf_log_pop_name` add a scoped frame to it — see
  [`name`, in more detail](#name-in-more-detail).
- **`%unset_level([name], [found])`** removes one per-name override, or every one when `name` is
  absent. Removing an override that is not there is a no-op; pass `found` to learn which it was.
- **`%set_format` / `%set_color`** with no `sink` apply to every sink *currently* attached, and do
  not become a default for later ones.
- **`%close`** closes units this logger opened and clears every sink, leaving it silent. It never
  aborts, and it is safe to call twice.
- **`%print([unit])`** writes a readable dump of the whole configuration — the logger's level, name
  and rank, every per-name override, and every sink with its own threshold, flush level, format and
  colour. Three thresholds can be in play at once, and this is the call that shows them side by
  side; reach for it whenever a record appears somewhere you did not expect, or fails to appear
  where you did.

### When records reach the disk

A file sink flushes after a record **at or above its `flush_level`**, which defaults to
`PF_LEVEL_WARNING`. A flush pushes *everything* buffered on the unit, so the moment trouble is
reported, the quieter records leading up to it reach the disk too — while bulk `INFO`/`DEBUG`
traffic avoids a per-record flush that costs several times the write itself.

- `flush_level = PF_LEVEL_ALL` flushes every record — the right choice when the program may die
  without ever logging a warning (a segfault, an external kill) and the tail of the log matters.
- `flush_level = PF_LEVEL_OFF` never flushes per record; only `%flush`, `%close` and `%fatal` do.
- `%flush`, `%close` and `%fatal` always flush everything, whatever the sinks' levels.
- A **console** sink always flushes — it is interactive, and output that appears late reads as a
  hang. A **unit** sink defaults to flushing every record too, because the caller interleaves
  their own writes on that unit; pass `flush_level` to opt out.

**A freshly declared `type(pf_logger)` owns no sinks and is silent.** That is what makes this module
usable *inside* a library: log into your own named logger and, until the application configures it,
you print nothing and pay one integer comparison per call site. The module's default logger is the
exception — it behaves as though it owned a stdout console at `PF_LEVEL_INFO` until you first
configure it, so a program that just calls `pf_log_info` sees output.

## Which threshold decides

There are two thresholds between a record and a sink, and they compose with **`max`** — the
stricter one wins. Each is a filter the record has to pass, so adding one can only ever remove
records, never restore them.

```
written to sink S  ⟺  level ≥ max( rule(name) or logger_level ,  S%level )
                      AND S's rank filter matches, if it has one
```

**The left operand is the logger side.** It is the logger's own threshold — set by `%init(level =)`
or `%set_level(level)` — *unless* a per-name rule matches the record's name, in which case the rule
**replaces** it outright. Not a min, not a max: the longest matching rule's level is used instead of
the logger's. A record with no name, or a name no rule covers, uses the logger's level.

**The right operand is the sink's own threshold**, set by `%add_*(level =)` or
`%set_level(level, sink =)`. Nothing on the logger side can lower it — a name rule governs which
records the logger *offers*, never which ones a sink *accepts*.

**`%init(level =)` sets the logger's threshold, and the console it installs is left at
`PF_LEVEL_ALL`.** So `%init(level = PF_LEVEL_DEBUG)` gives you `max(DEBUG, ALL)` = `DEBUG` on that
console, which looks exactly like having set the console's level — and stops looking like it the
moment a second sink appears:

| | logger | sink 1 | sink 2, added at `ALL` | a `TRACE` record reaches |
|---|---|---|---|---|
| `%init(level = DEBUG)` | `DEBUG` | `ALL` | `ALL` | nothing — the logger floors both |
| `%init()` then `%add_console(level = DEBUG)` | `ALL` | `DEBUG` | `ALL` | sink 2 |

So: put the level on the **logger** for "this program logs at DEBUG", a single floor under
everything and the thing per-name rules override. Put it on a **sink** for "the terminal shows
warnings, the file keeps everything". To let the sinks decide alone, leave the logger at
`PF_LEVEL_ALL`.

A worked example, with every combination on one line each — logger `INFO`, one sink at `DEBUG`, and
a rule putting `deep` at `TRACE`:

| record | logger side | sink | threshold | result |
|---|---|---|---|---|
| `TRACE`, name `deep` | 5 (the rule) | 10 | `max(5, 10)` = 10 | dropped |
| `DEBUG`, name `deep` | 5 (the rule) | 10 | 10 | **written** |
| `DEBUG`, name `other` | 20 (the logger) | 10 | `max(20, 10)` = 20 | dropped |
| `INFO`, name `other` | 20 (the logger) | 10 | 20 | **written** |

The rule pulled `deep` below the logger's `INFO`, but the sink's own `DEBUG` still blocked its
`TRACE`. That is the usual reason a lowered rule appears to reach one sink and not another.

**A per-name rule applies to dotted children, and the longest match wins.** A rule on `"a"` governs
`a`, `a.x` and `a.x.y`, but not `ab` — the dot is required. With rules on both `"a"` and `"a.b"`, a
record named `a.b.c` uses `"a.b"`'s level, whichever rule was set first.

**One `min` appears in this module and is not a threshold.** The cached `min_level` answers "could
this record reach *any* sink?", so it is a minimum over sinks (and over rules) used to reject a
hopeless record with one integer comparison. A record that passes it still faces the `max` above,
per sink.

## The line layout

A layout is a template with named placeholders, parsed once when you set it:

```fortran
call lg%set_format("{stamp} [{level}]{name| }{context| }: {message}")
```

`{date}` `{time}` `{stamp}` `{elapsed}` `{level}` `{name}` `{thread}` `{rank}` `{context}`
`{message}`. Timestamps are ISO-8601 ordered (`2026-08-28 14:03:12.481`), so a log file sorts
chronologically as text; `{elapsed}` comes from a monotonic clock, not from wall-clock arithmetic.

**`{field|sep}` emits `sep` immediately BEFORE the field, and only when the field is non-empty.**
That is what keeps an unnamed record from leaving a stray `[]` or a doubled separator mid-line, and
it is what lets an optional field sit in the middle of a template rather than only at the end.

**`sep` is a separator, not a default** — it is *prepended to* the value, never *substituted for* a
missing one. `{name|x}` on a logger named `general` renders `xgeneral`, not `general`; and on an
unnamed record it renders nothing at all, not `x`. There is no "value when absent" syntax, and that
is deliberate: a placeholder that silently substituted something else would make a missing field
indistinguishable from a field whose value happened to equal the substitute. If you want a literal
that always appears, put it outside the braces — `[{name}]` — and accept the empty brackets.

The three cases side by side. **Every `.` below is a space — in the layouts as well as in the
output**, since a separator that *is* a space is otherwise invisible in the very column that is
meant to show it. `{level}` pads to 8.

```
layout                       separator      named "general"        unnamed
{level}{name|.}: {message}   one space      INFO.....general:.hi   INFO....:.hi
{level}.{name}: {message}    none           INFO.....general:.hi   INFO.....:.hi   <- stray space
{level}{name|x}: {message}   the letter x   INFO....xgeneral:.hi   INFO....:.hi
```

**Rows 1 and 2 render identically when the name is present, and that is the point rather than a
mistake.** Both put exactly one space before the name — row 1 from the separator inside the braces,
row 2 from a literal space outside them — so with a name there is nothing to tell them apart. They
diverge only when the name is empty: row 1 drops the separator *with* the field, while row 2's
literal space has nothing to do with the field and is stranded, leaving `INFO.....:.hi` with a space
before the colon that no longer separates anything.

Row 3 is the same mechanism with a visible separator, and is there to show that `sep` is prepended
rather than substituted: `x` + `general`, and nothing at all when there is no name.

Two templates are provided: `PF_LOG_FMT_BRIEF` (`{time} [{level}] {message}`, the console default)
and `PF_LOG_FMT_FULL` (date, level, name, thread and context — the file default).

## Colour

Per sink: `PF_LOG_COLOR_AUTO` (the default), `_NEVER`, `_ALWAYS`. `AUTO` colours a console sink
only, and only when `NO_COLOR` is unset and `TERM` is set to something other than `dumb`. A file
sink under `AUTO` never colours, so there is nothing to strip out of a log file.

Colour is applied to the rendered `{level}` field, on this fixed mapping:

| level | colour | constant |
|---|---|---|
| `PF_LEVEL_CRITICAL` | bright red | `PF_LOG_C_BRIGHT_RED` |
| `PF_LEVEL_ERROR` | red | `PF_LOG_C_RED` |
| `PF_LEVEL_WARNING` | yellow | `PF_LOG_C_YELLOW` |
| `PF_LEVEL_INFO` | *none* | — |
| `PF_LEVEL_DEBUG` | cyan | `PF_LOG_C_CYAN` |
| `PF_LEVEL_TRACE` | dim | `PF_LOG_C_DIM` |

`INFO` is deliberately uncoloured: it is the ordinary case, and colouring it would colour every
line, which is what the colouring exists to avoid.

To colour message text yourself, use `pf_log_color(text, code, out)`:

```fortran
character(len=:), allocatable :: hot

call pf_log_color("42 rows dropped", PF_LOG_C_RED, hot)
call pf_log_warning("screening: " // hot)
```

It is a subroutine with an `intent(out)` allocatable result rather than a function, for the reason
given under [Building a message](#building-a-message) below.

Eight codes are published, and they are the ANSI SGR numbers as plain strings:

| constant | code | colour |
|---|---|---|
| `PF_LOG_C_RED` | `31` | red |
| `PF_LOG_C_GREEN` | `32` | green |
| `PF_LOG_C_YELLOW` | `33` | yellow |
| `PF_LOG_C_BLUE` | `34` | blue |
| `PF_LOG_C_MAGENTA` | `35` | magenta |
| `PF_LOG_C_CYAN` | `36` | cyan |
| `PF_LOG_C_BRIGHT_RED` | `91` | bright red |
| `PF_LOG_C_DIM` | `2` | dim (not a colour: reduced intensity) |

Being plain strings, they are a convenience rather than a closed set — `pf_log_color` accepts any
ANSI code, so `pf_log_color(text, "38;5;208", out)` gives 256-colour orange. `GREEN`, `BLUE` and
`MAGENTA` are exported for your use and are read by nothing in this module.

**Colouring message text yourself is independent of a sink's policy**, which governs only the
`{level}` field. Set the sink to `PF_LOG_COLOR_NEVER` if you want your own codes and no others,
and be aware that codes you embed reach a file sink as escape sequences whatever the policy says.

## Emitting a record: the call signature

Every emission procedure takes the same five arguments, and only the first is required. The
brackets mark the optional ones; they are not part of the syntax.

```fortran
call pf_log_info(text, [name], [context], [once], [every])       ! and _trace/_debug/_warning/
call lg%info     (text, [name], [context], [once], [every])      !     _error/_critical
call pf_log      (level, text, [name], [context], [once], [every])   ! explicit level
call lg%log      (level, text, [name], [context], [once], [every])
```

| argument | type | what it does |
|---|---|---|
| `text` | `character(len=*)` | the message. The only required argument |
| `level` | `integer` | **`pf_log`/`%log` only** — the severity, where the named procedures supply their own |
| `name` | `character(len=*)` | the record's name, overriding the logger's `%set_name` for this one call. Renders as `{name}`, and selects which per-name level override applies |
| `context` | `character(len=*)` | the record's context, overriding the ambient context stack for this one call. Renders as `{context}` |
| `once` | `logical` | `.true.` emits this record once per process and never again |
| `every` | `integer` | emit only every n-th occurrence of this record |

**Every optional argument is best passed by keyword.** `name` and `context` are both
`character(len=*)` in adjacent positions, so a positional second argument is a `name` whether or not
that is what was meant — `call pf_log_trace(msg, "deep")` sets the name, and there is no way for the
compiler to tell you if you wanted a context.

Two things the argument list deliberately does **not** have. There is no *value* argument — Fortran
has no varargs, and an unlimited-polymorphic one is indistinguishable from `name` at the same
position, so a message is built by concatenation with `pf_str` (see
[Building a message](#building-a-message)). And there is no *destination* argument: which sinks a
record reaches is a property of the sinks, not of the call.

### `name`, in more detail

A record's name says **which part of your program is speaking**. It is set at two levels, the
per-call argument winning:

```fortran
call lg%set_name("solver")                        ! every record from this logger
call lg%info("converged", name = "solver.newton") ! this record only
```

It does two things: it renders as `{name}` in the layout, and it selects the per-name level
override that applies to the record — see
[Turning down a library's noise](#turning-down-a-librarys-noise). A name it has no rule for simply
uses the logger's own threshold.

**A subprogram can add to the name without knowing what its caller chose**, by pushing a frame for
the duration of a call:

```fortran
subroutine io_read(...)
    integer :: frame
    call pf_log_push_name("io", frame)   ! "prog" -> "prog.io" for records from here down
    ...
    call pf_log_pop_name(frame)
end subroutine
```

Frames nest (`prog.io.stat`), they are joined with dots, and the composed name is what a per-name
override matches — so an application can turn one subprogram up to `PF_LEVEL_TRACE` while the rest
of the program stays quiet, and that subprogram needs to know nothing about it.

- **`frame` is an optional balance token.** Push reports the depth it created; pop asserts the
  frame on top is still yours and aborts if it is not. Without it, a callee that pushed and forgot
  to pop would have *its* frame silently removed by your pop while yours leaked on — a bug that
  then surfaces nowhere near its cause. Casual use can omit it on both calls.
- **`pf_log_clear_names()`** empties the stack from any depth. It is a *boundary* reset, not the
  counterpart of a push: put it at the top of a loop body and the body is self-healing whatever a
  callee left behind. `pf_log_name_depth()` reports the current depth.
- **Popping an empty stack is a no-op**, so a defensive pop in cleanup code is safe.
- **The stack is per thread, and it is not per logger** — every logger in the process renders the
  frames the calling thread has pushed, so a frame pushed inside a parallel region is private to the
  thread that pushed it but shared by every logger on it (see
  [What every logger shares](#what-every-logger-shares)). Push a frame *before* a region and only
  the initial thread sees it — a logger-wide name belongs on the logger, via `%set_name`; only
  dynamic scope belongs on the stack.
- **One segment per push, and an over-long name aborts.** A frame containing a dot is refused, and
  a composed name longer than `PF_LOG_MAX_NAME` (64) aborts rather than being truncated: a
  shortened name would silently change which override matches.
- **An explicit per-call `name=` replaces the stack rather than composing with it**, the same rule
  `context=` follows.
- **`%get_name(name)`** reports the logger's own base — what `%set_name` set, without the calling
  thread's frames. Prefer `push_name` where the addition is scoped to a call; reach for `get_name`
  when you genuinely need to read the base and compose something yourself.
- **`%get_full_name(name)`** reports the composed name instead — base plus this thread's frames,
  exactly the text `{name}` renders and exactly what a per-name override is matched against, so it
  is the call that answers "why did my override not fire?". Both have `pf_log_*` forms. The answer
  is per thread, and a composed name too long to fit aborts here just as it would on emission,
  rather than reporting a name the logger would never use.

**An override works in both directions.** It can make a name stricter than the logger, and it can
make one *more verbose* — which is how a single subsystem is turned up for debugging while the rest
of the program stays quiet:

```fortran
call pf_log_init(level = PF_LEVEL_DEBUG)
call pf_log_set_level(PF_LEVEL_TRACE, name = "deep")   ! "deep" alone drops to TRACE
call pf_log_trace("...", name = "deep")                ! emitted
call pf_log_trace("...", name = "other")               ! dropped: no rule, logger is at DEBUG
call pf_log_set_level(PF_LEVEL_DEBUG, name = "deep")   ! back to normal: the rule is retired
```

A rule is replaced by setting the same name again. `%enabled(level, name = ...)` reports the same
decision, so it can be used to check a rule is doing what you meant before relying on it.

**To undo an override, use `%unset_level`, not `%set_level` back to the logger's own level.** The
two look interchangeable and are not:

```fortran
call pf_log_unset_level("deep")            ! removes the rule: "deep" follows the logger again
call pf_log_unset_level()                  ! removes every rule
call pf_log_unset_level("deep", found = f) ! f says whether there was one to remove
```

Setting the level back instead leaves a rule in place holding a *snapshot* of the logger's level at
that moment. It stops tracking the logger, so a later `pf_log_set_level(...)` reaches every name but
that one — and the name you thought you had restored quietly diverges from the rest of the program.

It also matters for a long run: a logger holds `PF_LOG_MAX_NAME_RULES` (16) overrides and appends
each new name to the first free slot, so a program that turns tracing on and off for more than 16
*distinct* names aborts if the slots are never released. `%unset_level` frees one; setting the level
back does not. `%init` is the only other way to clear the table, and it destroys every sink with it.

A name rule never lowers a **sink's** threshold — it governs which records the logger offers, not
which ones a sink accepts. A sink at `PF_LEVEL_WARNING` stays at `WARNING` however low a rule goes;
[Which threshold decides](#which-threshold-decides) has the full composition rule.

**A lowering rule costs something, and the cost is bounded and reversible.** Below the logger's own
level, a record is normally rejected by one integer comparison. A lowering rule drops that cached
floor, so records between the new floor and the logger's level now reach the per-name test instead
— a string copy and a scan of at most `PF_LOG_MAX_NAME_RULES` (16) prefixes. The clock reads and the
context assembly still happen only for a record that *passes* that test, so a record dropped for
having the wrong name never pays for them. The cost lasts exactly until the rule is raised back, and
a logger with no lowering rule pays nothing at all.

### `context`, in more detail

The context is ambient by default — pushed and popped around a region of work, and carried by every
record emitted inside it (see
[Logging from an OpenMP parallel region](#logging-from-an-openmp-parallel-region)). The `context =`
argument overrides that for one record, which is what you want for a record *about* a different
piece of work than the one currently in scope:

```fortran
call pf_log_push_context("tile=17")
call pf_log_info("started")                         ! context "tile=17"
call pf_log_warning("neighbour stalled", context = "tile=18")   ! this one only
call pf_log_pop_context()
```

Passing `context = ""` renders no context at all, which is how a record opts out of an ambient one.

### `once` and `every`

Both suppress repeats of the same record; see
[Deduplicating a repeated record](#deduplicating-a-repeated-record) for the key they use and the
one call that clears the table. `once = .false.` and `every = 1` both mean "no suppression", so a
flag computed at runtime can be passed straight through without an `if`.

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

**The mode is sticky, and `%flush` does not clear it.** A flush drains what is buffered at that
moment; the logger stays in buffered mode afterwards, so every later record — including an ordinary
serial one, nowhere near a parallel region — goes into the collector too, and is lost if the program
ends without another flush. Switch back once the region is done:

```fortran
call lg%set_thread_mode(PF_LOG_THREAD_DIRECT)     ! flushes every slot on the way out
```

That flushes and restores immediate emission in one call, so it is the better closing move than a
bare `%flush()` whenever the parallel section is over. Records are not emitted directly while the
mode is on, deliberately: a serial record written straight to the stream would appear *ahead* of the
buffered records that preceded it.

## What every logger shares

A `type(pf_logger)` owns its level, its name, its rank, its sinks — each with its own format,
colour, threshold, `flush_level` and `only_rank` — and its per-name override rules. Declaring a
second logger gives you a second, independent destination set.

**Six things are not per-logger.** They live in the module, so a change made through any logger, or
through the `pf_log_*` default logger, is seen by every logger in the process:

| what | changed by | scope |
|---|---|---|
| the name stack | `pf_log_push_name` / `pf_log_pop_name` / `pf_log_clear_names` | one thread |
| the context frames | `pf_log_push_context` / `pf_log_pop_context` / `pf_log_clear_context` | one thread |
| the base context | `pf_log_set_context` | whole process |
| the `once=` / `every=` table | any record carrying either; `pf_log_reset_dedup` | whole process |
| the buffered-mode collector | `%set_thread_mode(PF_LOG_THREAD_BUFFERED, [slot_bytes])`, `%flush` | whole process |
| the `{elapsed}` origin | the first `%init` or `%add_*` anywhere | whole process |

So `call pf_log_push_name("io")` composes `.io` onto the name of records from *your* logger, the
default logger and every other logger alike, until it is popped. That is the intended behaviour —
the stack says which part of the program is speaking, which is a property of where you are, not of
which logger you happen to hold — but it is worth knowing before pushing a frame in library code.

Four consequences that are easy to meet by surprise:

- **`lg2%flush()` emits `lg1`'s buffered records too.** There is one collector, keyed by output
  unit, which is exactly what stops two loggers writing to the same unit from interleaving
  mid-line. The thread *mode* is per-logger, though — a record only reaches the collector when the
  logger emitting it is itself in buffered mode.
- **`%set_thread_mode` sizes the slots for everyone.** `slot_bytes` on one logger reallocates the
  one shared slot array, flushing whatever it held first.
- **The dedup key is `name|level|message`.** Two loggers with different names never share a
  counter, but they do share the 256-key table (`PF_LOG_MAX_DEDUP_KEYS`), and `%init`/`%close`
  deliberately do not clear it — one logger must not un-suppress another's messages.
- **Machinery warnings are one-shot per process**, not per logger: a truncated context, a full
  dedup table or a failed console write reports once, whichever logger provoked it.

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

The override applies to that name and to any name below it in dotted notation — including a name
a subprogram composed with `pf_log_push_name` — and it works in **both** directions: turning one
name *up* to `PF_LEVEL_TRACE` for debugging is the same call with a lower level. See
[`name`, in more detail](#name-in-more-detail) for that direction and what it costs.

`%enabled(level)` cannot see a name, so it answers conservatively (it may say yes for a record an
override will drop); pass `%enabled(level, name = ...)` for the exact answer.

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
| `<prefix>FORMAT` | sets the layout of every current sink, `<prefix>FILE`'s included |
| `<prefix>COLOR` | `auto`, `always` or `never` |

`prefix` is the whole literal prefix including its trailing separator, so a doubled separator cannot
arise. Reading is **additive and never destructive**: an unset variable changes nothing, a
set-but-empty one is ignored, and `<prefix>FILE` adds a sink beside whatever is already attached.
`<prefix>FILE` is applied before `<prefix>FORMAT`, so setting both gives the new file sink the
layout you asked for rather than the default one.
It is never applied implicitly — these are `PF_LOG_*` variables, not `PARQUET_FORTRAN_*` ones, and
they configure your program's logging rather than this library's behaviour. `NO_COLOR` is the one
variable honoured without an explicit call.

## Important behavior

- **Emission is thread-safe; configuration is not.** Configure a logger before entering a parallel
  region. There is no detection machinery for a violation, deliberately: a documented rule that is
  easy to follow beats a guard that cannot reliably fire.
- **A record costs only what its sinks render.** A suppressed record is a call and one integer
  comparison. An emitted one gathers and formats the clock fields only when some attached sink's
  template actually renders them, so a `{message}`-style sink emits several times faster than a
  timestamped one — and a suppressed or unconfigured call site is cheap enough to leave in a hot
  loop. The one cost a sink cannot avoid for you is building the message text itself: guard an
  expensive construction with `%enabled` (see [Levels](#levels)).
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
