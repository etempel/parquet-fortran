---
title: Settings
---

`parquet_settings` collects the parameters that apply to the whole library rather than to one
reader, writer or table, so a program can set them once at startup instead of passing the same
argument at every call site — and can ask what they currently are.

Everything below comes with `use parquet`; naming the module directly (`use parquet_settings`) works
too if you prefer a narrower import.

**If you are importing one of the narrower modules, you do not need `parquet_settings` at all.**
Each entry module re-exports, getter and setter both, the knobs its own code reads — `use
parquet_sorting` gives you the four sorting knobs, `use parquet_strings` gives you `string_threads`,
and both give you `verbosity` and `message_stream` because both can print. That matters for more
than convenience: `parquet_settings` owns this library's C++ boundary, so importing it would put the
Arrow stack back into a build that was deliberately staying clear of it. See
[Choosing a module](choosing-a-module.html) for which module carries which knob.

## What is a setting, and what is not

**A setting may change how fast, how large or how loud the library runs. It may never change what
the library answers.**

That line is deliberate and it is why some things you might expect here are absent. A global default
for "where do nulls sort", or for whether quality-control checks are enforced, would make the same
call return different results in two different programs, with nothing at the call site to hint at
it — and possibly set by a library the caller did not know was linked in. Those stay as arguments to
the specific call that needs them.

Two consequences worth knowing:

- **An explicit argument always wins.** A setting supplies the default a call uses when you say
  nothing; passing the argument overrides it.
- **Settings belong to the application, not to a library.** If you are writing a library that in
  turn uses parquet-fortran, do not set them on your caller's behalf.

**One procedure in this module is not a setting.** `parquet_get_arrow_version` reports which Arrow
and Parquet C++ libraries your program is actually linked against — `parquet_get_arrow_version(v)`
for Arrow's runtime version, `parquet_get_arrow_version(v, mode="parquet")` for the Parquet C++ one.
It lives here because reading it means calling into the C++ half, which is the boundary this module
already owns, and because `parquet_get_arrow_threads` is its neighbour in every practical sense. The
library's *own* version is a separate question with a separate answer: `parquet_get_version`, in the
leaf module `parquet_version`, which needs no C++ at all. See
[Choosing a module](choosing-a-module.html#the-entry-modules).

## Thread safety: set once, at startup

Set your settings during program initialisation — before other threads exist, and before opening any
reader, writer or table. Reads are unsynchronised, and a concurrent write is a data race that the
library does not defend against.

This is not a limitation the settings module introduces. `parquet_set_arrow_threads` resizes the single
CPU thread pool that every reader and writer in the process is already sharing, so calling it from
two threads with different values is a race whatever bookkeeping sits in front of it. In exchange,
reading a setting costs nothing on any hot path.

## When a setting takes effect

Each setting documents its own capture point, because they genuinely differ and assuming one gets
the others wrong. `parquet_set_arrow_threads` resizes a pool everyone already shares, so it takes
effect **immediately**, for readers and writers opened before the call as well as after.

**Four settings are read by the library's C++ half, and those reach it when a reader or writer is
opened — not at the moment you set them.** They are `verbosity`, `message_stream`,
`sort_counting_path` and `sort_counting_bucket_limit`. Setting one and then opening a reader works
exactly as you would expect; changing one *while a reader is already open* leaves that reader using
the value that was current when it was opened, for the rest of its life. Open the reader after
setting the knob, which is what the advice above already asks for.

The reason is worth one sentence, because it is what makes the rest of this library's module
structure possible: a setter that pushed its value across to C++ immediately would have to live in
the module that owns the C++ boundary, and every module re-exporting that setter would then depend
on Arrow. Pushing at the point of use instead lets `use parquet_sorting` offer the sorting knobs,
and `use parquet_strings` the string ones, without either import pulling in the Parquet C++ stack.

## Thread pool

`parquet_set_arrow_threads(n)` sets, and `parquet_get_arrow_threads()` reports, the capacity of Arrow's
global CPU thread pool — the pool used by every reader and writer opened with `use_threads` enabled,
which is the default.

```fortran
use parquet
integer :: n

n = parquet_get_arrow_threads()          ! Arrow's hardware-derived default
call parquet_set_arrow_threads(8)        ! cap the parquet layer at 8 threads
```

This is the setting to reach for in batch or HPC work, where `OMP_NUM_THREADS` is chosen for the
science code and the I/O layer would otherwise inherit it: it lets you give the computation all the
threads it wants while keeping parquet reads and writes to a smaller share.

`n` must be at least 1; anything lower aborts. There is no "auto" value — Arrow's own starting
capacity is hardware-derived, and `parquet_reset_settings` is how you get it back.

## All three thread counts at once

`parquet_set_threads(n)` sets Arrow's pool, the sort cap and the prefetch cap together — the common
case of "give this library `n` threads and no more".

```fortran
call parquet_set_threads(4)          ! all three
call parquet_set_sort_threads(1)     ! ...then keep sorting serial
```

It holds no state of its own: read the three back individually or with `parquet_print_settings`, and
set any one afterwards to override just that one, as above.

**`n` must be at least 1.** `0` means "automatic" to the sort and prefetch caps, but Arrow's pool has
no automatic value — its starting capacity is hardware-derived — so rather than let one argument mean
two things, this takes a real thread count only. Use the individual setters for automatic behaviour,
or `parquet_reset_settings()` to put everything back.

**The three do not take effect at the same moment.** Arrow's pool is resized immediately and is
shared, so readers and writers you have already opened are affected too; the sort and prefetch caps
are read per call and so apply to work started afterwards. Setting them together does not make them
simultaneous.

## Threads for sorting

`parquet_set_sort_threads(n)` caps how many threads a sort uses when it is not given an explicit
`threads=`. It covers every sort in the library at once — `pf_sort`/`pf_argsort` and friends, a
read-time `parquet_open_reader(..., sort_by=)`, and `parquet_table%sort_by` — because all three run
on one engine and ask one question.

```fortran
call parquet_set_sort_threads(4)     ! sorting uses at most 4 threads
n = pf_sort_threads()                ! what a sort would actually use right here
```

Read per sort call, so it takes effect immediately. `0` restores automatic behaviour. Three
properties are worth knowing, because each surprises someone:

- **It is a cap, never a request.** Setting it above `OMP_NUM_THREADS` changes nothing; the library
  never asks for threads OpenMP has not been given.
- **It does not override an explicit `threads=`.** A caller who names a thread count means it.
- **It does not lift the serial answer inside an OpenMP parallel region.** An unqualified sort
  called from inside your own parallel region stays serial, whatever this is set to — otherwise
  eight threads would each spawn eight more, which is slower than not threading at all. Pass
  `threads=` explicitly if you really want a threaded sort in there.

`pf_sort_threads()` reports the resolved answer for the current context; `parquet_get_sort_threads()`
reports the raw setting (`0` when automatic).

## Threads for reading a table

`parquet_set_prefetch_threads(n)` caps the threads `parquet_table%prefetch` and `%materialize_all`
use to read several columns at once, each on its own reader. Like the sort cap it is a cap rather
than a request, read per call, with `0` meaning automatic.

`1` makes the prefetch serial. That is not a special case in the library — a one-thread cap simply
fails the same applicability test that a single-threaded OpenMP environment already fails, and the
ordinary serial path takes over.

## Threads for mutating a table

`parquet_set_table_threads(n)` caps the threads a table's row-structural mutation uses to rewrite
its columns concurrently — `%sort_by`, `%filter_rows`, `%top_n`, and `%delete_rows` and `%truncate`,
which go through the same loop. Every column is rewritten independently of every other, so the work
divides cleanly; the parallelism is bounded by the **column count**, which means a wide table gains
a great deal and a two-column table almost nothing.

Like the other two it is a cap rather than a request, read per call, with `0` meaning automatic.
`1` makes the rewrite serial, which is also what happens on its own inside your own OpenMP parallel
region, on a table with fewer than two rewritable columns, and on a table too small to be worth a
thread team.

**This is also the memory control, and it is the one thing to know before leaving it automatic.**
Each thread rewriting a column holds a transient second copy of that column, so `n` threads hold `n`
copies where a serial rewrite holds one. Since the count never exceeds the column count, those
copies come to at most one extra copy of the table — so a whole-column rewrite (`%sort_by`) can
**double the table's peak memory for the duration of the call**. `%filter_rows` and `%top_n` are
proportionally cheaper, since their new storage is sized by the rows they keep. A program working
near its memory ceiling caps the threads here, which caps the copies with them.


## Threads inside one string column

`parquet_set_string_threads(n)` caps the threads **one `parquet_string_column` bulk operation** may
use internally — a reindex, a gather, a compaction, a materialization of one column's packed
payload.

This is the only one of the thread caps that divides work *within* a column rather than across
several. `parquet_set_table_threads` splits a table's work by **column**; this splits one column's
work by **row range**. They do not multiply: a string operation reached from inside the table's own
parallel region stands down, exactly as a sort does, so at most one of the two is ever active.

Read per operation, with `0` meaning automatic and `1` forcing serial. A string operation also stays
serial on its own inside your own OpenMP parallel region, and on a payload too small to be worth a
thread team — the floor is measured in **bytes of payload rather than rows**, because a column of ten
million one-character elements and one of ten thousand ten-kilobyte elements have very different row
counts and much the same amount of copying to do.

**This one behaves differently from the other thread settings in one respect, and on a large machine
the difference matters.** The others only ever *lower* the automatic answer. This one *replaces* it:
the automatic answer is deliberately **capped well below** what OpenMP offers, because past a certain
thread count a string rebuild stops scaling and begins losing ground — measured on a 384-logical-thread
dual-socket machine, where taking the full count was 18–40 % **worse** than the best available. If you
know your machine wants more than the default, say so and it is honoured, bounded only by what OpenMP
offers:

```fortran
call parquet_set_string_threads(128)   ! honoured, even though the automatic default is lower
```

Measured speedups for the operations this governs, for calibration: about 1.15x on an 8-core laptop,
1.6–2.7x on an 8-core desktop, and 3.4–4.5x on a 192-core server — larger columns gaining more than
smaller ones on the same hardware. The result is byte-identical at every thread count.

`parquet_string_threads()` reports the resolved answer for the current context;
`parquet_get_string_threads()` reports the raw setting (`0` when automatic).

## Threads for a bulk permutation

`parquet_set_random_threads(n)` caps the threads one `pf_random_permutation`, `pf_random_subset` or
`pf_random_resample` call may use. Like the sort, prefetch and table caps it is a cap rather than a
request, read per call, with `0` meaning automatic and `1` forcing serial.

**This is the one thread setting whose answer-invariance is provable rather than intended, and it is
worth knowing why.** `pf_random_perm_at(seed, m, k)` is a pure function of its coordinates: element
`k` is computed from the seed, the population size and `k` alone, reading nothing any other element
writes. Splitting the array into contiguous chunks therefore cannot change a single value, and the
one-thread and sixty-four-thread results are **bit-identical** — so you can switch thread counts
while debugging, or run the same program on two differently-sized machines, and get the same
permutation. That is what makes threading admissible here as a setting at all. The same argument
covers `pf_random_resample`, whose element `k` is a pure function of `(seed, stream, k)`; it splits
the draw axis rather than the element axis, and is equally bit-identical.

Measured on a 192-core dual-socket server, wall nanoseconds per element:

| threads | 1 | 2 | 4 | 8 | 16 | 32 | 64 |
|---|---|---|---|---|---|---|---|
| `m` = 10 000 | 9.53 | 5.01 | 2.79 | 1.58 | 1.17 | 1.09 | 1.60 |
| `m` = 1 000 000 | 9.55 | 4.78 | 2.39 | 1.20 | 0.60 | 0.31 | 0.17 |
| `pf_random_resample`, 10**7 draws | 12.04 | 6.02 | 3.01 | 1.51 | 0.75 | 0.38 | 0.19 |

The resample row scales almost perfectly to 64 threads (63×) and then flattens: 128 threads buys only
0.16 ns. Asking for one thread per core (192 here) was *slower in absolute terms* than asking for 64
— but that turned out to be a property of the **OpenMP runtime**, not of the machine: the same
binary built with a different compiler ran 192 threads with no collapse at all. So treat a very high
thread count as something to measure on your own toolchain rather than a number to assume, in either
direction.

`parquet_set_random_parallel_min_elements(n)` is the **work floor**: the fewest elements a thread
must be given before a team is opened at all. Below `threads * n` elements the call runs serially,
however many threads are available. The default is 1000.

This exists because threading a small permutation is not merely useless but actively harmful — the
team costs more than the whole job. The floor applies to an explicit `threads=` as well as to the
automatic answer, because it asks whether the array is worth splitting, which is a property of the
work rather than of the caller's intent: `threads=8` on a 100-element array is honoured by being
declined. Setting it to `0` disables the floor entirely, which is useful for testing and is not a
useful production setting.

Note the floor counts elements **produced**, not the population size. A 100-element subset drawn
from a population of a trillion is 100 elements of work, and stays serial.

`parquet_get_random_threads()` and `parquet_get_random_parallel_min_elements()` report the raw
settings.

## Writer defaults

`parquet_set_default_compression(name)` and `parquet_set_default_compression_level(n)` supply what
`parquet_open_writer` uses when the caller passes no `compression=`/`compression_level=`, and
`parquet_set_default_use_threads(flag)` does the same for `use_threads=` on both writers and
readers. All three are captured **at open**: a reader or writer already open keeps what it was
opened with.

```fortran
call parquet_set_default_compression("gzip")
call parquet_set_default_compression_level(6)
! every parquet_open_writer from here on defaults to gzip level 6
```

The codec must be one of `uncompressed`, `snappy`, `gzip`, `zstd`, `brotli`, `lz4`
(case-insensitive); anything else aborts, against the same list the `compression=` argument itself is
checked against. The *level* is not validated here, because the valid range belongs to the codec —
zstd, gzip and brotli each accept a different one, and snappy and lz4 have no levels at all — so an
out-of-range level is reported by the codec when a writer is actually opened with it.

**One rule that is easy to trip over.** The library's default level of 3 is tuned for its own default
codec, so it applies only when the codec was left alone **both** as an argument and as a setting.
Choose a codec by either route and the level falls back to that codec's own default unless you set a
level too. That is deliberate: it stops a zstd-tuned level being attached to, say, snappy just
because you changed the codec.

## Tuning the sort

Three knobs govern the sort engine. All three are read at each sort, so they take effect
immediately, and all three are process-global — a sort anywhere in your program sees the same
values.

```fortran
call parquet_set_sort_counting_bucket_limit(0)   ! 0 restores the built-in value
```

**There is no knob for the row count below which a sort refuses to thread.** That floor is
internal and scales with the team size, because the right value depends on how many threads are
being opened rather than on the data — a floor correct for four threads is far too low for
sixty-four. Threading a small array costs more than the sort saves, so the engine declines rather
than obeying a `threads=` it cannot use profitably; a sort that reports one thread on a small array
is behaving correctly.

`parquet_set_sort_counting_path(flag)` and `parquet_set_sort_counting_bucket_limit(n)` control the
integer counting fast path — a second sort implementation that a single, null-free integer key with
a narrow value range can use instead of the comparator. It is what stops a low-cardinality integer
sort costing many times what it should.

The full rule, in this order: **the counting path is used when the flag is on, *and* the key's value
range fits the bucket limit, *and* the key is a single null-free integer key.** Turning the flag off
overrides the limit; raising the limit does nothing while the flag is off.

Two things about the limit specifically:

- **It bounds the value *range*, not the number of distinct values.** A thousand values spread over
  a billion is far outside a limit that a million densely packed values sit comfortably inside.
- **The number is the memory control.** `n` buckets costs `8n` bytes of counters, so the built-in
  4194304 caps the counting path at 32 MB. Raising it trades memory for speed on wider-ranged
  integer keys.

Turning the counting path off has no performance case — it exists so the two implementations can be
compared against each other on the same data, which is how the library tests that they agree.

Both numbers accept `0`, meaning "restore the built-in value", and both accept either integer kind.

`parquet_set_sort_radix_path(flag)` controls the radix fast path — a stable
least-significant-digit radix sort that orders a column by bucketing its bytes rather than by
comparing rows at all. It applies to **every** key family, integer, real and string alike, so unlike
the counting path the key's type is never a reason it declines. The rule is: the radix path is used
when the flag is on *and* the row count clears an internal floor set at the measured point below
which the comparison sort is cheaper.

It handles a **multi-key** sort as well as a single-key one, by running one stable pass per key from
the last key to the first. There is one exception, and it is about cost rather than correctness: a
multi-key sort declines when one of its string keys holds a value longer than 64 bytes, because the
pass that orders a string key finishes long shared prefixes with an insertion sort whose cost is
quadratic in the size of a tied run. A single-key string sort has no such limit.

**This knob governs the pure-Fortran sort engine, which is not yet the default one** — until it
becomes so, setting it changes nothing about an ordinary sort. Everything below describes what it
will control, and is measured rather than projected.

**Unlike the counting path, this one has a real reason to turn off, and it is memory.** The radix
path needs up to about **32 bytes of scratch per row** — four `int64` buffers for a single-key sort,
three for a multi-key one — where the comparison sort needs none beyond the permutation itself.
Measured peak resident set, radix on against radix off:

| rows | comparison sort | radix path | difference |
|---|---|---|---|
| 5 million | 296 MB | 456 MB | 160 MB |
| 10 million | 536 MB | 856 MB | 320 MB |
| 20 million | 656 MB | 1295 MB | 638 MB |

At 50 million rows that is roughly 1.6 GB of scratch. A string column whose values share more than
eight leading bytes can add one further buffer — 8 bytes per row — but only if such a run actually
needs splitting, so an ordinary string column never allocates it. What you buy for it is several
times the throughput — at 20 million rows the same sort took 68 ns per element with the radix path and 1092 ns
without, a factor of 16 — so leave it on unless you are sorting near the edge of available memory.

If the scratch cannot be allocated the library does **not** fail: the radix path stands down and the
comparison sort finishes the job, which needs no scratch and gives the identical answer. One caveat
worth knowing — on Linux's default memory-overcommit policy a large allocation usually succeeds and
the kernel kills the process on first touch instead, so this safety net cannot engage there. If you
know you are memory-bound, turn the setting off rather than relying on it.

## Row-group size when writing

`parquet_set_target_row_group_bytes(n)` sets the size in **bytes** a row group aims for when a
writer is opened without an explicit `chunk_size=`. The built-in target is 268435456 (256 MiB), and
`0` restores it.

```fortran
call parquet_set_target_row_group_bytes(64 * 1024 * 1024)   ! ~64 MiB row groups
```

Sizing by bytes rather than by a flat row count is what makes a table of narrow `int32` columns and
a table of wide vector columns produce row groups of comparable size — which is what Parquet's own
guidance (roughly 128 MB to 1 GB) is about, and what drives per-row-group compression efficiency and
decode cost.

Three bounds the library applies afterwards are **not** settable: a floor of 1000 rows, a ceiling of
10,000,000 rows, and the `int32` element-count ceiling a vector column imposes. A target so small
that even the floor would overshoot it fourfold abandons the floor rather than the target, down to a
single row per row group — so a very small value really does produce very small row groups.

An explicit `chunk_size=` is never overridden by this; a caller who names a row count means it.

## Row-group pruning when reading

`parquet_set_statistics_prescreen(flag)` controls whether a filtered read screens each row group
against its footer statistics first, and skips the ones the filter provably cannot match — no column
data is read for those at all.

Leave it on, which is the default. It changes how much of a file is read and **nothing else**: the
rows returned are identical either way. That property is exactly why turning it off is useful for
testing (the library's own tests read the same fixture both ways and compare element for element)
and why there is otherwise no reason to.

The one real-world case for disabling it is a file whose statistics are known to be untrustworthy —
written by a tool that recorded them incorrectly. The screen declines on its own whenever it is
merely *uncertain*; it cannot detect statistics that are confidently wrong.

## Terminal output

Two settings control what the library prints and where. Both are read per message, so they take
effect immediately.

`parquet_set_verbosity(level)` takes one of three levels, in increasing order of quiet:

| level | what still prints |
|---|---|
| `"normal"` (default) | everything |
| `"silent"` | warnings and errors. Remarks and **explicitly-called print procedures** go quiet |
| `"errors_only"` | errors only |

```fortran
call parquet_set_verbosity("errors_only")   ! a clean run in a batch pipeline
```

**Errors are never suppressed, at any level** — an `error stop`, the C++ layer's fatal-error report,
and the context lines a failing writer close prints before aborting all survive. Your program's
control flow depends on that output being findable, so no setting may hide it.

**One consequence to know before you use `"silent"`:** it turns `%print_stat`, `%print_schema_info`
and `parquet_string_column`'s printers into no-ops, along with
`parquet_open_reader(..., print_stat=.true.)`. That is what a global output control means, and it is
a debugging trap worth naming — add a print, see nothing, and the table is not at fault.
`parquet_print_settings` is the one exemption: it prints at every level, so a silenced program can
always be asked why it is silent.

`parquet_set_message_stream(stream)` takes `"stdout"` (default) or `"stderr"` and decides where the
library's own messages go. The reason to change it is a program that pipes its own standard output
to a data consumer and does not want warnings mixed into that stream.

```fortran
call parquet_set_message_stream("stderr")   ! keep stdout clean for piped data
```

**Only those two values are accepted, and that is a constraint rather than a preference.** A Fortran
unit number means nothing to this library's C++ half, which prints several of the warnings and one
of the reports itself — so a setting holding an arbitrary unit could be honoured by the Fortran half
and silently ignored by the other. Sending messages to a log file is therefore not supported; a
shell redirect covers it. The explicitly-called print procedures are unaffected either way: they
keep their own `unit=` argument.

## Setting from the environment

`parquet_settings_from_env()` applies every `PARQUET_FORTRAN_*` variable that is set, through the
same setter — and the same validation — a direct call would use.

```fortran
call parquet_settings_from_env()     ! near the top of your program
```

**You call it; the library never does.** Reading the environment lazily on first access would be a
data race the first time two threads touched a setting, so there is no hidden call. Put it where
you would put your own `parquet_set_*` calls: before other threads exist and before any reader,
writer or table is opened.

**It applies over what is already set — it does not reset.** A variable that is absent leaves its
knob alone, so calling it *after* your own setters lets the environment override them, and calling
it *before* lets your code win. That choice is yours to make and the library has no opinion.

One variable per knob, named `PARQUET_FORTRAN_` plus the knob's name in capitals — the same name
`parquet_print_settings` prints:

| variable | accepts |
|---|---|
| `PARQUET_FORTRAN_THREADS` | integer >= 1 — sets the five below at once |
| `PARQUET_FORTRAN_ARROW_THREADS` | integer >= 1 |
| `PARQUET_FORTRAN_SORT_THREADS` | integer >= 0 (`0` = automatic) |
| `PARQUET_FORTRAN_PREFETCH_THREADS` | integer >= 0 (`0` = automatic) |
| `PARQUET_FORTRAN_TABLE_THREADS` | integer >= 0 (`0` = automatic) |
| `PARQUET_FORTRAN_STRING_THREADS` | integer >= 0 (`0` = automatic) |
| `PARQUET_FORTRAN_SORT_COUNTING_PATH` | `true`/`false`/`1`/`0` |
| `PARQUET_FORTRAN_SORT_RADIX_PATH` | `true`/`false`/`1`/`0` |
| `PARQUET_FORTRAN_SORT_COUNTING_BUCKET_LIMIT` | integer >= 0 (`0` = built-in) |
| `PARQUET_FORTRAN_DEFAULT_COMPRESSION` | `uncompressed`/`snappy`/`gzip`/`zstd`/`brotli`/`lz4` |
| `PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL` | integer |
| `PARQUET_FORTRAN_DEFAULT_USE_THREADS` | `true`/`false`/`1`/`0` |
| `PARQUET_FORTRAN_TARGET_ROW_GROUP_BYTES` | integer >= 0 (`0` = built-in) |
| `PARQUET_FORTRAN_STATISTICS_PRESCREEN` | `true`/`false`/`1`/`0` |
| `PARQUET_FORTRAN_VERBOSITY` | `normal`/`silent`/`errors_only` |
| `PARQUET_FORTRAN_MESSAGE_STREAM` | `stdout`/`stderr` |

`PARQUET_FORTRAN_THREADS` is `parquet_set_threads` and is applied **before** the other five, so a
specific variable always overrides it — `PARQUET_FORTRAN_THREADS=8 PARQUET_FORTRAN_SORT_THREADS=2`
gives eight threads to Arrow, the prefetch, the table and the string column, and two to sorting,
whichever order the two appear in your shell.

```bash
export PARQUET_FORTRAN_THREADS=4
export PARQUET_FORTRAN_SORT_THREADS=1          # ...but keep sorting serial
export PARQUET_FORTRAN_DEFAULT_COMPRESSION=gzip
export PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL=6
export PARQUET_FORTRAN_VERBOSITY=silent
./my_program
```

Tokens and booleans are case-insensitive, and surrounding blanks are ignored everywhere.

**A bad value aborts, naming the variable** — `PARQUET_FORTRAN_VERBOSITY=loud` stops the program
with a message saying so, rather than being ignored. Numbers are parsed strictly: `4` and `+4` and
` 4 ` are the same thing, while `4.5`, `4x` and `4 8` are all errors. That last one matters more
than it looks — a shell variable that expands to two words would otherwise apply the first number
and look like it worked.

**One exception to "a bad value aborts": an empty variable is treated as unset.**
`PARQUET_FORTRAN_VERBOSITY=` does nothing at all, and neither does a variable set to only spaces.
This is worth knowing when a variable seems to be ignored — `export PARQUET_FORTRAN_VERBOSITY=$LEVEL`
with `LEVEL` itself unset produces exactly that, and is skipped silently. `parquet_print_settings()`
is the quickest way to see what the environment actually did.

## Restoring and inspecting

`parquet_reset_settings()` restores every setting to what it was before your program changed it. For
the thread pool that means the capacity captured on the **first** `parquet_set_arrow_threads` call, so
several sets followed by one reset land back where you started. If you never changed a setting,
resetting it does nothing — the library will not resize a pool to a default it does not get to
choose.

`parquet_print_settings([unit])` writes every setting's current value and every read-only limit to
`unit` (default: standard output). Square brackets mark an optional argument here, as elsewhere in
this guide.

```fortran
call parquet_print_settings()
```

```
parquet-fortran settings
  arrow_threads                    8
  sort_threads                     0
  prefetch_threads                 0
  table_threads                    0
  string_threads                   0
  random_threads                   0
  random_parallel_min_elements     1000
  sort_counting_path               true
  sort_radix_path                  true
  sort_counting_bucket_limit       4194304
  default_compression              zstd
  default_compression_level        3
  default_use_threads              true
  target_row_group_bytes           268435456
  statistics_prescreen             true
  verbosity                        normal
  message_stream                   stdout
limits (read-only)
  parquet_max_filter_rule_len      8192
  parquet_max_filter_depth         32
  parquet_max_filter_nodes         1024
  parquet_max_sort_keys            16
  parquet_max_sort_key_len         320
  parquet_max_maml_line_len        1024
```

The two sections are separate on purpose: the first is what you can change, the second is what the
library enforces.

## Read-only limits

These are the caps the library applies to filter rules, sort keys and MAML source lines. They are
published so that code assembling any of those from user or configuration input can check a length
*before* tripping an `error stop`, which is useful when the input is not under your control:

| constant | value | what it bounds |
|---|---|---|
| `parquet_max_filter_rule_len` | 8192 | characters in one `filt%add` rule |
| `parquet_max_filter_depth` | 32 | parenthesis/`not` nesting levels within one rule |
| `parquet_max_filter_nodes` | 1024 | expression nodes across every `%add` of one filter |
| `parquet_max_sort_keys` | 16 | keys across every `%add` of one `parquet_sortkey` |
| `parquet_max_sort_key_len` | 320 | characters in one `srt%add` key |
| `parquet_max_maml_line_len` | 1024 | characters in one line of a MAML file |

They are constants rather than settings, and that is not an oversight: these exist so that
adversarial or accidentally-huge input fails as a clean error instead of exhausting memory or
overflowing the recursive-descent parser's own stack. Making them adjustable would turn a guard into
a way to crash the process.

Some limits the library enforces are **not** published here — notably Arrow's own 2<sup>31</sup>-1
ceilings on a vector column's `col_size` and on a file's column count. Those are enforced in the C++
layer, which reports them clearly when they are hit, and mirroring them into Fortran would create a
second copy of a number that has exactly one correct value. See
[Limitations](../../index.html) in the README for what those ceilings are.

## A note on `parquet_core`

`parquet_set_arrow_threads` used to live in the internal `parquet_core` module. It moved here without
changing its name or behaviour, so `use parquet` code is unaffected. Only code that imported it
narrowly from the internal module (`use parquet_core, only: parquet_set_arrow_threads`) would need to
change — and `parquet_core` is documented as internal and outside the library's API-stability
promise precisely so that this kind of tidying is possible. `use parquet` is the supported spelling.
