---
title: Settings
---

`parquet_settings` collects the parameters that apply to the whole library rather than to one
reader, writer or table, so a program can set them once at startup instead of passing the same
argument at every call site — and can ask what they currently are.

Everything below comes with `use parquet`; naming the module directly (`use parquet_settings`) works
too if you prefer a narrower import.

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

## Thread safety: set once, at startup

Set your settings during program initialisation — before other threads exist, and before opening any
reader, writer or table. Reads are unsynchronised, and a concurrent write is a data race that the
library does not defend against.

This is not a limitation the settings module introduces. `parquet_set_max_threads` resizes the single
CPU thread pool that every reader and writer in the process is already sharing, so calling it from
two threads with different values is a race whatever bookkeeping sits in front of it. In exchange,
reading a setting costs nothing on any hot path.

## When a setting takes effect

Each setting documents its own capture point, because they genuinely differ and assuming one gets
the others wrong. `parquet_set_max_threads` resizes a pool everyone already shares, so it takes
effect **immediately**, for readers and writers opened before the call as well as after.

## Thread pool

`parquet_set_max_threads(n)` sets, and `parquet_get_max_threads()` reports, the capacity of Arrow's
global CPU thread pool — the pool used by every reader and writer opened with `use_threads` enabled,
which is the default.

```fortran
use parquet
integer :: n

n = parquet_get_max_threads()          ! Arrow's hardware-derived default
call parquet_set_max_threads(8)        ! cap the parquet layer at 8 threads
```

This is the setting to reach for in batch or HPC work, where `OMP_NUM_THREADS` is chosen for the
science code and the I/O layer would otherwise inherit it: it lets you give the computation all the
threads it wants while keeping parquet reads and writes to a smaller share.

`n` must be at least 1; anything lower aborts. There is no "auto" value — Arrow's own starting
capacity is hardware-derived, and `parquet_reset_settings` is how you get it back.

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

## Restoring and inspecting

`parquet_reset_settings()` restores every setting to what it was before your program changed it. For
the thread pool that means the capacity captured on the **first** `parquet_set_max_threads` call, so
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
  default_compression              zstd
  default_compression_level        3
  default_use_threads              true
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
[Limitations](../index.html) in the README for what those ceilings are.

## A note on `parquet_core`

`parquet_set_max_threads` used to live in the internal `parquet_core` module. It moved here without
changing its name or behaviour, so `use parquet` code is unaffected. Only code that imported it
narrowly from the internal module (`use parquet_core, only: parquet_set_max_threads`) would need to
change — and `parquet_core` is documented as internal and outside the library's API-stability
promise precisely so that this kind of tidying is possible. `use parquet` is the supported spelling.
