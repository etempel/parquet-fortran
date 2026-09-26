---
title: Operating the library
ordered_subpage: choosing-a-module.md
ordered_subpage: error-handling.md
ordered_subpage: troubleshooting.md
ordered_subpage: thread-safety.md
ordered_subpage: performance.md
ordered_subpage: settings.md
---

The cross-cutting concerns, in roughly the order a reader meets them: which module to import, what
an error means, why a build, a link or a run fails, what may run concurrently, what memory and
speed to expect, and the knobs that change how loudly, how large and how fast the library runs.

Unlike the rest of the guide, most visits here start from a symptom rather than from the top: a
build that will not link, a program that aborts, threading that does nothing.

- [Choosing a module](choosing-a-module.html) — what each entry module gives you, what it costs to
  compile against, and which to reach for; that each re-exports the settings its own code reads,
  with one deliberate exception, so a narrow import can be configured without naming
  `parquet_settings`; that each is covered by the versioning promise in its own right; and the one
  caveat: no import makes the *package* Arrow-free.
- [Error handling](error-handling.html) — the two failure classes (Fortran `error stop` and a
  C++-level process exit), how to tell them apart by exit status, warnings, and the four shapes a
  call uses to report instead of aborting; plus the context a message carries, the `context=` you
  can add to it, why some of it arrives on the other stream, what calling before open or after
  close does, and what re-opening a handle does.
- [Troubleshooting](troubleshooting.html) — build, link and runtime symptoms with their fixes: a
  build that cannot find Arrow, a link that cannot resolve it, a program that will not start or
  dies in your own callback, and one compiler bug that looks like a data problem; and what to put
  in a bug report.
- [Thread safety](thread-safety.html) — the complete concurrency rules: per-thread readers and
  writers, what a shared `parquet_table` allows, transform sharing, logging from several threads,
  the Arrow-free tiers — where a bulk spatial query is the one operation that can corrupt a shared
  object with nothing to detect it — thread-pool tuning, and the guard that does detect a shared
  reader or writer and stops the process rather than corrupting it. `parquet_random`'s free
  functions are the one part of the library with nothing to say in these terms; its two objects
  follow the ordinary rules.
- [Performance and memory](performance.html) — what reads and writes cost in memory, how to reach a
  table's values without paying a name lookup per cell, and the OpenMP thread placement that
  silently collapses threading.
- [Settings](settings.html) — every process-global knob and when each takes effect: the thread
  caps, the defaults a reader or writer takes at open (compression codec, compression level and
  Arrow's own threads), the sort engine's fast paths, row-group sizing, row-group pruning, the
  pinned file date for reproducible output, verbosity and streams — plus the environment
  variables that set them, how to reset or print every setting at once, and the read-only limits
  on filter rules, sort keys and MAML lines.
