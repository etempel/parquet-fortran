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
an error means, why a build or read fails, what may run concurrently, what memory and speed to
expect, and the knobs that change how loudly, how large and how fast the library runs.

Unlike the rest of the guide, most visits here start from a symptom rather than from the top: a
build that will not link, a program that aborts, threading that does nothing.

- [Choosing a module](choosing-a-module.html) — what each entry module gives you, what it costs to
  compile against, and which to reach for; that each re-exports the settings its own code reads, so
  a narrow import can be configured without naming `parquet_settings`; that each is covered by the
  versioning promise in its own right; and the one caveat: no import makes the *package* Arrow-free.
- [Error handling](error-handling.html) — the two failure classes (Fortran `error stop` and a
  C++-level process exit), how to tell them apart by exit status, warnings, and asking with
  `found=` instead of aborting; plus the context a message carries, why some of it arrives on the
  other stream, what calling before open or after close does, and what re-opening a handle does.
- [Troubleshooting](troubleshooting.html) — build, link and runtime symptoms with their fixes,
  and what to put in a bug report.
- [Thread safety](thread-safety.html) — the complete concurrency rules: per-thread readers and
  writers, what a shared `parquet_table` allows, transform sharing, thread-pool tuning, and the
  guard that detects a shared reader or writer and stops the process rather than corrupting it.
  `parquet_random` is the one part of the library with no rules at all.
- [Performance and memory](performance.html) — what reads and writes cost in memory, how to reach a
  table's values without paying a name lookup per cell, and the OpenMP thread placement that
  silently collapses threading.
- [Settings](settings.html) — every process-global knob and when each takes effect: the thread
  caps, the writer's default codec, the sort engine's fast paths, row-group sizing, row-group
  pruning, verbosity and streams — plus the environment variables that set them, and the read-only
  limits on filter rules, sort keys and MAML lines.
