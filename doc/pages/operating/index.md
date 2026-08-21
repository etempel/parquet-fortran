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

- [Choosing a module](choosing-a-module.html) — what each entry module gives you and what it costs
  to compile against, plus the one caveat: no import makes the *package* Arrow-free.
- [Error handling](error-handling.html) — the two failure classes (`error stop` and the
  concurrency guard's abort) and what to catch where.
- [Troubleshooting](troubleshooting.html) — build and link problems and their fixes.
- [Thread safety](thread-safety.html) — the complete concurrency rules: per-thread readers and
  writers, what a shared `parquet_table` allows, transform sharing, and thread-pool tuning.
- [Performance and memory](performance.html) — what reads cost, and how the library holds and
  releases memory.
- [Settings](settings.html) — every process-global knob: verbosity, streams, thread caps,
  row-group sizing, and their environment variables.
