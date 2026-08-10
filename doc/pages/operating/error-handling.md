---
title: Error handling
---

This library reports failures through two distinct mechanisms, neither of which can be caught or recovered from — there are no status/`ierr` return codes anywhere in the public API, so check inputs (file existence, column names, array bounds) before calling into the library if you need to avoid aborting.

## The two failure classes

- **Fortran `error stop`**, printing `ERROR STOP <message>` followed by a backtrace — used for precondition/validation failures this library's own Fortran code detects directly: a missing file, invalid MAML, an unknown column name, calling a reader/writer procedure before it's open, a declared vector column's size not matching what was requested at the schema level, and so on.
- **A C++-level process abort (`std::abort()`)**, printing a one-line diagnostic to stderr with no `ERROR STOP` prefix and no Fortran backtrace — used where the failing check happens on the C++/Arrow side of the `parquet_wrapper.cpp` boundary rather than in Fortran. This covers: a column's physical Parquet type not matching what you asked for (e.g. `parquet-fortran: parquet_read_column: type mismatch for column: d (expected int32/int64, got timestamp[us])` — see [Limitations](../index.html#limitations)); reading a column containing a genuine Parquet Null without `null_value=`/`is_valid=` (see [Null values](supported-data-types.html#null-values)); exceeding the `col_size`/table-column-count/`chunk_size` int32 ceilings (see [Limitations](../index.html#limitations)); and the [concurrency guard](thread-safety.html#the-concurrency-guard) firing when a shared `reader`/`writer` is called from two threads at once. **Both are genuine, immediate process aborts** — the practical advice (validate before calling, don't rely on catching anything) is the same either way; the distinction mainly helps you recognize what you're looking at in a bare stderr line with no `ERROR STOP` prefix.

If you're contributing to this library and need to add or test one of these failure paths, see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md) for how that's done out-of-process, and for this project's own conventions around C++-level error reporting.

## Contextual error messages

Once a reader/writer/schema is far enough along to know it (i.e. `parquet_open_reader`/`parquet_open_writer` has already recorded the filename, or `schema%init`/`parquet_schema(...)` has already named the schema), most read/write/schema-building `error stop` messages append that context — e.g. `... (file: output.parquet, maml: my_schema.maml)` — so a failure is identifiable even when a program has several readers/writers/schemas in play at once. This wording isn't a fixed contract (exact phrasing may change between releases); only the presence of file/schema context, where available, is intended to be relied on.

## Calling before open, or after close

Calling any reader-taking procedure (`parquet_read_column`, `parquet_prefetch_columns`, `parquet_get_nrows`, `parquet_get_col_size`, `parquet_get_column_total_elements`, `parquet_get_string_length`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`) before `parquet_open_reader`, or `parquet_write_column` before `parquet_open_writer`, fails with `error stop`, naming the missing open call. The same applies to `parquet_close_reader`/`parquet_close_writer` themselves: closing a reader/writer that was never opened, or that was already closed, also fails with `error stop` rather than silently doing nothing.

## Re-opening and implicit finalization

Re-opening an already-open `reader`/`writer` variable (calling `parquet_open_reader`/`parquet_open_writer` again on one that's already in use, without closing it first) does **not** error and does **not** leak the old handle: Fortran automatically finalizes the previous handle first, since `reader`/`writer` are `intent(out)` arguments of a finalizable type. This finalization is *not* the same as an explicit `parquet_close_writer`/`parquet_close_reader` call, though: it skips the completeness checks those perform (e.g. "every enabled schema column was written"), so the abandoned file is not guaranteed to be a complete or valid parquet file. Always call `parquet_close_writer`/`parquet_close_reader` explicitly when you actually want the file that was being written/read to end up valid; rely on the implicit finalizer only as a safety net for a variable you're discarding or reusing anyway.
