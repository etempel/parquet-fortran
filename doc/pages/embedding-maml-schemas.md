---
title: Embedding your own MAML schemas in your own project
---

`tools/generate_parquet_maml.sh` (bundled with this library) is a generic tool any project depending on `parquet-fortran` can reuse to embed *its own* `.maml` schemas directly into compiled Fortran source, so a downstream pipeline doesn't need to locate/ship `.maml` files at run time. To do this in your own project:

1. Copy `tools/generate_parquet_maml.sh` into your own project (e.g. under your own `tools/`).
2. Put your own `.maml` schema files under a `schemas/` directory at your project's root — this is `parquet-fortran`'s own convention (see below) and the default the script looks for, but not required: pass `--dir=<name>` (or `--dir <name>`) to use a different directory name if your project already has its own convention.
3. Run it from your project's root: `tools/generate_parquet_maml.sh` (or `tools/generate_parquet_maml.sh --dir=<name>` for a non-default directory) — this writes `src/parquet_maml.f90` in *your* project, generated from *your* `.maml` files.
4. `use parquet_maml` (the module the script just generated for you) alongside `use parquet` in your code, to call `get_parquet_maml("your_schema.maml")` or `set_maml(...)` the same way this library's own tests do internally.

The generated `parquet_maml` module depends on `parquet` (`parquet_maml_file`, `parquet_validate_maml`, etc.) but not on `parquet_maml_base` directly — `parquet_maml_base` is this library's own internal module and isn't meant to be `use`d directly by consuming projects. (If you're contributing to `parquet-fortran` itself and need to regenerate its own built-in schema module, see [CONTRIBUTING.md](https://github.com/etempel/parquet-fortran/blob/main/CONTRIBUTING.md).)
