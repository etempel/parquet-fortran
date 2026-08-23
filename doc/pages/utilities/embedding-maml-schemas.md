---
title: Embedding your own MAML schemas in your own project
---

`tools/generate_parquet_maml.sh` (bundled with this library) is a generic tool any project
depending on `parquet-fortran` can reuse to embed *its own* `.maml` schemas directly into compiled
Fortran source, so a downstream pipeline doesn't need to locate/ship `.maml` files at run time. To
do this in your own project:

1. Copy `tools/generate_parquet_maml.sh` into your own project (e.g. under your own `tools/`). It
   is part of the published package, so if you consume this library as an fpm dependency the script
   arrives inside the resolved dependency tree — under `build/dependencies/parquet-fortran/tools/`
   — rather than in your own repository.
2. Put your own `.maml` schema files under a `schemas/` directory at your project's root — this is
   `parquet-fortran`'s own convention and the default the script looks for, but not required: pass
   `--dir=<name>` (or `--dir <name>`) to use a different directory name if your project already has
   its own convention. That directory is scanned **recursively**, so you are free to group schemas
   in subdirectories; every `.maml` filename must be unique across the whole tree, because a schema
   is addressed by its filename rather than by its path.
3. Run it from your project's root: `tools/generate_parquet_maml.sh` (or
   `tools/generate_parquet_maml.sh --dir=<name>` for a non-default directory) — this writes
   `src/parquet_maml.f90` in *your* project, generated from *your* `.maml` files. **Commit that
   file.** Like the generated table types, it is regenerated on demand rather than at build time,
   which is what keeps your own build free of any dependency on Python or on this script; re-run
   the generator whenever a `.maml` changes, and commit the result alongside it.
4. `use parquet_maml` (the module the script just generated for you) alongside `use parquet` in
   your code — the second is what supplies the `parquet_schema` type itself.
   `get_parquet_maml("your_schema.maml")` returns that schema **already parsed**, so there is no
   `parquet_parse_maml` call of your own to make; `set_maml(maml_default, [maml_file])` resolves
   either to the embedded default or, when `maml_file` names a file on disk, to that file validated
   against the default. Square brackets mark an optional argument.

## Reference

### A complete example

Two files: one schema and one program. The schema goes under `schemas/`; note that the `fields:`
list entries begin at column 0, which is what the format expects — see
[The MAML metadata format](../schema/maml-format.html).

```yaml
# schemas/mysurvey.maml
dataset: probe_dataset
table: mysurvey
fields:
- name: objid
  data_type: int64
  info: object identifier
- name: ra
  data_type: float64
  unit: deg
- name: flux
  data_type: float32
  unit: nJy
```

After `tools/generate_parquet_maml.sh` has written `src/parquet_maml.f90`:

```fortran
program embedded_schema_quickstart
    use parquet
    use parquet_maml, only : get_parquet_maml   ! the module the script generated
    use iso_fortran_env, only : int64, real64, real32
    implicit none
    type(parquet_schema) :: schema
    type(parquet_writer) :: w
    !
    schema = get_parquet_maml("mysurvey.maml")     ! or "mysurvey" -- both match
    print '(a,l1,a,i0)', "parsed=", schema%is_parsed(), " fields=", schema%get_num_fields()
    !
    call parquet_open_writer(w, "mysurvey.parquet", schema)
    call parquet_write_column(w, "objid", [1_int64, 2_int64, 3_int64])
    call parquet_write_column(w, "ra",    [1.0_real64, 2.0_real64, 3.0_real64])
    call parquet_write_column(w, "flux",  [1.0_real32, 2.0_real32, 3.0_real32])
    call parquet_close_writer(w)
end program embedded_schema_quickstart
```

prints `parsed=T fields=3` and writes the file. The schema arrives ready to use: nothing on this
path reads a `.maml` file at run time, and there is no separate parse step.

### Addressing a schema by name

`get_parquet_maml` matches a schema by its **filename**, with or without the `.maml` extension. One
held in a subdirectory answers to its full relative path as well, so all three of these reach the
same schema for `schemas/dr2/extra.maml`:

```fortran
schema = get_parquet_maml("dr2/extra.maml")
schema = get_parquet_maml("extra.maml")
schema = get_parquet_maml("extra")
```

That is why filenames have to be unique across the scanned tree: subdirectories organise your
schemas without changing how any of them is addressed.

Alongside `get_parquet_maml`, the generated module makes one accessor public per schema, named
after the file — `parquet_maml_extra()` for `extra.maml`. These take no argument, and are worth
using where you would rather the compiler than a string caught a typo.

### Starting from a default schema: `set_maml`

`set_maml(maml_default, [maml_file])` is for a pipeline that ships a default schema and lets its
user supply a replacement:

- with `maml_file` absent, or a blank string, it returns the embedded `maml_default` schema —
  exactly what `get_parquet_maml(maml_default)` would give you;
- with `maml_file` naming a file on disk, it loads that file, validates it against the embedded
  default, and returns it parsed.

The second form does more than load the user's file, and the difference is the reason to use it.
The schema you get back carries the **default's** full field list, not the user's: a field the
user's MAML omits is still there, reported by `schema%is_column_set(name)` as `.false.` and
recorded in `schema%maml%missing_columns`. A reader built from that schema therefore knows which
columns to skip, rather than simply never mentioning them. `schema%maml%name` is the user's file
path, so a diagnostic can say which schema was actually in force.

### Keeping the committed module current: `--check`

Because `src/parquet_maml.f90` is committed rather than generated at build time, it can drift from
the `.maml` files it came from. `--check` regenerates in memory and compares, writing nothing:

```bash
tools/generate_parquet_maml.sh --check
```

It exits 0 when the committed module is current and 1 when it is stale, naming the file and saying
to re-run the generator — which makes it a one-line addition to your own CI. Pass it whatever
`--dir`/`--module` options the generating run used; with different options it is comparing against
a different file.

### Choosing the directory and the module name

`--dir=<name>` selects the directory to scan (default `schemas/`), and `--module=<name>` names the
generated module, and therefore the file it is written to, `src/<name>.f90` (default
`parquet_maml`). The module name is the reason to reach for the second: some projects require every
module of their own to carry their package's prefix, and one embedded-schema module per project is
otherwise all you can have — a second run overwrites the first, since both the module name and the
output path come from it.

```bash
tools/generate_parquet_maml.sh --dir=metadata --module=mypipeline_maml
```

writes `src/mypipeline_maml.f90`, holding a module named `mypipeline_maml`, from every `.maml`
under `metadata/`.

### When the script refuses

Each of these is a message and a nonzero exit, before anything is written:

- **No `.maml` files** under the scanned directory, including a directory that does not exist —
  `No .maml files found under <dir>/`.
- **Two schemas sharing a filename**, anywhere in the tree, or two whose names differ only in case
  or punctuation. The script names both files and writes nothing. Allowed to run, it would emit a
  module with a duplicated accessor in it, and your build would fail on a line naming neither file.
- **An invalid `--module` name.** A Fortran module name is letters, digits and underscores,
  starting with a letter, and at most 63 characters.

At run time, `get_parquet_maml` `error stop`s on a name it does not have, with
`get_parquet_maml: unknown internal MAML file: <name>`.

## What the generated module depends on

The generated `parquet_maml` module imports four names from `parquet` — `parquet_schema`,
`parquet_load_maml_file`, `parquet_parse_maml` and `parquet_validate_user_maml` — and does not
import `parquet_maml_base` at all. `parquet_maml_base` is where *this library's* own bundled
schemas are embedded, so importing it gives you those fixtures and the shared `parquet_maml_file`
type, never your project's own schemas; your generated `parquet_maml` is what holds those. (If
you're contributing to `parquet-fortran` itself and need to regenerate its own built-in schema
module, see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md).)

## A second generator, for table types

A second, related generator ships alongside it:
[`tools/generate_user_table_code.py`](generated-tables.html) turns a MAML schema into a named
`parquet_table` extension type with one accessor per column, for programs that always read the same
columns. It follows the same copy-it-into-your-project, commit-the-output convention as this one.
