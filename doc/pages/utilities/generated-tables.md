---
title: Generated table types
---

A program that always reads the same columns can have them as **named accessors on its own table type**, generated from a MAML schema, instead of naming them as strings everywhere:

```fortran
use parquet
use parquet_table_example, only : parquet_table_test   ! the generated module

type(parquet_table_test) :: t
real(real64), pointer :: ra(:)

call t%init("catalogue.parquet")   ! opens, checks every declared column, converts, reads
ra => t%ra()                       ! zero-copy pointer into the live storage
print *, sum(ra) / t%nrows()       ! and every inherited parquet_table operation still applies
```

`tools/generate_user_table_code.py` writes that module from a MAML file. The generated type **extends [`parquet_table`](../tables/table.html)**, so nothing is given up: `%nrows`, `%get`, `%filter_rows`, `%clone`, `%append`, `%row`, `parquet_write_table` and the rest all work exactly as they do on a plain table.

Like [`tools/generate_parquet_maml.sh`](embedding-maml-schemas.html), this script is meant to be **copied into your own project**: you keep your schemas, you run the generator, you commit its output, and your build needs no code-generation step.

## Extending `parquet_table` with your own type

`parquet_table` is designed to be extended, and two public entry points exist for that. Most
programs meet them through a generated table type — the subject of the rest of this page, and
the intended way in — but both are ordinary API and work just as well on a type you write by hand.

**`clone_extra` is the hook `%clone` calls for an extension's own components.** `%clone` and
`%clone_structure` copy everything `parquet_table` itself holds, and cannot know about components
an extending type added; overriding this hook is how those come across. Both call it as their last
action, dispatching on the source's dynamic type, so an override runs wherever either is used:

```fortran
type, extends(parquet_table) :: my_table
    real(real64) :: zeropoint = 0.0_real64
contains
    procedure :: clone_extra => my_clone_extra
end type my_table
...
subroutine my_clone_extra(self, out, structure_only)
    class(my_table), intent(in) :: self
    class(parquet_table), intent(inout) :: out
    logical, intent(in) :: structure_only   ! .true. when called from %clone_structure
    select type (out)
    class is (my_table)                     ! `class is`, so a further extension still gets this
        out%zeropoint = self%zeropoint
    end select
end subroutine my_clone_extra
```

`%clone` has already checked that source and destination have the same dynamic type, so the
guarded branch always matches. **Without an override, an added component arrives
default-initialized and nothing reports it** — which is why the generator writes these assignments
for you.

A concrete-typed override of `%clone` itself is not possible: an overriding procedure has to keep
every dummy argument's characteristics, so `out` cannot be narrowed from `class(parquet_table)`.
This hook is the supported substitute, and it keeps one name for one operation.

**`%bind_predefined` is what a generated type's `%init` calls.** It takes a column's declared name,
kind, width and whether it comes from the file, then checks each one against the file, converts it
to the declared kind, reads them all in one pass, and marks the slot *predefined* — which is what
makes `%drop_column` refuse it without `force=.true.`. It is public because a generated module is
a different module and `parquet_table`'s components are private; hand-written code that opened a
table with `parquet_open_table` already reaches every column by name and rarely needs it. The rest
of this page is the full contract.

**Opening an extension** goes through the parent component:
`call parquet_open_table(t%parquet_table, filename)`. `parquet_open_table`'s dummy is
non-polymorphic on purpose, so an extension that needs a binding step cannot be opened without it.
`parquet_write_table`, by contrast, takes `class(parquet_table)` and accepts an extending type
directly.

## The schema

A table-type schema is an ordinary MAML file. Put these in their own directory — `table_types/` by convention, kept separate from `schemas/` because the two are read for opposite purposes: `schemas/` describes files you *write*, `table_types/` describes types you *generate*.

```yaml
dataset: parquet_table_example  # the MODULE to generate (and the file name)
table: test                     # the TYPE: parquet_table_<table>, i.e. parquet_table_test
author: A Maintainer <a@example.org>   # replaces the `! Author:` header line
fields:
- name: uberid
  info: Object ID field.
  data_type: int64
- name: ra
  unit: deg
  data_type: float64
- name: crd
  unit: Mpc
  data_type: float32
  col_size: 3                  # a vector column: 3 values per row
- name: name
  data_type: string
  array_size: 5
- name: flux
  unit: Jy
  data_type: float64
  source: computed             # no file column is read for it; your program fills it
```

- **`dataset:`** names the generated module, and therefore the file (`parquet_table_example.f90`). **`table:`** names the type, as `parquet_table_<table>`. Two schemas that pick the same `dataset:` are refused.
- **`dataset:` may not name what `table:` derives.** A module cannot declare a type — or a procedure — of its own name, so `dataset: parquet_table_x` together with `table: x` would not compile; the generator refuses it up front rather than emitting a broken module. Note this is *not* "`dataset:` and `table:` must differ": `dataset: foo` with `table: foo` is fine, because the type is `parquet_table_foo`. If you hit the collision, give `dataset:` a different name — this library's own precedent is a plural (module `parquet_strings` holds type `parquet_string`).
- **If you publish your package, the module name must satisfy your own module-naming rule.** fpm's registry enforces a package prefix, so a module called `example` inside a published `src/` is rejected while `parquet_table_example` is fine. `fpm build` reports this immediately, so it is not a silent trap — but it is worth knowing before choosing `dataset:`.
- **`author:`** becomes the generated file's `! Author:` header line. Without it the header says the file was generated and names no author.
- **`source:`** is `file` (the default) or `computed`. A computed column gets its accessor and a slot of all-null rows, but is never looked for in the file — so a column your program calculates is a *declared intent* rather than an error.
- Everything else is ordinary MAML (see [The MAML metadata format](../schema/maml-format.html)), and the same file still works as a **write schema** — which is convenient, since a program that reads a catalogue usually writes one with the same columns.
- `col_size: auto` / `array_size: auto` are **rejected** here: an accessor's kind and rank have to be known when the code is generated, and `auto` is resolved at write time.

## Running the generator

```console
$ tools/generate_user_table_code.py                     # every *.maml in table_types/ -> src/
$ tools/generate_user_table_code.py --dir=my_types      # a different input directory
$ tools/generate_user_table_code.py --out-dir=lib       # a different output directory
$ tools/generate_user_table_code.py --check             # verify the committed output is current
$ tools/generate_user_table_code.py --self-test         # run the generator's own tests
```

**Commit the generated `.f90`** and add `--check` to your CI, the same way this project does. That is what makes the file safe to hand-edit (see below): `--check` fails the build if a *generated* region was edited, or if the committed file is stale relative to its MAML — and it tells you which of the two happened.

## The accessors

| declaration | `%x()` | `%x(i)` | `%x(lo, hi)` |
|---|---|---|---|
| scalar numeric / logical / temporal | `pointer :: p(:)` | `pointer :: p` — row `i` | `pointer :: p(:)` — rows `lo:hi` |
| `col_size: n` (vector) | `pointer :: p(:,:)`, `(element, row)` | `pointer :: p(:)` — row `i`'s `n` values | `pointer :: p(:,:)` — rows `lo:hi`, full width |
| `string` | `type(parquet_string_column), pointer :: p` | `type(parquet_string)` handle | `type(parquet_string), allocatable :: h(:)` |
| `string` with `col_size: n` | *(none — see below)* | *(none)* | *(none)* |

Three things to know:

- **The index is always a ROW index**, on every column. `%crd(3)` is row 3's three values, *not* element 3 of every row. Reach an element with `p => t%crd(i)` and then `p(e)`. (The alternative cannot coexist with the range form: `%crd(3,6)` would have to mean both "rows 3 to 6" and "element 3 of row 6".)
- **`%x()` is assignable, but not subscriptable.** `t%ra() = 0.0_real64` works — a function reference returning a data pointer is a variable — but `t%ra()(3)` is a *syntax error* in Fortran. Take the pointer first, or use `%ra(3)`.
- **The indexed forms repeat the column lookup on every call.** They are for readable one-off access; in a loop, take `p => t%ra()` once and index `p` instead.

A **string** column gets a second accessor, `%<name>_chr(arr)`, a subroutine that copies the column out as a `character` array sized to the longest value present. A string *vector* column gets **only** that form, because `%col` has no pointer specific for `PK_STRING_VEC` to alias.

Every accessor pointer is **invalidated by a row-structural mutation** (`%filter_rows`, `%sort_by`, `%top_n`, `%delete_rows`, `%truncate`, `%append`) — Fortran cannot detect this, so take the pointer again afterwards. See [Tables](../tables/table.html) for the full rule.

## Opening one: `%init`, `%init_slice`, `%init_empty`

| constructor | what it opens |
|---|---|
| `%init(filename, [maml], [filter], [sort], [qc], [qc_soft], [use_threads], [sample_fraction], [sample_seed], [exact])` | the whole file |
| `%init_slice(filename, row_lo, row_hi, ...)` | one contiguous row range; **no `sort` argument** |
| `%init_empty([nrows])` | nothing — an in-memory table with the same columns |

Optional arguments are shown in square brackets. Every one of `%init`'s is forwarded to [`parquet_open_table`](../tables/table.html) unchanged, except `exact`, which belongs to the kind conversion below.

**A plain `parquet_open_table(t, file)` will not compile for a generated type**, deliberately — it would skip the binding step and leave every accessor failing later, far from the cause.

`%init` checks every declared column before returning:

- a `file` column **must exist**, or the open aborts naming it (and suggesting `source: computed`);
- its **width must match** the declared `col_size`;
- if its kind differs from the declared one it is **converted** — widening (`int32`→`int64`, `float32`→`float64`, `int32`→`float64`) silently, and anything that can lose information with a warning naming the table and column. Pass `exact=.true.` to make a value that would not survive the conversion an error instead; note that this also gives up the single-pass decode, so it costs an extra pass over each converted column;
- a conversion that is not between numeric kinds of the same rank is refused outright;
- every `file` column is then **read in one pass**, so a generated table is fully materialized when `%init` returns.

## Editing the generated module

The generated file has **six user windows**, marked like this:

```fortran
    ! >>>>> USER SECTION (components) -- your own table parameters; preserved on regeneration
    ! >>>>> END USER SECTION (components)
```

| window | for |
|---|---|
| `uses` | extra `use` statements and module parameters |
| `components` | your own table parameters |
| `bindings` | your own type-bound procedures |
| `init_extra` | your own initialization, run by every constructor |
| `clone_extra` | anything the generator could not copy for you |
| `procedures` | your own procedure bodies |

Everything outside them is regenerated. **Never delete or reorder a marker**: the generator reads the windows out of the existing file before rewriting it, so a missing marker is a hard error (it will not guess where your code belonged) and a moved one is put back in canonical order.

### Adding your own state, and keeping `%clone` correct

Declare the component in the `components` window; the generator does the rest:

```fortran
    ! >>>>> USER SECTION (components) -- your own table parameters; preserved on regeneration
        real(real64) :: zeropoint = 0.0_real64
    ! >>>>> END USER SECTION (components)
```

Regenerate, and it will have written `out%zeropoint = self%zeropoint` into `clone_extra` and `self%zeropoint = 0.0_real64` into `init_extra`. So:

- **`%clone` and `%clone_structure` carry your component across.** They call `clone_extra` as their last action, dispatching on the table's dynamic type.
- **Every constructor resets it.** `init_extra` runs at the end of `%init`, `%init_slice` and `%init_empty` alike — so **set your own parameters *after* `%init` returns**, not before.

A component the generator cannot handle safely is **reported by name** when you run it, and left for you to handle in the window:

- a **`pointer`** component — only you can say whether a clone should alias or copy its target;
- a **derived-type** component with no default initializer — the literal reset would be `self%x = <typename>()`, and that default structure constructor is rejected by some compilers when the type embeds another module's private components. Give the component an initializer, or reset it yourself.

> **Prefer plain scalars here.** `parquet_table` is a finalizable type, and adding allocatable or deeply nested components to a type that extends it makes the compiler generate a deeper recursive walk at every `intent(out)` entry and every finalization. This project has hit three separate compiler bugs in exactly that machinery on exactly this type — one of which segfaulted a compiler inside its own runtime. A large or deeply nested payload belongs in a separate object your table does not own.

### Column names you cannot use

A field name becomes a type-bound procedure, so it cannot collide with one `parquet_table` already has. The generator refuses the schema (naming the field) rather than emitting a module that fails to compile. Several of the reserved names are entirely plausible column names:

```
append  cast  clone  col  column_names  drop_column  filename  generation  get  get_element
get_slice  has_column  is_null  kind  ncols  nrows  prefetch  reload  residency  row  set
set_element  set_null  sort_by  truncate  unit  width   ... and the rest of parquet_table's API
```

plus `init`, `init_slice`, `init_empty`, `init_extra`, `clone_extra` and `bind_predefined`, which the generator itself adds. Two field names differing only in case are refused for the same reason (Fortran identifiers are case-insensitive), as is any name that is not a valid Fortran identifier.

If you cannot rename the column in the file, rename it *for the table* with a read-in MAML's `extra: remap:` (see [Renaming columns for reading](../schema/maml-format.html#renaming-columns-for-reading-with-extra-remap)) and declare the table-facing name in your schema.

## A schema with no fields

An empty (or absent) `fields:` is valid, and generates a **bare `parquet_table` extension**: the type, all three constructors, both hooks and all six windows, with no accessors and nothing predefined. That is a useful starting point if you want your own named table type with your own parameters and procedures, over columns you will reach by name.

## What the library side does

The generated code is deliberately thin — a data table and one delegation per accessor. Every rule lives in the library, in [`%bind_predefined`](../tables/table.html), so a downstream project gets a fixed rule by upgrading rather than by regenerating. The same is true of `clone_extra`, which is an ordinary overridable binding on `parquet_table`: you can extend `parquet_table` by hand and use both without the generator at all.

## This library's own example

`table_types/maml_example4.maml` generates `src/parquet_table_example.f90` — module
`parquet_table_example`, holding `type :: parquet_table_test` — which ships with the library as a
worked example. Nothing else in the library uses it; it is there to be read, and to be exercised by
the `table_codegen` test suite, which calls all five accessor forms on every one of its fourteen
declared columns. It is generated, committed and `--check`ed like any other generated file, so it
is also a live demonstration that the round trip works: its `components` window carries a
`zeropoint` parameter, and the `clone_extra`/`init_extra` statements next to it were written by the
generator from that declaration.
