---
title: Generated table types: named accessors from a MAML schema
---

A program that always reads the same columns can have them as **named accessors on its own table
type**, generated from a MAML schema, instead of naming them as strings everywhere. This example is
complete and runs as it stands — it uses `parquet_table_example`, the worked example this library
ships (see the last section), and needs no file, because `%init_empty` builds the table in memory:

```fortran
program generated_table_quickstart
    use parquet
    use parquet_table_example, only : parquet_table_test   ! the generated module
    use iso_fortran_env, only : int64, real64
    implicit none
    type(parquet_table_test) :: t
    real(real64), pointer :: ra(:), one
    !
    call t%init_empty(3)          ! every column the schema declares, three all-null rows
    call t%set("uberid", [101_int64, 102_int64, 103_int64])
    call t%set("ra", [10.5_real64, 20.25_real64, 30.125_real64])
    !
    ra  => t%ra()                 ! zero-copy pointer into the live storage
    one => t%ra(2)                ! row 2 alone, still zero copy
    print '(i0,2(1x,f0.6))', t%nrows(), one, sum(ra) / t%nrows()   ! 3 20.250000 20.291667
end program generated_table_quickstart
```

Reading a file is the same type and one different constructor:

```fortran
call t%init("catalogue.parquet")   ! opens, checks every declared column, converts, reads
ra => t%ra()
```

`tools/generate_user_table_code.py` writes that module from a MAML file. The generated type
**extends [`parquet_table`](../tables/table.html)**, so nothing is given up: `%nrows`, `%get`,
`%filter_rows`, `%clone`, `%append`, `%row`, `parquet_write_table` and the rest all work exactly as
they do on a plain table.

Like [`tools/generate_parquet_maml.sh`](embedding-maml-schemas.html), this script is meant to be
**copied into your own project**: you keep your schemas, you run the generator, you commit its
output, and your build needs no code-generation step.

## The schema

A table-type schema is an ordinary MAML file. Put these in their own directory — `table_types/` by
convention, kept separate from `schemas/` because the two are read for opposite purposes: `schemas/`
describes files you *write*, `table_types/` describes types you *generate*.

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

- **`dataset:`** names the generated module, and therefore the file (`parquet_table_example.f90`).
  **`table:`** names the type, as `parquet_table_<table>`. Two schemas that pick the same `dataset:`
  are refused.
- **`dataset:` may not name what `table:` derives.** A module cannot declare a type — or a procedure
  — of its own name, so `dataset: parquet_table_x` together with `table: x` would not compile; the
  generator refuses it up front rather than emitting a broken module. Note this is *not* "`dataset:`
  and `table:` must differ": `dataset: foo` with `table: foo` is fine, because the type is
  `parquet_table_foo`. If you hit the collision, give `dataset:` a different name — this library's
  own precedent is a plural (module `parquet_strings` holds type `parquet_string`).
- **If you publish your package, the module name must satisfy your own module-naming rule.** fpm's
  registry enforces a package prefix, so a module called `example` inside a published `src/` is
  rejected while `parquet_table_example` is fine. `fpm build` reports this immediately, so it is not
  a silent trap — but it is worth knowing before choosing `dataset:`.
- **`author:`** becomes the generated file's `! Author:` header line. Without it the header says the
  file was generated and names no author.
- **`source:`** is `file` (the default) or `computed`. A computed column gets its accessor and a
  slot of all-null rows, and no file column is read for it — so a column your program calculates is
  a *declared intent* rather than an error. **Its name must not exist in the file**: `%init` aborts
  with *"the schema declares column 'flux' as `source: computed`, but the table already has a column
  of that name"* if it does. See [Writing one out](#writing-one-out) for when that bites.
- **`unit:`** is not decoration here. `%init` gives the column that unit — but only if it does not
  already have one from a read-in `maml=`, which describes the physical file and wins. For a
  computed column, and for a file opened without a MAML, the schema is the only source of a unit
  there is, so `%unit(name)` would otherwise be `""`.
- **`info:`** becomes the accessor's own doc-comment in the generated module, alongside the unit,
  the data type and the width — so it reaches whoever reads the code, not just whoever reads the
  schema.
- **`array_size:`** is validated here but does **not** affect the generated accessors: a string
  column's accessor shape is the same whatever width it declares. It matters to the file's other
  role as a write schema, which is where the stored width is decided.
- Everything else is ordinary MAML (see [The MAML metadata format](../schema/maml-format.html)), and
  the same file still works as a **write schema** — which is convenient, since a program that reads
  a catalogue usually writes one with the same columns.
- `col_size: auto` / `array_size: auto` are **rejected** here: an accessor's kind and rank have to
  be known when the code is generated, and `auto` is resolved at write time.

## Running the generator

```bash
tools/generate_user_table_code.py                     # every *.maml in table_types/ -> src/
tools/generate_user_table_code.py --dir=my_types      # a different input directory
tools/generate_user_table_code.py --out-dir=lib       # a different output directory
tools/generate_user_table_code.py --check             # verify the committed output is current
tools/generate_user_table_code.py --self-test         # run the generator's own tests
```

Explicit paths work too — `tools/generate_user_table_code.py table_types/one.maml` — and `--dir` is
then ignored.

**Commit the generated `.f90`** and add `--check` to your CI, the same way this project does. That
is what makes the file safe to hand-edit (see below): `--check` fails the build if a *generated*
region was edited, or if the committed file is stale relative to its MAML — and it tells you which
of the two happened.

## The accessors

| declaration | `%x()` | `%x(i)` | `%x(lo, hi)` |
|---|---|---|---|
| scalar numeric / logical / temporal | `pointer :: p(:)` | `pointer :: p` — row `i` | `pointer :: p(:)` — rows `lo:hi` |
| `col_size: n` (vector) | `pointer :: p(:,:)`, `(element, row)` | `pointer :: p(:)` — row `i`'s `n` values | `pointer :: p(:,:)` — rows `lo:hi`, full width |
| `string` | `type(parquet_string_column), pointer :: p` | `type(parquet_string)` handle | `type(parquet_string), allocatable :: h(:)` |
| `string` with `col_size: n` | *(none — see below)* | *(none)* | *(none)* |

Three things to know:

- **The index is always a ROW index**, on every column. `%crd(3)` is row 3's three values, *not*
  element 3 of every row. Reach an element with `p => t%crd(i)` and then `p(e)`. (The alternative
  cannot coexist with the range form: `%crd(3,6)` would have to mean both "rows 3 to 6" and "element
  3 of row 6".)
- **`%x()` is assignable, but not subscriptable.** `t%ra() = 0.0_real64` works — a function
  reference returning a data pointer is a variable — but `t%ra()(3)` is a *syntax error* in Fortran.
  Take the pointer first, or use `%ra(3)`.
- **The indexed forms repeat the column lookup on every call.** They are for readable one-off
  access; in a loop, take `p => t%ra()` once and index `p` instead. `i`, `lo` and `hi` may each be
  `int32` or `int64`, so a plain default `INTEGER` works.

A **string** column gets a second accessor, `%<name>_chr(arr)`, a subroutine that copies the column
out as a `character` array sized to the longest value present. A string *vector* column gets
**only** that form, because `%col` has no pointer specific for `PK_STRING_VEC` to alias.

**Every indexed form is bounds-checked, and an out-of-range index aborts** rather than handing back
a pointer into nothing: `%ra(99)` on a three-row table stops with *"row index out of range for
column 'ra'"*, and `%ra(2, 99)` with *"row range out of range for column 'ra'"*. An **empty** range
is where the two column families differ — `%ra(5, 4)` on a numeric or temporal column is accepted
and yields a zero-length pointer, matching Fortran's own section rules, while the same call on a
string column aborts, because `parquet_string_column` rejects an empty range outright.

Every accessor pointer is **invalidated by a row-structural mutation** — every verb in the **row**
class of [Changing a table](../tables/table-mutate.html#changing-a-table), from `%filter_rows` and
`%sort_by` through `%explode`, `%drop_duplicates` and `%join`. That table is the list; it is not
repeated here, because a copy of it is a copy that will disagree. Fortran cannot detect a stale
pointer, so take it again afterwards, and see
[What "detaching" means](../tables/table-mutate.html#what-detaching-means) for the full rule.

## Opening one: `%init`, `%init_slice`, `%init_empty`

| constructor | what it opens |
|---|---|
| `%init(filename, [maml], [filter], [sort], [qc], [qc_soft], [use_threads], [sample_fraction], [sample_seed], [list_columns], [bounded], [exact])` | the whole file |
| `%init_slice(filename, row_lo, row_hi, ...)` | one contiguous row range; **no `sort` argument** |
| `%init_empty([nrows])` | nothing — an in-memory table with the same columns, `nrows` of them all null |

Optional arguments are shown in square brackets. Every one of `%init`'s is forwarded to
[`parquet_open_table`](../tables/table-open.html) unchanged, except `exact`, which belongs to the
kind conversion below. `row_lo`/`row_hi` and `nrows` may each be `int32` or `int64`.

**`filter=`, `sort=` and `qc=` name columns in the table's own vocabulary**, which for a column
renamed by a read-in `maml=` is *not* what the file calls it — see
[Column names: yours, not the file's](../tables/table-open.html#column-names-yours-not-the-files).

**`%init_empty(nrows)`'s rows really are null**, not zero: an unfilled column should say it holds
nothing, and a freshly created column with no null bitmap would claim every row is valid. Prefer
`%set(name, values)` to fill one — it writes the values *and* clears the null flags together.
Writing through an accessor pointer sets only the values, leaving each row still marked null until
`%clear_null(name, i)` says otherwise.

**A plain `parquet_open_table(t, file)` will not compile for a generated type**, deliberately — it
would skip the binding step and leave every accessor failing later, far from the cause.

`%init` checks every declared column before returning:

- a `file` column **must exist**, or the open aborts naming it (and suggesting `source: computed`);
- its **width must match** the declared `col_size`;
- if its kind differs from the declared one it is **converted** — widening (`int32`→`int64`,
  `float32`→`float64`, `int32`→`float64`) silently, and anything that can lose information with a
  warning naming the table and column. Pass `exact=.true.` to make a value that would not survive
  the conversion an error instead; note that this also gives up the single-pass decode, so it costs
  an extra pass over each converted column;
- a conversion that is not between numeric kinds of the same rank is refused outright;
- every `file` column is then **read in one pass**, so a generated table is fully materialized when
  `%init` returns.

## Writing one out

`parquet_write_table` takes `class(parquet_table)`, so a generated table writes itself with no
unwrapping — `call parquet_write_table(t, "out.parquet")`. Together with `%init_empty` that is the
output-catalogue shape: build the columns in memory, fill them through the accessors, write.

**It writes every column the table holds, and a `source: computed` column is one of them.** That is
deliberate — a column your program computed is exactly the kind you want stored — but it has one
consequence worth knowing before you meet it. The written file now contains a column your schema
declares as *not* coming from the file, so opening it with `%init` aborts:

```
ERROR STOP parquet_table: bind_predefined: the schema declares column 'flux' as `source: computed`,
but the table already has a column of that name (schema 'my_types.maml') (file 'out.parquet', ...)
```

Nothing is lost — the values are in the file and read back fine — and the failure is a clean abort
rather than a wrong answer. Two ways past it:

- **read it with a plain table** — `call parquet_open_table(plain, "out.parquet")` reaches every
  column by name and has no schema to contradict; or
- **drop the computed column before writing** — `call t%drop_column("flux", force=.true.)`, then the
  result opens with `%init` and the computed slot is recreated, all null, as usual. `force=` is
  required because `%init` marked every declared column predefined.

**That mark guards three verbs, and one of them guards by omission.** `%drop_column` and
`%drop_columns` refuse a predefined column without `force=.true.`, and so does `%keep_columns` —
which is the whole reason it takes a `force=` at all, since a projection drops a column by *not*
naming it rather than by naming it. `%rename_column` refuses one outright, with no override, because
an accessor is bound to the name.

**It guards a change of *kind* too, on the two verbs that can make one.** An accessor's type is
fixed when the module is generated, so `%parse_column` and `%format_column` refuse a predefined
column **in place** without `force=.true.` — pass `to_name=` to write the result into a new column
beside it, which is never refused. **`%cast` is the exception and carries no such guard**: it
converts a predefined column in place, after which the accessor aborts on its next call with a kind
mismatch rather than returning anything wrong — a clean failure, but at a site other than the one
that caused it.

A write schema does not help here: restricting the write to omit one column omits every column you
did not declare, and `%init` then fails on the first *missing* one instead.

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

Everything outside them is regenerated. **Never delete or reorder a marker**: the generator reads
the windows out of the existing file before rewriting it, so a missing marker is a hard error (it
will not guess where your code belonged) and a moved one is put back in canonical order.

There is one exception, and it is the case that costs you work rather than reporting it: a file with
**no markers at all** is treated as a fresh start and regenerated whole. That is the documented way
to start over — delete the file, or delete every marker — but it means the hard error above protects
you only while at least one marker survives.

### Adding your own state, and keeping `%clone` correct

Declare the component in the `components` window; the generator does the rest:

```fortran
    ! >>>>> USER SECTION (components) -- your own table parameters; preserved on regeneration
        real(real64) :: zeropoint = 0.0_real64
    ! >>>>> END USER SECTION (components)
```

Regenerate, and it will have written `out%zeropoint = self%zeropoint` into `clone_extra` and
`self%zeropoint = 0.0_real64` into `init_extra`. So:

- **`%clone` and `%clone_structure` carry your component across.** They call `clone_extra` as their
  last action, dispatching on the table's dynamic type.
- **Every constructor resets it.** `init_extra` runs at the end of `%init`, `%init_slice` and
  `%init_empty` alike — so **set your own parameters *after* `%init` returns**, not before.

A component the generator cannot handle safely is **reported by name** when you run it, and left for
you to handle in the window:

- a **`pointer`** component — only you can say whether a clone should alias or copy its target;
- a **derived-type** component with no default initializer — the literal reset would be `self%x =
  <typename>()`, and that default structure constructor is rejected by some compilers when the type
  embeds another module's private components. Give the component an initializer, or reset it
  yourself.

> **Prefer plain scalars here.** `parquet_table` is a finalizable type, and adding allocatable or
> deeply nested components to a type that extends it makes the compiler generate a deeper recursive
> walk at every `intent(out)` entry and every finalization. This project has hit three separate
> compiler bugs in exactly that machinery on exactly this type — one of which segfaulted a compiler
> inside its own runtime. A large or deeply nested payload belongs in a separate object your table
> does not own.

### Column names you cannot use

A field name becomes a type-bound procedure, so it cannot collide with one `parquet_table` already
has. The generator refuses the schema (naming the field) rather than emitting a module that fails to
compile. Several of the reserved names are entirely plausible column names:

```
append  cast  clone  col  column_names  drop_column  filename  generation  get  get_element
get_slice  has_column  is_null  kind  ncols  nrows  prefetch  reload  residency  row  set
set_element  set_null  sort_by  truncate  unit  width   ... and the rest of parquet_table's API
```

plus `init`, `init_slice`, `init_empty` and `init_extra`, which the generator itself adds
(`clone_extra` and `bind_predefined` are already in the inherited list above). Two field names
differing only in case are refused for the same reason (Fortran identifiers are case-insensitive),
as is any name that is not a valid Fortran identifier.

**Length is bounded too, and a `string` column is bounded tighter than the rest.** A field name may
be 63 characters, Fortran's identifier limit — but a `string` field also gets a `<name>_chr`
accessor, so its own name may be at most **59**. The same name is therefore legal on an `int64`
column and refused on a `string` one. `table:` is bounded the same way, since the type it derives is
`parquet_table_<table>`.

**A binding you write can collide too, in the other direction.** If your `bindings` window declares
a `procedure ::` or `generic ::` whose name an accessor is about to take, the generator refuses and
names it — rather than emitting a module that fails to compile with a duplicate-binding error
pointing at neither cause.

If you cannot rename the column in the file, rename it *for the table* with a read-in MAML's `extra:
remap:` (see [Renaming columns for
reading](../schema/maml-format.html#renaming-columns-for-reading-with-extra-remap)) and declare the
table-facing name in your schema.

## A schema with no fields

An empty (or absent) `fields:` is valid, and generates a **bare `parquet_table` extension**: the
type, all three constructors, both hooks and all six windows, with no accessors and nothing
predefined. That is a useful starting point if you want your own named table type with your own
parameters and procedures, over columns you will reach by name.

## What the library side does

The generated code is deliberately thin — a data table and one delegation per accessor. Every rule
lives in the library, in `%bind_predefined` — the checks listed under
[`%init`](#opening-one-init-init_slice-init_empty) above — so a downstream project gets a fixed rule
by upgrading rather than by regenerating. The same is true of `clone_extra`, which is an
ordinary overridable binding on `parquet_table`: you can extend `parquet_table` by hand and use both
without the generator at all.

## Extending `parquet_table` with your own type

`parquet_table` is designed to be extended, and two public entry points exist for that. Most
programs meet them through a generated table type — everything above — but both are ordinary API and
work just as well on a type you write by hand, with no generator involved.

**`clone_extra` is the hook `%clone` calls for an extension's own components.** `%clone` and
`%clone_structure` copy everything `parquet_table` itself holds, and cannot know about components an
extending type added; overriding this hook is how those come across. Both call it as their last
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

`%clone` has already checked that source and destination have the same dynamic type, so the guarded
branch always matches. **Without an override, an added component arrives default-initialized and
nothing reports it** — which is why the generator writes these assignments for you.

A concrete-typed override of `%clone` itself is not possible: an overriding procedure has to keep
every dummy argument's characteristics, so `out` cannot be narrowed from `class(parquet_table)`.
This hook is the supported substitute, and it keeps one name for one operation.

**`%bind_predefined` is what a generated type's `%init` calls.** It takes a column's declared name,
kind, width and whether it comes from the file, then checks each one against the file, converts it
to the declared kind, reads them all in one pass, and marks the slot *predefined* — which is what
makes `%drop_column` refuse it without `force=.true.`. It is public because a generated module is a
different module and `parquet_table`'s components are private; hand-written code that opened a table
with `parquet_open_table` already reaches every column by name and rarely needs it.
[`%init`](#opening-one-init-init_slice-init_empty) above lists the full contract.

**Opening an extension** goes through the parent component: `call
parquet_open_table(t%parquet_table, filename)`. `parquet_open_table`'s dummy is non-polymorphic on
purpose, so an extension that needs a binding step cannot be opened without it.
`parquet_write_table`, by contrast, takes `class(parquet_table)` and accepts an extending type
directly.

## This library's own example

`table_types/maml_example4.maml` generates `src/parquet_table_example.f90` — module
`parquet_table_example`, holding `type :: parquet_table_test` — which ships with the library as a
worked example. Nothing else in the library uses it; it is there to be read, and to be exercised by
the `table_codegen` test suite, which drives every emitted accessor shape across its fourteen
declared columns: all five specifics of each of the twelve numeric and temporal ones, both forms of
the scalar `string` one, and the copy-out form that is all a `string` vector column has. It is
generated, committed and `--check`ed like any other generated file, so it is
also a live demonstration that the round trip works: its `components` window carries a `zeropoint`
parameter, and the `clone_extra`/`init_extra` statements next to it were written by the generator
from that declaration.
