---
title: Struct columns with parquet_struct_column
---

`parquet_struct` stores a column whose every row holds **one value per declared field**, and whose
fields may have different types. `struct<name: string, age: int32>` is one column in which every
row carries a string and an integer. That is what the Parquet `STRUCT` logical type describes.

A struct column can be **read from a Parquet file** with `parquet_read_column` and **written back**
with `parquet_write_column`, whole or one row group at a time — see
[Reading a struct column from a file](#reading-a-struct-column-from-a-file) and
[Writing a struct column to a file](#writing-a-struct-column-to-a-file) below.

**This is not the only way to reach a struct's contents.** A struct's leaves are also addressable
as ordinary columns by their *dotted paths* —
`parquet_read_column(reader, "person.age", ages)` — to any depth of nesting, and that mechanism is
permanent. The two are complementary and the choice is about what you want back:

| | dotted path | `parquet_struct_column` |
|---|---|---|
| what you get | one flat column per leaf | the whole struct as one object |
| nesting | any depth | a field may be a list or a map; a field that is a struct is not read as one |
| the struct's own nullness | not visible — it is combined into each leaf's mask | `%is_null(i)`, separately |
| writing | not available | `parquet_write_column` |

The last row of that table is the one that decides most cases: a dotted read cannot tell "the
struct instance is absent" from "the struct is present and this field is null", because Parquet
stores both as a null leaf. A struct column can, and does.

The module itself is **independent**: a program whose only import is `use parquet_struct` compiles
eleven Fortran files and never reaches this library's C++ bindings (the *package* still links
Arrow; see [Choosing a module](../operating/choosing-a-module.html)). Reading from and writing to a
file naturally do reach them, and go through `use parquet` (or `use parquet_io`) like every other
read and write.

## A first example

```fortran
use parquet_struct
type(parquet_struct_column), target :: sc
type(parquet_struct_row) :: row
integer(int32) :: age
character(len=:), allocatable :: name

call sc%init(["name", "age "], [PK_STRING, PK_INT32])   ! the field set, fixed here

call sc%append_row()                                     ! row 1: present, both fields null
call sc%set_field(1, "name", "Ada")
call sc%set_field(1, "age",  36_int32)

call sc%append_null_row()                                ! row 2: the struct instance is absent

call sc%append_row()                                     ! row 3: present, `age` left null
call sc%set_field(3, "name", "Cy")

row = sc%view(1_int64)
call row%get_field("name", name)                         ! "Ada"
call row%get_field("age",  age)                          ! 36
print *, sc%is_null(2_int64)                             ! .true.  -- row 2 has no struct
print *, sc%is_null(3_int64)                             ! .false. -- row 3 has one, with a null field
```

**The column must be declared `target`** if you take a row handle from it, exactly as
`parquet_string_column` and `parquet_list_column` require: a handle stores a pointer into the
column, and Fortran leaves that pointer undefined otherwise.

## How a struct column is stored

Each declared field is **one `parquet_column`**, `nrows` long, carrying that field's own kind, its
own per-row null bitmap and its own unit string. The struct's own row nullness is a separate,
lazily allocated bitmap beside them.

```
field names   "name"        "age"
fields(1)     [Ada][ - ][Cy]        <- a parquet_column of kind PK_STRING
fields(2)     [ 36][ - ][ - ]       <- a parquet_column of kind PK_INT32
validity      [  1][  0][  1]       <- the STRUCT's own nullness, 1 = present
                    ^
                    row 2: the instance is absent
```

That is Arrow's own layout — a struct array is a validity bitmap over N equally long child arrays
— so handing these buffers to Arrow is a copy rather than a translation.

## The field set is fixed at `%init`

`%init(names, kinds, [nrows])` declares the fields, and nothing afterwards can change them.
Square brackets mark an optional argument here and throughout this page.

- **`names`, `kinds`** — the field set, in order. Every rule below is checked against them.
- **`nrows`** — how many **null** rows to create up front, defaulting to none. It is the presizing
  form of `%append_null_row` and is what a reader hands over when the row count is known first.

There is deliberately no per-field unit argument: a struct field's temporal unit cannot be
declared anywhere, so it could not be honoured — see
[Temporal fields are written at microseconds](#temporal-fields-are-written-at-microseconds).

Every rule below is checked, each with its own error message:

- at least one field — Arrow cannot construct a zero-field struct;
- `names` and `kinds` the same length;
- every name non-blank, unique, and free of `.` — a dot would collide with the dotted path a
  struct's own fields are addressed by;
- `nrows`, when given, not negative;
- every kind one of the nine scalar kinds (`PK_INT32`, `PK_INT64`, `PK_FLOAT32`, `PK_FLOAT64`,
  `PK_LOGICAL`, `PK_STRING`, `PK_DATE`, `PK_TIME`, `PK_TIMESTAMP`).

**A container kind is refused here, and that is not the same as being unsupported.** `kinds(:)`
carries one discriminator per field, while a list or map field is a kind *plus* an inner schema,
which `%init` has no way to express — so `PK_LIST`, `PK_MAP` and `PK_STRUCT` get their own message
directing you to `%adopt_fields`, which does accept them. That is the route to a struct holding a
list or a map: build the inner container, hand it to a `parquet_column` with `%adopt_container`,
and pass that column to `%adopt_fields`. Such a column reads back from a file but cannot be
written — see [What this module does not do yet](#what-this-module-does-not-do-yet).

Declaring the fields up front rather than inferring them from the first row appended is what lets
`%field_count()` and `%field_name(k, name)` answer for an **empty** column — which a writer has to
ask before any row exists, in order to build the file's schema at all. `%field_name` is a
subroutine writing into a `character(len=:), allocatable`, not a function returning one:

```fortran
character(len=:), allocatable :: nm
call sc%field_name(2, nm)                ! nm is now "age"
```

Field names are trimmed, so `["name", "age "]` declares `name` and `age`.

## Row nullness and field nullness are different things

A struct column has **two independent null levels**, and confusing them is the single easiest
mistake to make here:

- **A null ROW** means the struct instance is absent. `%is_null(i)` answers this, and
  `%append_null_row()` / `%set_null(i)` create it.
- **A null FIELD** means the struct is present and one of its values is missing.
  `%get_field(name, value, is_valid=ok)` answers this through `ok`, and `%set_field_null(i, name)`
  creates it.

**A row whose every field is null is NOT a null row.** Those two states are genuinely different and
both are representable:

```fortran
call sc%append_null_row()    ! the struct is absent
call sc%append_row()         ! the struct is present, and every field of it happens to be null
```

Every field of both reads back null, so no field query can tell them apart — `%is_null(i)` is what
does.

## Reading a field: two forms

A Fortran function has one compile-time return type, so `%field(name)` cannot hand back the
field's *value* — it returns another handle, narrowed to that field. Materializing a value is a
separate step, and there are two ways to write it:

```fortran
call row%get_field("age", age)          ! name it and materialize, in one call

slot = row%field("age")                 ! narrow ...
call slot%get(age)                      ! ... then materialize
```

Both are supported and both do the same thing. **They cannot be combined** —
`call row%field("age")%get(age)` does not compile, because Fortran does not allow a type-bound
`call` on a function result.

Reading a field through the wrong type, or calling `%get` on a handle that has not been narrowed,
is always a hard error — a type mismatch is never given a soft-fail escape. An **unknown field
name** is a different class and does have one:

```fortran
slot = row%field("nope")                 ! aborts, listing the declared field names
slot = row%field("nope", warn=.true.)    ! warns, and slot%is_valid() is .false.
```

`%field_index(name)` is the third option: it answers **0** rather than aborting, so it doubles as a
presence test and as the lookup to hoist out of a loop.

A handle answers three things about itself. `%row_index()` gives the row it refers to, which is
what a loop over handles reports when something goes wrong with one of them. `%is_valid()` reports
whether it still refers to a live row — a default-constructed handle answers `.false.`, and every
other accessor on it, `%row_index()` included, aborts rather than returning a plausible answer.
`%is_narrowed()` distinguishes a whole-row handle from one `%field` has narrowed, and
`%field_kind()` gives the narrowed field's kind, which is what code dispatching over the nine kinds
needs before it can pick a `%get`.

## Setting a field: by name or by index

```fortran
call sc%set_field(i, "age", 36_int32)    ! by name
k = sc%field_index("age")
call sc%set_field(i, k, 36_int32)        ! by index -- the form to use inside a loop
```

Both accept an `integer` or an `integer(int64)` row index. The index form is the primitive: a name
costs a string comparison per call, and filling a column is `nrows * nfields` calls.

Writing into a field the column does not declare is always an error — unlike a *read*, a write has
no sensible soft outcome.

## Null rows keep their field values

`%set_null(i)` marks the row absent and leaves its field values exactly where they are, which is
what makes it O(1) and lets `%clear_null(i)` restore the row intact. Such values are unreachable
while the row is null: `%get_field` reports `is_valid = .false.` and yields the type's default.

**They do not survive a file round trip.** Parquet's definition levels cannot encode "the struct is
absent but its field is present", so a written and re-read column has every field of every null row
null. A caller comparing an in-memory column against a read-back one must expect that.

## Growing, copying and reordering

- `%append_row()` adds a present row with every field null; `%append_null_row()` adds an absent one.
- `%reserve(n)` presizes every field; `%shrink_to_fit()` releases the slack.
- `%deep_copy(out)` produces a fully independent copy; `%move_from(src)` transfers the storage and
  leaves `src` empty.
- `%gather_rows(idx)` rebuilds the column so row `k` becomes the row that was at `idx(k)` — one
  operation serving reordering, filtering and duplication, applied to every field and to the row
  bitmap together.
- `%adopt_fields(names, fields, [row_valid])` builds a whole column from field columns you already
  have, moving them in. The row count and every field's kind come from what you hand over, and it
  is the one entry point that accepts a **container** field — see
  [The field set is fixed at `%init`](#the-field-set-is-fixed-at-init).
- `%append_from(src)` appends every row of another struct column, which must declare the same
  fields in the same order with the same kinds; each mismatch has its own message.
- `%validate([message])` checks the class invariants — one name per field column, every field the
  same length as the struct, every field kind supported, and the row bitmap allocated whenever the
  column claims to hold a null — and is cheap enough to call from a test after any structural
  change.
- `%summary(out)` composes a one-line description, and `%kind_text(out)` gives just the type
  spelling, `struct<name:string,age:int32>`. Both write into a string rather than printing, so the
  caller decides where it goes.

## Querying a column without reading a row

Four queries answer about the column as a whole, without touching a row or allocating anything:

- **`%size()`** — the number of rows.
- **`%field_count()`** — how many fields each row holds, fixed by `%init`.
- **`%null_count()`** — how many rows are **null structs** — absent instances. A present row whose
  fields are all null is not counted here; see
  [Row nullness and field nullness](#row-nullness-and-field-nullness-are-different-things).
- **`%capacity()`** — how many rows the column can hold before it grows again, which is the third
  member of the `%reserve`/`%shrink_to_fit` trio and the way to see what those two did.

**`%clear()`** is the matching mutation: it empties the column and returns it to the uninitialized
state, so the field set must be declared again with `%init`. Every outstanding row handle is
invalidated by it.

## Putting a struct column into a `parquet_column`

```fortran
type(parquet_column) :: col
type(parquet_struct_column) :: sc
class(parquet_container_column), allocatable :: cc

call sc%init(["v"], [PK_INT32])
call sc%clone_into(cc)               ! or build the container and move_alloc it
call col%adopt_container(cc)         ! col%kindof() is now PK_STRUCT
```

**The variable handed over must be declared `class(parquet_container_column), allocatable`**, not
as the struct type: `%adopt_container`'s argument is an allocatable polymorphic one, so a
`type(parquet_struct_column), allocatable` actual will not compile. `%clone_into(cc)` is the
one-call way to fill it; `%move_alloc` from a container you built yourself does as well.

`%adopt_container` moves the struct column in; `cc` comes back deallocated. From that point the
column's kind is `PK_STRUCT` and `parquet_kind_is_container(col%kindof())` is `.true.`.

## Threading

The type itself takes no locks and reads no settings. If several threads fill one column, call
`%ensure_validity()` **before** the parallel region: it materializes both the row bitmap and every
field's own validity storage, so the first null cannot race with a lazy allocation.

## Reading a struct column from a file

```fortran
use parquet
type(parquet_reader) :: r
type(parquet_struct_column), target :: sc

call parquet_open_reader(r, "people.parquet")
call parquet_read_column(r, "person", sc)              ! the whole column
call parquet_read_column_chunk(r, "person", 2, sc)     ! or one row group
```

The field set — names, order and kinds — comes from the **file**, not from the caller; `%init` is
not called first and anything the column held is replaced. Filtering, sampling and sorting compose
with it as with any other read: they only ever remove or reorder rows.

A field whose own type is a **list or a map** is read as that container, and is reached with
`%nested` on the narrowed row handle — see
[Nested containers](list-columns.html#nested-containers) for the shape, which is the same one a
list's own nested payload uses. A field that is itself a **struct** is refused, with a message
naming the field and pointing at its dotted leaves, which read at any depth.

## Writing a struct column to a file

```fortran
use parquet
type(parquet_writer) :: w
type(parquet_struct_column), target :: sc

call parquet_open_writer(w, "people.parquet")
call parquet_write_column(w, "person", sc)
call parquet_close_writer(w)
```

The file's field set, names and order come from `sc` itself. In a schema-enforced writer the column
is declared with the bare token `struct`:

```
fields:
  - name: person
    data_type: struct
```

There is **no MAML syntax for a struct's fields** — the field names, kinds and order come from the
`parquet_struct_column` object itself, which is data the caller already holds.

The other schema keys apply as follows, and the refusals are narrower than "not supported":

- **`col_size:`** — `auto` and any value **above 1** are rejected, since a struct row is one
  instance and has no width. `col_size: 1` is the default and is accepted, meaning nothing.
  `schema%set_col_size` refuses a struct column outright, on the same terms.
- **`array_size:`** — `auto` is rejected, as it is for every non-`string` column. A positive value
  is accepted and never consulted.
- **`qc: min:`/`max:`** — rejected; quality-control ranges apply to scalar leaves only.
- **`qc: miss:`** — supported, and applies to row nullness (an absent struct instance).

A `write_maml=.true.` sidecar carries **neither** size key for a struct column, even where the
source MAML declared one: what says the width is per-row is the `data_type:` token.

A **protected** column (`extra: protected_cols:`) may contain no Null at either level — neither a
null row nor a null field value. That is the only way to declare a *streamed* struct column
null-free, because a struct column carries its null state inside itself and there is no mask whose
presence could stand in for "might this contain a Null?".

Its opposite, **`extra: nullable_cols:`, applies at every level too**: a declared struct column's
row field and each of its field children are written nullable, whatever the values contain. A
streamed struct column is already nullable at every level unless it is protected, so the key changes
nothing there; it is the whole-column write, whose flags are otherwise read off the values, that it
governs. A column named under both keys is refused.

## Temporal fields are written at microseconds

A `date`, `time` or `timestamp` field is written at **microsecond** resolution, timezone-naive, and
there is no way to ask for anything else. The unit a temporal column normally takes from its schema
declaration has nowhere to come from here: the MAML token is a bare `struct`, so no per-field unit
can be declared.

**A value carrying sub-microsecond precision therefore aborts rather than being truncated.**
`parquet_timestamp` and `parquet_time` hold nanoseconds internally, so a field set from one keeps
whatever precision it was given, and the write refuses it:

```fortran
call ts%set(2024, 3, 1, 12, 0, 0, 123456789)   ! nanosecond-of-second: not a whole microsecond
call sc%set_field(1, "when", ts)
call parquet_write_column(w, "ev", sc)          ! aborts, naming the field and the file
```

The same value with a whole number of microseconds (`123456000`) round-trips exactly.

Both kinds report it the same way, naming the field:
*"timestamp value has finer precision than the column's declared unit for field when"*. They reach
it by different routes — a `time` field is checked as it crosses into Arrow, a `timestamp` field
before it is converted — so the two aborts differ in exit status but not in what they tell you.

Nothing is lost silently: the choice here is an abort over a quiet truncation. But it does mean a
struct is not the place for nanosecond timestamps. Where that precision matters, write the leaf as
its own column, whose `timestamp[ns]` declaration is honoured — see
[Dates, times and timestamps](date-time.html).

## In a `parquet_table`

A table column can be a `parquet_struct_column`. See
[Container columns in a table](../tables/table.html) for the five accessors it gets
(`%col`, `%get`, `%set`, `%add_column` and a handle's `%ref`), the two element-granular queries it
declines, and how row-structural mutations carry it along.

Build it with `%add_column` rather than expecting one from a file: a table lists a struct's
LEAVES under their dotted paths and never the struct itself, so an opened table never holds one.

## What this module does not do yet

- **No nested WRITING.** A field that is a list or a map reads, and cannot be written back:
  `parquet_write_column` refuses such a column, naming the field.
- **No struct-valued field, as a struct.** A field whose own type is a `STRUCT` is refused, because
  reading it would need an intermediate struct to be addressable by a dotted path — which this
  library declines on purpose, since it would change what a column-iterating caller sees. The
  dotted-path reader reaches its leaves at any depth, unchanged.
- **A struct read FROM A FILE is listed as its dotted leaves**, not as a struct column, because
  `parquet_get_column_names` expands a top-level struct into `person.name`, `person.age` and so on
  and never emits `person` itself. `parquet_read_column(reader, "person", sc)` reads it as a struct
  perfectly well — but an opened `parquet_table`, which takes its columns from that listing, never
  holds one. Building a struct column in memory and adding it with `%add_column` is unaffected.
