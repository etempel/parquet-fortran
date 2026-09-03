---
title: Configuration files with parquet_toml
---

`parquet_toml` reads and writes TOML configuration files. It is a convenience layer over
[toml-f](https://github.com/toml-f/toml-f), which does the parsing; what this module adds is
everything that makes a parsed document safe to *drive a program from*.

```fortran
use parquet_toml
use parquet_logging, only: pf_log_init

type(pf_toml) :: conf, gen
integer :: nproc
character(len=:), allocatable :: outdir

call pf_log_init(name = "myprog")             ! optional: where the diagnostics go
call pf_toml_load(conf, "config.toml")
call pf_toml_section(conf, "general", gen)
call pf_toml_get(gen, "nproc", nproc, default = 1)
call pf_toml_get(gen, "output_dir", outdir)   ! no default, so the key is required
call pf_toml_check_all(conf)                  ! anything unread is a mistake
call pf_toml_close(conf)
```

Signatures written out on this page show optional arguments in square brackets, with the comma
outside: `pf_toml_get(sect, key, value, [default])` means `default` may be omitted.

## The six things it adds

Each of these is a thing toml-f will not do for you, so every project that uses toml-f directly
ends up writing its own version.

1. **A wrong-typed value never leaves your variable undefined.** toml-f's own `get_value` reports
   a type mismatch through an optional argument nobody has to look at, and leaves its `intent(out)`
   result untouched — so `samples = 100.0` read as an integer sets nothing at all and the program
   runs on whatever was on the stack. Every getter here checks, and stops with the offending line
   quoted.
2. **A default is applied by this module, never by toml-f.** `get_value(..., default=)` *inserts*
   the default into the parsed document, which blinds any later check for keys nobody read. Nothing
   here writes to the document while reading.
3. **Whole-array reads for strings**, which toml-f has no getter for.
4. **Diagnostics that point at the source line**, from one call rather than four.
5. **A report for every key nobody read** — the only way to catch a misspelt *optional* key, which
   otherwise takes its default in silence and leaves the run using a value the user believes they
   overrode.
6. **A report for every section nobody read**, which is the same failure one level up.

## Loading, and the lifetime rule

```fortran
call pf_toml_load(doc, file, [status])            ! parse a file
call pf_toml_loads(doc, text, [name], [status])   ! parse a string
call pf_toml_new(doc, [name])                     ! an empty document to build
call pf_toml_close(doc)                           ! release it
```

Without `status`, a file that cannot be opened or does not parse is fatal, and toml-f's own
rendered diagnostic is written to the log first. With `status` the same two failures come back as
`PF_TOML_ERR_OPEN` or `PF_TOML_ERR_PARSE` and leave the handle closed, so "try this configuration,
else that one" is writable. That is the only soft failure in the module.

**A section handle borrows from its document, so close the document last.** `pf_toml_load` fills
the one handle that *owns* the parsed file; every handle `pf_toml_section` produces points into it.
Using a section handle after `pf_toml_close` is a dangling pointer. Closing a section handle is
itself a fatal error, because it would free a document other handles are still reading.

A `pf_toml_strings` value is the one exception: it copies its strings out of the document and stays
valid afterwards.

## Sections

```fortran
call pf_toml_section(parent, name, sect, [required], [found])        ! [name]
call pf_toml_section(parent, name, idx, sect, [required], [found])   ! [[name]] entry idx
n = pf_toml_section_count(parent, name)
yes = pf_toml_has_section(parent, name)
```

`parent` is any handle — the document or another section — so nesting is repetition, and the
handle's display path composes: `general`, then `general.nested`, then `region[3]`. That path is
what every later message quotes.

`required` defaults to `.true.`: a section you ask for is one your program needs. With
`required = .false.` an absent section leaves the handle **closed**, and reading from a closed
handle is fatal rather than returning defaults — guard the reads with `pf_toml_is_open`. Returning
defaults would make an absent section indistinguishable from an empty one, which is exactly the
silence this module exists to remove.

`pf_toml_section_count` answers `0` for an absent name, which makes this the whole idiom for an
optional repeated section:

```fortran
do i = 1, pf_toml_section_count(conf, "region")
    call pf_toml_section(conf, "region", i, ent)
    call pf_toml_get(ent, "id", id)
end do
```

**A key is looked up literally and never split on a dot.** `pf_toml_get(conf, "general.nproc", n)`
asks the root table for a single key spelled `general.nproc` and stops with absent-key; open
`[general]` first. TOML allows a dot inside a quoted key, so `"a.b" = 1` at the root is legal and
distinct from `[a]` / `b = 1`, and a dot-splitting getter could not read such a file at all.

**A key above the first section is read straight from the document handle**, with the same
diagnostics and the same coverage: `call pf_toml_get(conf, "title", title)`.

## Reading values

```fortran
call pf_toml_get(sect, key, value, [default])          ! scalar
call pf_toml_get(sect, key, values, [required])        ! array, exact length match
call pf_toml_get_alloc(sect, key, values, [required])  ! array, the file chooses the size
call pf_toml_get_strings(sect, key, strings, [required])
call pf_toml_get_level(sect, key, level, [default])
```

`value` is an `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`, `logical` or
`character(len=:), allocatable` scalar. `values` is a rank-1 array of any of those six, with
`character(len=*)` for the string form; `pf_toml_get_alloc` takes the same five numeric and logical
types and deliberately has no character form.

**Every form requires its key unless you opt out, and the two opt-outs differ by shape.** A scalar
opts out by taking a `default`; an array opts out with `required = .false.`, and then carries its
default *in the variable* — whatever it held on entry stands. That is what makes "inherit from the
previous entry unless this one overrides it" an ordinary assignment written before the call:

```fortran
this_region = previous_region                                  ! inherit everything
call pf_toml_get(sect, "airmass_limit", this_region%airmass, required = .false.)   ! then override
```

**Always write `default =` and `required =` as keyword arguments.** In fourth position a bare
`.true.` means "the default is true" for a `logical` scalar and "this key is required" for a
`logical` array. The compiler resolves it by rank and is never wrong; a reader might be.

A few rules worth knowing:

- An array's length must match `size(values)` **exactly, in both directions**. Silently using a
  prefix of a list that is too long pairs each value with the wrong slot, which nothing downstream
  could catch.
- A string element longer than `len(values)` is fatal rather than clipped. A silently truncated
  file name is the worst outcome available here.
- Reading a `real` accepts a TOML integer and converts it. Reading an `integer` does **not** accept
  a TOML float, and a value too large for `integer(int32)` is reported rather than wrapped.
- With `required = .false.`, `pf_toml_get_alloc` leaves its result **unallocated**, and
  `allocated(values)` is part of the contract.

### Lists of strings

`pf_toml_get_alloc` has no character form on purpose: a blank-padded array of one declared length
loses each element's own length and invites `trim` bugs at every use. A variable-length string list
is read into a `pf_toml_strings` instead, which keeps each element exact.

```fortran
type(pf_toml_strings) :: files
character(len=:), allocatable :: one

call pf_toml_get_strings(sect, "input_files", files, required = .false.)
do i = 1, files%count()
    call files%get(i, one)          ! `one` is allocated to this element's exact length
end do
n = files%length(2)                 ! element 2's length, without fetching it
```

An absent optional key gives a usable object whose `%count()` is `0`, so the loop above is the
whole idiom. The object copies its strings out of the document, so it survives `pf_toml_close`.

### Log levels

`pf_toml_get_level` reads the level **name** and converts it, so your program holds no second scale
and no lookup table:

```fortran
call pf_toml_get_level(gen, "log_level_console", level, default = "INFO")
call pf_log_add_console(level = level)
```

`default` is a name too, so the accepted spellings are the same on both sides. An unrecognised name
is fatal. The `PF_LEVEL_*` constants come from
[`parquet_logging`](logging.html), which is the module you are configuring.

## Catching mistakes in the file

```fortran
call pf_toml_check(sect, [severity])       ! keys nobody read, in this section
call pf_toml_check_all(doc, [severity])    ! everything nobody read, in the whole document
call pf_toml_require(sect, keys)           ! a ';'-separated list that must all be present
call pf_toml_retire(sect, key, advice)     ! stop if a retired key is still set
call pf_toml_mark(sect, key)               ! record a key you read some other way
```

**`pf_toml_check` is the centre of the module.** It compares the keys the file gives against
everything the program *asked for*, and reports each key that was never asked for. The list of
asked-for keys is accumulated automatically by every getter and every `pf_toml_section`, whether or
not the key was in the file — so there is no known-key list to write down anywhere, and none to
keep in step with the reads as they change.

The required half needs no list either, and no separate call: a getter with no `default` and no
`required = .false.` stops on an absent key at the point of the read, naming it.

**It may be called at any point, including last.** Nothing here writes a default into the parsed
document, so the document a sweep sees is the document the file described. Ordering is not a trap.

`pf_toml_check_all` runs that same sweep over every section that *was* opened, and additionally
reports every section, every `[[name]]` entry and every root-level key that was not. One call at
the end of your configuration reading is therefore enough, and it catches the one-level-up form of
the misspelt-key failure: `[sregion_al]` for `[sregion_all]` is otherwise a silent no-op that
leaves every region on its built-in default.

`severity` is `PF_TOML_FATAL` (the default), `PF_TOML_WARN` or `PF_TOML_IGNORE`. Fatal is the
default because calling a check is already an opt-in act. A project bringing old files forward
passes `PF_TOML_WARN` for a release, then tightens.

**`pf_toml_require` survives all that for one case: a file somebody else owns.** Such a file
legitimately carries far more keys than you read, so an unknown-key sweep over it is meaningless
while a required-key list is exactly right. It reports every missing key before stopping, not the
first — somebody bringing an old file forward is usually missing several.

**`pf_toml_retire` stops rather than warns.** A retired key that is merely ignored is worse than
one that is rejected: the run proceeds with a value written nowhere the user can see, and the file
looks as though it still works.

## Escape hatches

This module is never a ceiling. For a TOML construct it does not cover — a datetime, a deeply
nested inline table, toml-f's `merge_table` — reach toml-f directly for that one value and stay
here for everything else:

```fortran
call pf_toml_table(sect, tbl)      ! the raw type(toml_table) behind this handle
call pf_toml_context(sect, ctx)    ! the raw type(toml_context) for the document
```

Receiving either needs your own `use tomlf, only: toml_table` line — `parquet_toml` re-exports no
toml-f name, so that import is the point at which you have stepped outside the wrapper. Tell
`pf_toml_mark` about anything you read this way, or the sweep will report it.

`pf_toml_report` gives the same treatment to a complaint the module cannot know about, so a
validation message your own program writes still points at the right line:

```fortran
call pf_toml_report(gen, "n_sky_conditions", "must be 4", severity = PF_TOML_FATAL)
```

## Writing

```fortran
call pf_toml_new(doc, [name])
call pf_toml_new_section(parent, name, sect)
call pf_toml_append_section(parent, name, sect)   ! one more [[name]] entry
call pf_toml_set(sect, key, value)                ! ADDS a key that is not there
call pf_toml_update(sect, key, value)             ! CHANGES a key that is
call pf_toml_save(doc, file)
```

**`pf_toml_set` and `pf_toml_update` are separate on purpose, and each checks.** `pf_toml_set`
refuses a key that already exists and `pf_toml_update` refuses one that does not, so a typo'd key
is a named, stopped run rather than a second parameter nobody reads. Both take the same six scalar
types as `pf_toml_get` plus rank-1 arrays of each.

Note which of the two an override needs: **a key your program read from its *default* is not set**,
because a default is never written into the parsed document, so overriding one is `pf_toml_set`.

Every `character(len=*)` you write is **trimmed of trailing blanks**, scalar and array alike.
Fortran pads a fixed-length variable, and in an array of mixed-length names every element but the
longest arrives padded; writing those blanks out would make the saved file re-read as a different
value. A value with a genuine trailing blank goes through the escape hatch.

`pf_toml_new_section` is the one place that upserts — an existing section is returned rather than
refused, because a section is a place to put keys rather than a value to get wrong. It works on a
loaded document too, adding a section the file did not have.

### What `pf_toml_save` writes

**The effective configuration: the values the run actually used, with defaults made explicit.**
Every getter records the value it *resolved* — the file's, or the default it applied — and that is
what is written. Two consequences follow, and both are the point:

- A key your program read from its default is **present** in the saved file, with that default's
  value. The input file does not record what the defaults were on the day it ran; this does.
- A key the file set but your program never read is **absent**. The saved file states what the run
  used, not what it was handed.

For a verbatim copy of the input, copy the file. For a verbatim dump of the parsed document, use
`pf_toml_table` and toml-f's own `toml_dump`.

An existing file is overwritten.

## Thread safety

**Every public procedure is safe to call from inside an OpenMP parallel region.** The module
delivers that by *serialising*, not by being parallel: each entry takes one module-wide named
critical section, so concurrent calls queue and run one at a time. Configuration reading is nowhere
near a hot loop, so one lock per call costs nothing.

Two patterns are supported and both are ordinary:

- **Many documents, one file.** Each thread calls `pf_toml_load` on the same path and reads its own
  document.
- **One shared document, many readers.** Getters and the accumulator they append to serialise on
  the guard.

**What the guard does not cover is lifetimes.** It makes individual *calls* atomic, never the life
of a document: closing a document while another thread still holds a handle taken from it is yours
to synchronise, exactly as the lifetime rule above says.

One declaration detail if you use `pf_toml_strings` inside a parallel region: it has allocatable
components, so name it in a `private()` clause rather than declaring it block-local inside the
region. `pf_toml` itself has none and is safe either way.

## Where the messages go

**`parquet_toml` writes through [`parquet_logging`](logging.html), not through this library's own
message channels**, and it is the only module here that does. That is deliberate: configuration
diagnostics are read by the operator of *your* program, in your program's log, beside your own
startup messages.

Two consequences worth knowing before you go looking for a knob:

- **`parquet_verbosity` and `parquet_message_stream` do not govern anything printed here.** The
  logger's own levels and sinks do.
- **A program that never calls `pf_log_init` still gets output.** The default logger behaves as
  though it owned one stdout console sink at `PF_LEVEL_INFO`, so `parquet_toml` is usable with no
  logging setup at all. To send the diagnostics to a file, configure the logger before you load
  your configuration.

Every failure is fatal by default, through `pf_log_fatal`, which writes the message at
`PF_LEVEL_CRITICAL`, flushes every sink and then stops. The only opt-outs are `pf_toml_load`'s
`status`, `pf_toml_section`'s `required = .false.`, `pf_toml_has` and a check's
`severity = PF_TOML_WARN`.

A message looks like this, and the middle block is toml-f's own rendered report:

```
[general] nproc is not a whole number: config.toml
  error: [general] nproc is not a whole number
    |
  4 | nproc = 2.5
    |         ^^^
    |
... configuration file: config.toml
ERROR STOP: pf_logger%fatal: ERR: config value has the wrong type: nproc
```

## Constants

| Constant | Meaning |
|---|---|
| `PF_TOML_OK` | `status`: the document parsed |
| `PF_TOML_ERR_OPEN` | `status`: the file could not be opened |
| `PF_TOML_ERR_PARSE` | `status`: the file is not valid TOML |
| `PF_TOML_IGNORE` | severity: say nothing |
| `PF_TOML_WARN` | severity: log a warning and carry on |
| `PF_TOML_FATAL` | severity: log and stop. The default for every check |
| `PF_TOML_MAX_KEY` | longest key name handled |
| `PF_TOML_MAX_PATH` | longest `section.key` path composed for a message |

The two length caps are input-sanity bounds published as read-only constants, like the
`parquet_max_*` family: a key longer than the cap is a broken file, and exceeding either is a
named, fatal failure rather than a silent truncation.

## The dependency

`parquet_toml` is the reason this library has a Fortran package dependency at all. `fpm.toml` pins
[toml-f](https://github.com/toml-f/toml-f) — a fork of it, carrying two NAG compiler fixes — and
fpm resolves a package's dependency tree before it prunes modules, so every consumer of
parquet-fortran fetches toml-f whether or not they import this module.

The file count in
[Choosing a module](../operating/choosing-a-module.html)'s table counts **this library's own**
Fortran files. toml-f's own sources compile too, for any import that reaches `parquet_toml`; they
are left out of that bookkeeping because it tracks what this repository owns.
