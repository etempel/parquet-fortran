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
`required = .false.` an absent section leaves the handle **closed**.

**Reading a value from a closed handle is fatal** rather than returning defaults — guard the reads
with `pf_toml_is_open`. Returning defaults would make an absent section indistinguishable from an
empty one, which is exactly the silence this module exists to remove. That holds for
`pf_toml_get_opt` too, whose defaults live in the variables: the handle still has to be open.

**The validators are the exception and are silent on a closed handle.** `pf_toml_check`,
`pf_toml_retire`, `pf_toml_mark`, `pf_toml_mark_section` and `pf_toml_section_count` all do nothing
rather than stop, because a section that does not exist has no keys to sweep, none to mark and none
to warn about — and whether an optional section is present is not known before it is opened, so a
caller cannot be asked to test first. `pf_toml_require` still stops: its keys are required and they
are not there, which is a real failure at any level.

`pf_toml_section_count` answers `0` for an absent name, which makes this the whole idiom for an
optional repeated section:

```fortran
do i = 1, pf_toml_section_count(conf, "region")
    call pf_toml_section(conf, "region", i, ent)
    call pf_toml_get(ent, "id", id)
end do
```

**`required` governs an entry index exactly as it governs a name.** A bare call with an index
outside `1 .. count` is fatal; `required = .false.` leaves the handle closed and sets `found` to
`.false.`, so "is there an entry 7?" is a question you may ask rather than one you must already
know the answer to. A `name` that is present but is not an array of tables is fatal either way —
absence is a configuration choice, a name of the wrong shape is a programming error, and
`pf_toml_section_count` draws the same line.

**A key is looked up literally and never split on a dot.** `pf_toml_get(conf, "general.nproc", n)`
asks the root table for a single key spelled `general.nproc` and stops with absent-key; open
`[general]` first. TOML allows a dot inside a quoted key, so `"a.b" = 1` at the root is legal and
distinct from `[a]` / `b = 1`, and a dot-splitting getter could not read such a file at all.

**A key above the first section is read straight from the document handle**, with the same
diagnostics and the same coverage: `call pf_toml_get(conf, "title", title)`.

## Reading values

```fortran
call pf_toml_get(sect, key, value, [default])            ! scalar
call pf_toml_get(sect, key, values, [default])           ! array, exact length match
call pf_toml_get_alloc(sect, key, values)                ! array, the file chooses the size
call pf_toml_get_strings(sect, key, strings, [count])
call pf_toml_get_level(sect, key, level, [default])

call pf_toml_get_opt(sect, key, value)                   ! keep the variable if the key is absent
call pf_toml_get_opt(sect, key, values)
call pf_toml_get_alloc_opt(sect, key, values)
call pf_toml_get_strings_opt(sect, key, strings, [count])
```

`value` is an `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`, `logical` or
`character(len=:), allocatable` scalar. `values` is a rank-1 array of any of those six, with
`character(len=*)` for the string form; `pf_toml_get_alloc` takes the same five numeric and logical
types and deliberately has no character form.

**One rule covers every getter, at every shape:**

> The bare call requires its key. `pf_toml_get` opts out with `default =`. The `_opt` call opts out
> by keeping whatever the variable already holds. `default =` is offered exactly where the caller
> owns the size — the scalar and fixed-length-array forms — and never by `pf_toml_get_alloc` or
> `pf_toml_get_strings`, which take their size from the file.

The `_opt` forms are what make "inherit from the previous entry unless this one overrides it" two
ordinary lines:

```fortran
this_region = previous_region                                   ! inherit everything
call pf_toml_get_opt(sect, "airmass_limit", this_region%airmass) ! then override
```

**An `_opt` call asks you to have given the variable a value.** Its `value` is `intent(inout)`, so
passing an undefined variable is not conforming and a checked build will trap on it. That is the
assertion the separate name makes: `pf_toml_get` says "this program needs a value here",
`pf_toml_get_opt` says "this variable already has one".

**Never pass one variable as both `values` and `default`.** `call pf_toml_get(s, "k", x, x)`
associates `x` with an `intent(out)` dummy and an `intent(in)` one, which the standard forbids and
no compiler diagnoses; the "default" applied is then whatever undefining `x` left behind. That call
means `call pf_toml_get_opt(s, "k", x)`. Writing `default =` as a keyword argument is the habit that
keeps the two apart at a glance.

A few rules worth knowing:

- An array's length must match `size(values)` **exactly, in both directions**. Silently using a
  prefix of a list that is too long pairs each value with the wrong slot, which nothing downstream
  could catch. A rank-1 `default` must be `size(values)` long for the same reason.
- A string element longer than `len(values)` is fatal rather than clipped, in a `default` as much as
  in the file. A silently truncated file name is the worst outcome available here.
- Reading a `real` accepts a TOML integer and converts it. Reading an `integer` does **not** accept
  a TOML float, and a value too large for `integer(int32)` is reported rather than wrapped.
- `pf_toml_get_alloc_opt` leaves its result exactly as it found it, so an unallocated one stays
  unallocated and `allocated(values)` answers "did the file set this key?". A variable that already
  held a list keeps it.
- An `_opt` read records what the run actually used — the file's value, or the variable's own — so
  `pf_toml_save` writes it either way. The one thing recorded nowhere is an absent value: a
  `character(len=:), allocatable` scalar left unallocated is not an empty string.

### Lists of strings

`pf_toml_get_alloc` has no character form on purpose: a blank-padded array of one declared length
loses each element's own length and invites `trim` bugs at every use. A variable-length string list
is read into a `pf_toml_strings` instead, which keeps each element exact.

```fortran
type(pf_toml_strings) :: files
character(len=:), allocatable :: one

call pf_toml_get_strings_opt(sect, "input_files", files)
do i = 1, files%count()
    call files%get(i, one)          ! `one` is allocated to this element's exact length
end do
n = files%length(2)                 ! element 2's length, without fetching it
```

An index outside `1 .. %count()` is fatal and names both numbers, so the `%count()`-bounded loop
above is not merely tidy. A freshly declared `files` that the file does not set still answers
`%count() == 0`, so that loop is also the whole idiom for an optional list. The object copies
its strings out of the document, so it survives `pf_toml_close`.

`count =` demands an exact number of entries and is fatal in **both** directions. That is what a
list whose length must match some other setting needs — three column names for three sky conditions
is a correctness constraint, and a list of the wrong length pairs each name with the wrong
condition. It is checked only when the file sets the key, so an absent optional list is not measured
against it.

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
call pf_toml_mark_section(sect)            ! record a whole section, recursively
```

**`pf_toml_check` is the centre of the module.** It compares the keys the file gives against
everything the program *asked for*, and reports each key that was never asked for. The list of
asked-for keys is accumulated automatically by every getter and every `pf_toml_section`, whether or
not the key was in the file — so there is no known-key list to write down anywhere, and none to
keep in step with the reads as they change.

The required half needs no list either, and no separate call: a bare getter — no `default`, and not
one of the `_opt` forms — stops on an absent key at the point of the read, naming it.

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

**`pf_toml_mark_section` excuses a whole section from both sweeps**, for a section your program
deliberately does not read — a `[developer]` block whose keys are only read when a developer mode is
on, say. The alternative is to read every key unconditionally and re-apply the defaults by hand,
which rebuilds exactly the key list this module exists to delete. It marks **recursively**, so a
sub-table below the section is not then reported as a section nobody read; the cost is that a
misspelt *nested* section name inside a marked section is silenced too. Marking is not reading, so
`pf_toml_save` does not write a marked section's keys — which is right, because the program did not
use them.

## A section whose keys you do not know in advance

Everything above assumes your program knows the key names. For a table whose keys are the user's
own — an `[aliases]` block, a `[thresholds]` block keyed by band name — ask for them:

```fortran
character(len=PF_TOML_MAX_KEY), allocatable :: names(:)
character(len=:), allocatable :: value
integer :: i

call pf_toml_keys(sect, names)                  ! every key in this section, in file order
do i = 1, size(names)
    call pf_toml_get(sect, trim(names(i)), value)
end do
```

The names come back blank-padded to `PF_TOML_MAX_KEY`, which is what lets you enumerate a section
without importing a toml-f type — `trim` each one on the way into a getter. A closed handle yields
a zero-length result rather than stopping, so the loop is safe after an optional section.

Reading each key through `pf_toml_get` marks it, so the sweeps stay accurate with nothing extra to
do. Reaching into the raw table instead is what needs `pf_toml_mark` — see below.

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
call pf_toml_delete(sect, key)                    ! REMOVES a key; absent is a no-op
call pf_toml_save(doc, file)                      ! the EFFECTIVE configuration
call pf_toml_dump(doc, file)                      ! the document AS PARSED
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

**`pf_toml_delete` is the third of the trio and the one that does *not* refuse the other state.**
Deleting is idempotent — afterwards the key is not there, whether or not it was — so there is no
wrong outcome for a guard to prevent, and "remove this key if the file happens to set it" is one
line. The key goes from the parsed document *and* from the effective one, so a later `pf_toml_save`
does not write back a value an earlier read had resolved.

### What `pf_toml_save` writes

**The effective configuration: the values the run actually used, with defaults made explicit.**
Every getter records the value it *resolved* — the file's, or the default it applied — and that is
what is written. Two consequences follow, and both are the point:

- A key your program read from its default is **present** in the saved file, with that default's
  value. The input file does not record what the defaults were on the day it ran; this does.
- A key the file set but your program never read is **absent**. The saved file states what the run
  used, not what it was handed.

An existing file is overwritten.

### What `pf_toml_dump` writes

**The document as parsed**, plus every `pf_toml_set`, `pf_toml_update` and `pf_toml_delete` applied
since. Everything the file carried is there whether or not your program read it, which is exactly
where `pf_toml_save` differs — so loading a file, editing one key and writing it back out is
`pf_toml_dump`, while recording what a run used is `pf_toml_save`.

**Neither is a verbatim copy.** toml-f's serialiser writes values rather than source, so comments
are dropped and the key order and formatting are toml-f's. For a file people hand-edit and keep
comments in, copy the file.

Both take the document handle rather than a section — passing a section handle is fatal rather
than writing that subtree — and both overwrite.

## A cosmology in a configuration file

A run's cosmology is a run parameter, so it belongs in the run's `.toml` file beside `nproc` and
`output_dir`. It is not table metadata: a `.maml` file describes a *table* — its columns, units,
provenance and quality rules — while a cosmology describes the run that made it.

`parquet_cosmology_config` is the module that joins this one to the cosmology tier, and it is a
module of its own rather than part of either neighbour. The object it builds, and everything it
answers, is [Distances and times in an expanding universe](cosmology.html).
`use parquet_cosmology_config` compiles 15 of this library's Fortran files. Putting the two
procedures in `parquet_cosmology` would make every consumer of a dependency-free numerical tier
fetch toml-f, and putting them in `parquet_toml` would make every program that reads a
configuration file compile the cosmology tier, its integrator and its interpolator; this way
nothing anyone already imports grows.

```fortran
use parquet_cosmology, only: pf_cosmology
use parquet_toml, only: pf_toml, pf_toml_load, pf_toml_check_all, pf_toml_close, pf_toml_save
use parquet_cosmology_config, only: pf_cosmology_from_toml, pf_cosmology_to_toml

type(pf_toml)      :: conf
type(pf_cosmology) :: cosmo
logical            :: have_cosmology

call pf_toml_load(conf, "run.toml")
call pf_cosmology_from_toml(conf, cosmo)                          ! [cosmology] must be there
call pf_cosmology_from_toml(conf, cosmo, found = have_cosmology)  ! ... or need not be
call pf_toml_check_all(conf)                    ! a misspelt key stops the run here
...
call pf_toml_save(conf, "run_used.toml")        ! what the run actually resolved
call pf_toml_close(conf)
```

```fortran
call pf_cosmology_from_toml(conf, cosmo, [found], [section], [context])
call pf_cosmology_to_toml(cosmo, conf, [section])
```

`conf` may be a whole document or a section of one, so a file that nests the table under
`[run.cosmology]` needs no extra argument: take `run` with `pf_toml_section` and pass that handle.
`section` names a different table (`[cosmology_alt]`), and a program with three cosmologies calls
the reader three times. Neither procedure has a `stat=`: `parquet_toml`'s getters are fatal by
default and `%init` aborts on a parameter it will not accept, and a third convention in a call that
already has two would only be one more thing to forget.

### The section, and the two forms

**`[cosmology]` is one table of your configuration file, not the whole of it.** Both procedures
work through a section handle and touch nothing outside it, so everything else in the file survives
a write and your `pf_toml_check_all` still reports what nobody read — including the sections this
feature knows nothing about.

The **named** form selects one of the eight named cosmologies, matched without regard to case:

```toml
[general]
nproc      = 8
output_dir = "out"

[cosmology]
name = "Planck18"

[[region]]
id = 1
```

The **parameter** form gives the numbers, with `name` a label:

```toml
[cosmology]
name  = "my_sim"          # a label; not one of the eight
h0    = 67.66
om0   = 0.30966
tcmb0 = 2.7255
neff  = 3.046
m_nu  = [0.0, 0.0, 0.06]
ob0   = 0.04897
# ode0 absent: the model is flat
# w0, wa absent: a cosmological constant
zmax  = 1100.0
```

The keys are `%init`'s own argument names, lower case, one spelling only:

| key | absent means | note |
|---|---|---|
| `name` | `"custom"` | one of the eight selects that cosmology; anything else is a label |
| `h0` | **required** in the parameter form | `H0` in km/s/Mpc |
| `om0` | **required** in the parameter form | |
| `ode0` | the model is **flat** | its presence is the flag, so `%is_flat()` stays an exact bit test |
| `tcmb0` | `0` — no radiation and no neutrinos | |
| `neff` | `3.04` | `%init`'s default, which is not the `3.046` every Planck realization carries |
| `m_nu` | every species massless | a list; `floor(neff)` of them |
| `ob0` | unknown, and `%ob0()` answers NaN | |
| `w0` | `-1` | |
| `wa` | `0` | |
| `zmax` | `1100` | tabulation, not model: it changes speed, never an answer |
| `zmin` | `-0.9` | the blueshift half of the table |

Three of them — `ode0`, `ob0` and `m_nu` — have no value that could stand for "absent", so for
those three the key's **presence** is what it means. The rest have a documented default, which is
what the reader passes when the file leaves them out.

**Nothing is inferred that `%init` would not infer**: no `flat = true`, no class name, no `sigma8`,
no unit suffixes. And every validation is `%init`'s own — an `h0` outside its range, an `m_nu` of
the wrong length, an `ob0` above `om0` — so the message you get is the library's, with the file and
the section named in it.

Two shapes are refused rather than guessed at. A `name` that is one of the eight **beside** a model
parameter is fatal: `name = "Planck18"` with `om0 = 0.25` means two different cosmologies, and
neither reading is safe. And a `[[cosmology]]` array of tables is not this section; a program that
wants several cosmologies gives each its own table and passes `section=`. A `found=` does not
soften that one, because a name of the wrong shape is a programming error rather than a
configuration choice — the same line the rest of this page draws.

### Which cosmology did this run use?

If your program read its cosmology from the file, `pf_toml_save` already answers this with no code
from the cosmology side at all: every getter records the value it resolved, so the saved file
carries **every key, including the `neff` and `tcmb0` nobody typed**. A `[cosmology]` section that
said only `name = "Planck18"` saves as that name plus the `zmax` and `zmin` the run used, because
those are the keys the run resolved.

`pf_cosmology_to_toml` is for the other case, a program that built its cosmology in code. It
creates the section if the document has none and reuses it otherwise, and deletes each key before
setting it — so the call is idempotent, leaves behind no key the new model does not set (writing a
flat model over a curved one leaves no stale `ode0`), and never reaches outside its own section.
What it writes is what rebuilds the same object: `ode0` only where the model is not flat, `m_nu`
only where a species is massive, `ob0` only where the model has one, `w0` and `wa` only where they
are not a cosmological constant.

**It writes the parameters, not the realization.** A model whose name is one of the eight is written
*without* its `name`, for two reasons: a section carrying both is refused on reading, and a label
cannot be trusted to mean the realization — `%init` lets any model be labelled `"Planck18"`. What
the file records is the numbers the run used, which is what stays true if the eight are ever
re-frozen against a newer astropy; the label is what is given up. Read that file back and you get
the same model, answering the same numbers, with `%get_name` saying `custom`.

One more consequence worth knowing before you meet it: **a key the file sets but your program never
read is absent from a saved file.** A `[cosmology]` key this feature does not know survives in the
loaded document, and `pf_toml_check_all` reports it, but `pf_toml_save` writes what the run used,
not what it was handed.

### What the numbers look like in the file

The text is toml-f's, not this library's, and it is the same text every other `real64` in every
other section gets: seventeen significant digits above `1e3`, and sixteen digits *after the decimal
point* below it. So `h0 = 67.66` is saved as `67.6599999999999966` and comes back bit for bit, and
so does every other parameter of a realistic model — but the promise weakens by one digit per
decade below `1`, and a value smaller than about `5e-17` is written as `0.0000000000000000`.

Nothing a cosmology carries is anywhere near that: the smallest thing in the table above is a
neutrino mass of `0.06` eV, which round-trips exactly. It is worth stating only because the limit
is invisible — a model with `wa = 1e-18` would lose that key's meaning with no diagnostic at all.
Reading a file does not rewrite it, so a configuration you hand-edit keeps the `67.66` you typed.

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
`status`, `pf_toml_section`'s `required = .false.`, a getter's `default =` or its `_opt` form,
`pf_toml_has`, and a check's `severity = PF_TOML_WARN`.

A message looks like this, with `nproc = 2.5` in the file and no logging configured, so every line
carries the default console sink's timestamp and level. The middle block is toml-f's own rendered
report:

```
19:09:11.435 [ERROR   ] [general] nproc is not a whole number: config.toml
19:09:11.437 [ERROR   ]   error: [general] nproc is not a whole number
19:09:11.437 [ERROR   ]    --> config.toml:2:9-11
19:09:11.437 [ERROR   ]     |
19:09:11.437 [ERROR   ]   2 | nproc = 2.5
19:09:11.437 [ERROR   ]     |         ^^^
19:09:11.437 [ERROR   ]     |
19:09:11.437 [ERROR   ] ... configuration file: config.toml
```

After that the run stops, and the last line is the Fortran runtime's own — its exact wording and the
process's exit status differ by compiler, so match on the message above rather than on it.

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
