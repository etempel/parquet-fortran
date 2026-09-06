---
title: Text and path helpers with parquet_utils
---

`parquet_utils` is a small module of things a program built on this library keeps needing and
Fortran does not supply: ASCII case folding, turning a value into text and reading it back, and
joining and taking apart POSIX paths.

It is a leaf. `use parquet_utils` compiles **one** of this library's Fortran files and imports
nothing but `iso_fortran_env`, so its Fortran graph never reaches the C++ bindings. That is
narrower than "no C++": `link` is a package-level key in `fpm.toml`, so the wrapper is still
compiled and Arrow still linked whichever module you import — see
[Choosing a module](../operating/choosing-a-module.html). It is also available through
`use parquet` like everything else.

## Rules that apply to everything here

**Nothing in this module can fail.** There is no `error stop` anywhere in it, no argument you can
get wrong, and no precondition to violate. A minimum width that is too small grows the result
rather than refusing it; a path with no directory part has an empty directory part; even a format
string the runtime rejects comes back as a run of asterisks rather than ending your program.

**Every procedure here is `pure`.** Fortran forbids a `pure` procedure from printing, from any
other external I/O and from executing a `STOP`, so most of the paragraph above is enforced by the
compiler on every build rather than being a promise. It is also a capability: you can call these
from your own `pure` procedures and from inside a `do concurrent` block.

**Every result is allocated, on every path.** Where the answer is empty you get an allocated,
zero-length string — never an unallocated one. So `len(result) == 0` is the only test you ever
need, and you never have to ask whether a result came back allocated:

```fortran
character(len=:), allocatable :: dir

call pf_dirname("cat.parquet", dir)   ! no directory part
! len(dir) == 0, and dir IS allocated
```

Everything that produces text is a **subroutine** with the result as an argument, never a function.
That is a project-wide rule with a measured reason behind it — a gfortran function returning
`character(len=:), allocatable` corrupts its result under concurrency — and it is why every call
below reads `call pf_something(input, result)`.

## Case folding

```fortran
call pf_to_lower(s, out)   ! folded copy, out allocated to len(s)
call pf_to_lower(text)     ! folds a character(len=*) variable in place
call pf_to_upper(s, out)
call pf_to_upper(text)
```

**ASCII only, and that is a guarantee rather than a limitation.** Every byte outside `A`-`Z` and
`a`-`z` is copied through untouched, so UTF-8 text passes through byte-identical. A fold driven by
a 256-entry table would rewrite half of a multi-byte character; this one cannot.

The in-place form is usually what you want when you already hold a short key in a fixed-length
variable, since the copy form allocates on every call:

```fortran
character(len=16) :: key

key = "Verbosity"
call pf_to_lower(key)      ! key is now "verbosity", still length 16
```

**Neither form trims.** The copy is exactly `len(s)` characters long and the in-place form cannot
change its variable's length at all, so `pf_to_lower("AB   ", out)` gives `"ab   "`. These two are
the exception to the module's trimming rule — see [Trailing blanks](#trailing-blanks) below.

## Turning a value into text

One signature for all five types — `integer(int32)`, `integer(int64)`, `real(real32)`,
`real(real64)` and `logical`. Optional arguments are shown in square brackets:

```fortran
call pf_to_str(value, res, [min_width], [fmt], [pad])
```

```fortran
character(len=:), allocatable :: res

call pf_to_str(42, res)                     ! "42"
call pf_to_str(42, res, min_width=6)        ! "000042"
call pf_to_str(42, res, min_width=6, pad=" ")  ! "    42"
call pf_to_str(255, res, fmt='(z0)')        ! "FF"
call pf_to_str(3.25_real64, res, fmt='(f6.2)') ! "  3.25"
call pf_to_str(.true., res)                 ! "true"
```

**`min_width` is a minimum, not an exact width.** A value needing more characters gets them:
`pf_to_str(123456, res, min_width=4)` is `"123456"`. Nothing is ever truncated and nothing is
refused.

**`fmt` renders and `min_width` then pads what it produced**, in that order, so the two never
fight. `fmt` is a Fortran format specification and carries its own parentheses — `'(i0)'`, not
`'i0'`. `pad` defaults to `"0"` for the two integer types — the zero-padded file counter is the
case this exists for — and to `" "` for the reals and the logical.

**Exactly one case puts padding between the sign and the digits: an integer padded with `"0"`.**
Everything else pads in front of the sign.

```fortran
call pf_to_str(-7, res, min_width=5)                    ! "-0007"  (integer, default pad "0")
call pf_to_str(-7, res, min_width=5, pad=" ")           ! "   -7"
call pf_to_str(-7, res, min_width=5, pad="9")           ! "999-7"
call pf_to_str(-3.5_real64, res, min_width=8, fmt='(f4.1)', pad="0")  ! "0000-3.5"
```

The reason is narrow and it is why the rule is narrow: zeros inserted immediately after an
**integer's** sign do not change the value the text reads back as. That is not true of any other
pad character, and it is not the way a leading zero run reads on a real — there it is alignment,
so it goes outside the sign like a blank.

**A format the runtime rejects gives you asterisks**, not a crash. How many asterisks is not part
of the contract and differs between compilers, so test for "all asterisks" rather than a length.

**Asterisks have two other causes, and the first is much the commoner.** A perfectly valid format
whose field is too narrow makes Fortran itself write asterisks and report success, so
`pf_to_str(12345, res, fmt='(i2)')` is `"**"` — and `min_width` then pads *that*, so adding
`min_width=6` gives `"0000**"`. Separately, a rendering longer than **512 characters** overflows
the fixed buffer each value is rendered through, so `fmt='(f600.2)'` is asterisks as well; that
buffer is the only limit in the module and is far above any width these five types need.

So asterisks mean "look at your `fmt`", not specifically "your `fmt` was rejected" — and in all
three cases you get them instead of an abort, which is the rule this module never breaks.

### `pf_to_str` and `pf_str` are different, and they disagree about one value

`pf_str`, in [`parquet_logging`](logging.html), renders the same five types — but it is a
*function* returning a fixed `character(len=32)`, made for inline concatenation into a log line:

```fortran
call log%info("rows " // trim(pf_str(n)))       ! pf_str: inline, fixed length
call pf_to_str(n, text, min_width=8)            ! pf_to_str: exact length, padding, formats
```

**A `logical` is `T`/`F` from `pf_str` and `true`/`false` from `pf_to_str`.** That is deliberate,
not an oversight. The short form reads better in a log column; the words are what a **TOML**
boolean has to be, so `pf_to_str` gives you a value you can write straight into a config file. Both
are in scope at once under `use parquet`, so this is worth knowing before it surprises you. Pass
`fmt='(l1)'` to `pf_to_str` for Fortran's own spelling.

The default rendering of a real is **not** portable — `(g0)` produces a different number of digits
on different compilers. Pass `fmt` whenever the exact text matters.

## Reading a value back out of text

```fortran
call pf_from_str(text, value, ok)
```

The inverse of `pf_to_str`, over the same five types — `integer(int32)`, `integer(int64)`,
`real(real32)`, `real(real64)` and `logical`. `ok` says whether the text could be read at all.

```fortran
call pf_from_str("42", n, ok)          ! ok = .true.,  n = 42
call pf_from_str("42x", n, ok)         ! ok = .false., n is NOT set
call pf_from_str(" 3.5 ", x, ok)       ! ok = .true.,  x = 3.5
call pf_from_str("true", flag, ok)     ! ok = .true.,  flag = .true.
```

**Do not read `value` unless `ok` came back `.true.`** — it is deliberately left unset. Writing a
zero into it instead would hand a caller who forgot to test `ok` a plausible wrong number for
`"abc"`, which is the exact failure this procedure exists to prevent.

**It is strict, and that is the whole reason it exists rather than a `read`.** A list-directed
`read(text, *, iostat=)` rejects `"5abc"` and `"3.9"` as you would hope, and accepts **`"5 6"`
with `iostat == 0`, yielding 5** — so an identifier with a stray space silently becomes a
different, plausible identifier. Here the shape is checked first and the `read` runs only once it
is known to be sound.

Leading and trailing blanks are trimmed before anything else — a `character` array element is
blank-padded by construction, so trailing blanks cannot carry meaning — and after that no blank is
allowed anywhere. What each type then accepts:

| target | accepted | rejected |
|---|---|---|
| `integer(int32)`, `integer(int64)` | an optional `+`/`-`, then digits, then nothing else | `"3.9"`, `"1e3"`, `"0x10"`, `"12abc"`, `"+"`, `""` |
| `real(real32)`, `real(real64)` | a Fortran real literal: optional sign, digits with an optional `.` and fraction (`"12"`, `"12."`, `".5"` all read), optional `e`/`E`/`d`/`D` exponent with its own sign | `"1.2.3"`, `"1e"`, `"1q3"`, `"1.0_real64"`, `"nan"`, `"inf"` |
| `logical` | `true`, `false`, `t`, `f`, `1`, `0`, case-insensitively | `"yes"`, `"on"`, `".true."`, `"2"` |

The logical set is exactly what this library's two renderers produce — `pf_to_str` writes
`true`/`false` and `pf_str` writes `T`/`F` — plus the `1`/`0` a shell or a CSV produces. Guessing
what `"yes"` meant is the kind of silent decision this module has none of.

**`"nan"` and `"inf"` are refused deliberately.** Neither is a Fortran literal, and text reading
`nan` in a data file is far more often a placeholder for a missing value than a deliberate NaN —
under [`%parse_column`](../tables/table.html#changing-a-columns-type)'s `invalid="null"` such a row
becomes Null, which is usually what was meant. Build a NaN with `ieee_value` if you want one.

**A value the target kind cannot hold is not readable into it**, so strictness is per kind:
`"2147483648"` reads as an `integer(int64)` and not as an `integer(int32)`, and `"1e300"` reads as
a `real(real64)` and not as a `real(real32)`. **Rounding is not overflow** — `"0.1"` and
`"1e-300"` read into a `real32` as the nearest value it has, exactly as any other narrowing here
rounds silently.

`%parse_column` on a [`parquet_table`](../tables/table.html#changing-a-columns-type) is this
procedure applied to a whole column, with a policy for what to do about the rows that fail.

## Joining paths

```fortran
call pf_join_path(a, b, path)                  ! two to five components
call pf_join_path(a, b, c, path)
call pf_join_path(parts, path)                 ! or an array of them
```

**The rules are CPython's `posixpath.join`, exactly**, and the tests assert against values
generated from CPython rather than transcribed, so this is checked rather than claimed. Three rules
applied left to right:

| call | result | why |
|---|---|---|
| `pf_join_path("a", "b", p)` | `a/b` | one separator inserted |
| `pf_join_path("a/", "b", p)` | `a/b` | already ends with one |
| `pf_join_path("a", "/b", p)` | `/b` | an absolute component **restarts** the path |
| `pf_join_path("", "b", p)` | `b` | empty accumulator |
| `pf_join_path("a", "", p)` | `a/` | **the separator is still added** |
| `pf_join_path("./", "b", p)` | `./b` | `"./"` already ends with a separator |
| `pf_join_path("a/b", "../c", p)` | `a/b/../c` | nothing is resolved |

**The `pf_join_path("a", "", p)` row is the one that surprises people**, and it bites in a
plausible pattern: `pf_join_path(dir, name, out)` where `name` happens to be empty gives you a
directory-looking path rather than `dir` itself. It is Python's behaviour and it is kept
deliberately; test `name` yourself if that matters.

The absolute-restart rule means there is no second procedure to remember for "this might already be
an absolute path" — that case is just a join.

**Two Fortran-specific rules Python has no equivalent for.** An array of **no** components joins to
an allocated, zero-length string, so a loop accumulating components needs no special case for the
empty one; an array of **one** joins to that component.

## Taking a path apart

```fortran
call pf_dirname(path, dir)     ! everything before the last separator
call pf_basename(path, base)   ! everything after it
call pf_path_ext(path, ext)    ! the extension, INCLUDING its leading dot
call pf_path_stem(path, stem)  ! the basename with the extension removed
```

These follow `posixpath.dirname`, `posixpath.basename` and `posixpath.splitext`:

| path | `dirname` | `basename` | `path_ext` | `path_stem` |
|---|---|---|---|---|
| `/data/run3/cat.parquet` | `/data/run3` | `cat.parquet` | `.parquet` | `cat` |
| `myfile.txt` | | `myfile.txt` | `.txt` | `myfile` |
| `/a/b/` | `/a/b` | | | |
| `/x` | `/` | `x` | | `x` |
| `.bashrc` | | `.bashrc` | | `.bashrc` |
| `a.tar.gz` | | `a.tar.gz` | `.gz` | `a.tar` |
| `/a.b/c` | `/a.b` | `c` | | `c` |
| `a.` | | `a.` | `.` | `a` |
| `a//b` | `a` | `b` | | `b` |

Three of those rows are the ones a hand-rolled version gets wrong, and each is worth knowing:
**`.bashrc` has no extension** (a leading dot marks a hidden file), **`/a.b/c` has no extension**
(the dot is in a *directory* component), and **`a.tar.gz`'s extension is `.gz`**, not `.tar.gz`.

**The extension carries its dot**, which is what makes rebuilding a name compose correctly — and
what makes the no-extension case compose without leaving a stray dot behind.

Everything is purely lexical: nothing is normalised, no symlink is resolved, and the filesystem is
never touched. `a/b/../c` comes back as `a/b/../c`, because it is only `a/c` when `b` is not a
symlink, and answering that needs the filesystem.

**A repeated separator is preserved everywhere except in `dirname`**, which strips the whole run —
hence the `a//b` row above. That is Python's rule rather than a choice made here; joining and
`pf_path_add_suffix` both leave `a//b` exactly as they found it.

## Renaming a file, which is what most of this is for

The whole point of the extension carrying its dot is this:

```fortran
character(len=:), allocatable :: outfile

call pf_path_add_suffix("/data/myfile.txt", "_stat", outfile)
! "/data/myfile_stat.txt"
```

`pf_path_add_suffix(path, suffix, out)` inserts `suffix` immediately before the extension, keeping
the directory and the extension. It composes on every shape in the table above: no extension gives
`myfile_stat`, `a.tar.gz` gives `a.tar_stat.gz`, and `.bashrc` gives `.bashrc_stat` because a
leading dot is not an extension. A path ending in a separator has an empty stem, so `/a/b/` gives
`/a/b/_stat` — the lexically consistent answer, not a guess that you meant `b`.

When you want the pieces rather than that one recombination, `pf_split_path` gives you all three in
one call and one scan of the path:

```fortran
character(len=:), allocatable :: dir, stem, ext, outfile

call pf_split_path(infile, dir, stem, ext)
call pf_join_path(dir, stem // "_stat" // ext, outfile)
```

## Trailing blanks

**Every path argument, and every `suffix`, has its trailing blanks trimmed and its leading blanks
preserved.** It is forced rather than chosen: a `character(len=*)` actual is blank-padded and
nothing can tell padding from intent, while a leading blank is a legal filename character.

**`pf_to_lower` and `pf_to_upper` are the exception and do not trim** — they preserve length
exactly, which is the whole point of the in-place form. Nothing in the path or `pf_to_str` families
behaves that way.

It matters most for `suffix`, where the alternative is silently wrong:

```fortran
character(len=16) :: suffix

suffix = "_stat"
call pf_path_add_suffix("myfile.txt", suffix, outfile)
! "myfile_stat.txt", not "myfile_stat           .txt"
```

The cost is that a suffix genuinely ending in a blank cannot go through this procedure. Build it
yourself if you need one — ordinary concatenation preserves the padding of a fixed-length variable
exactly. Join the directory back on with `pf_join_path` rather than writing the separator yourself,
since `dir` is empty for a bare filename and `dir // "/"` would root the result at `/`:

```fortran
call pf_split_path(infile, dir, stem, ext)
call pf_join_path(dir, stem, prefix)        ! "" + "myfile" is "myfile", not "/myfile"
outfile = prefix // suffix // ext           ! every blank in suffix kept
```
