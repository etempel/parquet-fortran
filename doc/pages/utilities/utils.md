---
title: Text and path helpers with parquet_utils
---

`parquet_utils` is a small module of things a program built on this library keeps needing and
Fortran does not supply: ASCII case folding, turning a value into text, and joining and taking
apart POSIX paths.

It is a leaf. `use parquet_utils` compiles **one** of this library's Fortran files, imports nothing
but `iso_fortran_env`, and never crosses the C++ boundary. It is also available through
`use parquet` like everything else.

## Two rules that apply to everything here

**Nothing in this module can fail.** There is no `error stop` anywhere in it, no argument you can
get wrong, and no precondition to violate. A minimum width that is too small grows the result
rather than refusing it; a path with no directory part has an empty directory part; even a format
string the runtime rejects comes back as a run of asterisks rather than ending your program.

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
fight. `pad` defaults to `"0"` for the two integer types — the zero-padded file counter is the case
this exists for — and to `" "` for the reals and the logical.

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

Three of those rows are the ones a hand-rolled version gets wrong, and each is worth knowing:
**`.bashrc` has no extension** (a leading dot marks a hidden file), **`/a.b/c` has no extension**
(the dot is in a *directory* component), and **`a.tar.gz`'s extension is `.gz`**, not `.tar.gz`.

**The extension carries its dot**, which is what makes rebuilding a name compose correctly — and
what makes the no-extension case compose without leaving a stray dot behind.

Everything is purely lexical: nothing is normalised, no symlink is resolved, and the filesystem is
never touched. `a/b/../c` comes back as `a/b/../c`, because it is only `a/c` when `b` is not a
symlink, and answering that needs the filesystem.

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

**Every text argument has its trailing blanks trimmed and its leading blanks preserved.** This is
one rule for the whole module, and it is forced rather than chosen: a `character(len=*)` actual is
blank-padded and nothing can tell padding from intent, while a leading blank is a legal filename
character.

It matters most for `suffix`, where the alternative is silently wrong:

```fortran
character(len=16) :: suffix

suffix = "_stat"
call pf_path_add_suffix("myfile.txt", suffix, outfile)
! "myfile_stat.txt", not "myfile_stat           .txt"
```

The cost is that a suffix genuinely ending in a blank cannot go through this procedure. Build it
yourself if you need one — ordinary concatenation preserves the padding of a fixed-length variable
exactly:

```fortran
call pf_split_path(infile, dir, stem, ext)
outfile_text = dir // "/" // stem // suffix // ext   ! every blank kept
```
