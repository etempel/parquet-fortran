!> Small, self-contained text and path helpers for programs built on this library: ASCII case
!> folding, value-to-text rendering, and POSIX path joining and splitting.
!!
!! `parquet_utils` is a **leaf**: it imports `iso_fortran_env` and nothing else -- not even
!! `parquet_settings_base`. `use parquet_utils` therefore compiles one Fortran file and never
!! crosses the C++ boundary. `check_parquet_utils_stays_arrow_free`
!! (tools/check_source_conventions.py) is what keeps that true.
!!
!! **That leaf status is load-bearing rather than tidy.** `parquet_settings_base` carried its own
!! private ASCII fold with a doc-comment explaining that it could not call `parquet_core`'s copy
!! without creating a circular dependency. A module below everything is what removes that cycle, so
!! anything added here must not import a sibling -- the cycle returns the moment it does.
!!
!! **Every procedure in this module is total: nothing validates, nothing aborts, nothing prints.**
!! `min_width` grows the result rather than refusing it, Python's join rules have no error case,
!! taking a path apart has none either, and a `fmt` the I/O runtime rejects yields asterisks rather
!! than killing the process. There is no `error stop` anywhere in this file and no entry point has
!! a precondition a caller can violate. That is also why the module needs no out-of-process error
!! scenarios: it has no error paths to run in one.
!!
!! **`pure` on every procedure is what enforces most of that, on every build.** Fortran forbids a
!! `pure` procedure from printing, from any other external I/O and from executing a `STOP`, so
!! "nothing prints" and "nothing stops" are compile-time facts rather than promises. It does NOT
!! forbid `ERROR STOP` -- that third of the claim rests on there being none in the file, which is
!! the one part a future edit can break silently. A new procedure here is declared `pure` too.
!!
!! **Every allocatable result is ALLOCATED on every path, zero-length where the answer is empty.**
!! `pf_dirname("cat.parquet")`, `pf_path_ext("cat")` and `pf_join_path` over a zero-size array all
!! return an allocated, zero-length string, never an unallocated one -- so `len(result)` is the
!! only thing a caller ever has to test. An `intent(out)` allocatable is deallocated on entry, so a
!! path that merely failed to assign would hand back a variable whose `len` is undefined.
!!
!! **Every PATH argument, and `suffix`, has its TRAILING blanks trimmed and its LEADING blanks
!! preserved.** A `character(len=*)` actual is blank-padded and nothing can distinguish padding
!! from intent, so trailing blanks cannot be honoured; a leading blank is a legal filename
!! character and is kept. A caller who genuinely means a trailing blank builds the string with
!! `pf_split_path` plus concatenation.
!!
!! **`pf_to_lower`/`pf_to_upper` are the exception and do not trim**: the copy form is exactly
!! `len(s)` long and the in-place form cannot alter its variable's length at all, which is the
!! whole point of folding a fixed-length key in place. Do not widen the rule above to cover them.
!!
!! **Everything text-producing is a SUBROUTINE with a `character(len=:), allocatable, intent(out)`
!! result**, never a function returning a deferred-length character. GCC PR113797 makes the hidden
!! length variable of such a function unreliable under concurrency; this project measured 562670 of
!! 2000000 concurrent calls corrupted as a function and 0 as a subroutine.
!!
!! **Paths are POSIX and purely lexical.** The separator is `/` on every platform, and no `.` or
!! `..` is ever resolved: `a/b/../c` is only `a/c` when `b` is not a symlink, so a lexical answer
!! would be wrong in exactly the cases where it matters. Nothing here touches the filesystem, which
!! is what keeps every procedure `pure` and testable without fixtures. The reference is CPython's
!! `posixpath`, and `tools/generate_path_reference.py` emits the values the tests assert, so
!! "we follow Python" is checked rather than claimed.
!!
!! Guide: `doc/pages/utilities/utils.md`. Tests: `test/test_utils.f90` (suite name `utils`).
module parquet_utils
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private

    public :: pf_to_lower, pf_to_upper
    public :: pf_to_str
    public :: pf_join_path
    public :: pf_dirname, pf_basename, pf_path_ext, pf_path_stem
    public :: pf_split_path, pf_path_add_suffix

    !> The path separator. POSIX `/` on every platform, deliberately -- see the module header.
    character(len=1), parameter :: PATH_SEP = "/"
    !> The extension marker.
    character(len=1), parameter :: EXT_SEP = "."
    !> Scratch width for one `pf_to_str` rendering. A `fmt` producing more than this many
    !! characters is reported the same way any other rejected `fmt` is, as a run of asterisks.
    integer, parameter :: TO_STR_BUF = 512

    ! ---- Case folding ----

    !> Lowercases every ASCII `A`-`Z`, leaving every other byte untouched.
    !!
    !! ASCII only, with no option to widen it: a byte outside `A`-`Z` is copied through unchanged,
    !! so UTF-8 text passes through byte-identical rather than being corrupted the way a
    !! byte-at-a-time table fold corrupts a multi-byte character.
    !!
    !! Two forms. `call pf_to_lower(s, out)` allocates `out` to `len(s)` and writes the folded copy
    !! into it; `call pf_to_lower(text)` folds a `character(len=*)` variable in place, which is what
    !! most call sites want since they already hold a short key in a fixed-length variable.
    interface pf_to_lower
        module procedure pf_to_lower_copy    !! `(s, out)`: folded copy into an allocatable result.
        module procedure pf_to_lower_inplace !! `(text)`: folds a fixed-length variable in place.
    end interface pf_to_lower

    !> Uppercases every ASCII `a`-`z`, leaving every other byte untouched. Identical in shape and
    !! rules to `pf_to_lower`; see that interface for the ASCII-only guarantee and the two forms.
    interface pf_to_upper
        module procedure pf_to_upper_copy    !! `(s, out)`: folded copy into an allocatable result.
        module procedure pf_to_upper_inplace !! `(text)`: folds a fixed-length variable in place.
    end interface pf_to_upper

    ! ---- Value to text ----

    !> Renders one value as text: `call pf_to_str(value, res [, min_width] [, fmt] [, pad])`.
    !!
    !! Five specifics -- `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)` and
    !! `logical` -- all with the same optional tail, so there is one signature to learn. `value` is
    !! the value to render and `res` the `character(len=:), allocatable` result; `min_width` is a
    !! **minimum** field width, `fmt` a Fortran format specification including its parentheses, and
    !! `pad` the single character `min_width` pads with.
    !!
    !! **`min_width` is a minimum, never an exact width.** A value needing more characters gets
    !! them: `pf_to_str(123456, res, min_width=4)` is `"123456"`, not a truncation and not an
    !! abort. `min_width <= 0` is the same as omitting it. This is what leaves the module with no
    !! error paths at all.
    !!
    !! **`fmt` renders and `min_width` then pads what it produced**, in that order, so the two
    !! never fight. A `fmt` the I/O runtime rejects yields a run of asterisks -- Fortran's own
    !! overflow marker, and what `pf_str` in `parquet_logging` already does. **The length of that
    !! run is deliberately unspecified and must not be asserted**: the compilers disagree about it,
    !! because some reject a type-mismatched edit descriptor through `iostat` and others write
    !! asterisks themselves and report success.
    !!
    !! **Exactly one case puts padding between the sign and the digits: an INTEGER padded with
    !! `"0"`.** `pf_to_str(-7, res, min_width=5)` is `"-0007"`, because zeros inserted after an
    !! integer's sign do not change the value it reads back as. Everything else pads in front of
    !! the sign -- `pad=" "` gives `"   -7"`, `pad="x"` gives `"xxx-7"`, and `pad="0"` on a
    !! **real** gives `"0000-3.5"`, since there a leading zero run is alignment rather than part of
    !! the number.
    !!
    !! **`pad` defaults to `"0"` for the two integer specifics** -- the zero-padded file counter is
    !! the case this exists for -- **and to `" "` for the real and logical ones**, where
    !! zero-padding is almost never meant. `fmt` defaults to `'(i0)'` and `'(g0)'` respectively.
    !!
    !! **A `logical` renders as `true` or `false`**, lowercase and unabbreviated, so that the
    !! default is a valid TOML boolean and a caller assembling a config file needs no `fmt`.
    !! **This deliberately disagrees with `pf_str` in `parquet_logging`, which renders `T`/`F`**,
    !! and both are in scope at once under `use parquet`. The two have different destinations: a
    !! log column, and text something else will read back. Pass `fmt='(l1)'` for Fortran's own
    !! spelling.
    !!
    !! **The default rendering of a real is not portable and no test may pin it**: `(g0)` gives
    !! `3.1400000000000001` on one compiler and `3.140000000000000` on another for the same
    !! `3.14_real64`. Callers needing specific text pass `fmt`.
    interface pf_to_str
        module procedure pf_to_str_i32 !! `integer(int32)`; `fmt` defaults to `'(i0)'`, `pad` to `"0"`.
        module procedure pf_to_str_i64 !! `integer(int64)`; `fmt` defaults to `'(i0)'`, `pad` to `"0"`.
        module procedure pf_to_str_r32 !! `real(real32)`; `fmt` defaults to `'(g0)'`, `pad` to `" "`.
        module procedure pf_to_str_r64 !! `real(real64)`; `fmt` defaults to `'(g0)'`, `pad` to `" "`.
        module procedure pf_to_str_log !! `logical`; renders `true`/`false`, `pad` defaults to `" "`.
    end interface pf_to_str

    ! ---- Path joining ----

    !> Joins path components with exactly one `/` between them, following CPython's
    !! `posixpath.join` rules exactly.
    !!
    !! Available for two to five scalar components -- `call pf_join_path(a, b [, c] [, d] [, e],
    !! path)` -- and for an array, `call pf_join_path(parts, path)`.
    !!
    !! Three rules, applied left to right, and they are the whole specification:
    !!
    !! * a component beginning with `/` **restarts** the path, discarding everything before it, so
    !!   `pf_join_path("a", "/b", p)` is `"/b"` and there is no second procedure to remember for
    !!   the absolute case;
    !! * when the accumulated path is empty or already ends with `/`, the next component is
    !!   appended directly, so `pf_join_path("a/", "b", p)` is `"a/b"` and `pf_join_path("./", "b",
    !!   p)` is `"./b"`;
    !! * otherwise exactly one `/` is inserted.
    !!
    !! **An empty final component still gets its separator: `pf_join_path("a", "", p)` is `"a/"`,
    !! not `"a"`.** This is Python's behaviour and is kept deliberately, but it is the one rule that
    !! surprises people and it bites in a plausible pattern -- `pf_join_path(dir, name, out)` where
    !! `name` happens to be empty yields a directory-looking path rather than `dir` itself.
    !!
    !! Interior duplicate separators are preserved (`"a//b"` stays `"a//b"`); what the rules
    !! guarantee is that no *second* separator is ever added. Nothing is normalised: `"a/b/../c"`
    !! comes back unchanged.
    !!
    !! **Two Fortran-specific extensions of the Python contract**, since `posixpath.join()` with no
    !! arguments raises `TypeError` and cannot be consulted: the array form over a **zero-size**
    !! array returns an allocated, zero-length string -- the identity of the fold, so a caller
    !! accumulating components needs no special case -- and over a **one-element** array returns
    !! that component, trimmed.
    interface pf_join_path
        module procedure pf_join_path_2    !! `(path1, path2, path)`.
        module procedure pf_join_path_3    !! `(path1, path2, path3, path)`.
        module procedure pf_join_path_4    !! `(path1, path2, path3, path4, path)`.
        module procedure pf_join_path_5    !! `(path1, path2, path3, path4, path5, path)`.
        module procedure pf_join_path_many !! `(parts, path)`: an array of components.
    end interface pf_join_path

contains

    ! ================================================================================
    ! Case folding
    ! ================================================================================

    !> Folds one byte to lower case if it is an ASCII `A`-`Z`, and returns it unchanged otherwise.
    pure function lower_byte(c) result(res)
        character(len=1), intent(in) :: c !! the byte to fold.
        character(len=1) :: res !! `c` lowercased if it was an ASCII capital, else `c` itself.
        integer :: code

        code = iachar(c)
        if (code >= iachar("A") .and. code <= iachar("Z")) then
            res = achar(code - iachar("A") + iachar("a"))
        else
            res = c
        end if
    end function lower_byte

    !> Folds one byte to upper case if it is an ASCII `a`-`z`, and returns it unchanged otherwise.
    pure function upper_byte(c) result(res)
        character(len=1), intent(in) :: c !! the byte to fold.
        character(len=1) :: res !! `c` uppercased if it was an ASCII lower-case letter, else `c`.
        integer :: code

        code = iachar(c)
        if (code >= iachar("a") .and. code <= iachar("z")) then
            res = achar(code - iachar("a") + iachar("A"))
        else
            res = c
        end if
    end function upper_byte

    !> Writes a lowercased copy of `s` into a freshly allocated `out` of the same length.
    pure subroutine pf_to_lower_copy(s, out)
        character(len=*), intent(in) :: s !! text to fold; every byte is preserved except ASCII `A`-`Z`.
        character(len=:), allocatable, intent(out) :: out !! the folded copy, allocated to `len(s)`.
        integer :: i

        allocate (character(len=len(s)) :: out)
        do i = 1, len(s)
            out(i:i) = lower_byte(s(i:i))
        end do
    end subroutine pf_to_lower_copy

    !> Lowercases `text` in place, leaving its length and every non-`A`-`Z` byte unchanged.
    pure subroutine pf_to_lower_inplace(text)
        character(len=*), intent(inout) :: text !! text folded in place; trailing blanks stay blanks.
        integer :: i

        do i = 1, len(text)
            text(i:i) = lower_byte(text(i:i))
        end do
    end subroutine pf_to_lower_inplace

    !> Writes an uppercased copy of `s` into a freshly allocated `out` of the same length.
    pure subroutine pf_to_upper_copy(s, out)
        character(len=*), intent(in) :: s !! text to fold; every byte is preserved except ASCII `a`-`z`.
        character(len=:), allocatable, intent(out) :: out !! the folded copy, allocated to `len(s)`.
        integer :: i

        allocate (character(len=len(s)) :: out)
        do i = 1, len(s)
            out(i:i) = upper_byte(s(i:i))
        end do
    end subroutine pf_to_upper_copy

    !> Uppercases `text` in place, leaving its length and every non-`a`-`z` byte unchanged.
    pure subroutine pf_to_upper_inplace(text)
        character(len=*), intent(inout) :: text !! text folded in place; trailing blanks stay blanks.
        integer :: i

        do i = 1, len(text)
            text(i:i) = upper_byte(text(i:i))
        end do
    end subroutine pf_to_upper_inplace

    ! ================================================================================
    ! Value to text
    ! ================================================================================

    !> Pads `text` on the left to at least `min_width` characters.
    !!
    !! **Exactly one case puts the padding between the sign and the digits: an INTEGER padded with
    !! `"0"`.** `-7` at `min_width=5` is `"-0007"` there, because inserting zeros immediately after
    !! the sign of an integer does not change the value it reads back as. Every other combination
    !! pads in front of the sign, including `pad=" "` (`"   -7"`), any other character
    !! (`"xxx-7"`), and `"0"` on a real -- where a leading zero run is alignment rather than part
    !! of the number, so it goes outside the sign like any other pad.
    pure subroutine pad_left(text, min_width, padc, integer_value, res)
        character(len=*), intent(in) :: text !! the rendered text, used verbatim.
        integer, intent(in) :: min_width !! minimum total width; `<= len(text)` copies `text` through.
        character(len=1), intent(in) :: padc !! the padding character.
        logical, intent(in) :: integer_value !! `.true.` only from the two integer specifics.
        character(len=:), allocatable, intent(out) :: res !! `text`, padded; always allocated.
        integer :: nadd
        logical :: signed

        nadd = min_width - len(text)
        if (nadd <= 0) then
            res = text
            return
        end if
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit, so a single
        ! `if (len(text) > 0 .and. text(1:1) == "-")` would evaluate `text(1:1)` on an empty string.
        signed = .false.
        if (len(text) > 0) signed = (text(1:1) == "-" .or. text(1:1) == "+")
        if (signed .and. integer_value .and. padc == "0") then
            res = text(1:1)//repeat(padc, nadd)//text(2:)
        else
            res = repeat(padc, nadd)//text
        end if
    end subroutine pad_left

    !> The result returned for a `fmt` the I/O runtime rejects. A run of asterisks, Fortran's own
    !! overflow marker; its length is not part of the contract and no test may assert one.
    pure subroutine bad_format_result(res)
        character(len=:), allocatable, intent(out) :: res !! a short run of asterisks.

        res = repeat("*", 3)
    end subroutine bad_format_result

    !> Whether a `write` left usable text in `buf`, whatever its `iostat` said.
    !!
    !! **A nonzero `iostat` does not always mean the format was rejected.** A value too wide for
    !! its edit descriptor is not an error in the standard -- the runtime fills the field with
    !! asterisks and reports success -- but ifx's `-check output_conversion`, which `-check all`
    !! and so `--profile debug` turn on, reports `iostat = 63` for exactly that case. The text is
    !! rendered either way. Taking the `iostat` alone would therefore make a diagnostic build
    !! return a different string from a plain one: `pf_to_str(12345, s, fmt='(i2)')` gives `**`
    !! normally and, without this, `bad_format_result`'s `***` under `-check all`.
    !!
    !! The discriminator is the buffer, not the code number, because the code numbers are
    !! per-compiler (ifx 63 for the conversion and 62 for a rejected format, gfortran 5006) and
    !! nothing portable distinguishes them. Measured on ifx 2026.1 and gfortran 15.2: a rejected
    !! format renders NOTHING on both, while an overflowing field renders its asterisks on both.
    !! So text in the buffer means the runtime got far enough to produce a result, and that result
    !! is what a build without the check would have returned.
    !!
    !! **Callers must blank `buf` before the write.** It is an uninitialised local otherwise, and
    !! this would read whatever the stack held.
    pure logical function rendered_ok(ios, buf) result(ok)
        integer, intent(in) :: ios !! the `iostat` the write reported.
        character(len=*), intent(in) :: buf !! the buffer it wrote into, blanked beforehand.

        ok = ios == 0
        if (.not. ok) ok = len_trim(buf) > 0
    end function rendered_ok

    !> Applies the optional `min_width`/`pad` tail to already-rendered text.
    !!
    !! Factored out because all five specifics share it exactly; `min_width` absent or `<= 0` and
    !! the text is copied through unchanged.
    pure subroutine finish_to_str(text, res, min_width, pad, default_pad, integer_value)
        character(len=*), intent(in) :: text !! the rendered text.
        character(len=:), allocatable, intent(out) :: res !! the padded result; always allocated.
        integer, intent(in), optional :: min_width !! caller's minimum width, if any.
        character(len=1), intent(in), optional :: pad !! caller's padding character, if any.
        character(len=1), intent(in) :: default_pad !! this specific's default padding character.
        logical, intent(in) :: integer_value !! `.true.` only from the two integer specifics; see `pad_left`.
        character(len=1) :: padc

        if (.not. present(min_width)) then
            res = text
            return
        end if
        if (min_width <= 0) then
            res = text
            return
        end if
        padc = default_pad
        if (present(pad)) padc = pad
        call pad_left(text, min_width, padc, integer_value, res)
    end subroutine finish_to_str

    !> Renders one `integer(int32)` as text. See the `pf_to_str` interface for the full contract.
    pure subroutine pf_to_str_i32(value, res, min_width, fmt, pad)
        integer(int32), intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification with parentheses; default `'(i0)'`.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `"0"`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        buf = ''
        if (present(fmt)) then
            write (buf, fmt, iostat=ios) value
        else
            write (buf, '(i0)', iostat=ios) value
        end if
        if (.not. rendered_ok(ios, buf)) then
            call bad_format_result(res)
            return
        end if
        call finish_to_str(trim(buf), res, min_width, pad, "0", .true.)
    end subroutine pf_to_str_i32

    !> Renders one `integer(int64)` as text. See the `pf_to_str` interface for the full contract.
    pure subroutine pf_to_str_i64(value, res, min_width, fmt, pad)
        integer(int64), intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification with parentheses; default `'(i0)'`.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `"0"`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        buf = ''
        if (present(fmt)) then
            write (buf, fmt, iostat=ios) value
        else
            write (buf, '(i0)', iostat=ios) value
        end if
        if (.not. rendered_ok(ios, buf)) then
            call bad_format_result(res)
            return
        end if
        call finish_to_str(trim(buf), res, min_width, pad, "0", .true.)
    end subroutine pf_to_str_i64

    !> Renders one `real(real32)` as text. See the `pf_to_str` interface for the full contract, and
    !! note that the default `(g0)` rendering is not portable between compilers.
    pure subroutine pf_to_str_r32(value, res, min_width, fmt, pad)
        real(real32), intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification with parentheses; default `'(g0)'`.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `" "`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        buf = ''
        if (present(fmt)) then
            write (buf, fmt, iostat=ios) value
        else
            write (buf, '(g0)', iostat=ios) value
        end if
        if (.not. rendered_ok(ios, buf)) then
            call bad_format_result(res)
            return
        end if
        call finish_to_str(trim(buf), res, min_width, pad, " ", .false.)
    end subroutine pf_to_str_r32

    !> Renders one `real(real64)` as text. See the `pf_to_str` interface for the full contract, and
    !! note that the default `(g0)` rendering is not portable between compilers.
    pure subroutine pf_to_str_r64(value, res, min_width, fmt, pad)
        real(real64), intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification with parentheses; default `'(g0)'`.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `" "`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        buf = ''
        if (present(fmt)) then
            write (buf, fmt, iostat=ios) value
        else
            write (buf, '(g0)', iostat=ios) value
        end if
        if (.not. rendered_ok(ios, buf)) then
            call bad_format_result(res)
            return
        end if
        call finish_to_str(trim(buf), res, min_width, pad, " ", .false.)
    end subroutine pf_to_str_r64

    !> Renders one `logical` as `true` or `false` -- the TOML spelling, deliberately unlike
    !! `pf_str` in `parquet_logging`, which renders `T`/`F`. See the `pf_to_str` interface.
    pure subroutine pf_to_str_log(value, res, min_width, fmt, pad)
        logical, intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification; no default -- `true`/`false` is written directly.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `" "`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        if (present(fmt)) then
            buf = ''
            write (buf, fmt, iostat=ios) value
            if (.not. rendered_ok(ios, buf)) then
                call bad_format_result(res)
                return
            end if
            call finish_to_str(trim(buf), res, min_width, pad, " ", .false.)
        else if (value) then
            call finish_to_str("true", res, min_width, pad, " ", .false.)
        else
            call finish_to_str("false", res, min_width, pad, " ", .false.)
        end if
    end subroutine pf_to_str_log

    ! ================================================================================
    ! Path joining
    ! ================================================================================

    !> `.true.` when `p` ends with the path separator. **Total on a zero-length argument**, which
    !! is the whole reason it exists: the obvious `len(p) > 0 .and. p(len(p):len(p)) == "/"` is not
    !! safe, because Fortran does not guarantee short-circuit evaluation and the substring is then
    !! `p(0:0)`. That is the defect this module's ancestor carried.
    pure function ends_with_sep(p) result(res)
        character(len=*), intent(in) :: p !! the text to test.
        logical :: res !! `.true.` only when `p` is non-empty and its last character is `/`.

        res = .false.
        if (len(p) > 0) res = (p(len(p):len(p)) == PATH_SEP)
    end function ends_with_sep

    !> `.true.` when `p` begins with the path separator, i.e. when it is an absolute path. Total on
    !! a zero-length argument, for the reason given on `ends_with_sep`.
    pure function starts_with_sep(p) result(res)
        character(len=*), intent(in) :: p !! the text to test.
        logical :: res !! `.true.` only when `p` is non-empty and its first character is `/`.

        res = .false.
        if (len(p) > 0) res = (p(1:1) == PATH_SEP)
    end function starts_with_sep

    !> Joins two already-trimmed components, applying the three `pf_join_path` rules once.
    !!
    !! This is the whole algorithm; every scalar arity folds over it, and the array form re-derives
    !! the same walk in two passes so it can size its result before filling it.
    pure subroutine join_pair(a, b, out)
        character(len=*), intent(in) :: a !! the accumulated path so far, trailing blanks already gone.
        character(len=*), intent(in) :: b !! the component to append, trailing blanks already gone.
        character(len=:), allocatable, intent(out) :: out !! the joined path; always allocated.

        if (starts_with_sep(b)) then
            out = b
        else if (len(a) == 0 .or. ends_with_sep(a)) then
            out = a//b
        else
            out = a//PATH_SEP//b
        end if
    end subroutine join_pair

    !> Joins two path components. See the `pf_join_path` interface for the rules.
    pure subroutine pf_join_path_2(path1, path2, path)
        character(len=*), intent(in) :: path1 !! first component; trailing blanks trimmed, leading blanks kept.
        character(len=*), intent(in) :: path2 !! second component; an absolute one restarts the path.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated.

        call join_pair(path1(1:len_trim(path1)), path2(1:len_trim(path2)), path)
    end subroutine pf_join_path_2

    !> Joins three path components. See the `pf_join_path` interface for the rules.
    pure subroutine pf_join_path_3(path1, path2, path3, path)
        character(len=*), intent(in) :: path1 !! first component.
        character(len=*), intent(in) :: path2 !! second component.
        character(len=*), intent(in) :: path3 !! third component.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated.
        character(len=:), allocatable :: acc

        call pf_join_path_2(path1, path2, acc)
        call join_pair(acc, path3(1:len_trim(path3)), path)
    end subroutine pf_join_path_3

    !> Joins four path components. See the `pf_join_path` interface for the rules.
    pure subroutine pf_join_path_4(path1, path2, path3, path4, path)
        character(len=*), intent(in) :: path1 !! first component.
        character(len=*), intent(in) :: path2 !! second component.
        character(len=*), intent(in) :: path3 !! third component.
        character(len=*), intent(in) :: path4 !! fourth component.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated.
        character(len=:), allocatable :: acc

        call pf_join_path_3(path1, path2, path3, acc)
        call join_pair(acc, path4(1:len_trim(path4)), path)
    end subroutine pf_join_path_4

    !> Joins five path components. See the `pf_join_path` interface for the rules.
    pure subroutine pf_join_path_5(path1, path2, path3, path4, path5, path)
        character(len=*), intent(in) :: path1 !! first component.
        character(len=*), intent(in) :: path2 !! second component.
        character(len=*), intent(in) :: path3 !! third component.
        character(len=*), intent(in) :: path4 !! fourth component.
        character(len=*), intent(in) :: path5 !! fifth component.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated.
        character(len=:), allocatable :: acc

        call pf_join_path_4(path1, path2, path3, path4, acc)
        call join_pair(acc, path5(1:len_trim(path5)), path)
    end subroutine pf_join_path_5

    !> Joins every element of `parts` in order. See the `pf_join_path` interface for the rules and
    !! for the two zero- and one-element cases Python cannot be asked about.
    !!
    !! Sized in one pass and filled in a second, rather than reallocating per component: the
    !! obvious accumulate-as-you-go loop copies the whole prefix each time, which is fine at five
    !! components and quadratic at a thousand.
    pure subroutine pf_join_path_many(parts, path)
        character(len=*), intent(in) :: parts(:) !! the components, in order; each is trailing-trimmed.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated, zero-length for a zero-size array.
        integer :: k, n, ln, total, pos, first
        logical :: is_empty, ends_sep

        n = size(parts)
        if (n == 0) then
            path = ""
            return
        end if

        ! An absolute component discards every component before it (posixpath.join's first rule),
        ! so both walks start at the LAST absolute one instead of resetting when they reach it.
        ! Pass 1 could reset in place; pass 2 could not. The buffer is sized for what SURVIVES, so
        ! writing a prefix that is about to be discarded stores past the end of it -- and the
        ! answer still comes out right, because those bytes are then overwritten. Only a
        ! bounds-checking build reports it (--profile debug and --profile nagdeb both do; a plain
        ! `fpm test`, and CI, do not). Starting at `first` removes the reset from both loops, which
        ! is why neither has an "absolute" case at all. See feature_risks.md Risk-173.
        first = 1
        do k = n, 1, -1
            ln = len_trim(parts(k))
            if (ln > 0) then
                if (parts(k) (1:1) == PATH_SEP) then
                    first = k
                    exit
                end if
            end if
        end do

        ! Pass 1 -- the final length, by walking the fold without building anything.
        total = 0
        is_empty = .true.
        ends_sep = .false.
        do k = first, n
            ln = len_trim(parts(k))
            if (is_empty .or. ends_sep) then
                total = total + ln
                if (ln > 0) then
                    is_empty = .false.
                    ends_sep = (parts(k) (ln:ln) == PATH_SEP)
                end if
            else
                total = total + 1 + ln
                if (ln > 0) then
                    ends_sep = (parts(k) (ln:ln) == PATH_SEP)
                else
                    ends_sep = .true.
                end if
            end if
        end do

        allocate (character(len=total) :: path)

        ! Pass 2 -- the identical walk, writing. `pos` therefore tracks `total` exactly, and the
        ! final `pos` equals `total` on every input.
        pos = 0
        is_empty = .true.
        ends_sep = .false.
        do k = first, n
            ln = len_trim(parts(k))
            if (is_empty .or. ends_sep) then
                if (ln > 0) then
                    path(pos + 1:pos + ln) = parts(k) (1:ln)
                    pos = pos + ln
                    is_empty = .false.
                    ends_sep = (parts(k) (ln:ln) == PATH_SEP)
                end if
            else
                path(pos + 1:pos + 1) = PATH_SEP
                pos = pos + 1
                if (ln > 0) then
                    path(pos + 1:pos + ln) = parts(k) (1:ln)
                    pos = pos + ln
                    ends_sep = (parts(k) (ln:ln) == PATH_SEP)
                else
                    ends_sep = .true.
                end if
            end if
        end do
    end subroutine pf_join_path_many

    ! ================================================================================
    ! Taking a path apart
    ! ================================================================================

    !> Locates the two positions every path procedure derives its answer from: the last separator,
    !! and the extension dot if there is one.
    !!
    !! One scanner for five entry points, so the `.bashrc` and `/a.b/c` rules live in one place and
    !! no single-piece query allocates anything it was not asked for.
    !!
    !! `dot_pos` implements `posixpath.splitext`, which skips **all** leading dots of the basename:
    !! an extension exists only when there is a non-dot character between the last separator and
    !! the dot. That is why `.bashrc` has none and `a.tar.gz` has `.gz`.
    pure subroutine scan_path(p, sep_pos, dot_pos)
        character(len=*), intent(in) :: p !! the path, with trailing blanks already removed.
        integer, intent(out) :: sep_pos !! index of the last `/`, or 0 when there is none.
        integer, intent(out) :: dot_pos !! index of the extension dot, or 0 when there is no extension.
        integer :: i, n

        n = len(p)

        sep_pos = 0
        do i = n, 1, -1
            if (p(i:i) == PATH_SEP) then
                sep_pos = i
                exit
            end if
        end do

        dot_pos = 0
        do i = n, sep_pos + 1, -1
            if (p(i:i) == EXT_SEP) then
                dot_pos = i
                exit
            end if
        end do
        if (dot_pos == 0) return

        do i = sep_pos + 1, dot_pos - 1
            if (p(i:i) /= EXT_SEP) return
        end do
        dot_pos = 0
    end subroutine scan_path

    !> Builds the directory part from a scanned path, following `posixpath.dirname`.
    !!
    !! Not simply `p(1:sep_pos-1)`: Python keeps a head that is entirely separators (`"/x"` gives
    !! `"/"`, not `""`) and otherwise strips every trailing separator (`"a//b"` gives `"a"`).
    pure subroutine dirname_from_scan(p, sep_pos, dir)
        character(len=*), intent(in) :: p !! the path, with trailing blanks already removed.
        integer, intent(in) :: sep_pos !! index of the last `/`, from `scan_path`.
        character(len=:), allocatable, intent(out) :: dir !! the directory part; always allocated.
        integer :: i, j
        logical :: all_sep

        if (sep_pos == 0) then
            dir = ""
            return
        end if

        all_sep = .true.
        do i = 1, sep_pos
            if (p(i:i) /= PATH_SEP) then
                all_sep = .false.
                exit
            end if
        end do
        if (all_sep) then
            dir = p(1:sep_pos)
            return
        end if

        j = sep_pos
        do while (j > 0)
            if (p(j:j) /= PATH_SEP) exit
            j = j - 1
        end do
        dir = p(1:j)
    end subroutine dirname_from_scan

    !> Everything before the last separator, following `posixpath.dirname`.
    !!
    !! `"/data/run3/cat.parquet"` gives `"/data/run3"`, `"myfile.txt"` gives `""`, `"/x"` gives
    !! `"/"` and `"/a/b/"` gives `"/a/b"`. Purely lexical: nothing is normalised and the filesystem
    !! is never consulted.
    pure subroutine pf_dirname(path, dir)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: dir !! the directory part; always allocated, zero-length when there is none.
        integer :: n, sep_pos, dot_pos

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        call dirname_from_scan(path(1:n), sep_pos, dir)
    end subroutine pf_dirname

    !> Everything after the last separator, following `posixpath.basename`.
    !!
    !! `"/data/run3/cat.parquet"` gives `"cat.parquet"` and `"/a/b/"` gives `""` -- a trailing
    !! separator means an empty basename, and is not read as "the caller meant `b`".
    pure subroutine pf_basename(path, base)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: base !! the final component; always allocated, zero-length when there is none.
        integer :: n, sep_pos, dot_pos

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        base = path(sep_pos + 1:n)
    end subroutine pf_basename

    !> The extension, **including its leading dot**, following `posixpath.splitext(path)[1]`.
    !!
    !! `"cat.parquet"` gives `".parquet"`, `"a.tar.gz"` gives `".gz"` (the last dot only), and both
    !! `".bashrc"` and `"/a.b/c"` give `""` -- a leading dot marks a hidden file rather than a
    !! suffix, and a dot in a *directory* component is not an extension.
    !!
    !! The dot is included so that `stem // "_stat" // ext` rebuilds a name correctly, and so that
    !! the empty case composes without leaving a stray dot behind.
    pure subroutine pf_path_ext(path, ext)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: ext !! the extension with its dot; zero-length when there is none.
        integer :: n, sep_pos, dot_pos

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        if (dot_pos == 0) then
            ext = ""
        else
            ext = path(dot_pos:n)
        end if
    end subroutine pf_path_ext

    !> The basename with its extension removed, i.e.
    !! `posixpath.splitext(posixpath.basename(path))[0]`.
    !!
    !! `"/data/cat.parquet"` gives `"cat"`, `"a.tar.gz"` gives `"a.tar"`, `".bashrc"` gives
    !! `".bashrc"` and `"/a/b/"` gives `""`.
    !!
    !! **This is not `pathlib.Path.stem`**, which normalises a trailing separator before splitting
    !! and does not read a trailing dot as an extension -- the two disagree on `"/a/b/"` and on
    !! `"a."`. The lexical rule above is the one implemented, so `pathlib` is not a second opinion
    !! to check against.
    pure subroutine pf_path_stem(path, stem)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: stem !! the basename without its extension; always allocated.
        integer :: n, sep_pos, dot_pos, last

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        last = n
        if (dot_pos > 0) last = dot_pos - 1
        stem = path(sep_pos + 1:last)
    end subroutine pf_path_stem

    !> Splits a path into its directory, stem and extension in one call and one scan.
    !!
    !! ```fortran
    !! call pf_split_path(infile, dir, stem, ext)
    !! call pf_join_path(dir, stem // "_stat" // ext, outfile)
    !! ```
    !!
    !! Each piece is exactly what `pf_dirname`, `pf_path_stem` and `pf_path_ext` return
    !! individually; this form costs one scan instead of three and is the escape hatch for a caller
    !! who needs to recombine the pieces with text this module would otherwise have trimmed.
    pure subroutine pf_split_path(path, dir, stem, ext)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: dir !! the directory part; always allocated.
        character(len=:), allocatable, intent(out) :: stem !! the basename without its extension; always allocated.
        character(len=:), allocatable, intent(out) :: ext !! the extension with its dot; always allocated.
        integer :: n, sep_pos, dot_pos, last

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        call dirname_from_scan(path(1:n), sep_pos, dir)
        last = n
        if (dot_pos > 0) then
            last = dot_pos - 1
            ext = path(dot_pos:n)
        else
            ext = ""
        end if
        stem = path(sep_pos + 1:last)
    end subroutine pf_split_path

    !> Inserts `suffix` immediately before the extension, keeping the directory and the extension.
    !!
    !! `pf_path_add_suffix("/data/myfile.txt", "_stat", out)` gives `"/data/myfile_stat.txt"`. It
    !! composes on every shape the table in the guide covers: a name with no extension gives
    !! `"myfile_stat"`, `"a.tar.gz"` gives `"a.tar_stat.gz"`, and `".bashrc"` gives
    !! `".bashrc_stat"` because a leading dot is not an extension.
    !!
    !! **A path ending in a separator has an empty stem, so `"/a/b/"` gives `"/a/b/_stat"`.** That
    !! is the lexically consistent answer and the one the four single-piece procedures imply;
    !! reading the trailing separator as "the caller meant `b`" would be exactly the normalisation
    !! this module refuses to do everywhere else.
    !!
    !! **`suffix` is trimmed like every other text argument.** A `character(len=16)` variable
    !! holding `"_stat"` is blank-padded, and copying it verbatim would produce
    !! `myfile_stat     .txt` from a call site that looks correct. A caller who genuinely needs a
    !! trailing blank uses `pf_split_path` and ordinary concatenation, which preserves the padding
    !! of a fixed-length variable exactly.
    pure subroutine pf_path_add_suffix(path, suffix, out)
        character(len=*), intent(in) :: path !! the path to rename, e.g. `"/data/myfile.txt"`.
        character(len=*), intent(in) :: suffix !! the text to insert before the extension, e.g. `"_stat"`; trailing blanks trimmed.
        character(len=:), allocatable, intent(out) :: out !! the rebuilt path; always allocated.
        integer :: n, sep_pos, dot_pos, last

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        last = n
        if (dot_pos > 0) last = dot_pos - 1
        ! The literal prefix up to and including the last separator is kept rather than rebuilt
        ! through pf_dirname + pf_join_path, so an interior "a//b.txt" is not silently collapsed.
        out = path(1:sep_pos)//path(sep_pos + 1:last)//suffix(1:len_trim(suffix))//path(last + 1:n)
    end subroutine pf_path_add_suffix

    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.
end module parquet_utils ! GCOVR_EXCL_LINE
