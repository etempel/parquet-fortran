!> Tests for `parquet_utils`: ASCII case folding, value-to-text rendering, and POSIX path joining
!! and splitting.
!!
!! **The path expectations are NOT written here.** They come from `test/test_path_vectors.f90`,
!! which `tools/generate_path_reference.py` emits from CPython's `posixpath`, so "we follow
!! Python" is checked rather than claimed. A transcription error in a *reference* table is
!! invisible -- the implementation is then written to match the wrong value and every test passes.
!!
!! **Three groups of assertions are deliberately hand-written instead**, and each says so in its
!! own message: `pf_join_path` over a zero-size array (`posixpath.join()` raises `TypeError`), a
!! blank-padded fixed-length `suffix`, and a component carrying trailing blanks. CPython cannot be
!! asked about any of them, so putting them in the generated table would misrepresent where the
!! value came from.
!!
!! **No test compares a default-formatted real against a literal string**, and none asserts how
!! many asterisks a rejected `fmt` produces. Both are measured differences between compilers --
!! `(g0)` renders `3.14_real64` with a different number of digits, and a type-mismatched edit
!! descriptor is rejected through `iostat` by some runtimes and turned into asterisks by others.
!! Real rendering is asserted with an explicit `fmt` or by reading the text back.
!!
!! There are no error-path tests because `parquet_utils` has no error paths: nothing in it
!! validates, aborts or prints. That is why nothing here appears in `test/error_scenarios.f90`.
!!
!! This suite is pure computation with no files and no process-global state, so it stays out of
!! `run_tester.f90`'s parallelism exclusion list and runs concurrently.
module test_utils
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use parquet_utils
    use test_path_vectors
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_utils

contains

    !> Registers every test in this suite.
    subroutine collect_tests_utils(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("case folding covers every ASCII letter", test_fold_ascii), &
            new_unittest("case folding leaves every other byte alone", test_fold_leaves_others), &
            new_unittest("case folding in place preserves length", test_fold_inplace), &
            new_unittest("to_str renders integers exactly", test_to_str_integers), &
            new_unittest("to_str min_width grows rather than refusing", test_to_str_min_width), &
            new_unittest("to_str pads after the sign", test_to_str_sign), &
            new_unittest("to_str renders reals and logicals", test_to_str_reals_logicals), &
            new_unittest("to_str fmt renders and min_width then pads", test_to_str_fmt), &
            new_unittest("to_str returns asterisks for a rejected fmt", test_to_str_bad_fmt), &
            new_unittest("join_path matches CPython at every arity", test_join_matches_python), &
            new_unittest("join_path array form matches the scalar forms", test_join_many_agrees), &
            new_unittest("join_path array form on 0, 1 and n elements", test_join_many_degenerate), &
            new_unittest("path family matches CPython", test_path_matches_python), &
            new_unittest("split_path agrees with the single-piece procedures", test_split_agrees), &
            new_unittest("path_add_suffix matches its reference", test_add_suffix), &
            new_unittest("trailing blanks are trimmed, leading blanks kept", test_blank_handling), &
            new_unittest("every empty answer is allocated and zero-length", test_allocation_invariant), &
            new_unittest("dirname and basename recompose the path", test_round_trip) &
            ]
    end subroutine collect_tests_utils

    ! ================================================================================
    ! Case folding
    ! ================================================================================

    !> Every ASCII letter folds both ways, and folding is idempotent in each direction.
    subroutine test_fold_ascii(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=26) :: caps, lows
        character(len=:), allocatable :: got
        integer :: i

        do i = 1, 26
            caps(i:i) = achar(iachar("A") + i - 1)
            lows(i:i) = achar(iachar("a") + i - 1)
        end do

        call pf_to_lower(caps, got)
        call check(error, got == lows, "pf_to_lower must fold every ASCII A-Z")
        if (allocated(error)) return

        call pf_to_upper(lows, got)
        call check(error, got == caps, "pf_to_upper must fold every ASCII a-z")
        if (allocated(error)) return

        call pf_to_lower(lows, got)
        call check(error, got == lows, "pf_to_lower must leave already-lowercase text alone")
        if (allocated(error)) return

        call pf_to_upper(caps, got)
        call check(error, got == caps, "pf_to_upper must leave already-uppercase text alone")
    end subroutine test_fold_ascii

    !> Digits, punctuation, blanks and multi-byte UTF-8 all pass through byte-identical.
    !!
    !! The UTF-8 case is the regression test for the corruption a byte-at-a-time *table* fold
    !! produces: `U+0495` is two bytes, one of which shares a value with an ASCII letter's slot in
    !! such a table, so a table-driven fold silently rewrites half a character.
    subroutine test_fold_leaves_others(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=*), parameter :: mixed = "0123 -_./[]{}!?"
        ! U+0495 as UTF-8: two bytes, neither in an ASCII letter range. Deliberately carries no
        ! ASCII letter of its own -- an adjacent letter WOULD fold, which is correct, and would
        ! make a byte-identity assertion fail for the wrong reason.
        character(len=*), parameter :: utf8 = char(int(z'D2'))//char(int(z'95'))
        character(len=*), parameter :: mixed_utf8 = char(int(z'D2'))//char(int(z'95'))//"aB"
        character(len=:), allocatable :: got

        call pf_to_lower(mixed, got)
        call check(error, got == mixed, "pf_to_lower must leave digits, blanks and punctuation alone")
        if (allocated(error)) return

        call pf_to_upper(mixed, got)
        call check(error, got == mixed, "pf_to_upper must leave digits, blanks and punctuation alone")
        if (allocated(error)) return

        call pf_to_lower(utf8, got)
        call check(error, got == utf8, "pf_to_lower must return a UTF-8 sequence byte-identical")
        if (allocated(error)) return

        call pf_to_upper(utf8, got)
        call check(error, got == utf8, "pf_to_upper must return a UTF-8 sequence byte-identical")
        if (allocated(error)) return

        ! The two halves together: the multi-byte character survives while the ASCII letters
        ! beside it fold, which is what "ASCII only, everything else copied through" means.
        call pf_to_upper(mixed_utf8, got)
        call check(error, got == utf8//"AB", "an ASCII letter beside a UTF-8 sequence must fold while the bytes survive")
        if (allocated(error)) return

        call pf_to_lower(mixed_utf8, got)
        call check(error, got == utf8//"ab", "pf_to_lower must fold beside a UTF-8 sequence without touching its bytes")
        if (allocated(error)) return

        call pf_to_lower("", got)
        call check(error, allocated(got), "pf_to_lower of an empty string must allocate its result")
        if (allocated(error)) return
        call check(error, len(got) == 0, "pf_to_lower of an empty string must be zero-length")
    end subroutine test_fold_leaves_others

    !> The in-place form folds a fixed-length variable and leaves its padding as padding.
    subroutine test_fold_inplace(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=10) :: text
        character(len=6) :: blanks

        text = "AbC"
        call pf_to_lower(text)
        call check(error, text == "abc", "pf_to_lower in place must fold the value")
        if (allocated(error)) return
        call check(error, len(text) == 10, "pf_to_lower in place must not change the declared length")
        if (allocated(error)) return
        call check(error, text(4:10) == "       ", "pf_to_lower in place must leave trailing padding blank")
        if (allocated(error)) return

        text = "AbC"
        call pf_to_upper(text)
        call check(error, text == "ABC", "pf_to_upper in place must fold the value")
        if (allocated(error)) return

        blanks = "      "
        call pf_to_lower(blanks)
        call check(error, blanks == "      ", "pf_to_lower in place on an all-blank variable must change nothing")
    end subroutine test_fold_inplace

    ! ================================================================================
    ! Value to text
    ! ================================================================================

    !> The exact minimal representation, at both kinds and at both ends of their ranges.
    !!
    !! `999999999` is the direct regression test for the ancestor's `floor(log10(real(nr))) + 1`
    !! digit count: converted to default `real` it rounds to `1.0e9`, `log10` gives exactly `9.0`,
    !! and the width comes out 10. A faithful port of that implementation fails this line.
    subroutine test_to_str_integers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got

        call pf_to_str(0_int32, got)
        call check(error, got == "0", "pf_to_str(0_int32) must be ""0""")
        if (allocated(error)) return

        call pf_to_str(-1_int32, got)
        call check(error, got == "-1", "pf_to_str(-1_int32) must be ""-1""")
        if (allocated(error)) return

        call pf_to_str(999999999_int32, got)
        call check(error, len(got) == 9, "pf_to_str(999999999) must be 9 characters, not 10")
        if (allocated(error)) return
        call check(error, got == "999999999", "pf_to_str(999999999) must render every digit")
        if (allocated(error)) return

        call pf_to_str(huge(0_int32), got)
        call check(error, got == "2147483647", "pf_to_str(huge(int32)) must render exactly")
        if (allocated(error)) return

        call pf_to_str(huge(0_int64), got)
        call check(error, got == "9223372036854775807", "pf_to_str(huge(int64)) must render all 19 digits")
        if (allocated(error)) return

        call pf_to_str(999999999_int64, got)
        call check(error, len(got) == 9, "the int64 specific must agree with the int32 one on 999999999")
    end subroutine test_to_str_integers

    !> `min_width` is a minimum: a longer value keeps its length rather than aborting or truncating.
    !!
    !! The width of 40 is past both of the ancestor's fixed scratch buffers (15 for `int32`, 25 for
    !! `int64`), where slicing one produced an out-of-bounds substring.
    subroutine test_to_str_min_width(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got

        call pf_to_str(7_int32, got, min_width=4)
        call check(error, got == "0007", "min_width must zero-pad an integer by default")
        if (allocated(error)) return

        call pf_to_str(123456_int32, got, min_width=4)
        call check(error, got == "123456", "min_width below the digit count must GROW, not truncate")
        if (allocated(error)) return

        call pf_to_str(7_int32, got, min_width=40)
        call check(error, len(got) == 40, "min_width of 40 must be expressible, past any fixed scratch buffer")
        if (allocated(error)) return
        call check(error, got(40:40) == "7", "min_width of 40 must leave the value at the right")
        if (allocated(error)) return

        call pf_to_str(7_int32, got, min_width=0)
        call check(error, got == "7", "min_width = 0 must be the same as omitting it")
        if (allocated(error)) return

        call pf_to_str(7_int32, got, min_width=-5)
        call check(error, got == "7", "a negative min_width must be the same as omitting it")
        if (allocated(error)) return

        call pf_to_str(7_int32, got, min_width=4, pad="x")
        call check(error, got == "xxx7", "pad must override the default padding character")
    end subroutine test_to_str_min_width

    !> Exactly one case pads between the sign and the digits: an INTEGER padded with `"0"`.
    !!
    !! All four discriminating cases are asserted, because each of the plausible simpler rules
    !! passes some of them and fails others: "always after the sign" fails the blank pad, "always
    !! before it" fails the default, "any digit" fails `pad="9"`, and "any pad character on any
    !! type" fails `pad="0"` on a real. Only the four together pin the rule.
    subroutine test_to_str_sign(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got

        call pf_to_str(-7_int32, got, min_width=5)
        call check(error, got == "-0007", "a negative value must pad after its sign, not before it")
        if (allocated(error)) return
        call check(error, got /= "000-7", "padding must never precede the sign")
        if (allocated(error)) return

        call pf_to_str(-7_int64, got, min_width=5)
        call check(error, got == "-0007", "the int64 specific must pad after the sign too")
        if (allocated(error)) return

        call pf_to_str(-7_int32, got, min_width=5, pad=" ")
        call check(error, got == "   -7", "a BLANK pad is alignment and goes OUTSIDE the sign")
        if (allocated(error)) return
        call check(error, got /= "-   7", "a blank pad must not be split across the sign")
        if (allocated(error)) return

        call pf_to_str(-7_int32, got, min_width=5, pad="9")
        call check(error, got == "999-7", "only ""0"" may sit between the sign and the digits, not any digit")
        if (allocated(error)) return

        call pf_to_str(-7_int32, got, min_width=5, pad="x")
        call check(error, got == "xxx-7", "a non-digit pad goes outside the sign")
        if (allocated(error)) return

        call pf_to_str(7_int32, got, min_width=5, pad=" ")
        call check(error, got == "    7", "an unsigned value pads the same way whatever the pad character")
        if (allocated(error)) return

        ! The integer-only half of the rule. A leading zero run on a real is alignment rather than
        ! part of the number, so it goes outside the sign exactly like a blank would.
        call pf_to_str(-3.5_real64, got, min_width=8, fmt='(f4.1)', pad="0")
        call check(error, got == "0000-3.5", "pad=""0"" on a REAL goes outside the sign: the rule is integer-only")
        if (allocated(error)) return

        call pf_to_str(-3.5_real64, got, min_width=8, fmt='(f4.1)', pad=" ")
        call check(error, got == "    -3.5", "a real defaults to a blank pad, so it pads outside the sign")
    end subroutine test_to_str_sign

    !> Each real and logical specific renders, and `true`/`false` is a contract rather than a choice.
    !!
    !! The logical spelling is asserted exactly because it is what `pf_to_str` deliberately
    !! disagrees with `pf_str` about: `pf_str` renders `T`/`F` for a log column, and this renders
    !! the TOML words for text something else reads back. This assertion is what stops someone
    !! later "fixing" the inconsistency by making the two agree.
    subroutine test_to_str_reals_logicals(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got
        real(real64) :: back64
        real(real32) :: back32
        integer :: ios

        call pf_to_str(.true., got)
        call check(error, got == "true", "pf_to_str(.true.) must be exactly ""true"" (a valid TOML boolean)")
        if (allocated(error)) return

        call pf_to_str(.false., got)
        call check(error, got == "false", "pf_to_str(.false.) must be exactly ""false"" (a valid TOML boolean)")
        if (allocated(error)) return

        call pf_to_str(.true., got, fmt='(l1)')
        call check(error, got == "T", "an explicit fmt='(l1)' must give Fortran's own spelling")
        if (allocated(error)) return

        call pf_to_str(3.25_real64, got, fmt='(f6.2)')
        call check(error, got == "  3.25", "an explicit fmt must be honoured for real64")
        if (allocated(error)) return

        call pf_to_str(3.25_real32, got, fmt='(f6.2)')
        call check(error, got == "  3.25", "an explicit fmt must be honoured for real32")
        if (allocated(error)) return

        ! The default (g0) rendering differs between compilers, so it is read back rather than
        ! compared against a literal.
        call pf_to_str(3.25_real64, got)
        read (got, *, iostat=ios) back64
        call check(error, ios == 0, "the default real64 rendering must read back as a number")
        if (allocated(error)) return
        call check(error, back64 == 3.25_real64, "the default real64 rendering must round-trip exactly")
        if (allocated(error)) return

        call pf_to_str(3.25_real32, got)
        read (got, *, iostat=ios) back32
        call check(error, ios == 0, "the default real32 rendering must read back as a number")
        if (allocated(error)) return
        call check(error, back32 == 3.25_real32, "the default real32 rendering must round-trip exactly")
    end subroutine test_to_str_reals_logicals

    !> `fmt` decides the text and `min_width` pads what it produced, in that order.
    subroutine test_to_str_fmt(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got

        call pf_to_str(255_int32, got, fmt='(z0)')
        call check(error, got == "FF", "an explicit fmt must reach the integer specifics too")
        if (allocated(error)) return

        call pf_to_str(255_int32, got, fmt='(z0)', min_width=4)
        call check(error, got == "00FF", "min_width must pad what fmt produced")
        if (allocated(error)) return

        call pf_to_str(255_int32, got, fmt='(z0)', min_width=4, pad="_")
        call check(error, got == "__FF", "pad must apply to a fmt-rendered value as well")
        if (allocated(error)) return

        call pf_to_str(255_int32, got, fmt='(z6)', min_width=4)
        call check(error, len(got) == 6, "a fmt that already fills the width must leave min_width nothing to do")
        if (allocated(error)) return
        call check(error, got == "    FF", "a fmt's own leading blanks must be preserved, not stripped")
        if (allocated(error)) return

        call pf_to_str(255_int64, got, fmt='(z0)', min_width=4)
        call check(error, got == "00FF", "the int64 specific must compose fmt and min_width the same way")
    end subroutine test_to_str_fmt

    !> A `fmt` the runtime rejects yields asterisks rather than killing the process.
    !!
    !! **The number of asterisks is deliberately not asserted.** The compilers differ: some reject
    !! a type-mismatched edit descriptor through `iostat` and this module's fallback fires, while
    !! others write asterisks themselves and report success. "Every character is an asterisk" is
    !! the whole contract.
    subroutine test_to_str_bad_fmt(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got

        call pf_to_str(1_int32, got, fmt='(nonsense)')
        call check(error, len(got) > 0, "a rejected fmt must still produce an allocated, non-empty result")
        if (allocated(error)) return
        call check(error, verify(got, "*") == 0, "a rejected fmt must yield asterisks only")
        if (allocated(error)) return

        call pf_to_str(1.0_real64, got, fmt='(nonsense)')
        call check(error, verify(got, "*") == 0, "a rejected fmt must yield asterisks for a real too")
        if (allocated(error)) return

        call pf_to_str(.true., got, fmt='(nonsense)')
        call check(error, verify(got, "*") == 0, "a rejected fmt must yield asterisks for a logical too")
        if (allocated(error)) return

        ! The negative control: the same call with a good format must NOT be asterisks, or the
        ! assertions above would pass against a procedure that always returns them.
        call pf_to_str(1_int32, got, fmt='(i0)')
        call check(error, got == "1", "a good fmt must render normally (negative control for the asterisk path)")
    end subroutine test_to_str_bad_fmt

    ! ================================================================================
    ! Path joining
    ! ================================================================================

    !> Every generated CPython join case, at the arity it was recorded with.
    subroutine test_join_matches_python(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got
        integer :: k

        do k = 1, pv_n_join
            call join_at_arity(k, got)
            call check(error, got == trim(pv_join_want(k)), "pf_join_path must match posixpath.join for case "// &
                       trim(itoa(k))//": got ["//got//"] want ["//trim(pv_join_want(k))//"]")
            if (allocated(error)) return
        end do

        ! Spot-check the rows the design calls out, so a failure names the rule rather than an index.
        call pf_join_path("a", "/b", got)
        call check(error, got == "/b", "an absolute component must restart the path")
        if (allocated(error)) return

        call pf_join_path("a", "", got)
        call check(error, got == "a/", "an empty final component must still get its separator (Python's rule)")
        if (allocated(error)) return

        call pf_join_path("./", "b", got)
        call check(error, got == "./b", """./"" already ends with a separator, so ""b"" is appended directly")
        if (allocated(error)) return

        call pf_join_path("a/b", "../c", got)
        call check(error, got == "a/b/../c", "no .. resolution is attempted anywhere")
    end subroutine test_join_matches_python

    !> The array form and the scalar forms are separate code paths and must agree on every case.
    !!
    !! `pf_join_path_many` sizes its result in one pass and fills it in a second, rather than
    !! folding `join_pair` the way the scalar arities do, so agreement is a property to check and
    !! not one guaranteed by construction.
    subroutine test_join_many_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: scalar, many
        character(len=:), allocatable :: parts(:)
        integer :: k

        do k = 1, pv_n_join
            call join_at_arity(k, scalar)
            call parts_of(k, parts)
            call pf_join_path(parts, many)
            call check(error, many == scalar, "the array form must agree with the scalar form for case "// &
                       trim(itoa(k))//": array ["//many//"] scalar ["//scalar//"]")
            if (allocated(error)) return
            deallocate (parts)
        end do
    end subroutine test_join_many_agrees

    !> The array form at 0, 1 and n elements, plus the shortest-first fixture rule.
    !!
    !! **The zero-size case is a Fortran-specific extension of the Python contract**, not a
    !! transcription of it: `posixpath.join()` raises `TypeError`, so CPython cannot be asked and
    !! this assertion is written by hand. An allocated, zero-length result is the identity of the
    !! fold, so a caller accumulating components needs no special case for the empty one.
    !!
    !! The n-element fixture puts its **shortest element first** deliberately, per this project's
    !! "sized from the first element" rule: a fixture whose first element happens to be the longest
    !! passes even when that bug is present.
    !!
    !! The last fixture is the mirror image: a long prefix followed by an **absolute** component
    !! that discards it, so the allocated result is shorter than what the fill walks past.
    subroutine test_join_many_degenerate(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got
        character(len=8) :: empty(0), one(1), several(4)

        call pf_join_path(empty, got)
        call check(error, allocated(got), "a zero-size array must still allocate the result (posixpath.join() raises)")
        if (allocated(error)) return
        call check(error, len(got) == 0, "a zero-size array must join to a zero-length string")
        if (allocated(error)) return

        one(1) = "abc"
        call pf_join_path(one, got)
        call check(error, got == "abc", "a one-element array must join to that component")
        if (allocated(error)) return

        several = [character(len=8) :: "a", "bb", "ccc", "dddd"]
        call pf_join_path(several, got)
        call check(error, got == "a/bb/ccc/dddd", "an n-element array whose FIRST element is shortest must join in full")
        if (allocated(error)) return

        ! An absolute component discards every component before it, so the result here is far
        ! SHORTER than the text preceding it. The array form sizes in one pass and fills in a
        ! second, and the fill must not write the discarded prefix: it once did, five bytes past a
        ! two-byte buffer, while still returning the right answer because those bytes were then
        ! overwritten. Only a bounds-checking build turns that into a failure
        ! (fpm test run_tester --profile nagdeb -- utils), so this case exists for that build to
        ! run. The long components are what make the overrun large enough to be worth catching.
        several = [character(len=8) :: "wwwwwwww", "xxxxxxxx", "yyyyyyyy", "/z"]
        call pf_join_path(several, got)
        call check(error, got == "/z", "an absolute component must discard every component before it")
        if (allocated(error)) return
        call check(error, len(got) == 2, "the result must be sized for what SURVIVES the absolute component")
    end subroutine test_join_many_degenerate

    ! ================================================================================
    ! Taking a path apart
    ! ================================================================================

    !> Every generated CPython case, for all four single-piece procedures.
    subroutine test_path_matches_python(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: p, dir, base, ext, stem
        integer :: k

        do k = 1, pv_n_split
            p = trim(pv_split_path(k))
            call pf_dirname(p, dir)
            call pf_basename(p, base)
            call pf_path_ext(p, ext)
            call pf_path_stem(p, stem)

            call check(error, dir == trim(pv_split_dir(k)), "pf_dirname must match posixpath.dirname for ["// &
                       p//"]: got ["//dir//"] want ["//trim(pv_split_dir(k))//"]")
            if (allocated(error)) return
            call check(error, base == trim(pv_split_base(k)), "pf_basename must match posixpath.basename for ["// &
                       p//"]: got ["//base//"] want ["//trim(pv_split_base(k))//"]")
            if (allocated(error)) return
            call check(error, ext == trim(pv_split_ext(k)), "pf_path_ext must match posixpath.splitext for ["// &
                       p//"]: got ["//ext//"] want ["//trim(pv_split_ext(k))//"]")
            if (allocated(error)) return
            call check(error, stem == trim(pv_split_stem(k)), "pf_path_stem must match splitext(basename()) for ["// &
                       p//"]: got ["//stem//"] want ["//trim(pv_split_stem(k))//"]")
            if (allocated(error)) return
        end do

        ! The three rows a hand-rolled implementation gets wrong, named so a failure says which.
        call pf_path_ext(".bashrc", ext)
        call check(error, ext == "", "a leading dot marks a hidden file, not an extension")
        if (allocated(error)) return

        call pf_path_ext("/a.b/c", ext)
        call check(error, ext == "", "a dot in a DIRECTORY component is not an extension")
        if (allocated(error)) return

        call pf_path_ext("a.tar.gz", ext)
        call check(error, ext == ".gz", "the extension is the last dot only, not "".tar.gz""")
        if (allocated(error)) return

        call pf_path_ext("cat.parquet", ext)
        call check(error, ext == ".parquet", "the extension carries its leading dot")
    end subroutine test_path_matches_python

    !> `pf_split_path` must agree piece-for-piece with the four single-piece procedures.
    !!
    !! They share one private scanner but slice it separately, so agreement is a property worth
    !! checking rather than one guaranteed by construction.
    subroutine test_split_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: p, dir, stem, ext, d2, s2, e2
        integer :: k

        do k = 1, pv_n_split
            p = trim(pv_split_path(k))
            call pf_split_path(p, dir, stem, ext)
            call pf_dirname(p, d2)
            call pf_path_stem(p, s2)
            call pf_path_ext(p, e2)

            call check(error, dir == d2, "pf_split_path's dir must equal pf_dirname for ["//p//"]")
            if (allocated(error)) return
            call check(error, stem == s2, "pf_split_path's stem must equal pf_path_stem for ["//p//"]")
            if (allocated(error)) return
            call check(error, ext == e2, "pf_split_path's ext must equal pf_path_ext for ["//p//"]")
            if (allocated(error)) return
        end do
    end subroutine test_split_agrees

    !> Every generated suffix case, plus the recipe the procedure exists for.
    subroutine test_add_suffix(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got, dir, stem, ext
        integer :: k

        do k = 1, pv_n_suffix
            call pf_path_add_suffix(trim(pv_suffix_path(k)), trim(pv_suffix_sfx(k)), got)
            call check(error, got == trim(pv_suffix_want(k)), "pf_path_add_suffix must match its reference for case "// &
                       trim(itoa(k))//": got ["//got//"] want ["//trim(pv_suffix_want(k))//"]")
            if (allocated(error)) return
        end do

        call pf_path_add_suffix("/data/myfile.txt", "_stat", got)
        call check(error, got == "/data/myfile_stat.txt", "the worked example must produce /data/myfile_stat.txt")
        if (allocated(error)) return

        call pf_path_add_suffix("myfile", "_stat", got)
        call check(error, got == "myfile_stat", "no extension must compose without leaving a stray dot")
        if (allocated(error)) return

        call pf_path_add_suffix("a.tar.gz", "_stat", got)
        call check(error, got == "a.tar_stat.gz", "only the last extension is kept on the right of the suffix")
        if (allocated(error)) return

        call pf_path_add_suffix(".bashrc", "_stat", got)
        call check(error, got == ".bashrc_stat", "a hidden file has no extension, so the suffix goes at the end")
        if (allocated(error)) return

        call pf_path_add_suffix("/a/b/", "_stat", got)
        call check(error, got == "/a/b/_stat", "a trailing separator means an empty stem, lexically")
        if (allocated(error)) return

        ! The same recipe assembled by hand must reach the same place.
        call pf_split_path("/data/myfile.txt", dir, stem, ext)
        call pf_join_path(dir, stem//"_stat"//ext, got)
        call check(error, got == "/data/myfile_stat.txt", "split + join must reproduce what pf_path_add_suffix does")
    end subroutine test_add_suffix

    ! ================================================================================
    ! Module-wide rules
    ! ================================================================================

    !> Trailing blanks are trimmed from every text argument; leading blanks are preserved.
    !!
    !! **Hand-written rather than generated**: a blank-padded fixed-length actual has no CPython
    !! equivalent to consult. The `suffix` case is the one that bites -- a `character(len=16)`
    !! variable holding `"_stat"` would otherwise produce `myfile_stat<blanks>.txt` from a call
    !! site that looks correct.
    subroutine test_blank_handling(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got, base
        character(len=16) :: padded_suffix
        character(len=8) :: padded_parts(2)

        call pf_join_path("a   ", "b   ", got)
        call check(error, got == "a/b", "trailing blanks must be trimmed from a scalar component")
        if (allocated(error)) return

        padded_parts = [character(len=8) :: "a", "b"]
        call pf_join_path(padded_parts, got)
        call check(error, got == "a/b", "trailing blanks must be trimmed from every array component")
        if (allocated(error)) return

        call pf_join_path(" a", "b", got)
        call check(error, got == " a/b", "a LEADING blank is a legal filename character and must be preserved")
        if (allocated(error)) return

        padded_suffix = "_stat"
        call pf_path_add_suffix("myfile.txt", padded_suffix, got)
        call check(error, got == "myfile_stat.txt", "a blank-padded fixed-length suffix must not embed its padding")
        if (allocated(error)) return
        call check(error, index(got, " ") == 0, "no blank may survive from a padded suffix")
        if (allocated(error)) return

        call pf_basename("dir/name.txt   ", base)
        call check(error, base == "name.txt", "trailing blanks must be trimmed before a path is taken apart")
    end subroutine test_blank_handling

    !> Every procedure returning an allocatable allocates it on every path, zero-length when empty.
    !!
    !! Both halves are asserted separately: testing only `len(...) == 0` passes on an *unallocated*
    !! result, whose length is undefined, which is exactly the hazard this invariant closes.
    subroutine test_allocation_invariant(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got, dir, stem, ext
        character(len=4) :: none(0)

        call pf_dirname("cat.parquet", got)
        call check(error, allocated(got), "pf_dirname must allocate even when there is no directory")
        if (allocated(error)) return
        call check(error, len(got) == 0, "pf_dirname with no directory must be zero-length")
        if (allocated(error)) return

        call pf_path_ext("cat", got)
        call check(error, allocated(got), "pf_path_ext must allocate even when there is no extension")
        if (allocated(error)) return
        call check(error, len(got) == 0, "pf_path_ext with no extension must be zero-length")
        if (allocated(error)) return

        call pf_basename("/a/b/", got)
        call check(error, allocated(got), "pf_basename must allocate even when the basename is empty")
        if (allocated(error)) return
        call check(error, len(got) == 0, "pf_basename of a path ending in a separator must be zero-length")
        if (allocated(error)) return

        call pf_path_stem("/a/b/", got)
        call check(error, allocated(got), "pf_path_stem must allocate even when the stem is empty")
        if (allocated(error)) return
        call check(error, len(got) == 0, "pf_path_stem of a path ending in a separator must be zero-length")
        if (allocated(error)) return

        call pf_join_path(none, got)
        call check(error, allocated(got), "pf_join_path over a zero-size array must allocate its result")
        if (allocated(error)) return
        call check(error, len(got) == 0, "pf_join_path over a zero-size array must be zero-length")
        if (allocated(error)) return

        call pf_split_path("", dir, stem, ext)
        call check(error, allocated(dir) .and. allocated(stem), "pf_split_path must allocate every piece of an empty path")
        if (allocated(error)) return
        call check(error, allocated(ext), "pf_split_path must allocate ext for an empty path")
        if (allocated(error)) return
        call check(error, len(dir) + len(stem) + len(ext) == 0, "every piece of an empty path must be zero-length")
        if (allocated(error)) return

        call pf_path_add_suffix("", "", got)
        call check(error, allocated(got), "pf_path_add_suffix must allocate for an empty path and suffix")
        if (allocated(error)) return
        call check(error, len(got) == 0, "pf_path_add_suffix of two empty strings must be zero-length")
    end subroutine test_allocation_invariant

    !> `pf_join_path(dirname(p), basename(p))` reproduces `p` where Python's own composition does,
    !! and differs where it does not.
    !!
    !! The disagreement is information: `"/a/b/"` recomposes as `"/a/b/"` only because `dirname`
    !! drops the trailing separator and `join` puts one back for the empty basename, while
    !! `"a//b"` recomposes as `"a/b"` because `dirname` strips the duplicate. Pinning both stops
    !! someone "fixing" the second later.
    subroutine test_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: p, dir, base, got, want
        integer :: k

        do k = 1, pv_n_split
            p = trim(pv_split_path(k))
            call pf_dirname(p, dir)
            call pf_basename(p, base)
            call pf_join_path(dir, base, got)
            ! Not every path recomposes -- see below for the two that deliberately do not -- so the
            ! assertion is that the Fortran and the Python compositions agree, not that p returns.
            call python_recompose(k, want)
            call check(error, got == want, "dirname + basename + join must recompose ["// &
                       p//"] the way posixpath does: got ["//got//"] want ["//want//"]")
            if (allocated(error)) return
        end do

        call pf_dirname("a//b", dir)
        call pf_basename("a//b", base)
        call pf_join_path(dir, base, got)
        call check(error, got == "a/b", "a duplicate interior separator is NOT preserved by a recomposition")
        if (allocated(error)) return

        call pf_dirname("/data/run3/cat.parquet", dir)
        call pf_basename("/data/run3/cat.parquet", base)
        call pf_join_path(dir, base, got)
        call check(error, got == "/data/run3/cat.parquet", "an ordinary path must recompose to itself")
    end subroutine test_round_trip

    ! ================================================================================
    ! Local helpers
    ! ================================================================================

    !> Calls `pf_join_path` at the arity case `k` was recorded with.
    subroutine join_at_arity(k, got)
        integer, intent(in) :: k !! index into the generated join table.
        character(len=:), allocatable, intent(out) :: got !! the joined path.

        select case (pv_join_arity(k))
        case (2)
            call pf_join_path(trim(pv_join_p1(k)), trim(pv_join_p2(k)), got)
        case (3)
            call pf_join_path(trim(pv_join_p1(k)), trim(pv_join_p2(k)), trim(pv_join_p3(k)), got)
        case (4)
            call pf_join_path(trim(pv_join_p1(k)), trim(pv_join_p2(k)), trim(pv_join_p3(k)), &
                              trim(pv_join_p4(k)), got)
        case default
            call pf_join_path(trim(pv_join_p1(k)), trim(pv_join_p2(k)), trim(pv_join_p3(k)), &
                              trim(pv_join_p4(k)), trim(pv_join_p5(k)), got)
        end select
    end subroutine join_at_arity

    !> Builds the component array for join case `k`, sized to that case's arity.
    subroutine parts_of(k, parts)
        integer, intent(in) :: k !! index into the generated join table.
        character(len=:), allocatable, intent(out) :: parts(:) !! the case's components.
        integer :: n

        n = pv_join_arity(k)
        allocate (character(len=len(pv_join_p1)) :: parts(n))
        parts(1) = pv_join_p1(k)
        if (n >= 2) parts(2) = pv_join_p2(k)
        if (n >= 3) parts(3) = pv_join_p3(k)
        if (n >= 4) parts(4) = pv_join_p4(k)
        if (n >= 5) parts(5) = pv_join_p5(k)
    end subroutine parts_of

    !> What `posixpath.join(posixpath.dirname(p), posixpath.basename(p))` gives for split case `k`,
    !! derived from the generated table rather than from this module's own procedures.
    !!
    !! A subroutine rather than a function returning `character(len=:), allocatable`: that shape is
    !! banned project-wide because gfortran's hidden length variable is not reliably thread-local,
    !! and this suite's tests run concurrently.
    subroutine python_recompose(k, res)
        integer, intent(in) :: k !! index into the generated split table.
        character(len=:), allocatable, intent(out) :: res !! the recomposed path.
        character(len=:), allocatable :: dir, base

        dir = trim(pv_split_dir(k))
        base = trim(pv_split_base(k))
        if (len(base) > 0) then
            if (base(1:1) == "/") then
                res = base
                return
            end if
        end if
        if (len(dir) == 0) then
            res = base
        else if (dir(len(dir):len(dir)) == "/") then
            res = dir//base
        else
            res = dir//"/"//base
        end if
    end subroutine python_recompose

    !> Renders a small non-negative integer for a check message.
    function itoa(n) result(res)
        integer, intent(in) :: n !! the value to render.
        character(len=12) :: res !! the rendered value, blank-padded.

        write (res, '(i0)') n
    end function itoa

    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.
end module test_utils ! GCOVR_EXCL_LINE
