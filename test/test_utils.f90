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
!! many asterisks a REJECTED `fmt` produces. Both are measured differences between compilers --
!! `(g0)` renders `3.14_real64` with a different number of digits, and a type-mismatched edit
!! descriptor is rejected through `iostat` by some runtimes and turned into asterisks by others.
!! Real rendering is asserted with an explicit `fmt` or by reading the text back.
!!
!! **The asterisks from a too-narrow field ARE counted, and that is not a contradiction.** There
!! the format is valid and Fortran writes the asterisks itself, with the standard fixing the count
!! at the field width -- so `'(i2)'` on `12345` is `"**"` on every conforming compiler. Only the
!! rejected-`fmt` path, which goes through the module's own `bad_format_result`, has a length that
!! must not be pinned.
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
    use, intrinsic :: ieee_arithmetic, only: ieee_support_flag, ieee_get_flag, &
        ieee_set_flag, ieee_underflow, ieee_divide_by_zero, ieee_invalid, &
        ieee_is_nan, ieee_is_finite
    implicit none
    private

    public :: collect_tests_utils

contains

    !> Registers every test in this suite.
    subroutine collect_tests_utils(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("safe_div is plain division wherever the denominator is not zero", &
                         test_safe_div_matches_division), &
            new_unittest("safe_div gives the IEEE value a zero denominator would have", &
                         test_safe_div_zero_denominator), &
            new_unittest("safe_div raises no IEEE flag, which is the point of it", &
                         test_safe_div_raises_no_flag), &
            new_unittest("every wrap lands inside its advertised half-open range", test_wrap_ranges), &
            new_unittest("the wrap edge guard fires on the inputs that need it", test_wrap_edge_guard), &
            new_unittest("wrapping crosses several turns and is idempotent", test_wrap_turns), &
            new_unittest("deg2rad and rad2deg agree with the definition", test_angle_conversions), &
            new_unittest("cross_product is the right-handed cross product", test_cross_product), &
            new_unittest("case folding covers every ASCII letter", test_fold_ascii), &
            new_unittest("case folding leaves every other byte alone", test_fold_leaves_others), &
            new_unittest("case folding in place preserves length", test_fold_inplace), &
            new_unittest("case folding does not trim, unlike the path family", test_fold_does_not_trim), &
            new_unittest("to_str renders integers exactly", test_to_str_integers), &
            new_unittest("to_str min_width grows rather than refusing", test_to_str_min_width), &
            new_unittest("to_str pads after the sign", test_to_str_sign), &
            new_unittest("to_str renders reals and logicals", test_to_str_reals_logicals), &
            new_unittest("to_str fmt renders and min_width then pads", test_to_str_fmt), &
            new_unittest("to_str returns asterisks, and for three different reasons", test_to_str_bad_fmt), &
            new_unittest("from_str refuses every strictness vector", test_from_str_strictness), &
            new_unittest("from_str reads integers, and refuses what will not fit", test_from_str_integers), &
            new_unittest("from_str reads every real literal shape and no other", test_from_str_reals), &
            new_unittest("from_str refuses a real the kind cannot hold, but rounds one it can", &
                test_from_str_real_range), &
            new_unittest("from_str reads exactly six logical spellings", test_from_str_logical), &
            new_unittest("from_str inverts to_str", test_from_str_round_trip), &
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
    ! Total arithmetic
    ! ================================================================================

    !> Wherever the denominator is non-zero, `pf_safe_div` must be `a/b` and nothing else.
    !!
    !! Asserted as bit equality rather than within a tolerance: the contract is that a program's
    !! outputs are unchanged by adopting it, which a tolerance would not check.
    subroutine test_safe_div_matches_division(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real64) :: a(6), b(6)
        real(real32) :: a4(6), b4(6)
        integer :: i, nbad

        ! The fifth pair spans the exponent range deliberately, but its QUOTIENT must stay finite:
        ! an overflowing quotient is raised by `a(i)/b(i)` on the assertion's own side as much as
        ! inside the procedure, and nagfor unmasks the overflow trap by default, so the pair that
        ! overflows aborts the runner instead of testing anything.
        a = [1.0_real64, -1.0_real64, 3.0_real64, -7.5_real64, 1.0e300_real64, 0.0_real64]
        b = [3.0_real64, 7.0_real64, -11.0_real64, 0.25_real64, 1.0e-7_real64, 5.0_real64]
        a4 = [1.0_real32, -1.0_real32, 3.0_real32, -7.5_real32, 1.0e30_real32, 0.0_real32]
        b4 = [3.0_real32, 7.0_real32, -11.0_real32, 0.25_real32, 1.0e-7_real32, 5.0_real32]

        nbad = 0
        do i = 1, size(a)
            if (pf_safe_div(a(i), b(i)) /= a(i) / b(i)) nbad = nbad + 1
        end do
        call check(error, nbad, 0, "pf_safe_div disagrees with a/b on a real64 non-zero denominator")
        if (allocated(error)) return

        nbad = 0
        do i = 1, size(a4)
            if (pf_safe_div(a4(i), b4(i)) /= a4(i) / b4(i)) nbad = nbad + 1
        end do
        call check(error, nbad, 0, "pf_safe_div disagrees with a/b on a real32 non-zero denominator")
        if (allocated(error)) return

        ! Elemental, so a whole array goes through one call and must agree element for element.
        call check(error, all(pf_safe_div(a, b) == a / b), "the elemental real64 form disagrees with a/b")
        if (allocated(error)) return
        call check(error, all(pf_safe_div(a4, b4) == a4 / b4), "the elemental real32 form disagrees with a/b")
    end subroutine test_safe_div_matches_division

    !> A zero denominator gives the value the division would have given, by construction.
    subroutine test_safe_div_zero_denominator(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real64) :: zero, res
        real(real32) :: zero4, res4

        ! Both zeros are assembled at run time. A literal zero lets the compiler fold the whole
        ! call away and leave the test asserting on a constant it computed itself, which would
        ! pass whatever the procedure does.
        zero = real(command_argument_count(), real64) * 0.0_real64
        zero4 = real(command_argument_count(), real32) * 0.0_real32

        res = pf_safe_div(1.0_real64, zero)
        call check(error, res > 0.0_real64 .and. .not. ieee_is_finite(res), &
            "pf_safe_div(+x, 0) must be +Infinity")
        if (allocated(error)) return
        res = pf_safe_div(-1.0_real64, zero)
        call check(error, res < 0.0_real64 .and. .not. ieee_is_finite(res), &
            "pf_safe_div(-x, 0) must be -Infinity")
        if (allocated(error)) return
        res = pf_safe_div(zero, zero)
        call check(error, ieee_is_nan(res), "pf_safe_div(0, 0) must be NaN")
        if (allocated(error)) return

        res4 = pf_safe_div(1.0_real32, zero4)
        call check(error, res4 > 0.0_real32 .and. .not. ieee_is_finite(res4), &
            "the real32 form must give +Infinity too")
        if (allocated(error)) return
        res4 = pf_safe_div(zero4, zero4)
        call check(error, ieee_is_nan(res4), "the real32 form must give NaN for 0/0 too")
        if (allocated(error)) return

        ! Deliberately not zero: the whole point is that an unsatisfiable request stays visibly
        ! unsatisfiable instead of becoming an unremarkable number.
        call check(error, .not. (pf_safe_div(1.0_real64, zero) == 0.0_real64), &
            "pf_safe_div must not turn a division by zero into zero")
    end subroutine test_safe_div_zero_denominator

    !> The property the procedure exists for: dividing by a legitimate zero must leave the IEEE
    !! exception flags alone, so that a raised flag still means something worth investigating.
    subroutine test_safe_div_raises_no_flag(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        logical :: flag_div, flag_inv, saved_div, saved_inv, supported
        real(real64) :: zero, res(3)

        zero = real(command_argument_count(), real64) * 0.0_real64
        supported = ieee_support_flag(ieee_divide_by_zero, zero) .and. ieee_support_flag(ieee_invalid, zero)

        ! The suite runs concurrently, so the flags are saved and put back: leaving one raised
        ! would surface at the end of the whole run and read as a defect in whatever ran last.
        saved_div = .false.
        saved_inv = .false.
        if (supported) then
            call ieee_get_flag(ieee_divide_by_zero, saved_div)
            call ieee_get_flag(ieee_invalid, saved_inv)
            call ieee_set_flag(ieee_divide_by_zero, .false.)
            call ieee_set_flag(ieee_invalid, .false.)
        end if

        ! Kept as three separate results and never combined: adding +Infinity to -Infinity raises
        ! the invalid-operation flag by itself, so a test that summed them would measure its own
        ! arithmetic instead of pf_safe_div's.
        res(1) = pf_safe_div(1.0_real64, zero)
        res(2) = pf_safe_div(-1.0_real64, zero)
        res(3) = pf_safe_div(zero, zero)

        flag_div = .false.
        flag_inv = .false.
        if (supported) then
            call ieee_get_flag(ieee_divide_by_zero, flag_div)
            call ieee_get_flag(ieee_invalid, flag_inv)
            call ieee_set_flag(ieee_divide_by_zero, saved_div)
            call ieee_set_flag(ieee_invalid, saved_inv)
        end if

        call check(error, .not. flag_div, "pf_safe_div raised the IEEE divide-by-zero flag")
        if (allocated(error)) return
        call check(error, .not. flag_inv, "pf_safe_div raised the IEEE invalid-operation flag")
        if (allocated(error)) return
        ! Reading the results keeps the three calls above from being optimised away entirely.
        call check(error, ieee_is_nan(res(3)), "the 0/0 result was not the NaN the flag test relies on")
    end subroutine test_safe_div_raises_no_flag

    ! ================================================================================
    ! Angles
    ! ================================================================================

    !> Every wrap lands strictly inside its advertised half-open range, over a sweep crossing
    !! several turns in both directions, in both kinds.
    subroutine test_wrap_ranges(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real64), parameter :: PI = 3.141592653589793238462643_real64
        real(real32), parameter :: PI4 = real(PI, real32)
        real(real64) :: x
        real(real32) :: x4
        integer :: i, nbad, nchecked

        nbad = 0
        nchecked = 0
        do i = -2000, 2000
            x = real(i, real64) * 0.37_real64 - 400.0_real64
            x4 = real(x, real32)
            nchecked = nchecked + 1
            if (pf_wrap_deg(x) < 0.0_real64 .or. pf_wrap_deg(x) >= 360.0_real64) nbad = nbad + 1
            if (pf_wrap_180(x) < -180.0_real64 .or. pf_wrap_180(x) >= 180.0_real64) nbad = nbad + 1
            if (pf_wrap_rad(x) < 0.0_real64 .or. pf_wrap_rad(x) >= 2.0_real64 * PI) nbad = nbad + 1
            if (pf_wrap_pi(x) < -PI .or. pf_wrap_pi(x) >= PI) nbad = nbad + 1
            if (pf_wrap_deg(x4) < 0.0_real32 .or. pf_wrap_deg(x4) >= 360.0_real32) nbad = nbad + 1
            if (pf_wrap_180(x4) < -180.0_real32 .or. pf_wrap_180(x4) >= 180.0_real32) nbad = nbad + 1
            if (pf_wrap_rad(x4) < 0.0_real32 .or. pf_wrap_rad(x4) >= 2.0_real32 * PI4) nbad = nbad + 1
            if (pf_wrap_pi(x4) < -PI4 .or. pf_wrap_pi(x4) >= PI4) nbad = nbad + 1
        end do

        ! The loop is the assertion, so its having run is asserted too: a sweep that never
        ! executed would report success while checking nothing.
        call check(error, nchecked, 4001, "the wrap sweep did not run -- the range assertions asserted nothing")
        if (allocated(error)) return
        call check(error, nbad, 0, "a wrap returned a value outside its advertised half-open range")
    end subroutine test_wrap_ranges

    !> The closing comparison in each wrap is load-bearing, and these are the inputs that prove it.
    !!
    !! Each value below makes `modulo` return the divisor itself, measured on gfortran 15.2 -- so
    !! without the guard the result is exactly the excluded upper end of the range. Deleting any of
    !! the `if (res >= ...)` lines in `parquet_utils` makes this test fail, which is what makes it a
    !! negative control rather than a restatement of the code.
    subroutine test_wrap_edge_guard(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real64), parameter :: PI = 3.141592653589793238462643_real64
        real(real64) :: x

        call check(error, pf_wrap_deg(-1.0e-30_real64), 0.0_real64, &
            "wrap_deg of a tiny negative must be 0, not 360", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_wrap_deg(-1.0e-30_real32), 0.0_real32, &
            "the real32 wrap_deg must be 0 there too", thr=0.0_real32)
        if (allocated(error)) return
        call check(error, pf_wrap_rad(-1.0e-30_real64) < 2.0_real64 * PI, &
            "wrap_rad of a tiny negative must stay below 2*pi")
        if (allocated(error)) return

        ! One ulp below -180 is what drives the symmetric form to exactly +180.
        x = -180.0_real64 - epsilon(1.0_real64) * 180.0_real64
        call check(error, pf_wrap_180(x) < 180.0_real64, &
            "wrap_180 one ulp below -180 must stay below +180")
        if (allocated(error)) return
        call check(error, pf_wrap_180(-180.0_real32 - epsilon(1.0_real32) * 180.0_real32) < 180.0_real32, &
            "the real32 wrap_180 must stay below +180 there too")
        if (allocated(error)) return
        call check(error, pf_wrap_pi(-PI - epsilon(1.0_real64) * PI) < PI, &
            "wrap_pi one ulp below -pi must stay below +pi")
    end subroutine test_wrap_edge_guard

    !> Wrapping is correct across several turns -- which the single conditional it replaces is not
    !! -- and applying it twice changes nothing.
    subroutine test_wrap_turns(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real64), parameter :: PI = 3.141592653589793238462643_real64

        ! The case `if (x < 0.0) x = x + 360.0` gets wrong: one addition leaves -400 at -40.
        call check(error, pf_wrap_deg(-400.0_real64), 320.0_real64, &
            "wrap_deg(-400) must be 320, which a single conditional addition would not give", thr=1.0e-12_real64)
        if (allocated(error)) return
        call check(error, pf_wrap_deg(760.0_real64), 40.0_real64, "wrap_deg(760) must be 40", thr=1.0e-12_real64)
        if (allocated(error)) return
        call check(error, pf_wrap_deg(360.0_real64), 0.0_real64, "wrap_deg(360) must be 0", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_wrap_180(540.0_real64), -180.0_real64, "wrap_180(540) must be -180", thr=1.0e-12_real64)
        if (allocated(error)) return
        call check(error, pf_wrap_180(180.0_real64), -180.0_real64, &
            "wrap_180(180) must be -180 -- the range is half-open at the top", thr=0.0_real64)
        if (allocated(error)) return

        ! Idempotence, which is what the half-open convention buys.
        call check(error, pf_wrap_180(pf_wrap_180(180.0_real64)), pf_wrap_180(180.0_real64), &
            "wrapping an already-wrapped angle changed it", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_wrap_deg(pf_wrap_deg(-400.0_real64)), pf_wrap_deg(-400.0_real64), &
            "wrap_deg is not idempotent", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_wrap_pi(pf_wrap_pi(3.0_real64 * PI)), pf_wrap_pi(3.0_real64 * PI), &
            "wrap_pi is not idempotent", thr=0.0_real64)
    end subroutine test_wrap_turns

    !> The two conversions against the definition, and against each other.
    subroutine test_angle_conversions(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real64), parameter :: PI = 3.141592653589793238462643_real64

        call check(error, pf_deg2rad(180.0_real64), PI, "deg2rad(180) must be pi", thr=1.0e-15_real64)
        if (allocated(error)) return
        call check(error, pf_deg2rad(-90.0_real64), -0.5_real64 * PI, "deg2rad(-90) must be -pi/2", thr=1.0e-15_real64)
        if (allocated(error)) return
        call check(error, pf_rad2deg(PI), 180.0_real64, "rad2deg(pi) must be 180", thr=1.0e-13_real64)
        if (allocated(error)) return
        call check(error, pf_deg2rad(180.0_real32), real(PI, real32), &
            "the real32 deg2rad must give pi", thr=1.0e-6_real32)
        if (allocated(error)) return
        call check(error, pf_rad2deg(real(PI, real32)), 180.0_real32, &
            "the real32 rad2deg must give 180", thr=1.0e-4_real32)
        if (allocated(error)) return
        ! A round trip within a tolerance, never bit-for-bit: pi/180 and 180/pi are each rounded,
        ! so the two multiplications compose to a factor a rounding away from one.
        call check(error, pf_rad2deg(pf_deg2rad(37.25_real64)), 37.25_real64, &
            "deg2rad then rad2deg must return the angle", thr=1.0e-12_real64)
    end subroutine test_angle_conversions

    ! ================================================================================
    ! Vectors
    ! ================================================================================

    !> The cross product against hand-computed values, its two defining identities, and the
    !! right-handed orientation of the axis triple.
    subroutine test_cross_product(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real64) :: ex(3), ey(3), ez(3), a(3), b(3), c(3)
        real(real32) :: a4(3), b4(3)

        ex = [1.0_real64, 0.0_real64, 0.0_real64]
        ey = [0.0_real64, 1.0_real64, 0.0_real64]
        ez = [0.0_real64, 0.0_real64, 1.0_real64]

        call check(error, all(pf_cross_product(ex, ey) == ez), "x cross y must be z, not -z")
        if (allocated(error)) return
        call check(error, all(pf_cross_product(ey, ez) == ex), "y cross z must be x")
        if (allocated(error)) return
        call check(error, all(pf_cross_product(ez, ex) == ey), "z cross x must be y")
        if (allocated(error)) return

        a = [1.0_real64, 2.0_real64, 3.0_real64]
        b = [4.0_real64, 5.0_real64, 6.0_real64]
        c = pf_cross_product(a, b)
        call check(error, all(c == [-3.0_real64, 6.0_real64, -3.0_real64]), &
            "the cross product of (1,2,3) and (4,5,6) must be (-3,6,-3)")
        if (allocated(error)) return
        ! The two identities that pin the orientation and the plane at once.
        call check(error, all(pf_cross_product(b, a) == -c), "the cross product must be anticommutative")
        if (allocated(error)) return
        call check(error, abs(dot_product(c, a)) < 1.0e-12_real64 .and. abs(dot_product(c, b)) < 1.0e-12_real64, &
            "the cross product must be orthogonal to both operands")
        if (allocated(error)) return
        call check(error, all(pf_cross_product(a, a) == 0.0_real64), "a vector crossed with itself must be zero")
        if (allocated(error)) return

        a4 = real(a, real32)
        b4 = real(b, real32)
        call check(error, all(pf_cross_product(a4, b4) == [-3.0_real32, 6.0_real32, -3.0_real32]), &
            "the real32 form must give the same vector")
    end subroutine test_cross_product

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

    !> The COPY form does not trim either, which is the half nothing else pins.
    !!
    !! The module trims a trailing blank from every path argument and from `suffix`, and
    !! `pf_to_lower`/`pf_to_upper` are the deliberate exception: the copy is documented as exactly
    !! `len(s)` long. `doc/pages/utilities/utils.md` claimed the trimming rule held for the whole
    !! module until 2026-09-02, and it was wrong in exactly this direction -- so the exception is
    !! now stated on the page and pinned here.
    !!
    !! **Every assertion here is on `len` or on a fixed-width section, never on `==` against a
    !! blank-tailed literal.** Fortran blank-pads the shorter operand of a character comparison, so
    !! `got == "ab   "` is `.true.` for `got == "ab"` and would assert nothing at all about the
    !! very blanks this test exists for.
    !!
    !! The `pf_join_path` call at the end is the **negative control**: it feeds the same padded
    !! text to a procedure that does trim. Without it these assertions would pass just as happily
    !! against a module in which nothing trims anywhere, which is the opposite of the property.
    !!
    !! The in-place form needs nothing here -- `test_fold_inplace` above already asserts that it
    !! leaves `text(4:10)` blank, and a fixed-length variable cannot change length in any case.
    subroutine test_fold_does_not_trim(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: got

        call pf_to_lower("AB   ", got)
        call check(error, len(got) == 5, "pf_to_lower's copy must be len(s), trailing blanks included")
        if (allocated(error)) return
        call check(error, got(1:2) == "ab", "pf_to_lower's copy must fold the value")
        if (allocated(error)) return
        call check(error, got(3:5) == "   ", "pf_to_lower must leave a trailing blank exactly where it was")
        if (allocated(error)) return

        call pf_to_upper("ab   ", got)
        call check(error, len(got) == 5, "pf_to_upper's copy must be len(s), trailing blanks included")
        if (allocated(error)) return
        call check(error, got(1:2) == "AB", "pf_to_upper's copy must fold the value")
        if (allocated(error)) return
        call check(error, got(3:5) == "   ", "pf_to_upper must leave a trailing blank exactly where it was")
        if (allocated(error)) return

        ! The negative control: the same padded text through a procedure that DOES trim.
        call pf_join_path("AB   ", "b", got)
        call check(error, len(got) == 4, "a path component must be trimmed (control: the folds are the exception)")
        if (allocated(error)) return
        call check(error, got == "AB/b", "the control's value must be AB/b, with no blank surviving")
    end subroutine test_fold_does_not_trim

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
    !!
    !! **The three `verify` assertions below bite only under flang**, and are not decoration
    !! elsewhere for that reason: gfortran, ifx and nagfor leave the buffer EMPTY when they reject a
    !! format, so they satisfy the contract whatever rule `rendered_ok` (`src/parquet_utils.f90`)
    !! uses, while flang reports the rejection and still leaves partial text -- `1`, and
    !! `377600000000000000000` for the `real64` form -- which was returned as if it were a
    !! rendering. See `feature_risks.md` Risk-187 for why this function has no single build in
    !! which both of its rules are live.
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
        if (allocated(error)) return

        ! Cause 2, and by far the commonest: a PERFECTLY VALID format whose field is too narrow.
        ! Fortran fills the field with asterisks itself and reports success. Unlike the
        ! rejected-fmt case, the count here IS assertable: the standard fixes it at the field
        ! width, so every conforming compiler gives the same text.
        !
        ! **This pair only fails under `--profile debug`, so a plain `fpm test` cannot see it.**
        ! ifx's `-check output_conversion` (inside `-check all`) reports `iostat = 63` for the
        ! overflow the standard calls success, while still rendering the asterisks; taking that
        ! iostat at face value returned `bad_format_result`'s three asterisks instead of the
        ! field's two, i.e. a diagnostic build changing what the library returns. `rendered_ok`
        ! in src/parquet_utils.f90 is what keeps the two builds agreeing, and these two lines are
        ! its regression test -- run them with `--profile debug` or they assert nothing new.
        call pf_to_str(12345_int32, got, fmt='(i2)')
        call check(error, got == "**", "a field too narrow must give exactly its own width in asterisks")
        if (allocated(error)) return
        call pf_to_str(1234.5_real64, got, fmt='(f4.2)')
        call check(error, got == "****", "a narrow real field must give exactly its own width in asterisks")
        if (allocated(error)) return

        ! ...and min_width then pads THAT, which is the two documented rules composing into a
        ! result no reader would predict.
        call pf_to_str(12345_int32, got, fmt='(i2)', min_width=6)
        call check(error, got == "0000**", "min_width must pad the asterisks a narrow field produced")
        if (allocated(error)) return

        ! Cause 3: the 512-character buffer each value is rendered through. The COUNT is not
        ! asserted -- this path goes through bad_format_result, whose length is explicitly not
        ! part of the contract.
        call pf_to_str(1.0_real64, got, fmt='(f600.2)')
        call check(error, verify(got, "*") == 0, "a rendering past the 512-character buffer must give asterisks")
        if (allocated(error)) return

        ! The negative control for cause 3, and the half that makes it a CEILING rather than "a
        ! long format always fails". It is also what would catch someone lowering the buffer, which
        ! otherwise turns ordinary numbers into asterisks with nothing to report it.
        call pf_to_str(1.0_real64, got, fmt='(f400.2)')
        call check(error, len(got) == 400, "a 400-character rendering must fit the buffer and render")
        if (allocated(error)) return
        call check(error, got(397:400) == "1.00", "the 400-character rendering must be the number, right-justified")
    end subroutine test_to_str_bad_fmt

    ! ================================================================================
    ! Text to value
    ! ================================================================================

    !> The seven vectors the design named, at every specific that could plausibly accept them.
    !!
    !! `"5 6"` is the one that matters and the reason this procedure exists at all: a
    !! list-directed `read` accepts it with `iostat == 0` and yields 5, so an ID with a stray
    !! space silently becomes a different, plausible ID. Substituting a `read` back in is the
    !! mutation this test exists to catch (feature_risks.md, R-g), and it is the ONLY one of the
    !! seven a `read` would get wrong -- which is exactly why the vector list is not shorter.
    subroutine test_from_str_strictness(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        integer(int32) :: n32
        integer(int64) :: n64
        real(real64) :: x
        logical :: ok

        call pf_from_str("5 6", n32, ok)
        call check(error, .not. ok, "an integer with an embedded blank is refused, not read as 5")
        if (allocated(error)) return
        call pf_from_str("5 6", x, ok)
        call check(error, .not. ok, "and a real with one is refused too")
        if (allocated(error)) return
        call pf_from_str("3.9", n64, ok)
        call check(error, .not. ok, "a real is not an integer")
        if (allocated(error)) return
        call pf_from_str("+", n32, ok)
        call check(error, .not. ok, "a sign with no digits is not a number")
        if (allocated(error)) return
        call pf_from_str("", n32, ok)
        call check(error, .not. ok, "empty text is not a number")
        if (allocated(error)) return
        call pf_from_str("   ", x, ok)
        call check(error, .not. ok, "and neither is text that is all blanks")
        if (allocated(error)) return
        call pf_from_str(" 12 ", n32, ok)
        call check(error, ok, "leading and trailing blanks are trimmed")
        if (allocated(error)) return
        call check(error, n32 == 12, "and what is left reads as its own value")
        if (allocated(error)) return
        call pf_from_str("1e3", n64, ok)
        call check(error, .not. ok, "an exponent form is not an integer")
        if (allocated(error)) return
        call pf_from_str("1e3", x, ok)
        call check(error, ok .and. x == 1000.0_real64, "but it is a real")
        if (allocated(error)) return
        call pf_from_str("0x10", n32, ok)
        call check(error, .not. ok, "hexadecimal is not accepted as an integer")
        if (allocated(error)) return
        call pf_from_str("0x10", x, ok)
        call check(error, .not. ok, "nor as a real")
    end subroutine test_from_str_strictness

    !> Signs, leading zeros, and the range of each integer kind.
    !!
    !! The range half is per-KIND and that is the point: `"2147483648"` is a perfectly good
    !! number that an `int32` cannot hold, so it must be refused there and read here. A parser
    !! that let the `read` wrap would return a plausible negative instead.
    subroutine test_from_str_integers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        integer(int32) :: n32
        integer(int64) :: n64
        logical :: ok

        call pf_from_str("-7", n32, ok)
        call check(error, ok .and. n32 == -7, "a negative sign is read")
        if (allocated(error)) return
        call pf_from_str("+7", n32, ok)
        call check(error, ok .and. n32 == 7, "an explicit positive sign is read")
        if (allocated(error)) return
        call pf_from_str("007", n32, ok)
        call check(error, ok .and. n32 == 7, "leading zeros are read")
        if (allocated(error)) return
        call pf_from_str("12abc", n32, ok)
        call check(error, .not. ok, "trailing rubbish is refused, not ignored")
        if (allocated(error)) return
        call pf_from_str("2147483647", n32, ok)
        call check(error, ok .and. n32 == 2147483647_int32, "the largest int32 reads")
        if (allocated(error)) return
        call pf_from_str("2147483648", n32, ok)
        call check(error, .not. ok, "one past it does not")
        if (allocated(error)) return
        call pf_from_str("2147483648", n64, ok)
        call check(error, ok .and. n64 == 2147483648_int64, "and reads perfectly well as an int64")
        if (allocated(error)) return
        call pf_from_str("9223372036854775807", n64, ok)
        call check(error, ok .and. n64 == huge(0_int64), "the largest int64 reads")
        if (allocated(error)) return
        call pf_from_str("9223372036854775808", n64, ok)
        call check(error, .not. ok, "one past it does not")
    end subroutine test_from_str_integers

    !> Every shape of the documented real grammar, and the shapes just outside it.
    subroutine test_from_str_reals(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real64) :: x
        logical :: ok

        call pf_from_str("12", x, ok)
        call check(error, ok .and. x == 12.0_real64, "an integer with no point reads as a real")
        if (allocated(error)) return
        call pf_from_str("12.", x, ok)
        call check(error, ok .and. x == 12.0_real64, "a trailing point reads")
        if (allocated(error)) return
        ! "0." is not just another trailing-point case: it is what nagfor's own (g0) renders
        ! 0.0_real64 as, so %format_column emits it and %parse_column must read it back. Both
        ! forms were refused by nagfor at -O2 and above until significand_ok stopped forming an
        ! empty `s(dot+1:)` -- see that function's comment. Only a release build could see it.
        call pf_from_str("0.", x, ok)
        call check(error, ok .and. x == 0.0_real64, "a trailing point on zero reads, as (g0) writes it")
        if (allocated(error)) return
        call pf_from_str(".", x, ok)
        call check(error, .not. ok, "a bare point is not a number")
        if (allocated(error)) return
        call pf_from_str(".5", x, ok)
        call check(error, ok .and. x == 0.5_real64, "a leading point reads")
        if (allocated(error)) return
        call pf_from_str("-.5", x, ok)
        call check(error, ok .and. x == -0.5_real64, "signed, so does it")
        if (allocated(error)) return
        call pf_from_str("1d3", x, ok)
        call check(error, ok .and. x == 1000.0_real64, "a d exponent reads")
        if (allocated(error)) return
        call pf_from_str("1E-3", x, ok)
        call check(error, ok .and. abs(x - 0.001_real64) < 1.0e-15_real64, "a signed E exponent reads")
        if (allocated(error)) return
        call pf_from_str(".", x, ok)
        call check(error, .not. ok, "a bare point is not a number")
        if (allocated(error)) return
        call pf_from_str("1.2.3", x, ok)
        call check(error, .not. ok, "two points are not a number")
        if (allocated(error)) return
        call pf_from_str("1e", x, ok)
        call check(error, .not. ok, "an exponent letter with no digits is not a number")
        if (allocated(error)) return
        call pf_from_str("1e+", x, ok)
        call check(error, .not. ok, "nor one with only a sign")
        if (allocated(error)) return
        call pf_from_str("e5", x, ok)
        call check(error, .not. ok, "nor an exponent with no significand")
        if (allocated(error)) return
        call pf_from_str("1q3", x, ok)
        call check(error, .not. ok, "q is not one of the accepted exponent letters")
        if (allocated(error)) return
        call pf_from_str("1.0_real64", x, ok)
        call check(error, .not. ok, "a Fortran kind suffix is not accepted")
        if (allocated(error)) return
        ! The two spellings the design refuses on purpose: neither is a Fortran literal, and text
        ! reading `nan` in a data column is far more often a missing-value placeholder than a
        ! deliberate NaN -- under %parse_column's invalid="null" that row becomes Null, which is
        ! what the caller meant.
        call pf_from_str("nan", x, ok)
        call check(error, .not. ok, "nan is refused")
        if (allocated(error)) return
        call pf_from_str("inf", x, ok)
        call check(error, .not. ok, "and so is inf")
    end subroutine test_from_str_reals

    !> Overflow is refused per KIND; rounding is not overflow.
    !!
    !! The two compilers in this project's fleet report the overflow differently -- nagfor 7.2
    !! through `iostat` with the variable untouched, gfortran 15.2 with `iostat == 0` and an
    !! Infinity -- so this test is what pins the CONTRACT to one answer across both. Dropping
    !! either half of `pf_from_str`'s check leaves it passing on one compiler and failing on the
    !! other.
    subroutine test_from_str_real_range(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real32) :: x32
        real(real64) :: x64
        logical :: ok, has_flag, saved

        call pf_from_str("1e300", x64, ok)
        call check(error, ok, "1e300 is an ordinary real64")
        if (allocated(error)) return
        call pf_from_str("1e300", x32, ok)
        call check(error, .not. ok, "and is refused for a real32, which cannot hold it")
        if (allocated(error)) return
        call pf_from_str("-1e300", x32, ok)
        call check(error, .not. ok, "the negative overflow is refused too")
        if (allocated(error)) return
        call pf_from_str("1e400", x64, ok)
        call check(error, .not. ok, "and 1e400 is refused even for a real64")
        if (allocated(error)) return
        call pf_from_str("0.1", x32, ok)
        call check(error, ok .and. abs(x32 - 0.1_real32) < 1.0e-7_real32, &
            "a value that merely rounds is read, not refused")
        if (allocated(error)) return
        ! An underflow to zero is rounding too, and asserting it needs the IEEE flag saved and
        ! restored: the fixture raises IEEE_UNDERFLOW by construction under nagfor, which would
        ! otherwise surface as an unexplained warning at the end of the whole run and read as a
        ! defect in whatever ran last (CLAUDE.md's own rule for a deliberately extreme fixture).
        has_flag = ieee_support_flag(ieee_underflow, x32)
        saved = .false.
        if (has_flag) then
            call ieee_get_flag(ieee_underflow, saved)
            call ieee_set_flag(ieee_underflow, .false.)
        end if
        call pf_from_str("1e-300", x32, ok)
        if (has_flag) call ieee_set_flag(ieee_underflow, saved)
        call check(error, ok .and. x32 == 0.0_real32, "an underflow rounds to zero rather than being refused")
    end subroutine test_from_str_real_range

    !> The six accepted spellings, and the plausible ones that are not accepted.
    subroutine test_from_str_logical(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        logical :: v, ok

        call pf_from_str("true", v, ok)
        call check(error, ok .and. v, "true reads")
        if (allocated(error)) return
        call pf_from_str("FALSE", v, ok)
        call check(error, ok .and. .not. v, "FALSE reads, case-insensitively")
        if (allocated(error)) return
        call pf_from_str("T", v, ok)
        call check(error, ok .and. v, "T reads")
        if (allocated(error)) return
        call pf_from_str("f", v, ok)
        call check(error, ok .and. .not. v, "f reads")
        if (allocated(error)) return
        call pf_from_str("1", v, ok)
        call check(error, ok .and. v, "1 reads")
        if (allocated(error)) return
        call pf_from_str("0", v, ok)
        call check(error, ok .and. .not. v, "0 reads")
        if (allocated(error)) return
        ! Refused deliberately, and named in the interface's own doc-comment: guessing what "yes"
        ! meant is the class of silent decision this module has none of.
        call pf_from_str("yes", v, ok)
        call check(error, .not. ok, "yes is not accepted")
        if (allocated(error)) return
        call pf_from_str(".true.", v, ok)
        call check(error, .not. ok, "nor is Fortran's own .true. spelling")
        if (allocated(error)) return
        call pf_from_str("truex", v, ok)
        call check(error, .not. ok, "nor a prefix with trailing rubbish")
        if (allocated(error)) return
        call pf_from_str("2", v, ok)
        call check(error, .not. ok, "nor any digit but 1 and 0")
    end subroutine test_from_str_logical

    !> `pf_to_str` then `pf_from_str` returns the value it started with.
    !!
    !! The real half uses values that are exact in binary, so it asserts equality rather than a
    !! tolerance -- but it must not assert the TEXT, which `pf_to_str`'s own contract says is
    !! compiler-dependent for a real. The round trip is the portable oracle.
    subroutine test_from_str_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: text
        integer(int64) :: back_n
        integer(int64), parameter :: ns(4) = [-huge(0_int64), -1_int64, 0_int64, huge(0_int64)]
        real(real64) :: back_x
        real(real64), parameter :: xs(4) = [0.0_real64, -0.5_real64, 3.25_real64, 1.0e10_real64]
        logical :: b, back_b, ok
        integer :: k

        do k = 1, 4
            call pf_to_str(ns(k), text)
            call pf_from_str(text, back_n, ok)
            call check(error, ok .and. back_n == ns(k), "an int64 survives to_str then from_str")
            if (allocated(error)) return
        end do
        do k = 1, 4
            call pf_to_str(xs(k), text)
            call pf_from_str(text, back_x, ok)
            call check(error, ok .and. back_x == xs(k), "a real64 survives it exactly")
            if (allocated(error)) return
        end do
        do k = 1, 2
            b = k == 1
            call pf_to_str(b, text)
            call pf_from_str(text, back_b, ok)
            call check(error, ok .and. (back_b .eqv. b), "a logical survives it")
            if (allocated(error)) return
        end do
    end subroutine test_from_str_round_trip

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
