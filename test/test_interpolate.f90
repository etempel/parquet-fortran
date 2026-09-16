!> Tests for `parquet_interpolate`: `pf_interp_1d` under `method="linear"` and the natural cubic
!> spline, the out-of-range policies, the mask, descending tables, and the one-shot `pf_interp`.
!!
!! **Every expected value is a golden expectation or a closed form**, never a number read off a run.
!! The golden rows come from `tools/generate_interpolate_vectors.py`, which solves every spline
!! system exactly over rationals and whose `--self-test` holds that model against scipy; the closed
!! forms are error bounds, derived in `test_interpolate_support.f90` beside the table they bound.
!!
!! **Most assertions here are bit equalities between two routes to the same interpolant** -- a
!! descending table and its ascending twin, a masked table and its survivors, the one-shot form and
!! the object, the arithmetic bracket and bisection. Each route shares nothing above the point where
!! it diverges except the library's own build, so every such test sits beside a golden or
!! closed-form assertion on the same method: an equality alone would pass over a defect both routes
!! reach.
!!
!! This suite is pure computation with no fixture files and no process-global state, so it runs
!! concurrently in the runner. Its only library import is `use parquet_interpolate`: it is registered
!! in `run_tester_pf.f90`, the runner that executes no `bind(C)` call, and
!! `check_test_runner_partition` requires that the files feeding that runner never reach
!! `parquet_bindings`.
module test_interpolate

    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet_interpolate
    use test_interpolate_support
    use test_interpolate_golden, only : GI_NQ, GI_NO, P6_PROBES, P6_NATURAL, P6_FAR, P6_NATURAL_FAR
    use iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan, ieee_get_flag, ieee_set_flag, &
        ieee_support_flag, ieee_invalid

    implicit none
    private

    public :: collect_tests_interpolate

    !> Relative agreement asserted against a golden expectation, measured against the larger of the
    !! expectation and the table's largest ordinate. The spline systems are strictly diagonally
    !! dominant, so a double-precision solve lands within a few ulp of the exact one.
    real(real64), parameter :: GOLDEN_RTOL = 1.0e-13_real64

contains

    !> Registers this module's tests.
    subroutine collect_tests_interpolate(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("both methods follow a sine within their error bounds", &
                         test_spline_follows_a_sine), &
            new_unittest("two points, re-initialising, descending and uneven tables, elemental calls", &
                         test_edge_cases), &
            new_unittest("linear reproduces a straight line and matches the golden rows", &
                         test_linear_is_exact_on_lines), &
            new_unittest("the natural spline matches the exact model and the page's example", &
                         test_natural_spline_matches_the_model), &
            new_unittest("clamp, extrapolate and nan each answer as documented outside the table", &
                         test_outside_policies), &
            new_unittest("a NaN query answers NaN and raises no flag under every policy", &
                         test_nan_query_is_quiet), &
            new_unittest("a query on a knot answers that knot's ordinate exactly", &
                         test_knots_are_exact), &
            new_unittest("the arithmetic bracket and bisection answer the same bits", &
                         test_uniform_and_search_agree), &
            new_unittest("a descending table interpolates as its ascending twin, bit for bit", &
                         test_descending_tables), &
            new_unittest("is_valid drops exactly the points it marks", &
                         test_is_valid_drops_points), &
            new_unittest("pf_interp answers what the object answers, bit for bit", &
                         test_one_shot_equals_object), &
            new_unittest("clear releases an object and init rebuilds or replaces one", &
                         test_clear_and_rebuild) &
            ]

    end subroutine collect_tests_interpolate

    !> qfeet's `spline` test, with its thresholds replaced by the error bounds they stand for.
    !!
    !! `sin` on the integers `0..30`. Linear interpolation stays within `h**2/8 * max|f''|` of it
    !! everywhere; the natural spline within `SINE_CUBIC_BOUND` on `[3, 27]`, where the natural end
    !! condition's error has decayed (`test_interpolate_support.f90` derives both). Swapping
    !! `a**3 - a` for `a**3` in the segment formula moves the spline by up to a sixth of a second
    !! derivative, far outside the bound. Three objects over one table coexist and answer the same
    !! bits, and `extrapolate` changes nothing inside the table.
    subroutine test_spline_follows_a_sine(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d) :: lin, cub, ext, s1, s2, s3
        real(real64)       :: x(31), y(31), q, v
        integer            :: i, far_lin, far_cub, differ

        call sine_table(x, y)
        call cub%init(x, y)
        ! A second %init on a built object rebuilds it rather than aborting.
        call cub%init(x, y)
        call lin%init(x, y, method="linear")
        call ext%init(x, y, outside="extrapolate")
        call s1%init(x, y)
        call s2%init(x, y)
        call s3%init(x, y)

        far_lin = 0
        far_cub = 0
        differ = 0
        do i = 30, 270
            q = real(i, real64)/10.0_real64
            if (abs(lin%eval(q) - sin(q)) > SINE_LINEAR_BOUND) far_lin = far_lin + 1
            v = cub%eval(q)
            if (abs(v - sin(q)) > SINE_CUBIC_BOUND) far_cub = far_cub + 1
            if (ext%eval(q) /= v) differ = differ + 1
            if (s1%eval(q) /= v .or. s2%eval(q) /= v .or. s3%eval(q) /= v) differ = differ + 1
        end do
        call check(error, far_lin == 0, "linear interpolation strayed from sin beyond h**2/8")
        if (allocated(error)) return
        call check(error, far_cub == 0, "the natural spline strayed from sin beyond its error bound on [3, 27]")
        if (allocated(error)) return
        call check(error, differ == 0, &
                   "objects over one table, or with outside=""extrapolate"", answered different bits inside it")
        if (allocated(error)) return

        ! The default policy clamps, at both ends.
        call check(error, cub%eval(-1.0_real64) == y(1) .and. cub%eval(35.0_real64) == y(31), &
                   "the default policy must answer the end ordinate beyond either end")

    end subroutine test_spline_follows_a_sine

    !> qfeet's `edge_cases` test: the shapes a table can take that a large uniform one never does.
    !!
    !! A two-point table is the shortest system; a descending table is the regression for a clamp
    !! that compared against `x(1)` and `x(n)` positionally and returned the last ordinate for every
    !! query; an uneven table takes bisection; an even point count once took a different solver
    !! branch; and an array of queries must answer what the scalar calls answer.
    subroutine test_edge_cases(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d) :: fresh, desc_lin, desc_cub, uneven, uneven_ext, uneven_lin, even
        real(real64)       :: xd(11), yd(11), xn(6), yn(6), xe(32), ye(32), xs(3), vals(3)
        integer            :: i

        call check(error, .not. fresh%is_initialised(), "is_initialised must be .false. before init")
        if (allocated(error)) return
        call fresh%init([0.0_real64, 1.0_real64], [0.0_real64, 2.0_real64])
        call check(error, fresh%is_initialised(), "is_initialised must be .true. after init")
        if (allocated(error)) return

        ! Two points: the natural spline is the straight line through them.
        call check(error, abs(fresh%eval(0.5_real64) - 1.0_real64) <= 4.0_real64*epsilon(1.0_real64), &
                   "a two-point spline must be the straight line through its points")
        if (allocated(error)) return

        ! A second %init replaces the table.
        call fresh%init([0.0_real64, 1.0_real64], [0.0_real64, 6.0_real64])
        call check(error, abs(fresh%eval(0.5_real64) - 3.0_real64) <= 1.0e-15_real64, &
                   "re-initialising must rebuild the interpolant from the new table")
        if (allocated(error)) return

        ! A descending table, `x = 10, 9, ..., 0` and `y = x**2`.
        do i = 1, 11
            xd(i) = real(11 - i, real64)
            yd(i) = xd(i)*xd(i)
        end do
        call desc_lin%init(xd, yd, method="linear")
        call desc_cub%init(xd, yd)
        call check(error, abs(desc_lin%eval(5.5_real64) - 30.5_real64) <= 1.0e-14_real64, &
                   "descending table: linear interpolation must use the bracketing knots")
        if (allocated(error)) return
        call check(error, desc_cub%eval(5.0_real64) == 25.0_real64, &
                   "descending table: the spline must answer an interior knot's ordinate")
        if (allocated(error)) return
        call check(error, desc_cub%eval(20.0_real64) == 100.0_real64, &
                   "descending table: beyond the largest x the clamp must answer y at the largest x")
        if (allocated(error)) return
        call check(error, desc_cub%eval(-5.0_real64) == 0.0_real64, &
                   "descending table: below the smallest x the clamp must answer y at the smallest x")
        if (allocated(error)) return

        ! An uneven table takes bisection; a natural spline reproduces a straight line.
        xn = [0.0_real64, 1.0_real64, 3.0_real64, 7.0_real64, 15.0_real64, 31.0_real64]
        yn = 2.0_real64*xn
        call uneven%init(xn, yn)
        call uneven_lin%init(xn, yn, method="linear")
        call uneven_ext%init(xn, yn, method="linear", outside="extrapolate")
        call check(error, abs(uneven%eval(5.0_real64) - 10.0_real64) <= 1.0e-13_real64, &
                   "uneven table: the natural spline must reproduce a straight line")
        if (allocated(error)) return
        call check(error, abs(uneven_lin%eval(5.0_real64) - 10.0_real64) <= 1.0e-14_real64, &
                   "uneven table: linear interpolation must reproduce a straight line")
        if (allocated(error)) return
        call check(error, abs(uneven_ext%eval(40.0_real64) - 80.0_real64) <= 1.0e-13_real64, &
                   "outside=""extrapolate"" must continue the end segment's line")
        if (allocated(error)) return
        call check(error, uneven_lin%eval(40.0_real64) == 62.0_real64, &
                   "the default policy must clamp to the last ordinate")
        if (allocated(error)) return

        ! An even number of points.
        do i = 1, 32
            xe(i) = real(i, real64)
            ye(i) = 3.0_real64*xe(i) - 1.0_real64
        end do
        call even%init(xe, ye)
        call check(error, abs(even%eval(10.5_real64) - 30.5_real64) <= 1.0e-13_real64, &
                   "an even-length table must solve correctly too")
        if (allocated(error)) return

        ! An array of queries answers what the scalar calls answer.
        xs = [1.5_real64, 5.0_real64, 20.0_real64]
        vals = uneven%eval(xs)
        do i = 1, 3
            call check(error, vals(i) == uneven%eval(xs(i)), &
                       "the elemental call must answer each scalar call's bits")
            if (allocated(error)) return
        end do
        vals = uneven_lin%eval(xs)
        do i = 1, 3
            call check(error, vals(i) == uneven_lin%eval(xs(i)), &
                       "the elemental linear call must answer each scalar call's bits")
            if (allocated(error)) return
        end do

    end subroutine test_edge_cases

    !> `method="linear"` reproduces `2x + 1` to a few ulp at a thousand points of an uneven table,
    !! and matches the golden rows of every fixture, inside the table and extrapolated.
    subroutine test_linear_is_exact_on_lines(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d)        :: line, lin, ext
        real(real64), allocatable :: x(:), y(:)
        real(real64)              :: q(GI_NQ), o(GI_NO), linear(GI_NQ), linear_out(GI_NO)
        real(real64)              :: natural(GI_NQ), natural_out(GI_NO), knots(9), p, v
        character(len=3)          :: name
        integer                   :: i, which, far

        ! Dyadic knots, so each knot's ordinate is exact and the error is the interpolation's own.
        knots = [0.0_real64, 0.5_real64, 1.25_real64, 2.0_real64, 3.5_real64, 4.0_real64, 6.75_real64, &
                 8.0_real64, 10.0_real64]
        call line%init(knots, line_2x1(knots), method="linear")
        far = 0
        do i = 1, 1000
            p = 10.0_real64*real(i, real64)/1001.0_real64
            v = line%eval(p)
            if (abs(v - line_2x1(p)) > 4.0_real64*spacing(line_2x1(p))) far = far + 1
        end do
        call check(error, far == 0, "linear interpolation of 2x + 1 strayed by more than 4 ulp")
        if (allocated(error)) return

        do which = 1, GOLDEN_TABLES
            call golden_table(which, name, x, y, q, o, linear, linear_out, natural, natural_out)
            call lin%init(x, y, method="linear")
            call ext%init(x, y, method="linear", outside="extrapolate")
            call check(error, all(close_to(lin%eval(q), linear, maxval(abs(y)))), &
                       "linear disagrees with the golden rows of fixture " // trim(name))
            if (allocated(error)) return
            call check(error, all(close_to(ext%eval(o), linear_out, maxval(abs(y)))), &
                       "extrapolated linear disagrees with the golden rows of fixture " // trim(name))
            if (allocated(error)) return
        end do

    end subroutine test_linear_is_exact_on_lines

    !> The natural cubic spline matches the exact model on every fixture, inside the table and
    !! extrapolated, and the guide page's `x**2` example: `6.2237` at `2.5`, not the parabola's
    !! `6.25`, and a negative value where the parabola is `9801`.
    subroutine test_natural_spline_matches_the_model(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d)        :: cub, ext, parabola, parabola_ext
        real(real64), allocatable :: x(:), y(:)
        real(real64)              :: q(GI_NQ), o(GI_NO), linear(GI_NQ), linear_out(GI_NO)
        real(real64)              :: natural(GI_NQ), natural_out(GI_NO), px(6), v(3), far
        character(len=3)          :: name
        integer                   :: which, i

        do which = 1, GOLDEN_TABLES
            call golden_table(which, name, x, y, q, o, linear, linear_out, natural, natural_out)
            call cub%init(x, y)
            call ext%init(x, y, bc="natural", outside="extrapolate")
            call check(error, all(close_to(cub%eval(q), natural, maxval(abs(y)))), &
                       "the natural spline disagrees with the golden rows of fixture " // trim(name))
            if (allocated(error)) return
            call check(error, all(close_to(ext%eval(o), natural_out, maxval(abs(y)))), &
                       "the extrapolated natural spline disagrees with the golden rows of fixture " // trim(name))
            if (allocated(error)) return
        end do

        px = [(real(i, real64), i = 1, 6)]
        call parabola%init(px, px*px)
        call parabola_ext%init(px, px*px, outside="extrapolate")
        v = parabola%eval(P6_PROBES)
        call check(error, all(close_to(v, P6_NATURAL, 36.0_real64)), &
                   "the page's x**2 example disagrees with the golden values")
        if (allocated(error)) return
        call check(error, abs(v(2) - 6.2237_real64) < 5.0e-5_real64 .and. abs(v(2) - 6.25_real64) > 1.0e-3_real64, &
                   "the page prints 6.2237 at x = 2.5, which is not the parabola's 6.25")
        if (allocated(error)) return
        far = parabola_ext%eval(P6_FAR)
        call check(error, far < 0.0_real64 .and. close_to(far, P6_NATURAL_FAR, 36.0_real64), &
                   "extrapolating the page's example to x = 99 must give its negative golden value")

    end subroutine test_natural_spline_matches_the_model

    !> Each policy on each method, three points below the table and three above.
    !!
    !! `"clamp"` answers the end ordinate exactly; `"extrapolate"` the golden end-segment value at
    !! the two points the generator computed on each side; `"nan"` a quiet NaN, with IEEE_INVALID
    !! read around the calls so that a NaN made by arithmetic -- which raises it, and aborts a
    !! program under nagfor's default traps -- fails here.
    subroutine test_outside_policies(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d)        :: c
        real(real64), allocatable :: x(:), y(:)
        real(real64)              :: q(GI_NQ), o(GI_NO), linear(GI_NQ), linear_out(GI_NO)
        real(real64)              :: natural(GI_NQ), natural_out(GI_NO), below(3), above(3), span
        real(real64)              :: vb(3), va(3), ve(GI_NO)
        character(len=3)          :: name
        logical                   :: can_test, saved, raised
        integer                   :: which, im

        do which = 1, GOLDEN_TABLES
            call golden_table(which, name, x, y, q, o, linear, linear_out, natural, natural_out)
            span = x(size(x)) - x(1)
            below = [o(1), o(2), x(1) - 1.0e6_real64*span]
            above = [o(3), o(4), x(size(x)) + 1.0e6_real64*span]
            do im = 1, 2
                call c%init(x, y, method=trim(METHODS(im)))
                call check(error, all(c%eval(below) == y(1)) .and. all(c%eval(above) == y(size(y))), &
                           "clamp must answer the end ordinates, method " // trim(METHODS(im)) // &
                           ", fixture " // trim(name))
                if (allocated(error)) return

                call c%init(x, y, method=trim(METHODS(im)), outside="extrapolate")
                ve = c%eval(o)
                if (im == 1) then
                    call check(error, all(close_to(ve, linear_out, maxval(abs(y)))), &
                               "extrapolate must continue the end line, fixture " // trim(name))
                else
                    call check(error, all(close_to(ve, natural_out, maxval(abs(y)))), &
                               "extrapolate must continue the end cubic, fixture " // trim(name))
                end if
                if (allocated(error)) return

                call c%init(x, y, method=trim(METHODS(im)), outside="nan")
                call invalid_begin(can_test, saved)
                vb = c%eval(below)
                va = c%eval(above)
                call invalid_end(can_test, saved, raised)
                call check(error, all(ieee_is_nan(vb)) .and. all(ieee_is_nan(va)), &
                           "nan must answer a NaN beyond either end, method " // trim(METHODS(im)) // &
                           ", fixture " // trim(name))
                if (allocated(error)) return
                call check(error, .not. raised, "the NaN outside the table raised IEEE_INVALID, method " // &
                           trim(METHODS(im)) // ", fixture " // trim(name))
                if (allocated(error)) return
            end do
        end do

    end subroutine test_outside_policies

    !> A NaN query answers NaN on every method and policy, and raises nothing.
    !!
    !! The evenly spaced fixture `U9` is the one that makes a missing screen visible: its bracket is
    !! guessed with `floor`, which raises IEEE_INVALID on a NaN and returns an arbitrary integer.
    subroutine test_nan_query_is_quiet(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d)        :: c
        real(real64), allocatable :: x(:), y(:)
        real(real64)              :: q(GI_NQ), o(GI_NO), linear(GI_NQ), linear_out(GI_NO)
        real(real64)              :: natural(GI_NQ), natural_out(GI_NO), nan, v
        character(len=3)          :: name
        logical                   :: can_test, saved, raised
        integer                   :: which, im, ip

        nan = nan_value()
        do which = 1, GOLDEN_TABLES
            call golden_table(which, name, x, y, q, o, linear, linear_out, natural, natural_out)
            do im = 1, 2
                do ip = 1, 3
                    call c%init(x, y, method=trim(METHODS(im)), outside=trim(POLICIES(ip)))
                    call invalid_begin(can_test, saved)
                    v = c%eval(nan)
                    call invalid_end(can_test, saved, raised)
                    call check(error, ieee_is_nan(v), "a NaN query must answer NaN, method " // &
                               trim(METHODS(im)) // ", policy " // trim(POLICIES(ip)) // ", fixture " // trim(name))
                    if (allocated(error)) return
                    call check(error, .not. raised, "a NaN query raised IEEE_INVALID, method " // &
                               trim(METHODS(im)) // ", policy " // trim(POLICIES(ip)) // ", fixture " // trim(name))
                    if (allocated(error)) return
                end do
            end do
        end do

    end subroutine test_nan_query_is_quiet

    !> `%eval(x(k))` is `y(k)`, bit for bit, for every knot, method and policy, ascending and
    !! descending -- the last knot included, which is the right end of its segment.
    !!
    !! Two tables matter here beyond the golden ones, whose dyadic ordinates make every segment
    !! formula exact at either end. The four-point table ends on the segment from `0.7` to `0.1`,
    !! where `0.7 + (0.1 - 0.7)` is `0.09999999999999998`: a last knot answered through the linear
    !! segment formula rather than directly shows there. The bent table's arithmetic bracket guesses
    !! one segment too low exactly on half its knots, which the correction must walk up from.
    subroutine test_knots_are_exact(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: BENT_N = 4097

        type(pf_interp_1d)        :: c
        real(real64), allocatable :: x(:), y(:)
        real(real64)              :: q(GI_NQ), o(GI_NO), linear(GI_NQ), linear_out(GI_NO)
        real(real64)              :: natural(GI_NQ), natural_out(GI_NO), sx(31), sy(31)
        real(real64)              :: bx(BENT_N), by(BENT_N)
        character(len=3)          :: name
        integer                   :: table, im, ip, n, missed

        call sine_table(sx, sy)
        call bent_grid(BENT_N, bx, by)
        do table = 1, GOLDEN_TABLES + 3
            if (table <= GOLDEN_TABLES) then
                call golden_table(table, name, x, y, q, o, linear, linear_out, natural, natural_out)
            else if (table == GOLDEN_TABLES + 1) then
                name = "sin"
                x = sx
                y = sy
            else if (table == GOLDEN_TABLES + 2) then
                name = "bnt"
                x = bx
                y = by
            else
                name = "end"
                x = [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64]
                y = [0.3_real64, 0.2_real64, 0.7_real64, 0.1_real64]
            end if
            n = size(x)
            do im = 1, 2
                do ip = 1, 3
                    call c%init(x, y, method=trim(METHODS(im)), outside=trim(POLICIES(ip)))
                    missed = count(c%eval(x) /= y)
                    call c%init(x(n:1:-1), y(n:1:-1), method=trim(METHODS(im)), outside=trim(POLICIES(ip)))
                    missed = missed + count(c%eval(x) /= y)
                    call check(error, missed == 0, "a knot did not answer its own ordinate, method " // &
                               trim(METHODS(im)) // ", policy " // trim(POLICIES(ip)) // ", table " // trim(name))
                    if (allocated(error)) return
                end do
            end do
        end do

    end subroutine test_knots_are_exact

    !> The arithmetic bracket and bisection answer the same bits, and the hook that switches between
    !! them reports which one an object was using.
    !!
    !! Four tables. An exactly even one, where the arithmetic guess is exact, at ten thousand
    !! queries and every knot. A table bent up to a two-hundredth of a step either side of the
    !! straight line, which the tolerance still accepts as even and on which the guess is WRONG
    !! beside most knots, too high in one half and too low in the other: a query a four-hundredth of
    !! a step either side of every knot and one on it, with a guard that counts the queries the
    !! guess gets wrong in each direction, since a fixture where it never does would test nothing.
    !! Linear interpolation is what shows a wrong segment there -- a C2 spline's neighbouring cubics
    !! differ by far less than a bit so close to their shared knot. And two negative controls: an
    !! irregular table is not bracketed by arithmetic at all, and neither is an even table with one
    !! knot moved a fifth of a step.
    subroutine test_uniform_and_search_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 4097
        integer, parameter :: SPREAD = 10000

        type(pf_interp_1d)        :: fast, slow
        real(real64), allocatable :: x(:), y(:), q(:)
        real(real64)              :: gq(GI_NQ), go(GI_NO), linear(GI_NQ), linear_out(GI_NO)
        real(real64)              :: natural(GI_NQ), natural_out(GI_NO)
        character(len=3)          :: name
        logical                   :: was
        integer                   :: i, im, k, guess, misguessed, low, differ

        ! An exactly even table: every knot a multiple of 1/1024.
        allocate (x(N), y(N), q(SPREAD + N))
        do i = 1, N
            x(i) = real(i - 1, real64)/1024.0_real64
            y(i) = sin(3.0_real64*x(i))
        end do
        do i = 1, SPREAD
            q(i) = x(N)*real(mod(i*7919, SPREAD), real64)/real(SPREAD, real64)
        end do
        q(SPREAD + 1:) = x
        do im = 1, 2
            call fast%init(x, y, method=trim(METHODS(im)))
            slow = fast
            call parquet_debug_interp_force_search(slow, was)
            call check(error, was, "an exactly even table must be bracketed by arithmetic, method " // &
                       trim(METHODS(im)))
            if (allocated(error)) return
            differ = count(fast%eval(q) /= slow%eval(q))
            call check(error, differ == 0, "the arithmetic bracket and bisection disagreed on an even table, method " // &
                       trim(METHODS(im)))
            if (allocated(error)) return
        end do

        ! The bent table, queried beside every knot and on it.
        deallocate (q)
        allocate (q(3*N))
        call bent_grid(N, x, y)
        do i = 1, N
            q(3*i - 2) = x(i) - 0.0025_real64
            q(3*i - 1) = x(i)
            q(3*i) = x(i) + 0.0025_real64
        end do
        ! The guard: the mean step of the bent table is exactly one, so the guess is the floor of the
        ! query; count the queries inside the table on which that guess is too high, and too low.
        misguessed = 0
        low = 0
        do i = 1, size(q)
            if (q(i) < x(1) .or. q(i) >= x(N)) cycle
            guess = max(1, min(1 + floor(q(i) - x(1)), N - 1))
            k = max(1, min(count(x <= q(i)), N - 1))
            if (guess > k) misguessed = misguessed + 1
            if (guess < k) low = low + 1
        end do
        call check(error, misguessed > 0 .and. low > 0, &
                   "the bent table must hold queries the arithmetic guess puts both above and below " // &
                   "their segment, or a direction of the correction goes untested")
        if (allocated(error)) return
        do im = 1, 2
            call fast%init(x, y, method=trim(METHODS(im)))
            slow = fast
            call parquet_debug_interp_force_search(slow, was)
            call check(error, was, "a table within the tolerance must be bracketed by arithmetic, method " // &
                       trim(METHODS(im)))
            if (allocated(error)) return
            differ = count(fast%eval(q) /= slow%eval(q))
            call check(error, differ == 0, "a wrong arithmetic guess changed an answer on the bent table, method " // &
                       trim(METHODS(im)))
            if (allocated(error)) return
        end do

        ! Negative controls: neither of these is bracketed by arithmetic.
        call golden_table(3, name, x, y, gq, go, linear, linear_out, natural, natural_out)
        call fast%init(x, y)
        call parquet_debug_interp_force_search(fast, was)
        call check(error, .not. was, "an irregular table must be bracketed by bisection")
        if (allocated(error)) return
        deallocate (x, y)
        allocate (x(N), y(N))
        do i = 1, N
            x(i) = real(i - 1, real64)/1024.0_real64
            y(i) = sin(3.0_real64*x(i))
        end do
        x(2049) = x(2049) + 0.2_real64/1024.0_real64
        call fast%init(x, y)
        call parquet_debug_interp_force_search(fast, was)
        call check(error, .not. was, "an even table with one knot moved a fifth of a step must not count as even")

    end subroutine test_uniform_and_search_agree

    !> The same table given ascending and descending answers the same bits on every method and
    !! policy, inside the table, outside it and on its knots. A descending table reversed in `x` but
    !! not in `y` would interpolate the mirror image of the data.
    subroutine test_descending_tables(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d)        :: up, down
        real(real64), allocatable :: x(:), y(:), probes(:)
        real(real64)              :: q(GI_NQ), o(GI_NO), linear(GI_NQ), linear_out(GI_NO)
        real(real64)              :: natural(GI_NQ), natural_out(GI_NO)
        character(len=3)          :: name
        integer                   :: which, im, ip, n

        do which = 1, GOLDEN_TABLES
            call golden_table(which, name, x, y, q, o, linear, linear_out, natural, natural_out)
            n = size(x)
            probes = [q, o, x]
            do im = 1, 2
                do ip = 1, 3
                    call up%init(x, y, method=trim(METHODS(im)), outside=trim(POLICIES(ip)))
                    call down%init(x(n:1:-1), y(n:1:-1), method=trim(METHODS(im)), outside=trim(POLICIES(ip)))
                    call check(error, all(same_bits(up%eval(probes), down%eval(probes))), &
                               "a descending table answered differently from its ascending twin, method " // &
                               trim(METHODS(im)) // ", policy " // trim(POLICIES(ip)) // ", fixture " // trim(name))
                    if (allocated(error)) return
                end do
            end do
        end do

    end subroutine test_descending_tables

    !> A mask dropping an end point, an interior point and two adjacent points builds, bit for bit,
    !! the interpolant of the survivors; the dropped points hold a NaN and an infinity, which must not
    !! be judged; and a mask of all `.true.` changes nothing.
    subroutine test_is_valid_drops_points(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d) :: masked, survivors, all_true, bare
        real(real64)       :: x(31), y(31), xm(31), ym(31), probes(200)
        logical            :: keep(31)
        integer            :: i, im, ip

        call sine_table(x, y)
        keep = .true.
        keep([1, 16, 20, 21]) = .false.
        xm = x
        ym = y
        xm(1) = positive_infinity()
        xm(16) = nan_value()
        ym(20) = nan_value()
        do i = 1, size(probes)
            probes(i) = -3.0_real64 + 0.18_real64*real(i, real64)
        end do
        do im = 1, 2
            do ip = 1, 3
                call masked%init(xm, ym, method=trim(METHODS(im)), outside=trim(POLICIES(ip)), is_valid=keep)
                call survivors%init(pack(x, keep), pack(y, keep), method=trim(METHODS(im)), &
                                    outside=trim(POLICIES(ip)))
                call check(error, all(same_bits(masked%eval(probes), survivors%eval(probes))), &
                           "is_valid did not build the survivors' interpolant, method " // trim(METHODS(im)) // &
                           ", policy " // trim(POLICIES(ip)))
                if (allocated(error)) return
            end do
            call all_true%init(x, y, method=trim(METHODS(im)), is_valid=spread(.true., 1, 31))
            call bare%init(x, y, method=trim(METHODS(im)))
            call check(error, all(same_bits(all_true%eval(probes), bare%eval(probes))), &
                       "a mask of all .true. changed the interpolant, method " // trim(METHODS(im)))
            if (allocated(error)) return
        end do

    end subroutine test_is_valid_drops_points

    !> `pf_interp` answers what `%init` followed by `%eval` answers, bit for bit, in both of its
    !! ranks, on every method and policy and with a mask.
    subroutine test_one_shot_equals_object(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d)        :: c
        real(real64), allocatable :: x(:), y(:), probes(:), got(:), want(:)
        real(real64)              :: q(GI_NQ), o(GI_NO), linear(GI_NQ), linear_out(GI_NO)
        real(real64)              :: natural(GI_NQ), natural_out(GI_NO)
        logical, allocatable      :: keep(:)
        character(len=3)          :: name
        integer                   :: which, im, ip, i, differ

        do which = 1, GOLDEN_TABLES
            call golden_table(which, name, x, y, q, o, linear, linear_out, natural, natural_out)
            probes = [q, o, x]
            do im = 1, 2
                do ip = 1, 3
                    call c%init(x, y, method=trim(METHODS(im)), outside=trim(POLICIES(ip)))
                    want = c%eval(probes)
                    got = pf_interp(x, y, probes, method=trim(METHODS(im)), outside=trim(POLICIES(ip)))
                    differ = count(.not. same_bits(got, want))
                    do i = 1, size(probes)
                        if (.not. same_bits(pf_interp(x, y, probes(i), method=trim(METHODS(im)), &
                                                      outside=trim(POLICIES(ip))), want(i))) differ = differ + 1
                    end do
                    call check(error, differ == 0, "pf_interp differed from the object, method " // &
                               trim(METHODS(im)) // ", policy " // trim(POLICIES(ip)) // ", fixture " // trim(name))
                    if (allocated(error)) return
                end do
            end do
            keep = [(mod(i, 3) /= 0, i = 1, size(x))]
            call c%init(x, y, is_valid=keep)
            call check(error, all(same_bits(pf_interp(x, y, probes, is_valid=keep), c%eval(probes))), &
                       "pf_interp with a mask differed from the object, fixture " // trim(name))
            if (allocated(error)) return
        end do

    end subroutine test_one_shot_equals_object

    !> `%clear` returns an object to its unbuilt state, twice as harmlessly as once; `%init` builds a
    !! cleared object again, and replaces the table of a built one.
    subroutine test_clear_and_rebuild(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d) :: c

        call c%clear()
        call check(error, .not. c%is_initialised(), "clearing a fresh object must leave it unbuilt")
        if (allocated(error)) return
        call c%init([0.0_real64, 1.0_real64, 2.0_real64], [0.0_real64, 1.0_real64, 4.0_real64], method="linear")
        call check(error, c%is_initialised(), "init must build the object")
        if (allocated(error)) return
        call c%clear()
        call check(error, .not. c%is_initialised(), "clear must return a built object to its unbuilt state")
        if (allocated(error)) return
        call c%clear()
        call check(error, .not. c%is_initialised(), "a second clear must be harmless")
        if (allocated(error)) return
        call c%init([0.0_real64, 1.0_real64, 2.0_real64], [0.0_real64, 3.0_real64, 6.0_real64], method="linear")
        call check(error, c%is_initialised() .and. c%eval(1.5_real64) == 4.5_real64, &
                   "init must rebuild a cleared object from the new table")
        if (allocated(error)) return
        call c%init([0.0_real64, 2.0_real64], [10.0_real64, 20.0_real64], method="linear")
        call check(error, c%eval(1.0_real64) == 15.0_real64 .and. c%eval(3.0_real64) == 20.0_real64, &
                   "init on a built object must replace its table")

    end subroutine test_clear_and_rebuild

    ! ---- helpers -------------------------------------------------------------------------------

    !> Agreement with a golden expectation to `GOLDEN_RTOL`, relative to the larger of the
    !! expectation and `scale`.
    elemental function close_to(got, want, scale) result(ok)
        real(real64), intent(in) :: got   !! what the library answered
        real(real64), intent(in) :: want  !! the golden expectation
        real(real64), intent(in) :: scale !! the table's largest ordinate
        logical                  :: ok    !! within tolerance

        ok = abs(got - want) <= GOLDEN_RTOL*max(abs(want), scale)

    end function close_to

    !> The same bits, or both a NaN.
    elemental function same_bits(a, b) result(same)
        real(real64), intent(in) :: a    !! one answer
        real(real64), intent(in) :: b    !! the other
        logical                  :: same !! equal, or both NaN

        same = a == b
        if (.not. same) same = ieee_is_nan(a) .and. ieee_is_nan(b)

    end function same_bits

    !> Saves IEEE_INVALID and clears it, ahead of the calls a test reads it around.
    subroutine invalid_begin(can_test, saved)
        logical, intent(out) :: can_test !! the processor supports the flag for `real64`
        logical, intent(out) :: saved    !! the flag as it was, restored by `invalid_end`

        can_test = ieee_support_flag(ieee_invalid, 0.0_real64)
        saved = .false.
        if (can_test) then
            call ieee_get_flag(ieee_invalid, saved)
            call ieee_set_flag(ieee_invalid, .false.)
        end if

    end subroutine invalid_begin

    !> Reads IEEE_INVALID after the calls under test, and restores it as `saved .or. raised` so a flag
    !! raised elsewhere is neither hidden nor blamed on them.
    subroutine invalid_end(can_test, saved, raised)
        logical, intent(in)  :: can_test !! from `invalid_begin`
        logical, intent(in)  :: saved    !! from `invalid_begin`
        logical, intent(out) :: raised   !! the calls under test raised it

        raised = .false.
        if (can_test) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, saved .or. raised)
        end if

    end subroutine invalid_end

end module test_interpolate
