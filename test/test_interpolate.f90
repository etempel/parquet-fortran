!> Tests for `parquet_interpolate`: `pf_interp_1d` under every method and end condition, its
!> derivatives and integrals, the out-of-range policies, the mask, descending tables, and the
!> one-shot `pf_interp`.
!!
!! **Every expected value is a golden expectation or a closed form**, never a number read off a run.
!! The golden rows come from `tools/generate_interpolate_vectors.py`, which solves every spline
!! system and integrates every polynomial exactly over rationals and whose `--self-test` holds that
!! model against scipy; the closed forms are error bounds, derived in `test_interpolate_support.f90`
!! beside the table they bound, and polynomial identities, derived beside the test that uses them.
!!
!! **Most assertions here are bit equalities between two routes to the same interpolant** -- a
!! descending table and its ascending twin, a masked table and its survivors, the one-shot form and
!! the object, the arithmetic bracket and bisection. Each route shares nothing above the point where
!! it diverges except the library's own build, so every such test sits beside a golden or
!! closed-form assertion on the same variant: an equality alone would pass over a defect both routes
!! reach.
!!
!! **A variant** is one of the five interpolants a table can be built as, in the order of the golden
!! arrays' columns: `linear`, the cubic spline under `natural`, `not_a_knot` and `clamped`, and
!! `pchip` (`VARIANT_NAMES`). A table outside the golden fixtures is clamped with `TEST_SLOPES`.
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
    use test_interpolate_golden, only : GI_NQ, GI_NO, GI_NI, GI_NJ, GI_NV, GI_LINEAR, GI_NATURAL, GI_NOT_A_KNOT, &
        GI_CLAMPED, GI_PCHIP, P6_PROBES, P6_NATURAL, P6_FAR, P6_NATURAL_FAR
    use iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan, ieee_get_flag, ieee_set_flag, &
        ieee_support_flag, ieee_invalid, ieee_divide_by_zero, ieee_value, ieee_positive_inf, ieee_negative_inf

    implicit none
    private

    public :: collect_tests_interpolate

    !> Relative agreement asserted against a golden expectation, measured against the larger of the
    !! expectation and the scale of the quantity over the whole table. The spline systems are strictly
    !! diagonally dominant, so a double-precision solve lands within a few ulp of the exact one, and
    !! every golden row agrees to about `1.5e-15` under gfortran and under ifx's default
    !! `-fp-model=fast`.
    real(real64), parameter :: GOLDEN_RTOL = 1.0e-13_real64
    !> The end slopes a clamped spline is built with over every table that is not a golden fixture.
    real(real64), parameter :: TEST_SLOPES(2) = [0.5_real64, -0.25_real64]
    !> The rounding a consistency check allows, in units of the magnitude it measures: a check that
    !! combines `k` evaluations, each within a few ulp of its own polynomial, is off by a few ulp of the
    !! table's scale times `k`, divided by the step when it is a difference quotient. 64 epsilons is
    !! thirty times the largest such error measured under gfortran and ifx, and a slip in any closed
    !! form moves an answer by a fraction of the quantity itself -- ten orders of magnitude more.
    real(real64), parameter :: ROUNDING = 64.0_real64*epsilon(1.0_real64)

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
            new_unittest("the not-a-knot spline matches the exact model and reproduces a cubic", &
                         test_not_a_knot_matches_the_model), &
            new_unittest("the clamped spline matches the exact model and meets its end slopes", &
                         test_clamped_matches_the_model), &
            new_unittest("pchip matches the exact model, keeps monotone data monotone and stays local", &
                         test_pchip_matches_the_model), &
            new_unittest("derivatives and integrals agree with the values they come from", &
                         test_derivative_and_integral_are_consistent), &
            new_unittest("clamp, extrapolate and nan each answer as documented outside the table", &
                         test_outside_policies), &
            new_unittest("a NaN query or limit answers NaN and raises no flag under every policy", &
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

    !> `method="linear"` reproduces `2x + 1` to a few ulp at a thousand points of an uneven table, its
    !! slope is 2 and its curvature 0, and its integral is `x**2 + x` between the limits; and it matches
    !! the golden rows of every fixture.
    !!
    !! The knots are dyadic, so every ordinate and every difference of two is exact, and the slope of
    !! each segment is `2` to the rounding of one division.
    subroutine test_linear_is_exact_on_lines(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d)   :: line
        type(golden_fixture) :: f
        real(real64)         :: knots(9), p, v, lo, hi, want
        integer              :: i, which, far, sloped, curved, off

        knots = [0.0_real64, 0.5_real64, 1.25_real64, 2.0_real64, 3.5_real64, 4.0_real64, 6.75_real64, &
                 8.0_real64, 10.0_real64]
        call line%init(knots, line_2x1(knots), method="linear")
        far = 0
        sloped = 0
        curved = 0
        off = 0
        do i = 1, 1000
            p = 10.0_real64*real(i, real64)/1001.0_real64
            v = line%eval(p)
            if (abs(v - line_2x1(p)) > 4.0_real64*spacing(line_2x1(p))) far = far + 1
            if (abs(line%derivative(p) - 2.0_real64) > 4.0_real64*spacing(2.0_real64)) sloped = sloped + 1
            if (line%derivative(p, 2) /= 0.0_real64) curved = curved + 1
            ! The integral from a thousandth of the table in to `p`, against `F(x) = x**2 + x`; the
            ! first `p` lies below that lower limit, so the width is taken unsigned.
            lo = 0.01_real64
            hi = p
            want = (hi*hi + hi) - (lo*lo + lo)
            if (abs(line%integral(lo, hi) - want) > ROUNDING*abs(hi - lo)*21.0_real64) off = off + 1
        end do
        call check(error, far == 0, "linear interpolation of 2x + 1 strayed by more than 4 ulp")
        if (allocated(error)) return
        call check(error, sloped == 0, "the derivative of linear interpolation of 2x + 1 must be 2")
        if (allocated(error)) return
        call check(error, curved == 0, "the second derivative of linear interpolation must be 0")
        if (allocated(error)) return
        call check(error, off == 0, "the integral of linear interpolation of 2x + 1 must be x**2 + x")
        if (allocated(error)) return

        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            call check_golden_rows(error, f, GI_LINEAR)
            if (allocated(error)) return
        end do

    end subroutine test_linear_is_exact_on_lines

    !> The natural cubic spline matches the exact model on every fixture -- values, both derivatives
    !! and integrals -- and the guide page's `x**2` example: `6.2237` at `2.5`, not the parabola's
    !! `6.25`, and a negative value where the parabola is `9801`.
    subroutine test_natural_spline_matches_the_model(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d)   :: parabola, parabola_ext
        type(golden_fixture) :: f
        real(real64)         :: px(6), v(3), far
        integer              :: which, i

        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            call check_golden_rows(error, f, GI_NATURAL)
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

    !> The not-a-knot spline matches the exact model on every fixture, the four-point `K4` included --
    !! the smallest table it accepts, where it is the one cubic through all four points -- and
    !! reproduces the cubic `x**3 - 2*x`, its slope and its curvature, beyond the table too.
    !!
    !! A not-a-knot spline's third derivative is continuous at the second and the second-to-last knot,
    !! so on a table sampled from one cubic the cubic itself satisfies every row of its system: the
    !! reproduction is exact but for rounding, which on these ordinates of up to `20` is far below
    !! `1e-12` relative. A sign slip in either eliminated row, or in the recovery of `m(1)` or `m(n)`,
    !! breaks the reproduction at every query.
    subroutine test_not_a_knot_matches_the_model(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(golden_fixture)      :: f
        type(pf_interp_1d)        :: c
        real(real64), allocatable :: p(:)
        integer                   :: which

        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            call check_golden_rows(error, f, GI_NOT_A_KNOT)
            if (allocated(error)) return
        end do

        call golden_fixture_get(3, f)
        call c%init(f%x, f%x**3 - 2.0_real64*f%x, bc="not_a_knot", outside="extrapolate")
        p = [f%q, f%o]
        call check_reproduces_the_cubic(error, c, p, "the not-a-knot spline")

    end subroutine test_not_a_knot_matches_the_model

    !> The clamped spline matches the exact model on every fixture, its first derivative at each end
    !! is the slope it was given, and given the end slopes of the cubic `x**3 - 2*x` it reproduces that
    !! cubic.
    !!
    !! The end slopes are asserted to `1e-12` relative: the first derivative at an end is read off
    !! the segment formula at `b = 0` or `b = 1`, where it is the given slope but for the rounding of a
    !! handful of operations. Dropping the `6*` of a clamped row, or its sign, moves the end slope by
    !! the secant itself.
    subroutine test_clamped_matches_the_model(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(golden_fixture)      :: f
        type(pf_interp_1d)        :: c
        real(real64), allocatable :: p(:)
        real(real64)              :: s(2)
        integer                   :: which, n

        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            call check_golden_rows(error, f, GI_CLAMPED)
            if (allocated(error)) return
            n = size(f%x)
            call c%init(f%x, f%y, bc="clamped", slopes=f%slopes)
            s = [c%derivative(f%x(1)), c%derivative(f%x(n))]
            call check(error, all(abs(s - f%slopes) <= 1.0e-12_real64*max(1.0_real64, abs(f%slopes))), &
                       "the clamped spline's first derivative at an end is not the slope it was given, fixture " // &
                       trim(f%name))
            if (allocated(error)) return
        end do

        call golden_fixture_get(3, f)
        n = size(f%x)
        call c%init(f%x, f%x**3 - 2.0_real64*f%x, bc="clamped", outside="extrapolate", &
                    slopes=[3.0_real64*f%x(1)**2 - 2.0_real64, 3.0_real64*f%x(n)**2 - 2.0_real64])
        p = [f%q, f%o]
        call check_reproduces_the_cubic(error, c, p, "the clamped spline")

    end subroutine test_clamped_matches_the_model

    !> PCHIP matches the exact model on every fixture, and has the four properties the guide page
    !! promises; and building it over a zigzag, whose extrema meet the mean's zero denominator, raises
    !! no flag.
    !!
    !! **Monotone data stay monotone**: `M12` is non-decreasing, and `%eval` at 10 000 rising points
    !! across it never decreases. **A flat run stays flat**: `M12`'s third, fourth and fifth ordinates
    !! are equal, and `%eval` on both segments between them answers that ordinate exactly. **No
    !! segment overshoots its ordinates**: on `R33`, which rises and falls, every one of 10 000 points
    !! lies between the two ordinates of its segment, to the rounding of one evaluation -- a slope
    !! within three secants keeps a Hermite cubic inside them. The natural spline over the same tables
    !! does none of the three, which is what shows each assertion can fail. **A slope is local**:
    !! moving `R33`'s seventeenth ordinate changes the slopes at knots 16 to 18 only, so the segments
    !! from knot 15 to knot 19 are the only ones that may move -- every point outside them answers the
    !! same bits, and some point inside them does not.
    subroutine test_pchip_matches_the_model(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: FINE = 10000
        integer, parameter :: MOVED = 17

        type(golden_fixture)      :: f
        type(pf_interp_1d)        :: shape, smooth, before, after
        real(real64), allocatable :: y(:)
        real(real64)              :: p(FINE), vp(FINE), vs(FINE), flat
        logical                   :: can_test, saved(2), raised(2)
        integer                   :: which, i, n, outside_moved, inside_moved

        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            call check_golden_rows(error, f, GI_PCHIP)
            if (allocated(error)) return
        end do

        ! Monotone and flat, on M12.
        call golden_fixture_get(4, f)
        n = size(f%x)
        call shape%init(f%x, f%y, method="pchip")
        call smooth%init(f%x, f%y)
        do i = 1, FINE
            p(i) = f%x(1) + (f%x(n) - f%x(1))*real(i - 1, real64)/real(FINE - 1, real64)
        end do
        vp = shape%eval(p)
        vs = smooth%eval(p)
        call check(error, all(vp(2:) >= vp(:FINE - 1)), "pchip over non-decreasing data decreased somewhere")
        if (allocated(error)) return
        call check(error, .not. all(vs(2:) >= vs(:FINE - 1)), &
                   "the natural spline over M12 must decrease somewhere, or the monotonicity assertion tests nothing")
        if (allocated(error)) return
        call check(error, f%y(3) == f%y(4) .and. f%y(4) == f%y(5), "M12's third to fifth ordinates must be a flat run")
        if (allocated(error)) return
        flat = f%y(3)
        call check(error, all(pack(vp, p >= f%x(3) .and. p <= f%x(5)) == flat), &
                   "pchip must answer the ordinate of a flat run on the whole run")
        if (allocated(error)) return
        call check(error, any(pack(vs, p >= f%x(3) .and. p <= f%x(5)) /= flat), &
                   "the natural spline must leave M12's flat run, or the flatness assertion tests nothing")
        if (allocated(error)) return

        ! A zigzag over an even table: every interior knot is an extremum whose two secants are equal
        ! and opposite, where the harmonic mean's denominator is zero, so a mean formed and discarded
        ! divides by zero. The slope there is zero, and building raises nothing.
        call exceptions_begin(can_test, saved)
        call shape%init([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                        [0.0_real64, 1.0_real64, 0.0_real64, 1.0_real64, 0.0_real64], method="pchip")
        call exceptions_end(can_test, saved, raised)
        call check(error, .not. any(raised), "building pchip over a zigzag raised IEEE_INVALID or IEEE_DIVIDE_BY_ZERO")
        if (allocated(error)) return
        call check(error, all(shape%derivative([2.0_real64, 3.0_real64, 4.0_real64]) == 0.0_real64), &
                   "pchip's slope at every extremum of a zigzag must be zero")
        if (allocated(error)) return

        ! Within each segment's ordinates, on R33.
        call golden_fixture_get(3, f)
        n = size(f%x)
        call shape%init(f%x, f%y, method="pchip")
        call smooth%init(f%x, f%y)
        do i = 1, FINE
            p(i) = f%x(1) + (f%x(n) - f%x(1))*real(i - 1, real64)/real(FINE - 1, real64)
        end do
        call check(error, count(beyond_segment(shape%eval(p), p)) == 0, &
                   "pchip left the range of a segment's two ordinates")
        if (allocated(error)) return
        call check(error, count(beyond_segment(smooth%eval(p), p)) > 0, &
                   "the natural spline over R33 must overshoot a segment somewhere, or the range assertion tests nothing")
        if (allocated(error)) return

        ! Local, on R33.
        call before%init(f%x, f%y, method="pchip")
        y = f%y
        y(MOVED) = y(MOVED) + 0.125_real64
        call after%init(f%x, y, method="pchip")
        do i = 1, FINE
            p(i) = f%x(1) + (f%x(n) - f%x(1))*real(i - 1, real64)/real(FINE - 1, real64)
        end do
        vp = before%eval(p)
        vs = after%eval(p)
        outside_moved = count(vp /= vs .and. (p < f%x(MOVED - 2) .or. p >= f%x(MOVED + 2)))
        inside_moved = count(vp /= vs .and. p >= f%x(MOVED - 2) .and. p < f%x(MOVED + 2))
        call check(error, outside_moved == 0, "moving one ordinate moved pchip beyond the two knots on either side")
        if (allocated(error)) return
        call check(error, inside_moved > 0, "moving one ordinate must move pchip between the knots around it")

    contains

        !> `.true.` where `v`, the value at `q`, lies outside the two ordinates of the segment of `f`
        !! that holds `q` by more than 4 ulp of the table's largest ordinate.
        elemental function beyond_segment(v, q) result(out)
            real(real64), intent(in) :: v   !! the value
            real(real64), intent(in) :: q   !! where it was taken, inside the table
            logical                  :: out !! outside the segment's ordinates

            integer      :: k
            real(real64) :: slack

            k = max(1, min(count(f%x <= q), size(f%x) - 1))
            slack = 4.0_real64*spacing(maxval(abs(f%y)))
            out = v > max(f%y(k), f%y(k + 1)) + slack .or. v < min(f%y(k), f%y(k + 1)) - slack

        end function beyond_segment

    end subroutine test_pchip_matches_the_model

    !> Every variant's derivatives and integrals agree with its own values, on every fixture, inside
    !! the table and beyond it (risk 6: a derivative or an integral disagreeing with the value's
    !! polynomial is invisible to every test of the values).
    !!
    !! Each check is exact for a polynomial of the degree it meets, so what is left is rounding, bounded
    !! by `ROUNDING` in the magnitude the check combines:
    !!
    !! * `%derivative(p)` against Richardson's extrapolation of central differences of `%eval`,
    !!   `(4*D(e) - D(2*e))/3` with `D(e) = (f(p + e) - f(p - e))/(2*e)`, which is exact for a cubic;
    !! * `%derivative(p, 2)` against the central difference of `%derivative(p)`, exact for a quadratic;
    !! * `%integral(a, b)` against Simpson's rule on every piece between the limits and the knots,
    !!   exact for a cubic;
    !! * `%integral(b, a)` against `-%integral(a, b)`, bit for bit, and the integral over `[a, c]`
    !!   against the sum over `[a, b]` and `[b, c]` at every knot and between every two.
    !!
    !! Each difference stencil stays on one polynomial: its step is a power of two no more than a
    !! quarter of the distance from `p` to the nearest knot where the polynomial changes, and a query
    !! on a knot is skipped. Every step and point is dyadic, so `p + e` is exact.
    subroutine test_derivative_and_integral_are_consistent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(golden_fixture)      :: f
        type(pf_interp_1d)        :: c, flat
        real(real64), allocatable :: points(:)
        real(real64)              :: ymax
        integer                   :: which, v, i, n, stencils, pieces

        stencils = 0
        pieces = 0
        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            n = size(f%x)
            ymax = maxval(abs(f%y))
            points = [f%q, f%o]
            do v = 1, GI_NV
                call init_variant(c, v, f%x, f%y, f%slopes, "extrapolate")
                call init_variant(flat, v, f%x, f%y, f%slopes, "clamp")
                do i = 1, size(points)
                    call check_difference_quotients(error, c, f%x, ymax, points(i), stencils)
                    if (allocated(error)) then
                        error%message = error%message // ", variant " // trim(VARIANT_NAMES(v)) // ", fixture " // &
                                        trim(f%name)
                        return
                    end if
                end do
                do i = 1, GI_NI
                    call check_simpson(error, c, f%x, ymax, f%a(i), f%b(i), pieces)
                    if (allocated(error)) exit
                end do
                do i = 1, GI_NJ
                    if (allocated(error)) exit
                    call check_simpson(error, c, f%x, ymax, f%oa(i), f%ob(i), pieces)
                    if (allocated(error)) exit
                    call check_simpson(error, flat, f%x, ymax, f%oa(i), f%ob(i), pieces)
                end do
                if (allocated(error)) then
                    error%message = error%message // ", variant " // trim(VARIANT_NAMES(v)) // ", fixture " // &
                                    trim(f%name)
                    return
                end if
                do i = 2, n - 1
                    call check_additive(error, c, f%x, ymax, f%x(1), f%x(i), f%x(n))
                    if (allocated(error)) exit
                    call check_additive(error, c, f%x, ymax, f%oa(3), 0.5_real64*(f%x(i - 1) + f%x(i)), f%ob(3))
                    if (allocated(error)) exit
                end do
                if (allocated(error)) then
                    error%message = error%message // ", variant " // trim(VARIANT_NAMES(v)) // ", fixture " // &
                                    trim(f%name)
                    return
                end if
            end do
        end do
        ! The guards against a vacuous pass: stencils were taken, and integrals were cut into pieces.
        call check(error, stencils > 0, "no difference stencil fitted on one polynomial, so no derivative was checked")
        if (allocated(error)) return
        call check(error, pieces > 0, "no integral was cut into pieces, so no integral was checked")

    end subroutine test_derivative_and_integral_are_consistent

    !> Each policy on each variant, three points below the table and three above, and the integrals
    !! that reach beyond it.
    !!
    !! `"clamp"` answers the end ordinate exactly, a derivative of zero of either order, the golden
    !! clamped integrals, and a flat end's integral out to an infinite limit -- zero where that end's
    !! ordinate is zero. `"extrapolate"` answers the golden rows of the end segment's polynomial
    !! continued: value, both derivatives, integrals. `"nan"` answers a quiet NaN for each, and
    !! IEEE_INVALID is read around the calls so that a NaN made by arithmetic -- which raises it, and
    !! aborts a program under nagfor's default traps -- fails here.
    subroutine test_outside_policies(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(golden_fixture) :: f
        type(pf_interp_1d)   :: c
        real(real64)         :: below(3), above(3), span, ymax, scale, got(GI_NJ), vb(3), va(3), ve(GI_NO)
        real(real64)         :: flat_in(GI_NJ)
        character(len=:), allocatable :: what
        logical              :: can_test, saved, raised
        integer              :: which, v, i, n

        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            n = size(f%x)
            span = f%x(n) - f%x(1)
            ymax = maxval(abs(f%y))
            below = [f%o(1), f%o(2), f%x(1) - 1.0e6_real64*span]
            above = [f%o(3), f%o(4), f%x(n) + 1.0e6_real64*span]
            do v = 1, GI_NV
                what = ", variant " // trim(VARIANT_NAMES(v)) // ", fixture " // trim(f%name)

                call init_variant(c, v, f%x, f%y, f%slopes, "clamp")
                call check(error, all(c%eval(below) == f%y(1)) .and. all(c%eval(above) == f%y(n)), &
                           "clamp must answer the end ordinates" // what)
                if (allocated(error)) return
                call check(error, all(c%derivative([below, above]) == 0.0_real64) .and. &
                           all(c%derivative([below, above], 2) == 0.0_real64), &
                           "clamp must answer a derivative of zero beyond the table" // what)
                if (allocated(error)) return
                do i = 1, GI_NJ
                    got(i) = c%integral(f%oa(i), f%ob(i))
                end do
                call check(error, all(close_to(got, f%integral_clamp(:, v), span*ymax)), &
                           "clamp's integrals beyond the table disagree with the golden rows" // what)
                if (allocated(error)) return
                ! Entirely below the table: the first ordinate over the whole width.
                call check(error, abs(c%integral(below(1), below(2)) - (below(2) - below(1))*f%y(1)) <= &
                           ROUNDING*abs((below(2) - below(1))*f%y(1)), &
                           "clamp's integral below the table must be the first ordinate times the width" // what)
                if (allocated(error)) return

                call init_variant(c, v, f%x, f%y, f%slopes, "extrapolate")
                ve = c%eval(f%o)
                call check(error, all(close_to(ve, f%value_out(:, v), ymax)), &
                           "extrapolate must continue the end segment's polynomial" // what)
                if (allocated(error)) return
                scale = max(maxval(abs(f%d1_out(:, v))), ymax)
                call check(error, all(close_to(c%derivative(f%o), f%d1_out(:, v), scale)), &
                           "extrapolate must continue the end segment's slope" // what)
                if (allocated(error)) return
                scale = max(maxval(abs(f%d2_out(:, v))), ymax)
                call check(error, all(close_to(c%derivative(f%o, 2), f%d2_out(:, v), scale)), &
                           "extrapolate must continue the end segment's curvature" // what)
                if (allocated(error)) return
                do i = 1, GI_NJ
                    got(i) = c%integral(f%oa(i), f%ob(i))
                end do
                call check(error, all(close_to(got, f%integral_out(:, v), span*ymax)), &
                           "extrapolate's integrals beyond the table disagree with the golden rows" // what)
                if (allocated(error)) return

                call init_variant(c, v, f%x, f%y, f%slopes, "nan")
                call invalid_begin(can_test, saved)
                vb = c%eval(below)
                va = c%eval(above)
                ve = [c%derivative(below(1)), c%derivative(above(1)), c%derivative(below(2), 2), &
                      c%derivative(above(2), 2)]
                do i = 1, GI_NJ
                    got(i) = c%integral(f%oa(i), f%ob(i))
                    flat_in(i) = c%integral(f%a(i), f%b(i))
                end do
                call invalid_end(can_test, saved, raised)
                call check(error, all(ieee_is_nan(vb)) .and. all(ieee_is_nan(va)), &
                           "nan must answer a NaN beyond either end" // what)
                if (allocated(error)) return
                call check(error, all(ieee_is_nan(ve)), "nan must answer a NaN derivative beyond either end" // what)
                if (allocated(error)) return
                call check(error, all(ieee_is_nan(got)), "nan must answer a NaN integral with a limit beyond the table" // what)
                if (allocated(error)) return
                call check(error, .not. any(ieee_is_nan(flat_in)), "nan must integrate normally inside the table" // what)
                if (allocated(error)) return
                call check(error, .not. raised, "the NaN outside the table raised IEEE_INVALID" // what)
                if (allocated(error)) return
            end do
        end do

        ! An infinite limit beyond a flat end: M12's first ordinate is zero and its last is 2.
        call golden_fixture_get(4, f)
        n = size(f%x)
        call check(error, f%y(1) == 0.0_real64 .and. f%y(n) > 0.0_real64, "M12 must start at zero and end above it")
        if (allocated(error)) return
        call init_variant(c, GI_PCHIP, f%x, f%y, f%slopes, "clamp")
        call invalid_begin(can_test, saved)
        vb(1) = c%integral(ieee_value(1.0_real64, ieee_negative_inf), f%x(1))
        va(1) = c%integral(f%x(n), ieee_value(1.0_real64, ieee_positive_inf))
        call invalid_end(can_test, saved, raised)
        call check(error, vb(1) == 0.0_real64, "clamp's integral from -Infinity over a zero end ordinate must be zero")
        if (allocated(error)) return
        call check(error, va(1) > huge(1.0_real64), "clamp's integral to +Infinity over a positive end ordinate must be +Infinity")
        if (allocated(error)) return
        call check(error, .not. raised, "an infinite limit beyond a flat end raised IEEE_INVALID")

    end subroutine test_outside_policies

    !> A NaN query answers NaN from `%eval` and from `%derivative` of either order, and a NaN limit a
    !! NaN from `%integral`, on every variant and policy, and none of them raises a flag.
    !!
    !! The evenly spaced fixture `U9` is the one that makes a missing screen visible: its bracket is
    !! guessed with `floor`, which raises IEEE_INVALID on a NaN and returns an arbitrary integer.
    subroutine test_nan_query_is_quiet(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(golden_fixture) :: f
        type(pf_interp_1d)   :: c
        real(real64)         :: nan, got(5)
        logical              :: can_test, saved, raised
        integer              :: which, v, ip

        nan = nan_value()
        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            do v = 1, GI_NV
                do ip = 1, 3
                    call init_variant(c, v, f%x, f%y, f%slopes, trim(POLICIES(ip)))
                    call invalid_begin(can_test, saved)
                    got = [c%eval(nan), c%derivative(nan), c%derivative(nan, 2), c%integral(nan, f%x(2)), &
                           c%integral(f%x(1), nan)]
                    call invalid_end(can_test, saved, raised)
                    call check(error, all(ieee_is_nan(got)), "a NaN query or limit must answer NaN, variant " // &
                               trim(VARIANT_NAMES(v)) // ", policy " // trim(POLICIES(ip)) // ", fixture " // trim(f%name))
                    if (allocated(error)) return
                    call check(error, .not. raised, "a NaN query or limit raised IEEE_INVALID, variant " // &
                               trim(VARIANT_NAMES(v)) // ", policy " // trim(POLICIES(ip)) // ", fixture " // trim(f%name))
                    if (allocated(error)) return
                end do
            end do
        end do

    end subroutine test_nan_query_is_quiet

    !> `%eval(x(k))` is `y(k)`, bit for bit, for every knot, variant and policy, ascending and
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

        type(golden_fixture)      :: f
        type(pf_interp_1d)        :: c
        real(real64), allocatable :: x(:), y(:)
        real(real64)              :: sx(31), sy(31), bx(BENT_N), by(BENT_N), slopes(2)
        character(len=3)          :: name
        integer                   :: table, v, ip, n, missed

        call sine_table(sx, sy)
        call bent_grid(BENT_N, bx, by)
        do table = 1, GOLDEN_TABLES + 3
            slopes = TEST_SLOPES
            if (table <= GOLDEN_TABLES) then
                call golden_fixture_get(table, f)
                name = f%name
                x = f%x
                y = f%y
                slopes = f%slopes
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
            do v = 1, GI_NV
                do ip = 1, 3
                    call init_variant(c, v, x, y, slopes, trim(POLICIES(ip)))
                    missed = count(c%eval(x) /= y)
                    call init_variant(c, v, x(n:1:-1), y(n:1:-1), [slopes(2), slopes(1)], trim(POLICIES(ip)))
                    missed = missed + count(c%eval(x) /= y)
                    call check(error, missed == 0, "a knot did not answer its own ordinate, variant " // &
                               trim(VARIANT_NAMES(v)) // ", policy " // trim(POLICIES(ip)) // ", table " // trim(name))
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
    !! differ by far less than a bit so close to their shared knot. Every variant is run, since each
    !! has its own segment formula to be handed the wrong segment. And two negative controls: an
    !! irregular table is not bracketed by arithmetic at all, and neither is an even table with one
    !! knot moved a fifth of a step.
    subroutine test_uniform_and_search_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 4097
        integer, parameter :: SPREAD = 10000

        type(golden_fixture)      :: f
        type(pf_interp_1d)        :: fast, slow
        real(real64), allocatable :: x(:), y(:), q(:)
        logical                   :: was
        integer                   :: i, v, k, guess, misguessed, low, differ

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
        do v = 1, GI_NV
            call init_variant(fast, v, x, y, TEST_SLOPES, "clamp")
            slow = fast
            call parquet_debug_interp_force_search(slow, was)
            call check(error, was, "an exactly even table must be bracketed by arithmetic, variant " // &
                       trim(VARIANT_NAMES(v)))
            if (allocated(error)) return
            differ = count(fast%eval(q) /= slow%eval(q)) + count(fast%derivative(q) /= slow%derivative(q))
            call check(error, differ == 0, "the arithmetic bracket and bisection disagreed on an even table, variant " // &
                       trim(VARIANT_NAMES(v)))
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
        do v = 1, GI_NV
            call init_variant(fast, v, x, y, TEST_SLOPES, "clamp")
            slow = fast
            call parquet_debug_interp_force_search(slow, was)
            call check(error, was, "a table within the tolerance must be bracketed by arithmetic, variant " // &
                       trim(VARIANT_NAMES(v)))
            if (allocated(error)) return
            differ = count(fast%eval(q) /= slow%eval(q)) + count(fast%derivative(q) /= slow%derivative(q))
            call check(error, differ == 0, "a wrong arithmetic guess changed an answer on the bent table, variant " // &
                       trim(VARIANT_NAMES(v)))
            if (allocated(error)) return
        end do

        ! Negative controls: neither of these is bracketed by arithmetic.
        call golden_fixture_get(3, f)
        call fast%init(f%x, f%y)
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

    !> The same table given ascending and descending answers the same bits on every variant and
    !! policy -- values and both derivatives inside the table, outside it and on its knots, and every
    !! integral -- with the clamped end slopes given in each call's own order. A descending table
    !! reversed in `x` but not in `y` would interpolate the mirror image of the data; a clamped one
    !! whose slopes were not swapped with it would impose each slope at the wrong end (risk 8), which
    !! the derivative at the descending call's first point shows directly.
    subroutine test_descending_tables(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(golden_fixture)      :: f
        type(pf_interp_1d)        :: up, down
        real(real64), allocatable :: probes(:), xd(:), yd(:)
        real(real64)              :: sd(2), iu(GI_NI + GI_NJ), id(GI_NI + GI_NJ)
        character(len=:), allocatable :: what
        integer                   :: which, v, ip, n, i

        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            n = size(f%x)
            probes = [f%q, f%o, f%x]
            xd = f%x(n:1:-1)
            yd = f%y(n:1:-1)
            ! The descending call's first point is the table's last, so its first slope is that end's.
            sd = [f%slopes(2), f%slopes(1)]
            do v = 1, GI_NV
                do ip = 1, 3
                    what = ", variant " // trim(VARIANT_NAMES(v)) // ", policy " // trim(POLICIES(ip)) // &
                           ", fixture " // trim(f%name)
                    call init_variant(up, v, f%x, f%y, f%slopes, trim(POLICIES(ip)))
                    call init_variant(down, v, xd, yd, sd, trim(POLICIES(ip)))
                    call check(error, all(same_bits(up%eval(probes), down%eval(probes))), &
                               "a descending table answered a different value from its ascending twin" // what)
                    if (allocated(error)) return
                    call check(error, all(same_bits(up%derivative(probes), down%derivative(probes))) .and. &
                               all(same_bits(up%derivative(probes, 2), down%derivative(probes, 2))), &
                               "a descending table answered a different derivative from its ascending twin" // what)
                    if (allocated(error)) return
                    do i = 1, GI_NI
                        iu(i) = up%integral(f%a(i), f%b(i))
                        id(i) = down%integral(f%a(i), f%b(i))
                    end do
                    do i = 1, GI_NJ
                        iu(GI_NI + i) = up%integral(f%oa(i), f%ob(i))
                        id(GI_NI + i) = down%integral(f%oa(i), f%ob(i))
                    end do
                    call check(error, all(same_bits(iu, id)), &
                               "a descending table answered a different integral from its ascending twin" // what)
                    if (allocated(error)) return
                end do
            end do
            call down%init(xd, yd, bc="clamped", slopes=sd)
            call check(error, abs(down%derivative(xd(1)) - sd(1)) <= 1.0e-12_real64*max(1.0_real64, abs(sd(1))), &
                       "a descending clamped table must meet its first slope at its own first point, fixture " // &
                       trim(f%name))
            if (allocated(error)) return
        end do

    end subroutine test_descending_tables

    !> A mask dropping an end point, an interior point and two adjacent points builds, bit for bit,
    !! the interpolant of the survivors on every variant and policy; the dropped points hold a NaN and
    !! an infinity, which must not be judged; and a mask of all `.true.` changes nothing.
    subroutine test_is_valid_drops_points(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d) :: masked, survivors, all_true, bare
        real(real64)       :: x(31), y(31), xm(31), ym(31), probes(200)
        logical            :: keep(31)
        integer            :: i, v, ip

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
        do v = 1, GI_NV
            do ip = 1, 3
                call init_variant(masked, v, xm, ym, TEST_SLOPES, trim(POLICIES(ip)), is_valid=keep)
                call init_variant(survivors, v, pack(x, keep), pack(y, keep), TEST_SLOPES, trim(POLICIES(ip)))
                call check(error, all(same_bits(masked%eval(probes), survivors%eval(probes))) .and. &
                           all(same_bits(masked%derivative(probes), survivors%derivative(probes))), &
                           "is_valid did not build the survivors' interpolant, variant " // trim(VARIANT_NAMES(v)) // &
                           ", policy " // trim(POLICIES(ip)))
                if (allocated(error)) return
            end do
            call init_variant(all_true, v, x, y, TEST_SLOPES, "clamp", is_valid=spread(.true., 1, 31))
            call init_variant(bare, v, x, y, TEST_SLOPES, "clamp")
            call check(error, all(same_bits(all_true%eval(probes), bare%eval(probes))), &
                       "a mask of all .true. changed the interpolant, variant " // trim(VARIANT_NAMES(v)))
            if (allocated(error)) return
        end do

    end subroutine test_is_valid_drops_points

    !> `pf_interp` answers what `%init` followed by `%eval` answers, bit for bit, in both of its
    !! ranks, on every variant and policy and with a mask.
    subroutine test_one_shot_equals_object(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(golden_fixture)      :: f
        type(pf_interp_1d)        :: c
        real(real64), allocatable :: probes(:), got(:), want(:)
        logical, allocatable      :: keep(:)
        integer                   :: which, v, ip, i, differ

        do which = 1, GOLDEN_TABLES
            call golden_fixture_get(which, f)
            probes = [f%q, f%o, f%x]
            do v = 1, GI_NV
                do ip = 1, 3
                    call init_variant(c, v, f%x, f%y, f%slopes, trim(POLICIES(ip)))
                    want = c%eval(probes)
                    got = one_shot(v, f%x, f%y, f%slopes, trim(POLICIES(ip)), probes)
                    differ = count(.not. same_bits(got, want))
                    do i = 1, size(probes)
                        if (.not. same_bits(one_shot_at(v, f%x, f%y, f%slopes, trim(POLICIES(ip)), probes(i)), &
                                            want(i))) differ = differ + 1
                    end do
                    call check(error, differ == 0, "pf_interp differed from the object, variant " // &
                               trim(VARIANT_NAMES(v)) // ", policy " // trim(POLICIES(ip)) // ", fixture " // trim(f%name))
                    if (allocated(error)) return
                end do
            end do
            keep = [(mod(i, 3) /= 0, i = 1, size(f%x))]
            call c%init(f%x, f%y, is_valid=keep)
            call check(error, all(same_bits(pf_interp(f%x, f%y, probes, is_valid=keep), c%eval(probes))), &
                       "pf_interp with a mask differed from the object, fixture " // trim(f%name))
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

    !> Builds variant `v` of `VARIANT_NAMES` over `(x, y)` under the policy `outside`, clamped, when it
    !! is the clamped variant, with `slopes`.
    subroutine init_variant(c, v, x, y, slopes, outside, is_valid)
        type(pf_interp_1d), intent(out)   :: c           !! the interpolant to build
        integer, intent(in)               :: v           !! the variant, 1 to `GI_NV`
        real(real64), intent(in)          :: x(:)        !! abscissae
        real(real64), intent(in)          :: y(:)        !! ordinates
        real(real64), intent(in)          :: slopes(:)   !! two end slopes, read by the clamped variant only
        character(len=*), intent(in)      :: outside     !! the policy token
        logical, intent(in), optional     :: is_valid(:) !! the mask, when one is wanted

        select case (v)
        case (GI_LINEAR)
            call c%init(x, y, method="linear", outside=outside, is_valid=is_valid)
        case (GI_NATURAL)
            call c%init(x, y, bc="natural", outside=outside, is_valid=is_valid)
        case (GI_NOT_A_KNOT)
            call c%init(x, y, bc="not_a_knot", outside=outside, is_valid=is_valid)
        case (GI_CLAMPED)
            call c%init(x, y, bc="clamped", slopes=slopes, outside=outside, is_valid=is_valid)
        case default
            call c%init(x, y, method="pchip", outside=outside, is_valid=is_valid)
        end select

    end subroutine init_variant

    !> `pf_interp`'s array form for variant `v`, with the arguments `init_variant` would pass.
    function one_shot(v, x, y, slopes, outside, xq) result(yq)
        integer, intent(in)          :: v         !! the variant, 1 to `GI_NV`
        real(real64), intent(in)     :: x(:)      !! abscissae
        real(real64), intent(in)     :: y(:)      !! ordinates
        real(real64), intent(in)     :: slopes(:) !! two end slopes, read by the clamped variant only
        character(len=*), intent(in) :: outside   !! the policy token
        real(real64), intent(in)     :: xq(:)     !! the queries
        real(real64)                 :: yq(size(xq)) !! the answers

        select case (v)
        case (GI_LINEAR)
            yq = pf_interp(x, y, xq, method="linear", outside=outside)
        case (GI_NATURAL)
            yq = pf_interp(x, y, xq, bc="natural", outside=outside)
        case (GI_NOT_A_KNOT)
            yq = pf_interp(x, y, xq, bc="not_a_knot", outside=outside)
        case (GI_CLAMPED)
            yq = pf_interp(x, y, xq, bc="clamped", slopes=slopes, outside=outside)
        case default
            yq = pf_interp(x, y, xq, method="pchip", outside=outside)
        end select

    end function one_shot

    !> `pf_interp`'s scalar form for variant `v`, with the arguments `init_variant` would pass.
    function one_shot_at(v, x, y, slopes, outside, xq) result(yq)
        integer, intent(in)          :: v         !! the variant, 1 to `GI_NV`
        real(real64), intent(in)     :: x(:)      !! abscissae
        real(real64), intent(in)     :: y(:)      !! ordinates
        real(real64), intent(in)     :: slopes(:) !! two end slopes, read by the clamped variant only
        character(len=*), intent(in) :: outside   !! the policy token
        real(real64), intent(in)     :: xq        !! the query
        real(real64)                 :: yq        !! the answer

        select case (v)
        case (GI_LINEAR)
            yq = pf_interp(x, y, xq, method="linear", outside=outside)
        case (GI_NATURAL)
            yq = pf_interp(x, y, xq, bc="natural", outside=outside)
        case (GI_NOT_A_KNOT)
            yq = pf_interp(x, y, xq, bc="not_a_knot", outside=outside)
        case (GI_CLAMPED)
            yq = pf_interp(x, y, xq, bc="clamped", slopes=slopes, outside=outside)
        case default
            yq = pf_interp(x, y, xq, method="pchip", outside=outside)
        end select

    end function one_shot_at

    !> Asserts the golden rows of variant `v` on fixture `f` inside the table: the value, both
    !! derivatives and the integrals, each against the scale of its own quantity over the table --
    !! the largest ordinate for a value, the largest golden derivative of that order (or the largest
    !! ordinate, if larger) for a derivative, and the span times the largest ordinate for an integral.
    !!
    !! Building the interpolant and every evaluation are also asserted to raise neither IEEE_INVALID
    !! nor IEEE_DIVIDE_BY_ZERO: a fixture's table is valid and every query inside it, so a flag there
    !! is a quotient or a product the library formed and discarded, which ends a program under nagfor's
    !! default traps. `M12`'s flat run is where a PCHIP mean formed speculatively divides `0/0`.
    subroutine check_golden_rows(error, f, v)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        type(golden_fixture), intent(in)           :: f     !! the fixture
        integer, intent(in)                        :: v     !! the variant, 1 to `GI_NV`

        type(pf_interp_1d)            :: c
        real(real64)                  :: ymax, got(GI_NI), value(GI_NQ), d1(GI_NQ), d2(GI_NQ)
        character(len=:), allocatable :: what
        logical                       :: can_test, saved(2), raised(2)
        integer                       :: i

        what = ", variant " // trim(VARIANT_NAMES(v)) // ", fixture " // trim(f%name)
        ymax = maxval(abs(f%y))
        call exceptions_begin(can_test, saved)
        call init_variant(c, v, f%x, f%y, f%slopes, "clamp")
        value = c%eval(f%q)
        d1 = c%derivative(f%q)
        d2 = c%derivative(f%q, 2)
        do i = 1, GI_NI
            got(i) = c%integral(f%a(i), f%b(i))
        end do
        call exceptions_end(can_test, saved, raised)
        call check(error, .not. any(raised), &
                   "building or evaluating inside the table raised IEEE_INVALID or IEEE_DIVIDE_BY_ZERO" // what)
        if (allocated(error)) return
        call check(error, all(close_to(value, f%value(:, v), ymax)), &
                   "the value disagrees with the golden rows" // what)
        if (allocated(error)) return
        call check(error, all(close_to(d1, f%d1(:, v), max(maxval(abs(f%d1(:, v))), ymax))), &
                   "the first derivative disagrees with the golden rows" // what)
        if (allocated(error)) return
        call check(error, all(close_to(d2, f%d2(:, v), max(maxval(abs(f%d2(:, v))), ymax))), &
                   "the second derivative disagrees with the golden rows" // what)
        if (allocated(error)) return
        call check(error, all(close_to(got, f%integral(:, v), (f%x(size(f%x)) - f%x(1))*ymax)), &
                   "the integral disagrees with the golden rows" // what)

    end subroutine check_golden_rows

    !> Asserts that `c`, built over a table sampled from `x**3 - 2*x`, reproduces that cubic, its slope
    !! and its curvature at every point of `p`, to `1e-12` relative to the larger of the answer and the
    !! quantity's scale over the points.
    subroutine check_reproduces_the_cubic(error, c, p, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        type(pf_interp_1d), intent(in)             :: c     !! the interpolant
        real(real64), intent(in)                   :: p(:)  !! the points
        character(len=*), intent(in)               :: what  !! the variant, for the messages

        real(real64), allocatable :: want(:)

        want = p**3 - 2.0_real64*p
        call check(error, all(abs(c%eval(p) - want) <= 1.0e-12_real64*max(abs(want), maxval(abs(want)))), &
                   what // " did not reproduce x**3 - 2x")
        if (allocated(error)) return
        want = 3.0_real64*p**2 - 2.0_real64
        call check(error, all(abs(c%derivative(p) - want) <= 1.0e-12_real64*max(abs(want), maxval(abs(want)))), &
                   what // " did not reproduce the slope of x**3 - 2x")
        if (allocated(error)) return
        want = 6.0_real64*p
        call check(error, all(abs(c%derivative(p, 2) - want) <= 1.0e-12_real64*max(abs(want), maxval(abs(want)))), &
                   what // " did not reproduce the curvature of x**3 - 2x")

    end subroutine check_reproduces_the_cubic

    !> Asserts both derivatives at `p` against difference quotients taken on the one polynomial piece
    !! that holds `p`, and counts the stencil; a point on a knot has no such piece and is skipped.
    !!
    !! The piece is the segment's, extended to minus infinity for the first segment and to plus
    !! infinity for the last, which is what `"extrapolate"` evaluates there. With `e` the step, the
    !! evaluations are within a few ulp of `M`, the larger of the table's largest ordinate and the
    !! largest value the stencil meets, so Richardson's quotient is within `12` ulp of `M` over `e`
    !! (`1.5` times the error of one evaluation, doubled, over the step) and the central difference of
    !! the slope within a few ulp of the largest slope it meets over `e`.
    subroutine check_difference_quotients(error, c, x, ymax, p, stencils)
        type(error_type), allocatable, intent(out) :: error    !! test-drive's error handle.
        type(pf_interp_1d), intent(in)             :: c        !! the interpolant, under `"extrapolate"`
        real(real64), intent(in)                   :: x(:)     !! its knots
        real(real64), intent(in)                   :: ymax     !! its largest ordinate's magnitude
        real(real64), intent(in)                   :: p        !! the point
        integer, intent(inout)                     :: stencils !! stencils taken so far

        real(real64) :: room, e, f(-2:2), g(-1:1), slope, curvature, richardson, central, big
        integer      :: n, k, j

        n = size(x)
        if (p < x(2)) then
            room = x(2) - p
        else if (p > x(n - 1)) then
            room = p - x(n - 1)
        else
            k = count(x <= p)
            room = min(p - x(k), x(k + 1) - p)
        end if
        if (.not. (room > 0.0_real64)) return
        stencils = stencils + 1

        ! A power of two at most a quarter of the room, so that `p +- 2*e` stays on the piece.
        e = scale(1.0_real64, exponent(room) - 3)
        do j = -2, 2
            f(j) = c%eval(p + real(j, real64)*e)
        end do
        slope = c%derivative(p)
        richardson = (4.0_real64*(f(1) - f(-1))/(2.0_real64*e) - (f(2) - f(-2))/(4.0_real64*e))/3.0_real64
        big = max(maxval(abs(f)), ymax)
        call check(error, abs(richardson - slope) <= ROUNDING*(big/e + abs(slope)), &
                   "the first derivative disagrees with Richardson's difference quotient of the value")
        if (allocated(error)) return

        do j = -1, 1
            g(j) = c%derivative(p + real(j, real64)*e)
        end do
        curvature = c%derivative(p, 2)
        central = (g(1) - g(-1))/(2.0_real64*e)
        big = max(maxval(abs(g)), ymax/e)
        call check(error, abs(central - curvature) <= ROUNDING*(big/e + abs(curvature)), &
                   "the second derivative disagrees with the central difference of the first")

    end subroutine check_difference_quotients

    !> Asserts `%integral(a, b)` against Simpson's rule on every piece between the limits and the
    !! knots, and against `-%integral(b, a)` bit for bit; counts the pieces.
    !!
    !! Simpson's rule is exact for a cubic, so the difference is rounding: each of the `k` pieces
    !! combines three evaluations within a few ulp of the larger of the table's largest ordinate and
    !! the values it meets, and the library's own sum adds a piece per segment.
    subroutine check_simpson(error, c, x, ymax, a, b, pieces)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error handle.
        type(pf_interp_1d), intent(in)             :: c      !! the interpolant
        real(real64), intent(in)                   :: x(:)   !! its knots
        real(real64), intent(in)                   :: ymax   !! its largest ordinate's magnitude
        real(real64), intent(in)                   :: a      !! one limit
        real(real64), intent(in)                   :: b      !! the other
        integer, intent(inout)                     :: pieces !! pieces summed so far

        real(real64) :: lo, hi, cuts(size(x) + 2), simpson, magnitude, left, middle, right, got
        integer      :: j, m

        lo = min(a, b)
        hi = max(a, b)
        m = 1
        cuts(1) = lo
        do j = 1, size(x)
            if (x(j) > lo .and. x(j) < hi) then
                m = m + 1
                cuts(m) = x(j)
            end if
        end do
        m = m + 1
        cuts(m) = hi
        simpson = 0.0_real64
        magnitude = 0.0_real64
        do j = 1, m - 1
            left = c%eval(cuts(j))
            middle = c%eval(0.5_real64*(cuts(j) + cuts(j + 1)))
            right = c%eval(cuts(j + 1))
            simpson = simpson + (cuts(j + 1) - cuts(j))/6.0_real64*(left + 4.0_real64*middle + right)
            magnitude = magnitude + (cuts(j + 1) - cuts(j))*max(abs(left), abs(middle), abs(right), ymax)
            pieces = pieces + 1
        end do
        if (b < a) simpson = -simpson

        got = c%integral(a, b)
        call check(error, abs(got - simpson) <= ROUNDING*real(m, real64)*magnitude, &
                   "the integral disagrees with Simpson's rule on the pieces between the knots")
        if (allocated(error)) return
        call check(error, same_bits(c%integral(b, a), -got), "swapping the limits must negate the integral exactly")

    end subroutine check_simpson

    !> Asserts that the integral over `[a, c]` is the sum of those over `[a, b]` and `[b, c]`, to the
    !! rounding of three sums of as many segments as the table has, in the magnitude of the largest
    !! value on `[a, c]` times its width.
    subroutine check_additive(error, obj, x, ymax, a, b, c)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        type(pf_interp_1d), intent(in)             :: obj   !! the interpolant
        real(real64), intent(in)                   :: x(:)  !! its knots
        real(real64), intent(in)                   :: ymax  !! its largest ordinate's magnitude
        real(real64), intent(in)                   :: a     !! the lower limit
        real(real64), intent(in)                   :: b     !! the split point
        real(real64), intent(in)                   :: c     !! the upper limit

        real(real64) :: whole, parts, magnitude
        integer      :: j

        whole = obj%integral(a, c)
        parts = obj%integral(a, b) + obj%integral(b, c)
        magnitude = ymax
        do j = 0, 64
            magnitude = max(magnitude, abs(obj%eval(a + (c - a)*real(j, real64)/64.0_real64)))
        end do
        call check(error, abs(whole - parts) <= ROUNDING*real(size(x), real64)*(c - a)*magnitude, &
                   "the integral over [a, c] is not the sum of the integrals over [a, b] and [b, c]")

    end subroutine check_additive

    !> Agreement with a golden expectation to `GOLDEN_RTOL`, relative to the larger of the
    !! expectation and `scale`.
    elemental function close_to(got, want, scale) result(ok)
        real(real64), intent(in) :: got   !! what the library answered
        real(real64), intent(in) :: want  !! the golden expectation
        real(real64), intent(in) :: scale !! the quantity's scale over the table
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

    !> Saves IEEE_INVALID and IEEE_DIVIDE_BY_ZERO and clears both, ahead of calls that must raise neither.
    subroutine exceptions_begin(can_test, saved)
        logical, intent(out) :: can_test !! the processor supports both flags for `real64`
        logical, intent(out) :: saved(2) !! the two flags as they were, restored by `exceptions_end`

        can_test = ieee_support_flag(ieee_invalid, 0.0_real64) .and. ieee_support_flag(ieee_divide_by_zero, 0.0_real64)
        saved = .false.
        if (can_test) then
            call ieee_get_flag([ieee_invalid, ieee_divide_by_zero], saved)
            call ieee_set_flag([ieee_invalid, ieee_divide_by_zero], .false.)
        end if

    end subroutine exceptions_begin

    !> Reads IEEE_INVALID and IEEE_DIVIDE_BY_ZERO after the calls under test, and restores each as
    !! `saved .or. raised`.
    subroutine exceptions_end(can_test, saved, raised)
        logical, intent(in)  :: can_test  !! from `exceptions_begin`
        logical, intent(in)  :: saved(2)  !! from `exceptions_begin`
        logical, intent(out) :: raised(2) !! the calls under test raised IEEE_INVALID, IEEE_DIVIDE_BY_ZERO

        raised = .false.
        if (can_test) then
            call ieee_get_flag([ieee_invalid, ieee_divide_by_zero], raised)
            call ieee_set_flag([ieee_invalid, ieee_divide_by_zero], saved .or. raised)
        end if

    end subroutine exceptions_end

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
