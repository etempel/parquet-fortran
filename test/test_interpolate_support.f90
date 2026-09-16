!> The tables `test_interpolate.f90`, `test_interpolate_omp.f90` and `test/error_scenarios.f90`
!> share, with the closed form of each beside it, and the golden fixtures and grids gathered one per
!> object.
!!
!! **No expected value here is read off a run.** A table is either a golden fixture, whose
!! expectations `tools/generate_interpolate_vectors.py` derives exactly over rationals, or a
!! function whose interpolant's error has a closed-form bound stated where the table is built.
!!
!! It imports no library module at all, only the generated `test_interpolate_golden`: a scenario
!! and a test must be able to name the same table without either of them reaching the facade, and
!! `check_test_runner_partition` requires the files feeding `run_tester_pf` to reach no `bind(C)`
!! call.
module test_interpolate_support

    use test_interpolate_golden
    use iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf

    implicit none
    private

    public :: golden_fixture, golden_fixture_get, GOLDEN_TABLES
    public :: golden_grid, golden_grid_get, GOLDEN_GRIDS
    public :: VARIANT_NAMES, GRID_VARIANT_NAMES, POLICIES
    public :: sine_table, SINE_LINEAR_BOUND, SINE_CUBIC_BOUND
    public :: bent_grid, BENT_AMPLITUDE
    public :: line_2x1
    public :: nan_value, positive_infinity

    !> The golden fixtures `golden_fixture_get` hands out, numbered 1 to this.
    integer, parameter :: GOLDEN_TABLES = 5
    !> The golden grids `golden_grid_get` hands out, numbered 1 to this.
    integer, parameter :: GOLDEN_GRIDS = 3

    !> The variants a one-dimensional interpolant offers, one per column of every golden expectation
    !! array and in the same order: `method="linear"`; `method="cubic"` under `bc="natural"`,
    !! `bc="not_a_knot"` and `bc="clamped"`; and `method="pchip"`.
    character(len=10), parameter :: VARIANT_NAMES(GI_NV) = [character(len=10) :: "linear", "natural", "not_a_knot", &
                                                            "clamped", "pchip"]
    !> The variants a two-dimensional interpolant offers, one per column of every golden grid
    !! expectation array and in the same order: `method="linear"`, and `method="cubic"` under
    !! `bc="natural"` and `bc="not_a_knot"`.
    character(len=10), parameter :: GRID_VARIANT_NAMES(G2_NV) = [character(len=10) :: "linear", "natural", &
                                                                 "not_a_knot"]
    !> The three out-of-range policies, as their tokens.
    character(len=11), parameter :: POLICIES(3) = [character(len=11) :: "clamp", "extrapolate", "nan"]

    !> One golden fixture of `test_interpolate_golden`: its table, queries and limits, and every
    !! expectation, one column per variant of `VARIANT_NAMES`.
    type :: golden_fixture
        character(len=3)          :: name = ""                        !! its name in the golden file
        real(real64), allocatable :: x(:)                             !! abscissae, strictly increasing
        real(real64), allocatable :: y(:)                             !! ordinates
        real(real64) :: slopes(2) = 0.0_real64                        !! end slopes for `bc="clamped"`
        real(real64) :: q(GI_NQ) = 0.0_real64                         !! interior queries
        real(real64) :: o(GI_NO) = 0.0_real64                         !! outside queries
        real(real64) :: a(GI_NI) = 0.0_real64                         !! lower limits inside the table
        real(real64) :: b(GI_NI) = 0.0_real64                         !! upper limits inside the table
        real(real64) :: oa(GI_NJ) = 0.0_real64                        !! lower limits reaching beyond it
        real(real64) :: ob(GI_NJ) = 0.0_real64                        !! upper limits reaching beyond it
        real(real64) :: value(GI_NQ, GI_NV) = 0.0_real64              !! the value at each of `q`
        real(real64) :: value_out(GI_NO, GI_NV) = 0.0_real64          !! extrapolated, at each of `o`
        real(real64) :: d1(GI_NQ, GI_NV) = 0.0_real64                 !! the first derivative at `q`
        real(real64) :: d2(GI_NQ, GI_NV) = 0.0_real64                 !! the second derivative at `q`
        real(real64) :: d1_out(GI_NO, GI_NV) = 0.0_real64             !! extrapolated, at `o`
        real(real64) :: d2_out(GI_NO, GI_NV) = 0.0_real64             !! extrapolated, at `o`
        real(real64) :: integral(GI_NI, GI_NV) = 0.0_real64           !! over each `(a, b)`
        real(real64) :: integral_out(GI_NJ, GI_NV) = 0.0_real64       !! extrapolated, over `(oa, ob)`
        real(real64) :: integral_clamp(GI_NJ, GI_NV) = 0.0_real64     !! clamped, over `(oa, ob)`
    end type golden_fixture

    !> One golden grid of `test_interpolate_golden`: its lines, values and queries, and every
    !! expectation, one column per variant of `GRID_VARIANT_NAMES`.
    type :: golden_grid
        character(len=3)          :: name = ""                        !! its name in the golden file
        real(real64), allocatable :: x(:)                             !! lines along `x`, strictly increasing
        real(real64), allocatable :: y(:)                             !! lines along `y`, strictly increasing
        real(real64), allocatable :: z(:, :)                          !! values, `z(i, j)` at `(x(i), y(j))`
        real(real64) :: xq(G2_NQ) = 0.0_real64                        !! the x of each query inside the grid
        real(real64) :: yq(G2_NQ) = 0.0_real64                        !! the y of each query inside the grid
        real(real64) :: xo(G2_NO) = 0.0_real64                        !! the x of each query beyond it
        real(real64) :: yo(G2_NO) = 0.0_real64                        !! the y of each query beyond it
        real(real64) :: value(G2_NQ, G2_NV) = 0.0_real64              !! the value at each query inside
        real(real64) :: value_out(G2_NO, G2_NV) = 0.0_real64          !! extrapolated, at each query beyond
        real(real64) :: value_clamp(G2_NO, G2_NV) = 0.0_real64        !! clamped, at each query beyond
    end type golden_grid

    !> How far the linear interpolant of `sin` on unit-spaced knots may stray from `sin`: the
    !! piecewise-linear error bound `h**2/8 * max|f''|`, with `h = 1` and `max|sin''| = 1`.
    real(real64), parameter :: SINE_LINEAR_BOUND = 0.125_real64
    !> How far the natural cubic spline of `sin` on the knots `0..30` may stray from `sin` on
    !! `[3, 27]`, three knots in from either end.
    !!
    !! Two terms. The complete spline's error bound, `5/384 * h**4 * max|f''''|`, is `5/384` with
    !! `h = 1`. The natural condition then sets `m = 0` where `sin''` is `-sin(30)`, a second-derivative
    !! error of at most 1 that decays by the factor `2 - sqrt(3)` per knot into the table, and a
    !! second-derivative error `e` moves a value by at most `e*h**2/(9*sqrt(3))`, the largest
    !! `|b**3 - b|/6` on `[0, 1]`. Three knots in, that is `(2 - sqrt(3))**3/(9*sqrt(3))`, about
    !! `0.00123`. The sum is `0.014255`; this is it rounded up in the last place.
    real(real64), parameter :: SINE_CUBIC_BOUND = 0.01426_real64

    !> Amplitude of `bent_grid`'s bend, in steps. Small enough that every spacing stays within the
    !! uniformity tolerance `0.1*h/n` of the first -- the largest departure is `4*pi*A/(n - 1)` of a
    !! step, `1.5e-5` against a tolerance of `2.4e-5` for `n = 4097` -- and large enough that knots
    !! sit up to a two-hundredth of a step either side of the straight line through the ends, where
    !! an arithmetic bracket guesses one segment too high or one too low.
    real(real64), parameter :: BENT_AMPLITUDE = 0.005_real64

contains

    !> Golden fixture `which` of `test_interpolate_golden`, numbered 1 to `GOLDEN_TABLES`.
    subroutine golden_fixture_get(which, f)
        integer, intent(in)               :: which !! the fixture, 1 to `GOLDEN_TABLES`
        type(golden_fixture), intent(out) :: f     !! receives it

        select case (which)
        case (1)
            f = golden_fixture("U9", U9_X, U9_Y, U9_SLOPES, U9_Q, U9_O, U9_A, U9_B, U9_OA, U9_OB, U9_VALUE, &
                               U9_VALUE_OUT, U9_D1, U9_D2, U9_D1_OUT, U9_D2_OUT, U9_INTEGRAL, U9_INTEGRAL_OUT, &
                               U9_INTEGRAL_CLAMP)
        case (2)
            f = golden_fixture("G16", G16_X, G16_Y, G16_SLOPES, G16_Q, G16_O, G16_A, G16_B, G16_OA, G16_OB, G16_VALUE, &
                               G16_VALUE_OUT, G16_D1, G16_D2, G16_D1_OUT, G16_D2_OUT, G16_INTEGRAL, G16_INTEGRAL_OUT, &
                               G16_INTEGRAL_CLAMP)
        case (3)
            f = golden_fixture("R33", R33_X, R33_Y, R33_SLOPES, R33_Q, R33_O, R33_A, R33_B, R33_OA, R33_OB, R33_VALUE, &
                               R33_VALUE_OUT, R33_D1, R33_D2, R33_D1_OUT, R33_D2_OUT, R33_INTEGRAL, R33_INTEGRAL_OUT, &
                               R33_INTEGRAL_CLAMP)
        case (4)
            f = golden_fixture("M12", M12_X, M12_Y, M12_SLOPES, M12_Q, M12_O, M12_A, M12_B, M12_OA, M12_OB, M12_VALUE, &
                               M12_VALUE_OUT, M12_D1, M12_D2, M12_D1_OUT, M12_D2_OUT, M12_INTEGRAL, M12_INTEGRAL_OUT, &
                               M12_INTEGRAL_CLAMP)
        case default
            f = golden_fixture("K4", K4_X, K4_Y, K4_SLOPES, K4_Q, K4_O, K4_A, K4_B, K4_OA, K4_OB, K4_VALUE, &
                               K4_VALUE_OUT, K4_D1, K4_D2, K4_D1_OUT, K4_D2_OUT, K4_INTEGRAL, K4_INTEGRAL_OUT, &
                               K4_INTEGRAL_CLAMP)
        end select

    end subroutine golden_fixture_get

    !> Golden grid `which` of `test_interpolate_golden`, numbered 1 to `GOLDEN_GRIDS`.
    subroutine golden_grid_get(which, g)
        integer, intent(in)            :: which !! the grid, 1 to `GOLDEN_GRIDS`
        type(golden_grid), intent(out) :: g     !! receives it

        select case (which)
        case (1)
            g = golden_grid("N57", N57_X, N57_Y, N57_Z, N57_XQ, N57_YQ, N57_XO, N57_YO, N57_VALUE, N57_VALUE_OUT, &
                            N57_VALUE_CLAMP)
        case (2)
            g = golden_grid("E86", E86_X, E86_Y, E86_Z, E86_XQ, E86_YQ, E86_XO, E86_YO, E86_VALUE, E86_VALUE_OUT, &
                            E86_VALUE_CLAMP)
        case default
            g = golden_grid("S55", S55_X, S55_Y, S55_Z, S55_XQ, S55_YQ, S55_XO, S55_YO, S55_VALUE, S55_VALUE_OUT, &
                            S55_VALUE_CLAMP)
        end select

    end subroutine golden_grid_get

    !> `sin` tabulated at the integers `0..30`, qfeet's own spline fixture.
    subroutine sine_table(x, y)
        real(real64), intent(out) :: x(31) !! the knots, `0..30`
        real(real64), intent(out) :: y(31) !! `sin` at each

        integer :: i

        do i = 1, 31
            x(i) = real(i - 1, real64)
            y(i) = sin(x(i))
        end do

    end subroutine sine_table

    !> `n` knots from 0 to `n - 1` bent off the straight line by up to `BENT_AMPLITUDE` of a step,
    !! above it in the first half and below it in the second, and `sin(x/8)` at each.
    !!
    !! The ends sit on the straight line, so the mean step is one; knot `i` sits at
    !! `i - 1 + A*sin(2*pi*(i - 1)/(n - 1))`. The table is uniform to the module's tolerance and not
    !! exactly even, which is the case an arithmetic bracket has to be corrected on -- downward
    !! where the knots lie above the line, upward where they lie below it. The ordinates are not
    !! dyadic, so a segment formula evaluated at the wrong end of a segment shows in the last bits.
    subroutine bent_grid(n, x, y)
        integer, intent(in)       :: n    !! knots
        real(real64), intent(out) :: x(n) !! the bent knots, strictly increasing
        real(real64), intent(out) :: y(n) !! `sin(x/8)` at each

        real(real64), parameter :: PI = 4.0_real64*atan(1.0_real64)
        integer :: i

        do i = 1, n
            x(i) = real(i - 1, real64) + BENT_AMPLITUDE*sin(2.0_real64*PI*real(i - 1, real64)/real(n - 1, real64))
            y(i) = sin(x(i)/8.0_real64)
        end do
        ! The last knot exactly on the line: `sin(2*pi)` is not an exact zero in floating point.
        x(n) = real(n - 1, real64)
        y(n) = sin(x(n)/8.0_real64)

    end subroutine bent_grid

    !> `2*x + 1`, which a linear interpolant and a natural cubic spline both reproduce.
    elemental function line_2x1(x) result(v)
        real(real64), intent(in) :: x !! the point
        real(real64)             :: v !! the line's value there

        v = 2.0_real64*x + 1.0_real64

    end function line_2x1

    !> A quiet NaN, built with `ieee_value` so no fixture's own arithmetic raises a flag.
    function nan_value() result(v)
        real(real64) :: v !! a quiet NaN

        v = ieee_value(1.0_real64, ieee_quiet_nan)

    end function nan_value

    !> Positive infinity, built with `ieee_value` rather than by overflowing an expression.
    function positive_infinity() result(v)
        real(real64) :: v !! `+Infinity`

        v = ieee_value(1.0_real64, ieee_positive_inf)

    end function positive_infinity

end module test_interpolate_support
