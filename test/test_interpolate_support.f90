!> The tables `test_interpolate.f90`, `test_interpolate_omp.f90` and `test/error_scenarios.f90`
!> share, with the closed form of each beside it.
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

    public :: golden_table, GOLDEN_TABLES
    public :: sine_table, SINE_LINEAR_BOUND, SINE_CUBIC_BOUND
    public :: bent_grid, BENT_AMPLITUDE
    public :: line_2x1
    public :: nan_value, positive_infinity
    public :: METHODS, POLICIES

    !> The golden fixtures `golden_table` hands out, numbered 1 to this.
    integer, parameter :: GOLDEN_TABLES = 3

    !> The two methods a one-dimensional interpolant offers, as their tokens.
    character(len=6), parameter :: METHODS(2) = [character(len=6) :: "linear", "cubic"]
    !> The three out-of-range policies, as their tokens.
    character(len=11), parameter :: POLICIES(3) = [character(len=11) :: "clamp", "extrapolate", "nan"]

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

    !> One of the golden fixtures of `test_interpolate_golden`, with its queries and expectations.
    !!
    !! `which` runs from 1 to `GOLDEN_TABLES`. `linear` and `natural` are the expectations at `q`;
    !! `linear_out` and `natural_out` those at `o` under `outside="extrapolate"`.
    subroutine golden_table(which, name, x, y, q, o, linear, linear_out, natural, natural_out)
        integer, intent(in)                    :: which       !! the fixture, 1 to GOLDEN_TABLES
        character(len=3), intent(out)          :: name        !! its name in the golden file
        real(real64), allocatable, intent(out) :: x(:)        !! abscissae, strictly increasing
        real(real64), allocatable, intent(out) :: y(:)        !! ordinates
        real(real64), intent(out)              :: q(GI_NQ)    !! interior queries
        real(real64), intent(out)              :: o(GI_NO)    !! outside queries
        real(real64), intent(out)              :: linear(GI_NQ)      !! linear at `q`
        real(real64), intent(out)              :: linear_out(GI_NO)  !! linear extrapolated at `o`
        real(real64), intent(out)              :: natural(GI_NQ)     !! natural spline at `q`
        real(real64), intent(out)              :: natural_out(GI_NO) !! natural spline extrapolated at `o`

        select case (which)
        case (1)
            name = "U9"
            x = U9_X
            y = U9_Y
            q = U9_Q
            o = U9_O
            linear = U9_LINEAR
            linear_out = U9_LINEAR_OUT
            natural = U9_NATURAL
            natural_out = U9_NATURAL_OUT
        case (2)
            name = "G16"
            x = G16_X
            y = G16_Y
            q = G16_Q
            o = G16_O
            linear = G16_LINEAR
            linear_out = G16_LINEAR_OUT
            natural = G16_NATURAL
            natural_out = G16_NATURAL_OUT
        case default
            name = "R33"
            x = R33_X
            y = R33_Y
            q = R33_Q
            o = R33_O
            linear = R33_LINEAR
            linear_out = R33_LINEAR_OUT
            natural = R33_NATURAL
            natural_out = R33_NATURAL_OUT
        end select

    end subroutine golden_table

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
