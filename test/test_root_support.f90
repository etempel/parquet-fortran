!> The functions `test_root.f90` and `test/error_scenarios.f90` share, as module procedures and as
!> `pf_rootfun` extensions.
!!
!! **Every callback here is a module procedure or a type-bound procedure, never an internal one.**
!! flang SIGSEGVs before the callee runs when an internal procedure is passed as an actual
!! argument, and gfortran implements one with a trampoline that makes the binary demand an
!! executable stack (`fortran-gotchas.md`). That is the whole library's callback convention, so the
!! tests are written the way the guide tells a user to write them.
!!
!! **The root of every function here is stated beside it**, so that no test asserts a location
!! that was read off a run. **No function here raises a floating-point flag on any argument a test
!! passes it**: an infinity is built with `ieee_value`, a pole is formed without dividing by zero,
!! and a distance is taken in halves, so a flag a test reads is the solver's own.
!!
!! Its only library import is `use parquet_root`: a scenario and a test must be able to name the
!! same function without either of them reaching the facade.
module test_root_support

    use parquet_root, only : pf_rootfun
    use iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
        ieee_negative_inf

    implicit none
    private

    public :: root_sq2, root_line_03, root_two_roots, root_gentle, root_cos_minus_x
    public :: root_cubic, root_exp_minus_2, root_log_plus_x, root_x_minus_tanh
    public :: root_neg_inf_tail, root_inf_step, root_inf_both_tails, root_tiny_line, root_huge_line
    public :: root_pole
    public :: root_tiny_root, root_cube, root_steep, root_nan_beyond_two
    public :: root_even_no_root
    public :: shifted_line, counted_sq2, shaped_root
    public :: TINY_ROOT, CUBIC_ROOT, OMEGA, TANH_ROOT, DOTTIE

    !> The Dottie number, the root of `cos(x) - x`.
    real(real64), parameter :: DOTTIE = 0.7390851332151607_real64
    !> The root of `root_tiny_root`: a squared bandwidth on unit-scaled data is of this order.
    real(real64), parameter :: TINY_ROOT = 1.234567e-9_real64
    !> The real root of `x**3 - 2*x - 5`, Wallis's cubic.
    real(real64), parameter :: CUBIC_ROOT = 2.0945514815423265_real64
    !> The omega constant, the root of `log(x) + x`: `W(1)`.
    real(real64), parameter :: OMEGA = 0.5671432904097838_real64
    !> The positive root of `x - tanh(2*x)`.
    real(real64), parameter :: TANH_ROOT = 0.9575040240772687_real64

    !> `x - root`, counting its own evaluations: the object form with a parameter.
    type, extends(pf_rootfun) :: shifted_line
        real(real64) :: root = 0.0_real64 !! where the line crosses zero
        integer      :: calls = 0         !! evaluations, written by `eval` and read by the caller
    contains
        procedure :: eval => shifted_line_eval !! Evaluates `x - root` and counts the call.
    end type shifted_line

    !> `root_sq2` as an object, counting its evaluations. Its `eval` CALLS `root_sq2`, so the two
    !! forms evaluate the same arithmetic and must agree to the bit.
    type, extends(pf_rootfun) :: counted_sq2
        integer :: calls = 0 !! evaluations, written by `eval` and read by the caller
    contains
        procedure :: eval => counted_sq2_eval !! Evaluates `x*x - 2` and counts the call.
    end type counted_sq2

    !> One of several shapes with a sign change at `root`, formed so that no argument in the finite
    !! range makes it raise a flag: the stress fixture for the flag test.
    !!
    !! `kind` 1 is a line scaled by `scale <= 1`; 2 a unit step; 3 a step from `-Infinity` to
    !! `+Infinity`; 4 a cubic in a unit-bounded variable; 5 a step from `-1e-300` to `+1e300`; 6 a
    !! line whose tail above `2*root` is `-Infinity`, the shape of the Improved Sheather-Jones fixed
    !! point. The signed distance is taken in halves, so it is finite for every pair of arguments.
    type, extends(pf_rootfun) :: shaped_root
        real(real64) :: root = 0.0_real64  !! where the sign changes
        real(real64) :: scale = 1.0_real64 !! the slope of kinds 1 and 6; at most 1
        integer      :: kind = 1           !! which shape, 1 to 6
    contains
        procedure :: eval => shaped_root_eval !! Evaluates the chosen shape.
    end type shaped_root

contains

    !> `x*x - 2`; root `sqrt(2)`.
    function root_sq2(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = x*x - 2.0_real64

    end function root_sq2

    !> `x - 0.3`; root `0.3`, far above a bracket of `[0, 1e-11]`: the shape the Improved
    !! Sheather-Jones rule's bracket search starts from.
    function root_line_03(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = x - 0.3_real64

    end function root_line_03

    !> `(x - 1)*(3 - x)`: negative below 1, positive between the roots 1 and 3, negative above.
    function root_two_roots(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = (x - 1.0_real64)*(3.0_real64 - x)

    end function root_two_roots

    !> `2 + x/huge`, between 1 and 3 for every finite `x`: no root anywhere, and no overflow at any
    !! argument, for the expansion tests that run to the ends of the range.
    function root_gentle(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = 2.0_real64 + x/huge(1.0_real64)

    end function root_gentle

    !> `x*x + 1`: EVEN, and positive everywhere, so a bracket centred on zero has no sign
    !! change and the SAME `|f|` at both ends. That tie is what `take_the_better_end`
    !! (`src/parquet_root_solve.f90`) settles on the lower end, which
    !! `doc/pages/utilities/root-finding.md` states under "Growing the bracket".
    function root_even_no_root(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = x*x + 1.0_real64

    end function root_even_no_root

    !> `cos(x) - x`; root `DOTTIE`.
    function root_cos_minus_x(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = cos(x) - x

    end function root_cos_minus_x

    !> `x**3 - 2*x - 5`, Wallis's cubic; root `CUBIC_ROOT`.
    function root_cubic(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = (x*x - 2.0_real64)*x - 5.0_real64

    end function root_cubic

    !> `exp(x) - 2`; root `log(2)`.
    function root_exp_minus_2(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = exp(x) - 2.0_real64

    end function root_exp_minus_2

    !> `log(x) + x` for `x > 0`; root `OMEGA`.
    function root_log_plus_x(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate, positive
        real(real64)             :: y !! the value there

        y = log(x) + x

    end function root_log_plus_x

    !> `x - tanh(2*x)`; positive root `TANH_ROOT`.
    function root_x_minus_tanh(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = x - tanh(2.0_real64*x)

    end function root_x_minus_tanh

    !> `1 - x` below 3 and `-Infinity` from 3 on: a root at 1 and an infinite value at the top of
    !! any bracket reaching past 3, the way a fixed point whose denominator collapses to zero looks.
    function root_neg_inf_tail(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        if (x < 3.0_real64) then
            y = 1.0_real64 - x
        else
            y = ieee_value(1.0_real64, ieee_negative_inf)
        end if

    end function root_neg_inf_tail

    !> `-Infinity` below 1 and `+Infinity` from 1 on: a sign change with no finite value anywhere,
    !! which only bisection can locate.
    function root_inf_step(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        if (x < 1.0_real64) then
            y = ieee_value(1.0_real64, ieee_negative_inf)
        else
            y = ieee_value(1.0_real64, ieee_positive_inf)
        end if

    end function root_inf_step

    !> `-Infinity` below 0, `x - 0.3` on `[0, 2]`, `+Infinity` above 2: a root at 0.3 between two
    !! infinite tails, so a bracket reaching into both has an infinite value on each side of a
    !! finite one -- the one arrangement where an interpolation formed through the infinities
    !! would divide one infinity by the other.
    function root_inf_both_tails(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        if (x < 0.0_real64) then
            y = ieee_value(1.0_real64, ieee_negative_inf)
        else if (x > 2.0_real64) then
            y = ieee_value(1.0_real64, ieee_positive_inf)
        else
            y = x - 0.3_real64
        end if

    end function root_inf_both_tails

    !> `1e-200*(x - 0.3)`; root `0.3`, with values whose product underflows to zero.
    function root_tiny_line(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = 1.0e-200_real64*(x - 0.3_real64)

    end function root_tiny_line

    !> `1e200*(x - 0.3)`; root `0.3`, with values whose product overflows.
    function root_huge_line(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = 1.0e200_real64*(x - 0.3_real64)

    end function root_huge_line

    !> `1/(x - 0.5)`: a POLE at 0.5 and no root at all. `+Infinity` at exactly 0.5, formed without
    !! dividing by zero: the divisor is replaced wherever it would be zero and STORED before the
    !! division reads it, because an optimiser may otherwise split the selection into a division
    !! on each arm and form both (`fortran-gotchas.md`).
    function root_pole(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        real(real64)           :: dist
        real(real64), volatile :: divisor !! `dist`, or one where `dist` is zero

        dist = x - 0.5_real64
        divisor = merge(dist, 1.0_real64, dist /= 0.0_real64)
        y = merge(ieee_value(1.0_real64, ieee_positive_inf), 1.0_real64/divisor, dist == 0.0_real64)

    end function root_pole

    !> `x*x - TINY_ROOT**2`; root `TINY_ROOT`, a squared bandwidth's order of magnitude.
    function root_tiny_root(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = x*x - TINY_ROOT*TINY_ROOT

    end function root_tiny_root

    !> `x**3`: a TRIPLE root at exactly zero, where no relative tolerance on `x` can ever be met --
    !! the bracket cannot become narrow relative to `|x|` when `x` is zero -- and the interpolation
    !! does not land on it, so the search bisects until `atol` stops it or the budget runs out.
    function root_cube(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = x*x*x

    end function root_cube

    !> `sinh(700*(x - 0.3))` on `[0, 1]`: a root at 0.3 with values from `-1e91` to `+1e212` either
    !! side of it, so the ratios the inverse quadratic step forms span the whole exponent range.
    function root_steep(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate, in `[0, 1]`
        real(real64)             :: y !! the value there

        y = sinh(700.0_real64*(x - 0.3_real64))

    end function root_steep

    !> `x - 1` up to 2 and a quiet NaN above it: a bracket reaching past 2 makes the function
    !! return a NaN, which the solver refuses.
    function root_nan_beyond_two(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        if (x <= 2.0_real64) then
            y = x - 1.0_real64
        else
            y = ieee_value(1.0_real64, ieee_quiet_nan)
        end if

    end function root_nan_beyond_two

    !> Evaluates `x - root` and counts the call.
    function shifted_line_eval(self, x) result(f)
        class(shifted_line), intent(inout) :: self !! the line, whose counter is updated
        real(real64), intent(in)           :: x    !! where to evaluate
        real(real64)                       :: f    !! the value there

        self%calls = self%calls + 1
        f = x - self%root

    end function shifted_line_eval

    !> Evaluates `x*x - 2` through `root_sq2` and counts the call.
    function counted_sq2_eval(self, x) result(f)
        class(counted_sq2), intent(inout) :: self !! the object, whose counter is updated
        real(real64), intent(in)          :: x    !! where to evaluate
        real(real64)                      :: f    !! the value there

        self%calls = self%calls + 1
        f = root_sq2(x)

    end function counted_sq2_eval

    !> Evaluates the shape `kind` names; see `shaped_root`.
    function shaped_root_eval(self, x) result(f)
        class(shaped_root), intent(inout) :: self !! the shape
        real(real64), intent(in)          :: x    !! where to evaluate
        real(real64)                      :: f    !! the value there

        real(real64) :: half_dist

        half_dist = (0.5_real64*x) - (0.5_real64*self%root)
        select case (self%kind)
        case (1)
            f = self%scale*half_dist
        case (2)
            f = merge(1.0_real64, -1.0_real64, x > self%root)
        case (3)
            f = merge(ieee_value(1.0_real64, ieee_positive_inf), &
                      ieee_value(1.0_real64, ieee_negative_inf), x > self%root)
        case (4)
            half_dist = half_dist/huge(1.0_real64)
            f = half_dist*half_dist*half_dist
        case (5)
            f = merge(1.0e300_real64, -1.0e-300_real64, x > self%root)
        case default
            ! Compared in halves: doubling the clamped root could overflow on the arm the
            ! clamp does not take, if an optimiser formed both.
            f = self%scale*half_dist
            if (self%root > 0.0_real64) then
                if (0.5_real64*x > min(self%root, 0.25_real64*huge(1.0_real64))) &
                    f = ieee_value(1.0_real64, ieee_negative_inf)
            end if
        end select

    end function shaped_root_eval

end module test_root_support
