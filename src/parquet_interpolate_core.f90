!> The workers every interpolant shares: the abort and its message helpers, the token folding, the
!> screens a table passes, the bracket search, the tridiagonal solver, the per-knot coefficients of
!> the cubic spline and of PCHIP, and every segment polynomial's value, derivatives and integral,
!> inside the table and continued beyond it.
!!
!! The linear and natural-cubic arithmetic is taken over from qfeet's `interpolation` module, by the
!! same author, and reworked to this library's rules: `real64` only, no logging, every abort an
!! `error stop` naming the entry point, a NaN screened before any comparison, and a bracket that is
!! guessed by arithmetic on an evenly spaced table but always walked to the segment its inequality
!! names. The not-a-knot and clamped end conditions, PCHIP, and every derivative and integral are
!! written here. PCHIP's slopes are those of F. N. Fritsch and R. E. Carlson, "Monotone Piecewise
!! Cubic Interpolation", SIAM J. Numer. Anal. 17 (1980), with the weighted harmonic mean of
!! F. N. Fritsch and J. Butland, "A Method for Constructing Local Monotone Piecewise Cubic
!! Interpolants", SIAM J. Sci. Stat. Comput. 5 (1984), in the form SLATEC's `dpchim` computes it and
!! with the end rule scipy's `PchipInterpolator` applies; they are cited as references, and no text
!! of either is copied.
!!
!! **Why the solver carries no pivoting and no singularity check.** Every entry point refuses a
!! table whose abscissae are not strictly monotonic, and the table is stored ascending, so every
!! spacing `h(i) = x(i+1) - x(i)` is positive. The interior rows of the spline system,
!! `h(i-1)*m(i-1) + 2*(h(i-1) + h(i))*m(i) + h(i)*m(i+1) = 6*(delta(i) - delta(i-1))`, then have a
!! diagonal exactly twice the sum of the off-diagonal magnitudes. The end rows keep that dominance
!! under every end condition:
!!
!! * natural: unit rows, `m(1) = 0` and `m(n) = 0`, with no off-diagonal at all;
!! * clamped: `2*h(1)*m(1) + h(1)*m(2) = 6*(delta(1) - s1)` and its mirror, a diagonal twice its one
!!   off-diagonal;
!! * not-a-knot: the third derivative continuous at `x(2)`, `h(2)*m(1) - (h(1) + h(2))*m(2) +
!!   h(1)*m(3) = 0`, reaches one knot beyond the band, so it is solved for `m(1)` and substituted
!!   into row 2, which becomes `m(2)*(h(1) + h(2))*(h(1) + 2*h(2))/h(2) +
!!   m(3)*(h(2)**2 - h(1)**2)/h(2) = 6*(delta(2) - delta(1))`, and mirrored at `x(n-1)`. The
!!   reduced system in `m(2) .. m(n-1)` is tridiagonal. Its first row's diagonal exceeds its
!!   off-diagonal's magnitude by `(h(1) + h(2))*(2*h(1) + h(2))/h(2)` when `h(2) >= h(1)` and by
!!   `3*(h(1) + h(2))` otherwise, positive either way, and the last row likewise. `m(1)` and `m(n)`
!!   are recovered afterwards. With four points both substitutions land on the only two rows, which
!!   is why four is the minimum.
!!
!! A strictly diagonally dominant matrix is non-singular, and the Thomas sweep over one meets no zero
!! pivot and needs no pivoting: each pivot stays above the magnitude of the row's super-diagonal. If
!! the monotonicity screen is ever loosened, that guarantee goes with it.
!!
!! **Why PCHIP's interior slope cannot overflow.** Where the secants on both sides of a knot share a
!! sign, the slope is Butland's weighted harmonic mean, `(w1 + w2)/(w1/delta(k-1) + w2/delta(k))`.
!! Written that way a tiny secant makes a quotient overflow; `dpchim`'s form divides the smaller
!! secant's magnitude by a weighted sum of the two secants scaled by the larger, whose magnitude is
!! at least a third, and is the same number. The sign tests compare with zero rather than calling
!! `sign()`, whose answer for a zero argument is processor-dependent.
!!
!! **Why an evenly spaced table cannot choose a different segment from bisection.** The arithmetic
!! bracket `1 + floor((xq - x(1))/step)` is exact only on a table whose knots sit exactly on
!! multiples of `step`. The uniformity tolerance accepts tables whose knots lie up to a tenth of a
!! step off those multiples, and a query that close to a knot would be evaluated on the neighbouring
!! segment's polynomial continued -- a different number wherever the slope or the curvature changes
!! at that knot, not the same number to rounding. So the guess is walked, one knot at a time, until
!! `x(k) <= xq < x(k+1)` holds: on an exactly even table the walk takes no step, on an accepted one
!! at most one, and the two paths agree for every query.
!!
!! **Why a query on a knot answers that knot's ordinate exactly.** The walk makes every interior
!! knot the LEFT end of its segment, where every segment formula here multiplies what it adds to the
!! left ordinate by an exact zero, or, in the cubic spline's value, answers the left ordinate
!! directly; and the evaluators answer `y(n)` for `x(n)` directly. Neither depends on how a compiler
!! rounds a division, which under a value-unsafe floating-point model is not guaranteed to give
!! `h/h == 1`.
!!
!! **Why the cubic spline's value measures both fractions.** Its curvature term,
!! `((a**3 - a)*m0 + (b**3 - b)*m1)*h**2/6` with `a` and `b` the fractions of the segment from its two
!! knots, is `-a*b*((1 + a)*m0 + (1 + b)*m1)*h**2/6`, a product that vanishes at either knot through
!! the fraction measured from that knot. Beside a knot where the second derivatives dwarf the
!! ordinates, that fraction must vanish to the bits: `a` taken as `1 - b` carries the rounding of `b`,
!! all of `a` beside the right knot, into a term that can be millions of times the value. So `a` is
!! measured from `x1` as `b` is from `x0`, and the linear part is `a*y0 + b*y1`, which beside either
!! knot takes that knot's ordinate with a weight near one; a linear part anchored at `y0` loses the
!! digits of `y0` beside the right knot when `y0` dwarfs `y1`.
!!
!! **Why a query beyond the table is answered about the end knot.** A segment's formula in the
!! fraction of its own origin, evaluated millions of widths away, forms terms in the cube of that
!! fraction that must cancel to the answer, and beyond an end segment far shorter than the distance
!! they cancel to nothing: PCHIP over the line `2*x + 1` on the knots `0, 2**-30, 1, 2, 3, 4`
!! answered 0 at `x = -1`. The end segment is instead expanded about the end knot the query lies
!! beyond, with coefficients formed from differences of the segment's data (`interp_end_value`), so
!! that a straight segment continues as its line exactly and a curved one without cancellation.
submodule (parquet_interpolate) parquet_interpolate_core

    implicit none

contains

    ! ---- messages and tokens -----------------------------------------------------------------

    module procedure interp_abort

        character(len=:), allocatable :: msg

        msg = entry//": "//text
        if (present(context)) then
            if (len_trim(context) > CONTEXT_CAP) then
                msg = msg//" (context: "//context(1:CONTEXT_CAP)//"...)"
            else
                msg = msg//" (context: "//trim(context)//")"
            end if
        end if

        ! One thread aborts, not several: two threads reaching ERROR STOP at once leave the exit
        ! status nondeterministic under ifx (`api-conventions.md`).
        !$omp critical (parquet_interpolate_abort)
        error stop msg
        !$omp end critical (parquet_interpolate_abort)

    end procedure interp_abort

    module procedure interp_i2s

        write (text, '(i0)') n

    end procedure interp_i2s

    module procedure interp_r2s

        ! The width is D + E + 5, the narrowest that always fits: a NEGATIVE value needs eleven
        ! columns (`-1.234E-300`), and a narrower field renders it as asterisks. `adjustl` plus the
        ! callers' `trim` means a positive value reads exactly as it did at a width of ten.
        write (text, '(es11.3e3)') v
        text = adjustl(text)

    end procedure interp_r2s

    module procedure interp_fold_token

        integer :: i, c

        folded = ""
        ! Refused whole rather than cut short: a cut token with blanks inside it could trim to a name.
        if (len_trim(token) > TOKEN_CAP) return
        do i = 1, len_trim(token)
            c = iachar(token(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) c = c + 32
            folded(i:i) = achar(c)
        end do

    end procedure interp_fold_token

    module procedure interp_quote

        if (len_trim(token) > TOKEN_CAP) then
            text = '"'//token(1:TOKEN_CAP)//'..."'
        else
            text = '"'//trim(token)//'"'
        end if

    end procedure interp_quote

    ! ---- validation -----------------------------------------------------------------------------

    module procedure interp_is_monotonic

        integer :: i, n

        ok = .false.
        ascending = .true.
        n = size(x)
        ! Every NaN first, as its own pass: an ordered comparison against one raises IEEE_INVALID,
        ! which nagfor's default `-ieee=stop` turns into an abort ahead of this module's message. By
        ! self-comparison rather than `ieee_is_nan`: this pass runs over every value of the table, and
        ! `ieee_is_nan` is a runtime call under ifx and nagfor.
        do i = 1, n
            if (x(i) /= x(i)) return
        end do
        ascending = x(2) > x(1)
        if (ascending) then
            do i = 2, n
                if (.not. (x(i) > x(i - 1))) return
            end do
        else
            do i = 2, n
                if (.not. (x(i) < x(i - 1))) return
            end do
        end if
        ok = .true.

    end procedure interp_is_monotonic

    module procedure interp_all_finite

        integer(int64), parameter :: EXPONENT_BITS = int(z'7FF0000000000000', int64) !! all-ones exponent

        integer :: i, bad

        ! Counted rather than left at the first: a loop with no exit is one a compiler can vectorise, and a
        ! table that passes is read to its end either way.
        bad = 0
        do i = 1, size(v)
            if (iand(transfer(v(i), 0_int64), EXPONENT_BITS) == EXPONENT_BITS) bad = bad + 1
        end do
        ok = bad == 0

    end procedure interp_all_finite

    module procedure interp_uniform_step

        real(real64) :: tol
        integer      :: i, n

        n = size(x)
        step = (x(n) - x(1))/real(n - 1, real64)
        uniform = .false.
        ! A span beyond `huge()` makes the step infinite, and every position test below a NaN.
        if (step > huge(step)) return
        tol = UNIFORM_TOL_FRACTION*step
        do i = 2, n - 1
            if (abs((x(i) - x(1)) - real(i - 1, real64)*step) > tol) return
        end do
        uniform = .true.

    end procedure interp_uniform_step

    module procedure interp_widest_spacing

        integer :: i, n

        n = size(x)
        widest = 0.0_real64
        if (x(n) - x(1) <= SPLINE_MAX_SPACING) return
        do i = 1, n - 1
            widest = max(widest, x(i + 1) - x(i))
        end do

    end procedure interp_widest_spacing

    module procedure interp_narrowest_spacing

        integer :: i

        narrowest = x(2) - x(1)
        do i = 2, size(x) - 1
            narrowest = min(narrowest, x(i + 1) - x(i))
        end do

    end procedure interp_narrowest_spacing

    ! ---- the bracket search -------------------------------------------------------------------

    module procedure interp_bracket

        integer :: lo, hi, mid

        if (uniform) then
            ! The guess. `xq` lies inside the table, so the quotient lies in `[0, n-1]` plus the drift
            ! the tolerance allows, and `floor` cannot overflow.
            k = 1 + floor((xq - x(1))/step)
            k = max(1, min(k, n - 1))
            ! The walk: both tests index only `x(1:n)`, so neither needs a short-circuit.
            do while (k > 1 .and. xq < x(k))
                k = k - 1
            end do
            do while (k < n - 1 .and. xq >= x(k + 1))
                k = k + 1
            end do
        else
            ! Bisection, keeping `x(lo) <= xq` and `xq < x(hi)` or `hi == n`.
            lo = 1
            hi = n
            do while (hi - lo > 1)
                mid = lo + (hi - lo)/2
                if (xq >= x(mid)) then
                    lo = mid
                else
                    hi = mid
                end if
            end do
            k = lo
        end if

    end procedure interp_bracket

    module procedure interp_bracket_near

        integer :: lo, hi, mid

        k = k0
        if (xq >= x(k)) then
            ! At or above `x(k0)`: segment `k0`, or the next, or a bisection above that.
            if (k == n - 1) return
            if (xq < x(k + 1)) return
            k = k + 1
            if (k == n - 1) return
            if (xq < x(k + 1)) return
            lo = k + 1
            hi = n
        else
            ! Below `x(k0)`, which is then not `x(1)`: the segment before, or a bisection below that.
            k = k - 1
            if (xq >= x(k)) return
            lo = 1
            hi = k
        end if
        ! Bisection, keeping `x(lo) <= xq` and `xq < x(hi)` or `hi == n`, as `interp_bracket` does.
        do while (hi - lo > 1)
            mid = lo + (hi - lo)/2
            if (xq >= x(mid)) then
                lo = mid
            else
                hi = mid
            end if
        end do
        k = lo

    end procedure interp_bracket_near

    ! ---- the tridiagonal solver and the per-knot coefficients ------------------------------------

    module procedure interp_thomas

        real(real64), allocatable :: gam(:)
        real(real64)              :: bet
        integer                   :: j, n

        n = size(b)
        ! On the heap rather than automatic: ifx places an automatic array on the stack, and a table of
        ! a million knots would then need eight megabytes of it per work array.
        allocate (gam(n))
        bet = b(1)
        u(1) = r(1)/bet
        do j = 2, n
            gam(j) = c(j - 1)/bet
            bet = b(j) - a(j)*gam(j)
            u(j) = (r(j) - a(j)*u(j - 1))/bet
        end do
        do j = n - 1, 1, -1
            u(j) = u(j) - gam(j + 1)*u(j + 1)
        end do

    end procedure interp_thomas

    module procedure interp_spline_coeffs

        real(real64), allocatable :: sub(:), diag(:), sup(:), rhs(:)
        real(real64)              :: h0, h1
        integer                   :: i, n

        n = size(x)
        allocate (sub(n), diag(n), sup(n), rhs(n))
        ! The interior rows every end condition shares: the first derivative continuous at `x(i)`.
        do i = 2, n - 1
            h0 = x(i) - x(i - 1)
            h1 = x(i + 1) - x(i)
            sub(i) = h0
            diag(i) = 2.0_real64*(h0 + h1)
            sup(i) = h1
            rhs(i) = 6.0_real64*((y(i + 1) - y(i))/h1 - (y(i) - y(i - 1))/h0)
        end do

        select case (bc)
        case (B_NATURAL)
            ! `m(1) = 0` and `m(n) = 0`.
            sub(1) = 0.0_real64
            diag(1) = 1.0_real64
            sup(1) = 0.0_real64
            rhs(1) = 0.0_real64
            sub(n) = 0.0_real64
            diag(n) = 1.0_real64
            sup(n) = 0.0_real64
            rhs(n) = 0.0_real64
            call interp_thomas(sub, diag, sup, rhs, m)
        case (B_CLAMPED)
            ! The first derivative of the first segment at `x(1)` is `s1`, and of the last at `x(n)`
            ! is `sn`.
            h0 = x(2) - x(1)
            sub(1) = 0.0_real64
            diag(1) = 2.0_real64*h0
            sup(1) = h0
            rhs(1) = 6.0_real64*((y(2) - y(1))/h0 - s1)
            h1 = x(n) - x(n - 1)
            sub(n) = h1
            diag(n) = 2.0_real64*h1
            sup(n) = 0.0_real64
            rhs(n) = 6.0_real64*(sn - (y(n) - y(n - 1))/h1)
            call interp_thomas(sub, diag, sup, rhs, m)
        case default
            ! `B_NOT_A_KNOT`, the only other code an entry point stores. Rows 2 and n-1 absorb `m(1)`
            ! and `m(n)` (the header derives the coefficients), the reduced system is swept, and the
            ! two ends are recovered from the not-a-knot conditions themselves. The differences of
            ! squares are factored, so a spacing equal on both sides gives an exact zero.
            h0 = x(2) - x(1)
            h1 = x(3) - x(2)
            diag(2) = (h0 + h1)*(h0 + 2.0_real64*h1)/h1
            sup(2) = (h1 - h0)*(h1 + h0)/h1
            h0 = x(n - 1) - x(n - 2)
            h1 = x(n) - x(n - 1)
            sub(n - 1) = (h0 - h1)*(h0 + h1)/h0
            diag(n - 1) = (h0 + h1)*(2.0_real64*h0 + h1)/h0
            call interp_thomas(sub(2:n - 1), diag(2:n - 1), sup(2:n - 1), rhs(2:n - 1), m(2:n - 1))
            h0 = x(2) - x(1)
            h1 = x(3) - x(2)
            m(1) = ((h0 + h1)*m(2) - h0*m(3))/h1
            h0 = x(n - 1) - x(n - 2)
            h1 = x(n) - x(n - 1)
            m(n) = ((h0 + h1)*m(n - 1) - h1*m(n - 2))/h0
        end select

    end procedure interp_spline_coeffs

    module procedure interp_pchip_slopes

        real(real64) :: h0, h1, left, right, hsum, left_used, right_used, big, small, mean
        integer      :: k, n
        logical      :: same

        n = size(x)
        if (n == 2) then
            ! One segment: the straight line.
            d(1) = (y(2) - y(1))/(x(2) - x(1))
            d(2) = d(1)
            return
        end if

        do k = 2, n - 1
            h0 = x(k) - x(k - 1)
            h1 = x(k + 1) - x(k)
            hsum = h0 + h1
            left = (y(k) - y(k - 1))/h0
            right = (y(k + 1) - y(k))/h1
            ! Where the secants share a sign, the slope is Butland's weighted harmonic mean in `dpchim`'s
            ! overflow-free form, the weight `(hsum + h0)/(3*hsum)` belonging to the left secant and
            ! `(hsum + h1)/(3*hsum)` to the right. A flat secant on either side, or a change of
            ! direction, is a plateau or a local extremum, where the slope is zero -- which is what
            ! keeps the curve from overshooting.
            !
            ! The mean is formed branch-free, over secants replaced by 1 wherever it is not taken. ifx
            ! evaluates a guarded quotient whether or not its guard holds, so a mean formed only inside
            ! an `if` still divides `0/0` at a plateau and by zero at a symmetric peak, raising
            ! IEEE_INVALID or IEEE_DIVIDE_BY_ZERO from a `%init` whose slopes are right -- which ends a
            ! program running under nagfor's default `-ieee=stop`.
            same = (left > 0.0_real64 .and. right > 0.0_real64) .or. (left < 0.0_real64 .and. right < 0.0_real64)
            left_used = merge(left, 1.0_real64, same)
            right_used = merge(right, 1.0_real64, same)
            big = max(abs(left_used), abs(right_used))
            small = min(abs(left_used), abs(right_used))
            mean = small/(((hsum + h0)/(3.0_real64*hsum))*(left_used/big) + ((hsum + h1)/(3.0_real64*hsum))*(right_used/big))
            d(k) = merge(mean, 0.0_real64, same)
        end do

        d(1) = interp_pchip_end_slope(x(2) - x(1), x(3) - x(2), (y(2) - y(1))/(x(2) - x(1)), (y(3) - y(2))/(x(3) - x(2)))
        d(n) = interp_pchip_end_slope(x(n) - x(n - 1), x(n - 1) - x(n - 2), (y(n) - y(n - 1))/(x(n) - x(n - 1)), &
                               (y(n - 1) - y(n - 2))/(x(n - 1) - x(n - 2)))

    end procedure interp_pchip_slopes

    ! ---- the segment polynomials -----------------------------------------------------------------

    module procedure interp_linear_seg_value

        real(real64) :: t

        t = (xq - x0)/(x1 - x0)
        v = y0 + t*(y1 - y0)

    end procedure interp_linear_seg_value

    module procedure interp_linear_seg_derivative

        if (order == 1) then
            v = (y1 - y0)/(x1 - x0)
        else
            v = 0.0_real64
        end if

    end procedure interp_linear_seg_derivative

    module procedure interp_linear_seg_integral

        real(real64) :: t

        ! The width times the mean of the two ends of the piece, the second end being the line's
        ! value at `xq`.
        t = (xq - x0)/(x1 - x0)
        v = (xq - x0)*(y0 + 0.5_real64*t*(y1 - y0))

    end procedure interp_linear_seg_integral

    module procedure interp_cubic_seg_value

        real(real64) :: h, a, b

        h = x1 - x0
        b = (xq - x0)/h
        ! The left knot answers its ordinate directly: `a` is measured below, and `h/h` need not be
        ! exactly one under a value-unsafe floating-point model.
        if (b == 0.0_real64) then
            v = y0
            return
        end if
        ! Each fraction from its own knot, and the curvature term as their product (the header says why).
        a = (x1 - xq)/h
        v = a*y0 + b*y1 - a*b*((1.0_real64 + a)*m0 + (1.0_real64 + b)*m1)*(h*h)/6.0_real64

    end procedure interp_cubic_seg_value

    module procedure interp_cubic_seg_derivative

        real(real64) :: h, a, b

        h = x1 - x0
        b = (xq - x0)/h
        a = 1.0_real64 - b
        if (order == 1) then
            v = (y1 - y0)/h + ((3.0_real64*b*b - 1.0_real64)*m1 - (3.0_real64*a*a - 1.0_real64)*m0)*h/6.0_real64
        else
            v = a*m0 + b*m1
        end if

    end procedure interp_cubic_seg_derivative

    module procedure interp_cubic_seg_integral

        real(real64) :: h, a, b, c

        h = x1 - x0
        b = (xq - x0)/h
        a = 1.0_real64 - b
        ! The antiderivative of the segment formula that vanishes at `x0`, as a polynomial in `b`:
        ! `h*(b*y0 + b**2*(y1 - y0)/2 - h**2*((b*(2 - b))**2*m0 + b**2*(2 - b**2)*m1)/24)`. Every
        ! coefficient is a product, so nothing cancels near the left knot; `c` is `b*(2 - b)`.
        c = b*(1.0_real64 + a)
        v = h*(b*y0 + 0.5_real64*b*b*(y1 - y0) - (h*h)*(c*c*m0 + b*b*(2.0_real64 - b*b)*m1)/24.0_real64)

    end procedure interp_cubic_seg_integral

    module procedure interp_hermite_seg_value

        real(real64) :: h, t, s

        h = x1 - x0
        t = (xq - x0)/h
        s = 1.0_real64 - t
        ! The Hermite basis with `y0` taken out: `h00 = 1 - h01`, so the value is `y0` plus the rise
        ! times `h01 = t**2*(3 - 2*t)`, plus the slope terms `h10 = t*s**2` and `h11 = -t**2*s`. At the
        ! left knot every added term carries a factor `t` that is an exact zero, and on a flat segment
        ! with zero slopes the value is `y0` itself.
        v = y0 + t*t*(3.0_real64 - 2.0_real64*t)*(y1 - y0) + h*t*s*(s*d0 - t*d1)

    end procedure interp_hermite_seg_value

    module procedure interp_hermite_seg_derivative

        real(real64) :: h, t, s

        h = x1 - x0
        t = (xq - x0)/h
        s = 1.0_real64 - t
        if (order == 1) then
            v = 6.0_real64*t*s*(y1 - y0)/h + s*(1.0_real64 - 3.0_real64*t)*d0 + t*(3.0_real64*t - 2.0_real64)*d1
        else
            v = (6.0_real64*(1.0_real64 - 2.0_real64*t)*(y1 - y0)/h + (6.0_real64*t - 4.0_real64)*d0 + &
                 (6.0_real64*t - 2.0_real64)*d1)/h
        end if

    end procedure interp_hermite_seg_derivative

    module procedure interp_hermite_seg_integral

        real(real64) :: h, t

        h = x1 - x0
        t = (xq - x0)/h
        ! `h` times the integral over `[0, t]` of the basis: `t*y0 + t**3*(2 - t)*(y1 - y0)/2 +
        ! h*(t**2*(6 - 8*t + 3*t**2)*d0 + t**3*(3*t - 4)*d1)/12`.
        v = h*(t*y0 + 0.5_real64*t*t*t*(2.0_real64 - t)*(y1 - y0) + &
               h*(t*t*(6.0_real64 + t*(3.0_real64*t - 8.0_real64))*d0 + t*t*t*(3.0_real64*t - 4.0_real64)*d1)/12.0_real64)

    end procedure interp_hermite_seg_integral

    module procedure interp_end_value

        real(real64) :: yend, slope, c2, c3, h, e, t

        call interp_end_coeffs(method, x0, x1, y0, y1, k0, k1, above, yend, slope, c2, c3, h)
        e = xq - merge(x1, x0, above)
        if (method == M_LINEAR) then
            ! Two terms, so that an infinite query meets no product of an infinity with a zero
            ! coefficient: the line's value there is the infinity of its slope's sign.
            v = yend + e*slope
        else
            t = e/h
            v = yend + e*(slope + t*(c2 + t*c3))
        end if

    end procedure interp_end_value

    module procedure interp_end_derivative

        real(real64) :: yend, slope, c2, c3, h, t

        call interp_end_coeffs(method, x0, x1, y0, y1, k0, k1, above, yend, slope, c2, c3, h)
        if (method == M_LINEAR) then
            v = merge(slope, 0.0_real64, order == 1)
        else
            t = (xq - merge(x1, x0, above))/h
            if (order == 1) then
                v = slope + t*(2.0_real64*c2 + 3.0_real64*t*c3)
            else
                v = (2.0_real64*c2 + 6.0_real64*t*c3)/h
            end if
        end if

    end procedure interp_end_derivative

    module procedure interp_end_integral

        real(real64) :: yend, slope, c2, c3, h, e, t

        call interp_end_coeffs(method, x0, x1, y0, y1, k0, k1, above, yend, slope, c2, c3, h)
        e = xq - merge(x1, x0, above)
        if (method == M_LINEAR) then
            v = e*(yend + 0.5_real64*e*slope)
        else
            t = e/h
            v = e*(yend + e*(0.5_real64*slope + t*(c2/3.0_real64 + 0.25_real64*t*c3)))
        end if

    end procedure interp_end_integral

    ! ---- helpers private to this submodule -------------------------------------------------------

    !> The coefficients of an end segment's polynomial about its end knot: `yend + e*(slope + t*(c2 +
    !! t*c3))` at a distance `e` from the knot, `t = e/h`.
    !!
    !! `slope` is the polynomial's first derivative at the knot, `c2` half its second derivative there
    !! times `h`, and `c3` a sixth of its third derivative times `h**2`, so that all three are in
    !! units of a slope and none needs a square of the spacing. For the cubic spline in
    !! second-derivative form they are its end slope, `delta - h*(2*m0 + m1)/6` at `x0` and
    !! `delta + h*(m0 + 2*m1)/6` at `x1`, the end's `m*h/2`, and `(m1 - m0)*h/6`; for a Hermite segment
    !! the end slope as given, and the differences of the two slopes from the secant `delta`, grouped so
    !! that a slope equal to the secant contributes an exact zero.
    pure subroutine interp_end_coeffs(method, x0, x1, y0, y1, k0, k1, above, yend, slope, c2, c3, h)
        integer, intent(in)       :: method !! `M_LINEAR`, `M_CUBIC` or `M_PCHIP`
        real(real64), intent(in)  :: x0     !! the segment's left knot
        real(real64), intent(in)  :: x1     !! the segment's right knot
        real(real64), intent(in)  :: y0     !! the ordinate at `x0`
        real(real64), intent(in)  :: y1     !! the ordinate at `x1`
        real(real64), intent(in)  :: k0     !! the second derivative (`M_CUBIC`) or slope (`M_PCHIP`) at `x0`
        real(real64), intent(in)  :: k1     !! the same at `x1`
        logical, intent(in)       :: above  !! about `x1`, rather than `x0`
        real(real64), intent(out) :: yend   !! the ordinate at the end knot
        real(real64), intent(out) :: slope  !! the first derivative there
        real(real64), intent(out) :: c2     !! half the second derivative there, times `h`
        real(real64), intent(out) :: c3     !! a sixth of the third derivative, times `h**2`
        real(real64), intent(out) :: h      !! the segment's width

        real(real64) :: delta

        h = x1 - x0
        delta = (y1 - y0)/h
        yend = merge(y1, y0, above)
        select case (method)
        case (M_CUBIC)
            c3 = (k1 - k0)*h/6.0_real64
            if (above) then
                slope = delta + h*(k0 + 2.0_real64*k1)/6.0_real64
                c2 = 0.5_real64*k1*h
            else
                slope = delta - h*(2.0_real64*k0 + k1)/6.0_real64
                c2 = 0.5_real64*k0*h
            end if
        case (M_PCHIP)
            c3 = (k0 - delta) + (k1 - delta)
            if (above) then
                slope = k1
                c2 = (k0 - delta) + (k1 - delta) + (k1 - delta)
            else
                slope = k0
                c2 = (delta - k0) + (delta - k0) + (delta - k1)
            end if
        case default
            slope = delta
            c2 = 0.0_real64
            c3 = 0.0_real64
        end select

    end subroutine interp_end_coeffs

    !> PCHIP's slope at an end of the table: the three-point estimate from the two end secants, set to
    !! zero where it points against the end secant, and held to three times that secant where the data
    !! turn at the second knot.
    pure function interp_pchip_end_slope(h0, h1, m0, m1) result(d)
        real(real64), intent(in) :: h0 !! the width of the end segment
        real(real64), intent(in) :: h1 !! the width of the segment beside it
        real(real64), intent(in) :: m0 !! the end segment's secant
        real(real64), intent(in) :: m1 !! the secant beside it
        real(real64)             :: d  !! the slope at the end knot

        d = ((2.0_real64*h0 + h1)*m0 - h0*m1)/(h0 + h1)
        if (interp_sign_of(d) /= interp_sign_of(m0)) then
            d = 0.0_real64
        else if (interp_sign_of(m0) /= interp_sign_of(m1)) then
            if (abs(d) > 3.0_real64*abs(m0)) d = 3.0_real64*m0
        end if

    end function interp_pchip_end_slope

    !> -1, 0 or +1 by comparison with zero, so that a negative zero counts as zero on every processor.
    pure elemental function interp_sign_of(v) result(s)
        real(real64), intent(in) :: v !! the value, never a NaN here
        integer                  :: s !! its sign

        s = 0
        if (v > 0.0_real64) s = 1
        if (v < 0.0_real64) s = -1

    end function interp_sign_of

end submodule parquet_interpolate_core
