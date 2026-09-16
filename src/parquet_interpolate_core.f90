!> The workers every interpolant shares: the abort and its message helpers, the token folding, the
!> monotonicity and uniformity tests, the bracket search, the tridiagonal solver, the spline
!> coefficients and the segment polynomials.
!!
!! The linear and natural-cubic arithmetic is taken over from qfeet's `interpolation` module, by the
!! same author, and reworked to this library's rules: `real64` only, no logging, every abort an
!! `error stop` naming the entry point, a NaN screened before any comparison, and a bracket that is
!! guessed by arithmetic on an evenly spaced table but always walked to the segment its inequality
!! names.
!!
!! **Why the solver carries no pivoting and no singularity check.** Every entry point refuses a
!! table whose abscissae are not strictly monotonic, and the table is stored ascending, so every
!! spacing `h(i) = x(i+1) - x(i)` is positive. The interior rows of the spline system,
!! `h(i-1)*m(i-1) + 2*(h(i-1) + h(i))*m(i) + h(i)*m(i+1) = 6*(delta(i) - delta(i-1))`, then have a
!! diagonal exactly twice the sum of the off-diagonal magnitudes, and the natural end rows are unit
!! rows with no off-diagonal at all. A strictly diagonally dominant matrix is non-singular, and the
!! Thomas sweep over one meets no zero pivot and needs no pivoting: each pivot stays above the
!! magnitude of the row's super-diagonal. If the monotonicity screen is ever loosened, that
!! guarantee goes with it.
!!
!! **Why an evenly spaced table cannot choose a different segment from bisection.** The arithmetic
!! bracket `1 + floor((xq - x(1))/step)` is exact only on a table whose knots sit exactly on
!! multiples of `step`. The uniformity tolerance accepts tables that drift from that by up to a fifth
!! of a step, and a query that close to a knot would be evaluated on the neighbouring segment's
!! polynomial continued -- a different number wherever the slope or the curvature changes at that
!! knot, not the same number to rounding. So the guess is walked, one knot at a time, until
!! `x(k) <= xq < x(k+1)` holds: on an exactly even table the walk takes no step, on an accepted one
!! at most one, and the two paths agree for every query.
!!
!! **Why a query on a knot answers that knot's ordinate exactly.** The walk makes every interior
!! knot the LEFT end of its segment, where the segment formulas multiply the right-hand ordinate by
!! an exact zero; and the evaluators answer `y(n)` for `x(n)` directly. Neither depends on how a
!! compiler rounds a division, which under a value-unsafe floating-point model is not guaranteed to
!! give `h/h == 1`.
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

    module procedure interp_fold_token

        integer :: i, c

        folded = ""
        do i = 1, min(len_trim(token), len(folded))
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
        ! which nagfor's default `-ieee=stop` turns into an abort ahead of this module's message.
        do i = 1, n
            if (ieee_is_nan(x(i))) return
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

    module procedure interp_uniform_step

        real(real64) :: first, tol
        integer      :: i, n

        n = size(x)
        step = (x(n) - x(1))/real(n - 1, real64)
        first = x(2) - x(1)
        tol = UNIFORM_TOL_FRACTION*first/real(n, real64)
        uniform = .false.
        do i = 3, n
            if (abs(first - (x(i) - x(i - 1))) > tol) return
        end do
        uniform = .true.

    end procedure interp_uniform_step

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

    ! ---- the tridiagonal solver and the spline ---------------------------------------------------

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
        select case (bc)
        case (B_NATURAL)
            allocate (sub(n), diag(n), sup(n), rhs(n))
            ! The natural end rows: `m(1) = 0` and `m(n) = 0`.
            sub(1) = 0.0_real64
            diag(1) = 1.0_real64
            sup(1) = 0.0_real64
            rhs(1) = 0.0_real64
            do i = 2, n - 1
                h0 = x(i) - x(i - 1)
                h1 = x(i + 1) - x(i)
                sub(i) = h0
                diag(i) = 2.0_real64*(h0 + h1)
                sup(i) = h1
                rhs(i) = 6.0_real64*((y(i + 1) - y(i))/h1 - (y(i) - y(i - 1))/h0)
            end do
            sub(n) = 0.0_real64
            diag(n) = 1.0_real64
            sup(n) = 0.0_real64
            rhs(n) = 0.0_real64
            call interp_thomas(sub, diag, sup, rhs, m)
        case default
            ! Unreachable: every entry point refuses the end conditions that are not available yet
            ! before it builds anything. Kept so that a token wired up without its rows aborts here
            ! rather than being solved as a natural spline.
            ! GCOVR_EXCL_START
            error stop "parquet_interpolate: this end condition is not available yet"
            ! GCOVR_EXCL_STOP
        end select

    end procedure interp_spline_coeffs

    ! ---- the segment polynomials -----------------------------------------------------------------

    module procedure interp_linear_seg_value

        real(real64) :: t

        t = (xq - x0)/(x1 - x0)
        v = y0 + t*(y1 - y0)

    end procedure interp_linear_seg_value

    module procedure interp_cubic_seg_value

        real(real64) :: h, a, b

        h = x1 - x0
        b = (xq - x0)/h
        ! `a = 1 - b` rather than `(x1 - xq)/h`: at the left knot `b` is an exact zero and so `a` an
        ! exact one, which is what makes a knot answer its own ordinate whatever the division rounds
        ! to.
        a = 1.0_real64 - b
        v = a*y0 + b*y1 + ((a*a*a - a)*m0 + (b*b*b - b)*m1)*(h*h)/6.0_real64

    end procedure interp_cubic_seg_value

end submodule parquet_interpolate_core
