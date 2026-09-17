!> `pf_interp_2d`'s bindings, its one-shot `pf_interp` specifics and its test-only hook: every
!> validation of a grid and every `error stop` it can reach.
!!
!! **Both methods are tensor products of the one-dimensional ones.** Bilinear interpolation is linear
!! interpolation along `x` at the cell's two `y` lines, then along `y` between the two. The bicubic
!! spline is the cubic spline along each axis in turn, under the same end condition on every edge:
!! `%init` computes three tables of second derivatives through the one-dimensional solver
!! `interp_spline_coeffs` -- along `x` of every column of `z` (`zxx`), along `y` of every row of `z`
!! (`zyy`), and along `x` of every column of `zyy` (`zxxyy`) -- and a query evaluates four cubic
!! segments along `x` at the cell's two `y` lines, two of the values and two of their second
!! derivatives in `y`, then one segment along `y` between them. That is O(1) per query and O(nx*ny)
!! to build. The one-dimensional spline under `natural` or `not_a_knot` is LINEAR in its data, which
!! is what makes the construction exact: interpolating every row along `y` and then the results
!! along `x` is the same function, its restriction to any grid line is the one-dimensional spline of
!! that line, and `zxxyy` taken along `y` of `zxx` instead is the same table. A clamped spline is
!! affine in its data rather than linear, and a shape-preserving surface is not a tensor product of
!! shape-preserving curves; neither is offered here.
!!
!! **The validation order is the order of the guide page's table**, and each abort's text is what
!! the matching out-of-process scenario in `test/error_scenarios.f90` asserts. Changing a message
!! means changing that scenario in the same commit. `%init` and both one-shot specifics validate
!! through one body, `interp_2d_build`, and differ only in the entry name their messages carry.
!!
!! **A grid node answers its value exactly.** Each coordinate is placed on its cell as a fraction of
!! the cell's width, and every formula here multiplies what it takes from the far side of the cell
!! by that fraction. The bracket walk makes every interior grid line the LOW end of its cell, where
!! the fraction is an exact zero, and a formula evaluated at an exact zero answers the near side's
!! value exactly however the compiler rearranges it. A fraction of exactly one does not: ifx's
!! default `-fp-model=fast` rewrote `(1 - s)*a + s*b` so that `s = 1` answered `0.09999999999999998`
!! for `a = 0.7`, `b = 0.1`. So no formula here is evaluated at the high end of a cell. A coordinate on
!! an axis's last grid line -- given there, or moved there by `"clamp"` -- is answered from that
!! line's own values, interpolated along the other axis alone, and the last node from its value, as
!! the one-dimensional `%eval` answers its last knot.
submodule (parquet_interpolate) parquet_interpolate_2d

    implicit none

contains

    ! ---- the binding the one-shot specifics call, implemented first ------------------------------
    !
    ! nagfor rejects a separate module procedure whose body appears BELOW a call to it in the same
    ! submodule (`code-style.md`), so the evaluator sits above the one-shot specifics that use it.

    module procedure interp_2d_eval

        real(real64) :: s, t, hx, hy, v0, v1, w0, w1
        integer      :: i, j, nx, ny
        logical      :: x_beyond, y_beyond, x_high, y_high

        if (this%nx == 0) error stop "pf_interp_2d%eval: the interpolant is not initialised"

        ! A NaN coordinate is answered before any comparison, as `xq /= xq` rather than `ieee_is_nan`:
        ! this is the per-element path, and `ieee_is_nan` is a runtime call under ifx and nagfor.
        if (xq /= xq) then
            v = xq
            return
        end if
        if (yq /= yq) then
            v = yq
            return
        end if

        nx = this%nx
        ny = this%ny
        call interp_2d_locate(this%x, nx, this%x_uniform, this%x_step, this%outside, xq, i, s, x_beyond, x_high)
        call interp_2d_locate(this%y, ny, this%y_uniform, this%y_step, this%outside, yq, j, t, y_beyond, y_high)
        if (this%outside == O_NAN) then
            if (x_beyond .or. y_beyond) then
                v = ieee_value(1.0_real64, ieee_quiet_nan)
                return
            end if
        end if

        ! On an axis's last grid line: that line's values, along the other axis alone (the header says why).
        if (x_high .and. y_high) then
            v = this%z(nx, ny)
        else if (x_high) then
            if (this%method == M_LINEAR) then
                v = (1.0_real64 - t)*this%z(nx, j) + t*this%z(nx, j + 1)
            else
                v = interp_2d_cubic(this%z(nx, j), this%z(nx, j + 1), this%zyy(nx, j), this%zyy(nx, j + 1), &
                                    this%y(j + 1) - this%y(j), t)
            end if
        else if (y_high) then
            if (this%method == M_LINEAR) then
                v = (1.0_real64 - s)*this%z(i, ny) + s*this%z(i + 1, ny)
            else
                v = interp_2d_cubic(this%z(i, ny), this%z(i + 1, ny), this%zxx(i, ny), this%zxx(i + 1, ny), &
                                    this%x(i + 1) - this%x(i), s)
            end if
        else if (this%method == M_LINEAR) then
            v0 = (1.0_real64 - s)*this%z(i, j) + s*this%z(i + 1, j)
            v1 = (1.0_real64 - s)*this%z(i, j + 1) + s*this%z(i + 1, j + 1)
            v = (1.0_real64 - t)*v0 + t*v1
        else
            ! Along `x` at the cell's two `y` lines: the values, and their second derivatives in `y`.
            hx = this%x(i + 1) - this%x(i)
            hy = this%y(j + 1) - this%y(j)
            v0 = interp_2d_cubic(this%z(i, j), this%z(i + 1, j), this%zxx(i, j), this%zxx(i + 1, j), hx, s)
            v1 = interp_2d_cubic(this%z(i, j + 1), this%z(i + 1, j + 1), this%zxx(i, j + 1), this%zxx(i + 1, j + 1), hx, s)
            w0 = interp_2d_cubic(this%zyy(i, j), this%zyy(i + 1, j), this%zxxyy(i, j), this%zxxyy(i + 1, j), hx, s)
            w1 = interp_2d_cubic(this%zyy(i, j + 1), this%zyy(i + 1, j + 1), this%zxxyy(i, j + 1), &
                                 this%zxxyy(i + 1, j + 1), hx, s)
            ! Then along `y` between them.
            v = interp_2d_cubic(v0, v1, w0, w1, hy, t)
        end if

    end procedure interp_2d_eval

    module procedure interp_2d_ready

        ok = this%nx > 0

    end procedure interp_2d_ready

    module procedure interp_2d_clear

        if (allocated(this%x)) deallocate (this%x)
        if (allocated(this%y)) deallocate (this%y)
        if (allocated(this%z)) deallocate (this%z)
        if (allocated(this%zxx)) deallocate (this%zxx)
        if (allocated(this%zyy)) deallocate (this%zyy)
        if (allocated(this%zxxyy)) deallocate (this%zxxyy)
        this%nx = 0
        this%ny = 0
        this%method = 0
        this%outside = 0
        this%x_reversed = .false.
        this%y_reversed = .false.
        this%x_uniform = .false.
        this%y_uniform = .false.
        this%x_step = 0.0_real64
        this%y_step = 0.0_real64

    end procedure interp_2d_clear

    module procedure interp_2d_force_search

        x_was_uniform = this%x_uniform
        y_was_uniform = this%y_uniform
        this%x_uniform = .false.
        this%y_uniform = .false.

    end procedure interp_2d_force_search

    ! ---- building --------------------------------------------------------------------------------

    module procedure interp_2d_init

        call interp_2d_build(this, "pf_interp_2d%init", x, y, z, method, bc, outside, context)

    end procedure interp_2d_init

    module procedure interp_2d_oneshot_scalar

        type(pf_interp_2d) :: g

        call interp_2d_build(g, "pf_interp", x, y, z, method, bc, outside, context)
        zq = g%eval(xq, yq)

    end procedure interp_2d_oneshot_scalar

    ! The FULLY RESTATED form, not `module procedure interp_2d_oneshot_array`, for the reason
    ! `interp_1d_oneshot_array` gives: a result shaped by a later assumed-shape dummy is miscompiled
    ! by nagfor 7.2 in the abbreviated form.
    module function interp_2d_oneshot_array(x, y, z, xq, yq, method, bc, outside, context) result(zq)
        implicit none
        real(real64), intent(in)               :: x(:)          !! the grid lines along `x`
        real(real64), intent(in)               :: y(:)          !! the grid lines along `y`
        real(real64), intent(in)               :: z(:, :)       !! the values, shaped `(size(x), size(y))`
        real(real64), intent(in)               :: xq(:)         !! the queries' `x`
        real(real64), intent(in)               :: yq(:)         !! the queries' `y`, one per `xq`
        character(len=*), intent(in), optional :: method        !! `"linear"` or `"cubic"`
        character(len=*), intent(in), optional :: bc            !! the spline's end condition
        character(len=*), intent(in), optional :: outside       !! the out-of-range policy
        character(len=*), intent(in), optional :: context       !! call-site text
        real(real64)                           :: zq(size(xq))  !! the interpolated values

        type(pf_interp_2d) :: g

        ! The two coordinate arrays pair up, before anything is built: an elemental call over arrays
        ! of different sizes is not a call a program may make.
        if (size(xq) /= size(yq)) then
            call interp_abort("pf_interp", "xq and yq differ in size: "//trim(interp_i2s(size(xq)))//" and "// &
                              trim(interp_i2s(size(yq))), context)
        end if
        call interp_2d_build(g, "pf_interp", x, y, z, method, bc, outside, context)
        zq = g%eval(xq, yq)

    end function interp_2d_oneshot_array

    !> Validates a grid and its options, in the order of the guide page's table, and builds the
    !! interpolant over it.
    !!
    !! Each check below carries the number of its row in the one-dimensional table, the rows that
    !! concern a table's points checked along `x` first and then along `y`. The first failure aborts,
    !! so a scenario provoking a later row passes every earlier one.
    subroutine interp_2d_build(this, entry, x, y, z, method, bc, outside, context)
        type(pf_interp_2d), intent(out)        :: this    !! the interpolant to build
        character(len=*), intent(in)           :: entry   !! the entry point, for messages
        real(real64), intent(in)               :: x(:)    !! the grid lines along `x`
        real(real64), intent(in)               :: y(:)    !! the grid lines along `y`
        real(real64), intent(in)               :: z(:, :) !! the values
        character(len=*), intent(in), optional :: method  !! the method token
        character(len=*), intent(in), optional :: bc      !! the end-condition token
        character(len=*), intent(in), optional :: outside !! the out-of-range token
        character(len=*), intent(in), optional :: context !! call-site text

        real(real64), allocatable     :: line(:), curvature(:)
        character(len=TOKEN_CAP + 1)  :: tok
        character(len=:), allocatable :: quoted
        integer                       :: nx, ny, bc_code, need, i, j
        logical                       :: x_ascending, y_ascending

        nx = size(x)
        ny = size(y)

        ! Row 1': the values are laid out along the two axes, `z(i, j)` at `(x(i), y(j))`.
        if (size(z, 1) /= nx .or. size(z, 2) /= ny) then
            call interp_abort(entry, "z must be shaped (size(x), size(y)): got ("//trim(interp_i2s(size(z, 1)))// &
                              ", "//trim(interp_i2s(size(z, 2)))//") for ("//trim(interp_i2s(nx))//", "// &
                              trim(interp_i2s(ny))//")", context)
        end if

        ! Rows 3 and 4: the method, of which the shape-preserving one has no grid form.
        this%method = M_CUBIC
        if (present(method)) then
            call interp_fold_token(method, tok)
            select case (trim(tok))
            case ("linear")
                this%method = M_LINEAR
            case ("cubic")
                this%method = M_CUBIC
            case ("pchip")
                call interp_abort(entry, 'method "pchip" is not offered in two dimensions', context)
            case default
                call interp_quote(method, quoted)
                call interp_abort(entry, "unknown method "//quoted//'; expected "linear" or "cubic"', context)
            end select
        end if

        ! Row 5: an end condition belongs to the cubic spline alone.
        if (present(bc)) then
            if (this%method /= M_CUBIC) call interp_abort(entry, 'bc applies only to method "cubic"', context)
        end if

        ! Rows 6 and 7: the end condition, of which the clamped one has no grid form.
        bc_code = B_NATURAL
        if (present(bc)) then
            call interp_fold_token(bc, tok)
            select case (trim(tok))
            case ("natural")
                bc_code = B_NATURAL
            case ("not_a_knot")
                bc_code = B_NOT_A_KNOT
            case ("clamped")
                call interp_abort(entry, 'bc "clamped" is not offered in two dimensions', context)
            case default
                call interp_quote(bc, quoted)
                call interp_abort(entry, "unknown bc "//quoted//'; expected "natural" or "not_a_knot"', context)
            end select
        end if

        ! Row 11: the out-of-range policy.
        this%outside = O_CLAMP
        if (present(outside)) then
            call interp_fold_token(outside, tok)
            select case (trim(tok))
            case ("clamp")
                this%outside = O_CLAMP
            case ("extrapolate")
                this%outside = O_EXTRAPOLATE
            case ("nan")
                this%outside = O_NAN
            case default
                call interp_quote(outside, quoted)
                call interp_abort(entry, "unknown outside "//quoted//'; expected "clamp", "extrapolate" or "nan"', &
                                  context)
            end select
        end if

        ! Row 12, along each axis: enough grid lines for the method. A not-a-knot spline needs four.
        need = 2
        if (bc_code == B_NOT_A_KNOT) need = 4
        if (this%method == M_LINEAR) then
            quoted = '"linear"'
        else if (bc_code == B_NOT_A_KNOT) then
            quoted = '"cubic" with bc "not_a_knot"'
        else
            quoted = '"cubic"'
        end if
        if (nx < need) then
            call interp_abort(entry, "at least "//trim(interp_i2s(need))//" points are needed along x for method "// &
                              quoted//"; got "//trim(interp_i2s(nx)), context)
        end if
        if (ny < need) then
            call interp_abort(entry, "at least "//trim(interp_i2s(need))//" points are needed along y for method "// &
                              quoted//"; got "//trim(interp_i2s(ny)), context)
        end if

        ! Row 13, along each axis: strictly monotonic, which a NaN and a repeated line both fail.
        if (.not. interp_is_monotonic(x, x_ascending)) then
            call interp_abort(entry, "x must be strictly increasing or strictly decreasing", context)
        end if
        if (.not. interp_is_monotonic(y, y_ascending)) then
            call interp_abort(entry, "y must be strictly increasing or strictly decreasing", context)
        end if

        ! Row 13a, along each axis: finite, since an infinite end line passes the ordering and would
        ! make its cells infinitely wide.
        if (.not. all(ieee_is_finite(x))) call interp_abort(entry, "x must be finite", context)
        if (.not. all(ieee_is_finite(y))) call interp_abort(entry, "y must be finite", context)

        ! Row 14: finite values.
        if (.not. all(ieee_is_finite(z))) call interp_abort(entry, "z must be finite", context)

        ! Stored ascending along each axis, with the values reversed along the same axis.
        this%nx = nx
        this%ny = ny
        this%x_reversed = .not. x_ascending
        this%y_reversed = .not. y_ascending
        if (x_ascending) then
            this%x = x
        else
            this%x = x(nx:1:-1)
        end if
        if (y_ascending) then
            this%y = y
        else
            this%y = y(ny:1:-1)
        end if
        if (x_ascending .and. y_ascending) then
            this%z = z
        else if (y_ascending) then
            this%z = z(nx:1:-1, :)
        else if (x_ascending) then
            this%z = z(:, ny:1:-1)
        else
            this%z = z(nx:1:-1, ny:1:-1)
        end if

        call interp_uniform_step(this%x, this%x_uniform, this%x_step)
        call interp_uniform_step(this%y, this%y_uniform, this%y_step)

        if (this%method == M_CUBIC) then
            allocate (this%zxx(nx, ny), this%zyy(nx, ny), this%zxxyy(nx, ny))
            ! Along `x`, every column of the values.
            do j = 1, ny
                call interp_spline_coeffs(this%x, this%z(:, j), bc_code, 0.0_real64, 0.0_real64, this%zxx(:, j))
            end do
            ! Along `y`, every row of the values, each copied into a contiguous line first.
            allocate (line(ny), curvature(ny))
            do i = 1, nx
                line = this%z(i, :)
                call interp_spline_coeffs(this%y, line, bc_code, 0.0_real64, 0.0_real64, curvature)
                this%zyy(i, :) = curvature
            end do
            ! Along `x`, every column of the second derivatives along `y`.
            do j = 1, ny
                call interp_spline_coeffs(this%x, this%zyy(:, j), bc_code, 0.0_real64, 0.0_real64, this%zxxyy(:, j))
            end do
        end if

    end subroutine interp_2d_build

    ! ---- helpers private to this submodule -------------------------------------------------------

    !> Places one coordinate on its axis: the cell it is evaluated on, how far along that cell, and
    !! whether it lies on the axis's last grid line instead.
    !!
    !! Inside the grid the cell is the one its bracket names, and the fraction is measured from the
    !! cell's low line -- an exact zero on that line. On the axis's last line, and beyond it under
    !! `"clamp"`, `high` is set and the fraction is not measured: the caller answers from that line's
    !! values. Below the grid the cell is the first; `"extrapolate"` measures the fraction below it, and
    !! `"clamp"` puts the coordinate on the first line, at an exact zero. Under `"nan"` nothing measured
    !! beyond the grid is read.
    pure subroutine interp_2d_locate(lines, n, uniform, step, outside, q, k, frac, beyond, high)
        real(real64), intent(in)  :: lines(:) !! the axis's grid lines, strictly increasing
        integer, intent(in)       :: n        !! lines on the axis, at least two
        logical, intent(in)       :: uniform  !! guess the bracket by arithmetic
        real(real64), intent(in)  :: step     !! the mean spacing, used when `uniform`
        integer, intent(in)       :: outside  !! the object's `O_*` policy
        real(real64), intent(in)  :: q        !! the coordinate, not a NaN
        integer, intent(out)      :: k        !! the cell's low line; `n - 1` when `high`
        real(real64), intent(out) :: frac     !! how far along the cell, in cell widths; zero when `high`
        logical, intent(out)      :: beyond   !! `q` lies beyond the grid
        logical, intent(out)      :: high     !! `q` is on the last line, or clamped onto it

        beyond = .false.
        high = .false.
        frac = 0.0_real64
        if (q < lines(1)) then
            beyond = .true.
            k = 1
            if (outside == O_EXTRAPOLATE) frac = (q - lines(1))/(lines(2) - lines(1))
        else if (q > lines(n)) then
            beyond = .true.
            k = n - 1
            if (outside == O_EXTRAPOLATE) then
                frac = (q - lines(n - 1))/(lines(n) - lines(n - 1))
            else
                high = .true.
            end if
        else if (q == lines(n)) then
            k = n - 1
            high = .true.
        else
            k = interp_bracket(lines, n, uniform, step, q)
            frac = (q - lines(k))/(lines(k + 1) - lines(k))
        end if

    end subroutine interp_2d_locate

    !> One cubic-spline segment in second-derivative form, a fraction `b` of the way along it.
    !!
    !! The segment formula of `interp_cubic_seg_value` from its fraction rather than from a point, so
    !! that the caller measures the fraction once for four segments; a fraction of exactly zero answers
    !! `y0` exactly.
    pure function interp_2d_cubic(y0, y1, m0, m1, h, b) result(v)
        real(real64), intent(in) :: y0 !! the value at the segment's low end
        real(real64), intent(in) :: y1 !! the value at its high end
        real(real64), intent(in) :: m0 !! the second derivative at the low end
        real(real64), intent(in) :: m1 !! the second derivative at the high end
        real(real64), intent(in) :: h  !! the segment's width
        real(real64), intent(in) :: b  !! how far along it, in widths; beyond `[0, 1]` it is continued
        real(real64)             :: v  !! the cubic's value there

        real(real64) :: a

        a = 1.0_real64 - b
        v = a*y0 + b*y1 + ((a*a*a - a)*m0 + (b*b*b - b)*m1)*(h*h)/6.0_real64

    end function interp_2d_cubic

end submodule parquet_interpolate_2d
