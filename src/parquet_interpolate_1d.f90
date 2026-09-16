!> `pf_interp_1d`'s bindings, the one-shot `pf_interp` specifics and the test-only hook: every
!> validation of a one-dimensional table and every `error stop` it can reach.
!!
!! **The validation order is the order of the guide page's table**, and each abort's text is what
!! the matching out-of-process scenario in `test/error_scenarios.f90` asserts. Changing a message
!! means changing that scenario in the same commit. `%init` and both one-shot specifics validate
!! through one body, `interp_1d_build`, and differ only in the entry name their messages carry.
!!
!! **The evaluator is `pure elemental` and aborts in one case only**, an object that was never built;
!! a `pure` procedure cannot reach the named `critical` the other aborts take, so that message is the
!! literal alone. Everything else it meets -- a NaN query, a query beyond the table -- is answered.
submodule (parquet_interpolate) parquet_interpolate_1d

    implicit none

contains

    ! ---- the bindings every other procedure here calls, implemented first ------------------------
    !
    ! nagfor rejects a separate module procedure whose body appears BELOW a call to it in the same
    ! submodule (`code-style.md`), so the evaluator sits above the one-shot specifics that use it.

    module procedure interp_1d_eval

        integer :: k, n

        if (this%n == 0) error stop "pf_interp_1d%eval: the interpolant is not initialised"

        ! A NaN query is answered before any comparison, as `xq /= xq` rather than `ieee_is_nan`:
        ! this is the per-element path, and `ieee_is_nan` is a runtime call under ifx and nagfor.
        if (xq /= xq) then
            v = xq
            return
        end if

        n = this%n
        if (xq < this%x(1)) then
            select case (this%outside)
            case (O_CLAMP)
                v = this%y(1)
                return
            case (O_NAN)
                v = ieee_value(1.0_real64, ieee_quiet_nan)
                return
            end select
            k = 1
        else if (xq > this%x(n)) then
            select case (this%outside)
            case (O_CLAMP)
                v = this%y(n)
                return
            case (O_NAN)
                v = ieee_value(1.0_real64, ieee_quiet_nan)
                return
            end select
            k = n - 1
        else if (xq == this%x(n)) then
            ! The one knot that is the RIGHT end of its segment, where a segment formula would need
            ! `h/h` to round to exactly one; answered directly instead.
            v = this%y(n)
            return
        else
            k = interp_bracket(this%x, n, this%uniform, this%step, xq)
        end if

        select case (this%method)
        case (M_LINEAR)
            v = interp_linear_seg_value(this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), xq)
        case default
            v = interp_cubic_seg_value(this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), &
                                       this%d(k), this%d(k + 1), xq)
        end select

    end procedure interp_1d_eval

    module procedure interp_1d_ready

        ok = this%n > 0

    end procedure interp_1d_ready

    module procedure interp_1d_clear

        if (allocated(this%x)) deallocate (this%x)
        if (allocated(this%y)) deallocate (this%y)
        if (allocated(this%d)) deallocate (this%d)
        this%n = 0
        this%method = 0
        this%outside = 0
        this%reversed = .false.
        this%uniform = .false.
        this%step = 0.0_real64

    end procedure interp_1d_clear

    module procedure interp_1d_force_search

        was_uniform = this%uniform
        this%uniform = .false.

    end procedure interp_1d_force_search

    ! ---- building --------------------------------------------------------------------------------

    module procedure interp_1d_init

        call interp_1d_build(this, "pf_interp_1d%init", x, y, method, bc, slopes, outside, is_valid, &
                             context)

    end procedure interp_1d_init

    module procedure interp_1d_oneshot_scalar

        type(pf_interp_1d) :: c

        call interp_1d_build(c, "pf_interp", x, y, method, bc, slopes, outside, is_valid, context)
        yq = c%eval(xq)

    end procedure interp_1d_oneshot_scalar

    module procedure interp_1d_oneshot_array

        type(pf_interp_1d) :: c

        call interp_1d_build(c, "pf_interp", x, y, method, bc, slopes, outside, is_valid, context)
        yq = c%eval(xq)

    end procedure interp_1d_oneshot_array

    !> Validates a table and its options, in the order of the guide page's table, and builds the
    !! interpolant over it.
    !!
    !! Each check below carries the number of its row. The first failure aborts, so a scenario
    !! provoking a later row passes every earlier one. The points `is_valid` drops are dropped
    !! before the count, the ordering and the finiteness are judged: they are the nulls the mask
    !! exists for, and a NaN in one of them is not an error.
    subroutine interp_1d_build(this, entry, x, y, method, bc, slopes, outside, is_valid, context)
        type(pf_interp_1d), intent(out)        :: this        !! the interpolant to build
        character(len=*), intent(in)           :: entry       !! the entry point, for messages
        real(real64), intent(in)               :: x(:)        !! abscissae
        real(real64), intent(in)               :: y(:)        !! ordinates
        character(len=*), intent(in), optional :: method      !! the method token
        character(len=*), intent(in), optional :: bc          !! the end-condition token
        real(real64), intent(in), optional     :: slopes(:)   !! end slopes for `bc="clamped"`
        character(len=*), intent(in), optional :: outside     !! the out-of-range token
        logical, intent(in), optional          :: is_valid(:) !! `.false.` drops that point
        character(len=*), intent(in), optional :: context     !! call-site text

        real(real64), allocatable     :: xs(:), ys(:)
        character(len=TOKEN_CAP + 1)  :: tok
        character(len=:), allocatable :: quoted
        integer                       :: n, bc_code
        logical                       :: ascending

        ! Row 1: the two halves of the table pair up.
        if (size(x) /= size(y)) then
            call interp_abort(entry, "x and y differ in size: "//trim(interp_i2s(size(x)))//" and "// &
                              trim(interp_i2s(size(y))), context)
        end if

        ! Row 2: the mask covers the table.
        if (present(is_valid)) then
            if (size(is_valid) /= size(x)) then
                call interp_abort(entry, "is_valid has "//trim(interp_i2s(size(is_valid)))// &
                                  " elements for "//trim(interp_i2s(size(x)))//" points", context)
            end if
        end if

        ! Row 3: the method.
        this%method = M_CUBIC
        if (present(method)) then
            call interp_fold_token(method, tok)
            select case (trim(tok))
            case ("linear")
                this%method = M_LINEAR
            case ("cubic")
                this%method = M_CUBIC
            case ("pchip")
                call interp_abort(entry, 'method "pchip" is not available yet', context)
            case default
                call interp_quote(method, quoted)
                call interp_abort(entry, "unknown method "//quoted//'; expected "linear", "cubic" or "pchip"', &
                                  context)
            end select
        end if

        ! Row 5: an end condition belongs to the cubic spline alone.
        if (present(bc)) then
            if (this%method /= M_CUBIC) call interp_abort(entry, 'bc applies only to method "cubic"', context)
        end if

        ! Row 6: the end condition.
        bc_code = B_NATURAL
        if (present(bc)) then
            call interp_fold_token(bc, tok)
            select case (trim(tok))
            case ("natural")
                bc_code = B_NATURAL
            case ("not_a_knot", "clamped")
                call interp_abort(entry, 'bc "'//trim(tok)//'" is not available yet', context)
            case default
                call interp_quote(bc, quoted)
                call interp_abort(entry, "unknown bc "//quoted//'; expected "natural", "not_a_knot" or "clamped"', &
                                  context)
            end select
        end if

        ! Row 9: no end condition available yet reads slopes.
        if (present(slopes)) call interp_abort(entry, 'slopes apply only to bc "clamped"', context)

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

        ! Row 12: enough points survive the mask for the method.
        if (present(is_valid)) then
            xs = pack(x, is_valid)
            ys = pack(y, is_valid)
        else
            xs = x
            ys = y
        end if
        n = size(xs)
        if (n < 2) then
            if (this%method == M_LINEAR) then
                quoted = '"linear"'
            else
                quoted = '"cubic"'
            end if
            call interp_abort(entry, "at least 2 points are needed for method "//quoted//"; got "// &
                              trim(interp_i2s(n)), context)
        end if

        ! Row 13: strictly monotonic, which a NaN and a repeated value both fail.
        if (.not. interp_is_monotonic(xs, ascending)) then
            call interp_abort(entry, "x must be strictly increasing or strictly decreasing", context)
        end if

        ! Row 13a: finite. An infinite end knot passes the ordering and would make its segment's
        ! width infinite, so every query on that segment would answer NaN.
        if (.not. all(ieee_is_finite(xs))) call interp_abort(entry, "x must be finite", context)

        ! Row 14: finite ordinates.
        if (.not. all(ieee_is_finite(ys))) call interp_abort(entry, "y must be finite", context)

        ! Stored ascending: the workers see one order only.
        this%n = n
        this%reversed = .not. ascending
        if (ascending) then
            call move_alloc(xs, this%x)
            call move_alloc(ys, this%y)
        else
            this%x = xs(n:1:-1)
            this%y = ys(n:1:-1)
        end if

        call interp_uniform_step(this%x, this%uniform, this%step)
        if (this%method == M_CUBIC) then
            allocate (this%d(n))
            call interp_spline_coeffs(this%x, this%y, bc_code, this%d)
        end if

    end subroutine interp_1d_build

end submodule parquet_interpolate_1d
