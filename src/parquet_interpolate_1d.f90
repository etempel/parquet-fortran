!> `pf_interp_1d`'s bindings, the one-shot `pf_interp` specifics and the test-only hook: every
!> validation of a one-dimensional table and every `error stop` it can reach.
!!
!! **The validation order is the order of the guide page's table**, and each abort's text is what
!! the matching out-of-process scenario in `test/error_scenarios.f90` asserts. Changing a message
!! means changing that scenario in the same commit. `%init` and both one-shot specifics validate
!! through one body, `interp_1d_build`, and differ only in the entry name their messages carry.
!!
!! **The evaluators are `pure` and abort in two cases only**, an object that was never built and a
!! derivative order that does not exist; a `pure` procedure cannot reach the named `critical` the
!! other aborts take, so those messages are the literal alone. Everything else they meet -- a NaN
!! query or limit, a point beyond the table -- is answered.
!!
!! **Every evaluator decides a point beyond the table the same way, before it brackets**: `"clamp"`
!! and `"nan"` answer there and then, and `"extrapolate"` continues the end segment's polynomial,
!! expanded about the end knot the point lies beyond (`interp_end_value`), so that no segment formula
!! is ever evaluated outside its own segment. The one other exception is `%eval` at the last knot,
!! which answers `y(n)` directly, so that a knot's value never depends on a segment formula at its
!! right end.
submodule (parquet_interpolate) parquet_interpolate_1d

    implicit none

contains

    ! ---- the bindings every other procedure here calls, implemented first ------------------------
    !
    ! nagfor rejects a separate module procedure whose body appears BELOW a call to it in the same
    ! submodule (`code-style.md`), so the evaluator sits above the one-shot specifics that use it.

    module procedure interp_1d_eval

        integer :: k
        logical :: near

        if (this%n == 0) error stop "pf_interp_1d%eval: the interpolant is not initialised"
        k = 0
        near = .false.
        call interp_1d_value(this, xq, k, near, v)

    end procedure interp_1d_eval

    module procedure interp_1d_eval_rank1

        real(real64) :: q, c0, c1
        integer      :: i, k, n, kend, found
        logical      :: near

        ! Refused before the queries are looked at, so an array holding none is refused as one query is.
        if (this%n == 0) error stop "pf_interp_1d%eval: the interpolant is not initialised"
        n = this%n
        allocate (v(size(xq)))
        k = 0
        near = .false.
        ! `interp_1d_value` written out, since a call per query is not inlined across a `-fPIC` build and
        ! this is the loop a column of queries runs; keep the two in step.
        do i = 1, size(xq)
            q = xq(i)
            if (q /= q) then
                v(i) = q
                cycle
            end if
            if (q < this%x(1)) then
                select case (this%outside)
                case (O_CLAMP)
                    v(i) = this%y(1)
                case (O_NAN)
                    v(i) = ieee_value(1.0_real64, ieee_quiet_nan)
                case default
                    call interp_1d_end_segment(this, .false., kend, c0, c1)
                    v(i) = interp_end_value(this%method, this%x(kend), this%x(kend + 1), this%y(kend), this%y(kend + 1), &
                                            c0, c1, .false., q)
                end select
                cycle
            else if (q > this%x(n)) then
                select case (this%outside)
                case (O_CLAMP)
                    v(i) = this%y(n)
                case (O_NAN)
                    v(i) = ieee_value(1.0_real64, ieee_quiet_nan)
                case default
                    call interp_1d_end_segment(this, .true., kend, c0, c1)
                    v(i) = interp_end_value(this%method, this%x(kend), this%x(kend + 1), this%y(kend), this%y(kend + 1), &
                                            c0, c1, .true., q)
                end select
                cycle
            else if (q == this%x(n)) then
                v(i) = this%y(n)
                cycle
            end if

            if (this%uniform) then
                found = interp_bracket(this%x, n, .true., this%step, q)
            else
                if (near) then
                    found = interp_bracket_near(this%x, n, k, q)
                else
                    found = interp_bracket(this%x, n, .false., this%step, q)
                end if
                near = k > 0 .and. abs(found - k) <= 1
                k = found
            end if

            select case (this%method)
            case (M_LINEAR)
                v(i) = interp_linear_seg_value(this%x(found), this%x(found + 1), this%y(found), this%y(found + 1), q)
            case (M_CUBIC)
                v(i) = interp_cubic_seg_value(this%x(found), this%x(found + 1), this%y(found), this%y(found + 1), &
                                              this%d(found), this%d(found + 1), q)
            case default
                v(i) = interp_hermite_seg_value(this%x(found), this%x(found + 1), this%y(found), this%y(found + 1), &
                                                this%d(found), this%d(found + 1), q)
            end select
        end do

    end procedure interp_1d_eval_rank1

    module procedure interp_1d_eval_rank2

        integer :: i1, i2, k
        logical :: near

        if (this%n == 0) error stop "pf_interp_1d%eval: the interpolant is not initialised"
        allocate (v(size(xq, 1), size(xq, 2)))
        k = 0
        near = .false.
        do i2 = 1, size(xq, 2)
            do i1 = 1, size(xq, 1)
                call interp_1d_value(this, xq(i1, i2), k, near, v(i1, i2))
            end do
        end do

    end procedure interp_1d_eval_rank2

    module procedure interp_1d_eval_rank3

        integer :: i1, i2, i3, k
        logical :: near

        if (this%n == 0) error stop "pf_interp_1d%eval: the interpolant is not initialised"
        allocate (v(size(xq, 1), size(xq, 2), size(xq, 3)))
        k = 0
        near = .false.
        do i3 = 1, size(xq, 3)
            do i2 = 1, size(xq, 2)
                do i1 = 1, size(xq, 1)
                    call interp_1d_value(this, xq(i1, i2, i3), k, near, v(i1, i2, i3))
                end do
            end do
        end do

    end procedure interp_1d_eval_rank3

    module procedure interp_1d_eval_rank4

        integer :: i1, i2, i3, i4, k
        logical :: near

        if (this%n == 0) error stop "pf_interp_1d%eval: the interpolant is not initialised"
        allocate (v(size(xq, 1), size(xq, 2), size(xq, 3), size(xq, 4)))
        k = 0
        near = .false.
        do i4 = 1, size(xq, 4)
            do i3 = 1, size(xq, 3)
                do i2 = 1, size(xq, 2)
                    do i1 = 1, size(xq, 1)
                        call interp_1d_value(this, xq(i1, i2, i3, i4), k, near, v(i1, i2, i3, i4))
                    end do
                end do
            end do
        end do

    end procedure interp_1d_eval_rank4

    module procedure interp_1d_eval_rank5

        integer :: i1, i2, i3, i4, i5, k
        logical :: near

        if (this%n == 0) error stop "pf_interp_1d%eval: the interpolant is not initialised"
        allocate (v(size(xq, 1), size(xq, 2), size(xq, 3), size(xq, 4), size(xq, 5)))
        k = 0
        near = .false.
        do i5 = 1, size(xq, 5)
            do i4 = 1, size(xq, 4)
                do i3 = 1, size(xq, 3)
                    do i2 = 1, size(xq, 2)
                        do i1 = 1, size(xq, 1)
                            call interp_1d_value(this, xq(i1, i2, i3, i4, i5), k, near, v(i1, i2, i3, i4, i5))
                        end do
                    end do
                end do
            end do
        end do

    end procedure interp_1d_eval_rank5

    module procedure interp_1d_eval_rank6

        integer :: i1, i2, i3, i4, i5, i6, k
        logical :: near

        if (this%n == 0) error stop "pf_interp_1d%eval: the interpolant is not initialised"
        allocate (v(size(xq, 1), size(xq, 2), size(xq, 3), size(xq, 4), size(xq, 5), size(xq, 6)))
        k = 0
        near = .false.
        do i6 = 1, size(xq, 6)
            do i5 = 1, size(xq, 5)
                do i4 = 1, size(xq, 4)
                    do i3 = 1, size(xq, 3)
                        do i2 = 1, size(xq, 2)
                            do i1 = 1, size(xq, 1)
                                call interp_1d_value(this, xq(i1, i2, i3, i4, i5, i6), k, near, v(i1, i2, i3, i4, i5, i6))
                            end do
                        end do
                    end do
                end do
            end do
        end do

    end procedure interp_1d_eval_rank6

    module procedure interp_1d_eval_rank7

        integer :: i1, i2, i3, i4, i5, i6, i7, k
        logical :: near

        if (this%n == 0) error stop "pf_interp_1d%eval: the interpolant is not initialised"
        allocate (v(size(xq, 1), size(xq, 2), size(xq, 3), size(xq, 4), size(xq, 5), size(xq, 6), size(xq, 7)))
        k = 0
        near = .false.
        do i7 = 1, size(xq, 7)
            do i6 = 1, size(xq, 6)
                do i5 = 1, size(xq, 5)
                    do i4 = 1, size(xq, 4)
                        do i3 = 1, size(xq, 3)
                            do i2 = 1, size(xq, 2)
                                do i1 = 1, size(xq, 1)
                                    call interp_1d_value(this, xq(i1, i2, i3, i4, i5, i6, i7), k, near, &
                                                         v(i1, i2, i3, i4, i5, i6, i7))
                                end do
                            end do
                        end do
                    end do
                end do
            end do
        end do

    end procedure interp_1d_eval_rank7

    module procedure interp_1d_derivative

        real(real64) :: c0, c1
        integer      :: k, n, ord

        if (this%n == 0) error stop "pf_interp_1d%derivative: the interpolant is not initialised"
        ord = 1
        if (present(order)) ord = order
        if (ord /= 1 .and. ord /= 2) error stop "pf_interp_1d%derivative: order must be 1 or 2"

        ! As in `%eval`: the NaN first, by self-comparison.
        if (xq /= xq) then
            v = xq
            return
        end if

        n = this%n
        if (xq < this%x(1) .or. xq > this%x(n)) then
            select case (this%outside)
            case (O_CLAMP)
                ! Clamped, the interpolant is constant beyond the table.
                v = 0.0_real64
            case (O_NAN)
                v = ieee_value(1.0_real64, ieee_quiet_nan)
            case default
                call interp_1d_end_segment(this, xq > this%x(n), k, c0, c1)
                v = interp_end_derivative(this%method, this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), c0, c1, &
                                          xq > this%x(n), xq, ord)
            end select
            return
        else
            k = interp_bracket(this%x, n, this%uniform, this%step, xq)
        end if

        select case (this%method)
        case (M_LINEAR)
            v = interp_linear_seg_derivative(this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), ord)
        case (M_CUBIC)
            v = interp_cubic_seg_derivative(this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), &
                                            this%d(k), this%d(k + 1), xq, ord)
        case default
            v = interp_hermite_seg_derivative(this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), &
                                              this%d(k), this%d(k + 1), xq, ord)
        end select

    end procedure interp_1d_derivative

    module procedure interp_1d_integral

        real(real64) :: lo, hi, below, above, inner, c0, c1
        integer      :: n, k, klo, khi, kend
        logical      :: flipped

        if (this%n == 0) error stop "pf_interp_1d%integral: the interpolant is not initialised"

        ! A NaN limit answers itself, before any comparison.
        if (a /= a) then
            s = a
            return
        end if
        if (b /= b) then
            s = b
            return
        end if
        if (a == b) then
            s = 0.0_real64
            return
        end if

        ! Integrate upwards, and negate at the end when the limits came the other way round.
        flipped = b < a
        if (flipped) then
            lo = b
            hi = a
        else
            lo = a
            hi = b
        end if

        n = this%n
        below = 0.0_real64
        above = 0.0_real64
        if (lo < this%x(1) .or. hi > this%x(n)) then
            select case (this%outside)
            case (O_NAN)
                s = ieee_value(1.0_real64, ieee_quiet_nan)
                return
            case (O_CLAMP)
                ! Clamped, the interpolant is the end ordinate beyond each end, so what lies outside
                ! contributes its width times that ordinate, and the rest is integrated inside.
                if (hi <= this%x(1)) then
                    s = interp_1d_flat(hi - lo, this%y(1))
                    if (flipped) s = -s
                    return
                end if
                if (lo >= this%x(n)) then
                    s = interp_1d_flat(hi - lo, this%y(n))
                    if (flipped) s = -s
                    return
                end if
                if (lo < this%x(1)) then
                    below = interp_1d_flat(this%x(1) - lo, this%y(1))
                    lo = this%x(1)
                end if
                if (hi > this%x(n)) then
                    above = interp_1d_flat(hi - this%x(n), this%y(n))
                    hi = this%x(n)
                end if
            case default
                ! Extrapolated, what lies beyond each end is the end segment's polynomial about the end
                ! knot, integrated from that knot; the rest is integrated inside.
                if (hi <= this%x(1)) then
                    call interp_1d_end_segment(this, .false., kend, c0, c1)
                    s = interp_1d_end_piece(this, kend, c0, c1, .false., hi) - &
                        interp_1d_end_piece(this, kend, c0, c1, .false., lo)
                    if (flipped) s = -s
                    return
                end if
                if (lo >= this%x(n)) then
                    call interp_1d_end_segment(this, .true., kend, c0, c1)
                    s = interp_1d_end_piece(this, kend, c0, c1, .true., hi) - &
                        interp_1d_end_piece(this, kend, c0, c1, .true., lo)
                    if (flipped) s = -s
                    return
                end if
                if (lo < this%x(1)) then
                    call interp_1d_end_segment(this, .false., kend, c0, c1)
                    below = -interp_1d_end_piece(this, kend, c0, c1, .false., lo)
                    lo = this%x(1)
                end if
                if (hi > this%x(n)) then
                    call interp_1d_end_segment(this, .true., kend, c0, c1)
                    above = interp_1d_end_piece(this, kend, c0, c1, .true., hi)
                    hi = this%x(n)
                end if
            end select
        end if

        ! Both limits now lie inside the table: the segments they fall in, then the partial segment of
        ! each limit and every whole segment between them. Each piece is integrated from its segment's
        ! left knot, so a limit on a knot contributes an exact zero.
        klo = interp_bracket(this%x, n, this%uniform, this%step, lo)
        khi = interp_bracket(this%x, n, this%uniform, this%step, hi)
        if (klo == khi) then
            inner = interp_1d_piece(this, khi, hi) - interp_1d_piece(this, klo, lo)
        else
            inner = interp_1d_piece(this, klo, this%x(klo + 1)) - interp_1d_piece(this, klo, lo)
            do k = klo + 1, khi - 1
                inner = inner + interp_1d_piece(this, k, this%x(k + 1))
            end do
            inner = inner + interp_1d_piece(this, khi, hi)
        end if

        s = below + inner + above
        if (flipped) s = -s

    end procedure interp_1d_integral

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

    ! The result is ALLOCATABLE: ifx builds the caller's temporary for an explicit-shape function result
    ! on the stack, which a large `xq` overflows, on an OpenMP worker's small stack first
    ! (`test_large_tables_on_worker_threads`, test/test_interpolate_omp.f90). And the interface is
    ! restated in FULL rather than abbreviated to `module procedure interp_1d_oneshot_array`: nagfor 7.2
    ! compiles the abbreviated form of a result shaped by `xq`, an assumed-shape dummy that is not the
    ! first argument, with every dummy's descriptor wrong -- so a result given an explicit shape again
    ! needs this form (`fortran-gotchas.md`).
    module function interp_1d_oneshot_array(x, y, xq, method, bc, slopes, outside, is_valid, &
                                            context) result(yq)
        implicit none
        real(real64), intent(in)               :: x(:)          !! abscissae, strictly monotonic
        real(real64), intent(in)               :: y(:)          !! ordinates, one per abscissa
        real(real64), intent(in)               :: xq(:)         !! the query points
        character(len=*), intent(in), optional :: method        !! `"linear"`, `"cubic"` or `"pchip"`
        character(len=*), intent(in), optional :: bc            !! the spline's end condition
        real(real64), intent(in), optional     :: slopes(:)     !! end slopes for `bc="clamped"`
        character(len=*), intent(in), optional :: outside       !! the out-of-range policy
        logical, intent(in), optional          :: is_valid(:)   !! `.false.` drops that point
        character(len=*), intent(in), optional :: context       !! call-site text
        real(real64), allocatable              :: yq(:)         !! the interpolated values, one per query

        type(pf_interp_1d) :: c

        call interp_1d_build(c, "pf_interp", x, y, method, bc, slopes, outside, is_valid, context)
        yq = c%eval(xq)

    end function interp_1d_oneshot_array

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
        real(real64)                  :: s1, sn, widest
        character(len=TOKEN_CAP)      :: tok
        character(len=:), allocatable :: quoted
        integer                       :: n, bc_code, need, i
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
                this%method = M_PCHIP
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
            case ("not_a_knot")
                bc_code = B_NOT_A_KNOT
            case ("clamped")
                bc_code = B_CLAMPED
            case default
                call interp_quote(bc, quoted)
                call interp_abort(entry, "unknown bc "//quoted//'; expected "natural", "not_a_knot" or "clamped"', &
                                  context)
            end select
        end if

        ! Row 8: the clamped end condition is its slopes.
        if (bc_code == B_CLAMPED .and. .not. present(slopes)) then
            call interp_abort(entry, 'bc "clamped" needs slopes', context)
        end if

        s1 = 0.0_real64
        sn = 0.0_real64
        if (present(slopes)) then
            ! Row 9: slopes belong to the clamped end condition alone.
            if (bc_code /= B_CLAMPED) call interp_abort(entry, 'slopes apply only to bc "clamped"', context)
            ! Row 9a: one slope per end.
            if (size(slopes) /= 2) then
                call interp_abort(entry, "slopes must hold exactly 2 values, one per end; got "// &
                                  trim(interp_i2s(size(slopes))), context)
            end if
            ! Row 10: finite slopes.
            if (.not. interp_all_finite(slopes)) call interp_abort(entry, "slopes must be finite", context)
            s1 = slopes(1)
            sn = slopes(2)
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

        ! Row 12: enough points survive the mask for the method. A not-a-knot spline needs four: its
        ! end conditions are two distinct rows only then. The survivors are copied by hand rather than
        ! with `pack`, whose result ifx builds on the stack, which a table of a few hundred thousand
        ! points overflows on an OpenMP worker thread (`test_large_tables_on_worker_threads`,
        ! test/test_interpolate_omp.f90).
        if (present(is_valid)) then
            allocate (xs(count(is_valid)), ys(count(is_valid)))
            n = 0
            do i = 1, size(x)
                if (is_valid(i)) then
                    n = n + 1
                    xs(n) = x(i)
                    ys(n) = y(i)
                end if
            end do
        else
            xs = x
            ys = y
        end if
        n = size(xs)
        need = 2
        if (bc_code == B_NOT_A_KNOT) need = 4
        if (n < need) then
            select case (this%method)
            case (M_LINEAR)
                quoted = '"linear"'
            case (M_PCHIP)
                quoted = '"pchip"'
            case default
                if (bc_code == B_NOT_A_KNOT) then
                    quoted = '"cubic" with bc "not_a_knot"'
                else
                    quoted = '"cubic"'
                end if
            end select
            call interp_abort(entry, "at least "//trim(interp_i2s(need))//" points are needed for method "// &
                              quoted//"; got "//trim(interp_i2s(n)), context)
        end if

        ! Row 13: strictly monotonic, which a NaN and a repeated value both fail.
        if (.not. interp_is_monotonic(xs, ascending)) then
            call interp_abort(entry, "x must be strictly increasing or strictly decreasing", context)
        end if

        ! Row 13a: finite. An infinite end knot passes the ordering and would make its segment's
        ! width infinite, so every query on that segment would answer NaN. Only an end can be
        ! infinite in a strictly monotonic table, and neither is a NaN by now, so the comparisons are
        ! quiet.
        if (.not. (abs(xs(1)) <= huge(xs(1)) .and. abs(xs(n)) <= huge(xs(n)))) then
            call interp_abort(entry, "x must be finite", context)
        end if

        ! Row 14: finite ordinates.
        if (.not. interp_all_finite(ys)) call interp_abort(entry, "y must be finite", context)

        ! Stored ascending: the workers see one order only. A slope is a derivative in `x`, which
        ! reversing the table leaves alone, so the two end slopes change places and not sign.
        this%n = n
        this%reversed = .not. ascending
        if (ascending) then
            call move_alloc(xs, this%x)
            call move_alloc(ys, this%y)
        else
            this%x = xs(n:1:-1)
            this%y = ys(n:1:-1)
            call interp_swap(s1, sn)
        end if

        call interp_uniform_step(this%x, this%uniform, this%step)
        select case (this%method)
        case (M_CUBIC)
            ! Row 15: no spacing wider than the spline's limit, since its segment formula squares one.
            widest = interp_widest_spacing(this%x)
            if (widest > SPLINE_MAX_SPACING) then
                call interp_abort(entry, "x has a spacing of "//trim(interp_r2s(widest))//", above the cubic "// &
                                  "spline's limit of "//trim(interp_r2s(SPLINE_MAX_SPACING))// &
                                  '; rescale x, or use method "linear" or "pchip"', context)
            end if
            allocate (this%d(n))
            call interp_spline_coeffs(this%x, this%y, bc_code, s1, sn, this%d)
            ! Row 16: second derivatives that did not overflow in the solve, as they do over spacings far
            ! below the ordinates' scale.
            if (.not. interp_all_finite(this%d)) then
                call interp_abort(entry, "the cubic spline's second derivatives overflow, with x as closely spaced "// &
                                  "as "//trim(interp_r2s(interp_narrowest_spacing(this%x)))// &
                                  '; rescale the table, or use method "linear" or "pchip"', context)
            end if
        case (M_PCHIP)
            allocate (this%d(n))
            call interp_pchip_slopes(this%x, this%y, this%d)
        end select

    end subroutine interp_1d_build

    ! ---- helpers private to this submodule -------------------------------------------------------

    !> The value every specific of `%eval` answers at one query: a NaN its own NaN, a point beyond the
    !! table what the object's policy says, the last knot its ordinate, and any other point its
    !! segment's polynomial.
    !!
    !! `k` is the segment the last query searched for fell in, 0 before any, and becomes this query's;
    !! `near` says to search from `k` rather than bisect from scratch. It is set when two queries in a
    !! row fall in the same or neighbouring segments, as queries in order do, and cleared when a query
    !! lands further off, as queries in no order do: they then pay one comparison of two integers over
    !! a bisection, and queries in order a comparison or two in all. An evenly spaced table is bracketed
    !! by arithmetic whatever the two hold. Neither can change the answer: the segment a query falls in
    !! is unique.
    pure subroutine interp_1d_value(this, q, k, near, v)
        type(pf_interp_1d), intent(in) :: this !! a built interpolant
        real(real64), intent(in)       :: q    !! the query
        integer, intent(inout)         :: k    !! the last query's segment, or 0; this query's after
        logical, intent(inout)         :: near !! search from `k`; whether the next query should, after
        real(real64), intent(out)      :: v    !! the interpolated value

        real(real64) :: c0, c1
        integer      :: n, kend, found

        ! A NaN query is answered before any comparison, as `q /= q` rather than `ieee_is_nan`: this is
        ! the per-element path, and `ieee_is_nan` is a runtime call under ifx and nagfor.
        if (q /= q) then
            v = q
            return
        end if

        n = this%n
        if (q < this%x(1)) then
            select case (this%outside)
            case (O_CLAMP)
                v = this%y(1)
            case (O_NAN)
                v = ieee_value(1.0_real64, ieee_quiet_nan)
            case default
                call interp_1d_end_segment(this, .false., kend, c0, c1)
                v = interp_end_value(this%method, this%x(kend), this%x(kend + 1), this%y(kend), this%y(kend + 1), &
                                     c0, c1, .false., q)
            end select
            return
        else if (q > this%x(n)) then
            select case (this%outside)
            case (O_CLAMP)
                v = this%y(n)
            case (O_NAN)
                v = ieee_value(1.0_real64, ieee_quiet_nan)
            case default
                call interp_1d_end_segment(this, .true., kend, c0, c1)
                v = interp_end_value(this%method, this%x(kend), this%x(kend + 1), this%y(kend), this%y(kend + 1), &
                                     c0, c1, .true., q)
            end select
            return
        else if (q == this%x(n)) then
            ! The one knot that is the RIGHT end of its segment, where a segment formula would need
            ! `h/h` to round to exactly one; answered directly instead.
            v = this%y(n)
            return
        end if

        if (this%uniform) then
            ! Arithmetic needs no history, and keeps none.
            found = interp_bracket(this%x, n, .true., this%step, q)
        else
            if (near) then
                found = interp_bracket_near(this%x, n, k, q)
            else
                found = interp_bracket(this%x, n, .false., this%step, q)
            end if
            near = k > 0 .and. abs(found - k) <= 1
            k = found
        end if

        select case (this%method)
        case (M_LINEAR)
            v = interp_linear_seg_value(this%x(found), this%x(found + 1), this%y(found), this%y(found + 1), q)
        case (M_CUBIC)
            v = interp_cubic_seg_value(this%x(found), this%x(found + 1), this%y(found), this%y(found + 1), &
                                       this%d(found), this%d(found + 1), q)
        case default
            v = interp_hermite_seg_value(this%x(found), this%x(found + 1), this%y(found), this%y(found + 1), &
                                         this%d(found), this%d(found + 1), q)
        end select

    end subroutine interp_1d_value

    !> Exchanges two values.
    pure subroutine interp_swap(a, b)
        real(real64), intent(inout) :: a !! one value
        real(real64), intent(inout) :: b !! the other

        real(real64) :: t

        t = a
        a = b
        b = t

    end subroutine interp_swap

    !> The end segment `"extrapolate"` continues beyond one end of the table: its left knot, and the
    !! method's coefficients at its two knots -- second derivatives for the spline, slopes for PCHIP,
    !! and zeros for `"linear"`, which has none stored.
    pure subroutine interp_1d_end_segment(this, above, k, c0, c1)
        type(pf_interp_1d), intent(in) :: this  !! a built interpolant
        logical, intent(in)            :: above !! the last segment, rather than the first
        integer, intent(out)           :: k     !! the segment's left knot
        real(real64), intent(out)      :: c0    !! the coefficient at `x(k)`
        real(real64), intent(out)      :: c1    !! the coefficient at `x(k + 1)`

        k = merge(this%n - 1, 1, above)
        c0 = 0.0_real64
        c1 = 0.0_real64
        if (this%method /= M_LINEAR) then
            c0 = this%d(k)
            c1 = this%d(k + 1)
        end if

    end subroutine interp_1d_end_segment

    !> The integral of the end segment `k`'s polynomial, continued beyond the table, from its end knot
    !! to `q`.
    pure function interp_1d_end_piece(this, k, c0, c1, above, q) result(v)
        type(pf_interp_1d), intent(in) :: this  !! a built interpolant
        integer, intent(in)            :: k     !! the end segment's left knot
        real(real64), intent(in)       :: c0    !! the coefficient at `x(k)`
        real(real64), intent(in)       :: c1    !! the coefficient at `x(k + 1)`
        logical, intent(in)            :: above !! the last segment, rather than the first
        real(real64), intent(in)       :: q     !! the upper limit, at or beyond the end knot
        real(real64)                   :: v     !! the integral

        v = interp_end_integral(this%method, this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), c0, c1, above, q)

    end function interp_1d_end_piece

    !> The integral of segment `k`'s polynomial from its left knot to `q` inside it.
    pure function interp_1d_piece(this, k, q) result(v)
        type(pf_interp_1d), intent(in) :: this !! a built interpolant
        integer, intent(in)            :: k    !! the segment's left knot
        real(real64), intent(in)       :: q    !! the upper limit
        real(real64)                   :: v    !! the integral

        select case (this%method)
        case (M_LINEAR)
            v = interp_linear_seg_integral(this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), q)
        case (M_CUBIC)
            v = interp_cubic_seg_integral(this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), &
                                          this%d(k), this%d(k + 1), q)
        case default
            v = interp_hermite_seg_integral(this%x(k), this%x(k + 1), this%y(k), this%y(k + 1), &
                                            this%d(k), this%d(k + 1), q)
        end select

    end function interp_1d_piece

    !> A constant's integral over a width: an infinity of the constant's sign over an infinite width, and
    !! zero for a zero constant whatever the width -- never `Infinity*0`, which is a NaN and raises
    !! IEEE_INVALID.
    !!
    !! No product with an infinite width is formed at all, not even one that would be discarded: ifx
    !! compiles `if (value == 0) then; v = 0; else; v = width*value; end if` without a branch, forming the
    !! product first and selecting afterwards, so that form raises IEEE_INVALID while answering zero. The
    !! width is replaced by 1 before the product wherever it is infinite.
    pure function interp_1d_flat(width, value) result(v)
        real(real64), intent(in) :: width !! the width, positive, possibly infinite
        real(real64), intent(in) :: value !! the constant
        real(real64)             :: v     !! the integral

        real(real64) :: finite_width

        finite_width = merge(width, 1.0_real64, width <= huge(width))
        if (width <= huge(width)) then
            v = finite_width*value
        else if (value > 0.0_real64) then
            v = ieee_value(1.0_real64, ieee_positive_inf)
        else if (value < 0.0_real64) then
            v = ieee_value(1.0_real64, ieee_negative_inf)
        else
            v = 0.0_real64
        end if

    end function interp_1d_flat

end submodule parquet_interpolate_1d
