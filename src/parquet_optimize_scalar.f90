!> Brent's method for a function of one variable on a bracket (`pf_minimize_scalar`).
!!
!! Golden-section steps with successive parabolic interpolation, in the shape of Brent's `fmin`
!! (Algorithms for Minimization without Derivatives, 1973) as Netlib's `fmin.f` of Forsythe,
!! Malcolm and Moler writes it. A parabola through the three best points is accepted only when its
!! vertex falls inside the bracket and moves less than half the step before last; otherwise the
!! bracket is cut in the golden ratio. Written here rather than vendored: it is about a hundred
!! lines and has no upstream worth tracking.
!!
!! **One deviation from `fmin.f`**: the effective tolerance carries a floor of `epsilon` times the
!! caller's own bracket width. Without it the tolerance vanishes as the best point approaches zero
!! and a minimiser AT zero can never be converged on -- `fmin.f` itself does not stop on `x**2`
!! over `[-1, 1]`, and neither does SciPy's bounded method at `xatol = 0`. So the evaluations of a
!! run here can differ from `fmin.f`'s by a few, while the minimiser agrees to the last few units
!! in the last place.
!!
!! **The method assumes only a bracket, not unimodality.** On a function with several minima in
!! `[a, b]` it converges to one of them and reports success, which is what `info%converged` means
!! everywhere in this module.
submodule (parquet_optimize) parquet_optimize_scalar

    implicit none

contains

    module procedure minimize_scalar_obj

        !> Squared inverse of the golden ratio, the golden-section fraction Brent's method cuts by.
        real(real64), parameter :: GOLD = 0.5_real64*(3.0_real64 - sqrt(5.0_real64))

        real(real64) :: lo, hi        !! the bracket, narrowed as the search proceeds
        real(real64) :: v, w, xb      !! the three best points: previous-previous, previous, best
        real(real64) :: fv, fw, fxb   !! their values
        real(real64) :: u, fu         !! the trial point and its value
        real(real64) :: d, e          !! the step taken, and the step before last
        real(real64) :: p, q, r       !! the parabola's numerator, denominator and scratch
        real(real64) :: xm, tol1, tol2!! bracket midpoint and the effective tolerances
        real(real64) :: tol_use       !! tolerance actually in force
        real(real64) :: point(1)      !! the objective's one-element argument
        integer      :: budget        !! evaluation budget actually in force
        integer      :: neval, niter  !! evaluations and iterations performed
        integer      :: status        !! the PF_OPT_* code this run ends on
        logical      :: golden, bad   !! take a golden-section step; a validation test failed

        call refuse_constrained("pf_minimize_scalar", f, context)

        ! ---- validate ---------------------------------------------------------------------
        !
        ! Finiteness first and the ordered comparison only on what survived: `a >= b` signals IEEE
        ! invalid on a NaN operand, which is the very input this rejects (nagfor `-ieee=stop`).
        bad = .not. (ieee_is_finite(a) .and. ieee_is_finite(b))
        if (.not. bad) bad = (a >= b)
        if (bad) call optimize_abort("pf_minimize_scalar", &
            "the bracket must satisfy a < b with finite ends", context)

        ! Two finite ends can still have an infinite WIDTH, and the width is what every tolerance
        ! and every golden section here is measured against. Tested as `b > huge + a` rather than
        ! by forming `b - a`: the subtraction is the overflow, and under nagfor's default
        ! `-ieee=stop` it ends the process before this line could refuse it. `a < 0` makes
        ! `huge + a` finite, so the guard cannot overflow either.
        if (a < 0.0_real64) then
            if (b > huge(1.0_real64) + a) call optimize_abort("pf_minimize_scalar", &
                "the bracket width must be finite", context)
        end if

        tol_use = 0.0_real64
        if (present(atol)) then
            call validate_tolerance("pf_minimize_scalar", "atol", atol, context)
            tol_use = atol
        end if

        budget = SCALAR_MAX_NEVAL
        if (present(max_neval)) then
            call validate_budget("pf_minimize_scalar", max_neval, context)
            budget = max_neval
        end if

        if (present(history)) then
            history%n = 0
            allocate(history%x(1, 0))
            allocate(history%f(0))
        end if

        ! ---- the first point, one golden section in from the lower end ----------------------
        lo = a
        hi = b
        xb = lo + GOLD*(hi - lo)
        w = xb
        v = xb
        d = 0.0_real64
        e = 0.0_real64

        point(1) = xb
        fxb = f%eval(point)
        call screen_value(fxb, context)
        neval = 1
        niter = 0
        if (present(history)) call history%add(point, fxb)
        fw = fxb
        fv = fxb

        status = PF_OPT_OK
        search: do

            xm = 0.5_real64*(lo + hi)
            ! **The last term is a floor, and it is this library's rather than `fmin.f`'s.** The
            ! other two vanish as the best point approaches zero -- `sqrt(epsilon)*abs(x)` with it,
            ! and `atol/3` when the caller asks for all the accuracy the arithmetic allows -- so on
            ! a minimiser AT zero the stopping test chases a target that recedes as fast as the
            ! bracket narrows, and the run spends its whole budget on an answer it reached early.
            ! A floor proportional to the ORIGINAL bracket keeps the test meetable there while
            ! staying far below what any caller asks for: over the reference suite it moved no
            ! answer by more than a few units in the last place.
            tol1 = sqrt(epsilon(1.0_real64))*abs(xb) + tol_use/3.0_real64 &
                 + epsilon(1.0_real64)*(b - a)
            tol2 = 2.0_real64*tol1

            ! Brent's stopping rule: the best point is within tol2 of the bracket's midpoint,
            ! allowing for how much bracket is left. With `atol = 0` this is the arithmetic's own
            ! floor rather than a caller's request.
            if (abs(xb - xm) <= tol2 - 0.5_real64*(hi - lo)) exit search

            if (neval >= budget) then
                status = PF_OPT_LIMIT
                exit search
            end if

            niter = niter + 1

            ! ---- choose the step: a parabola if it is safe, golden section otherwise --------
            golden = .true.
            if (abs(e) > tol1) then
                r = (xb - w)*(fxb - fv)
                q = (xb - v)*(fxb - fw)
                p = (xb - v)*q - (xb - w)*r
                q = 2.0_real64*(q - r)
                if (q > 0.0_real64) p = -p
                q = abs(q)
                r = e
                e = d
                ! The parabola is taken only when its vertex lands inside the bracket AND moves
                ! less than half the step before last -- without the second test the iteration can
                ! cycle on a function the parabola models badly.
                if (abs(p) < abs(0.5_real64*q*r) .and. p > q*(lo - xb) .and. p < q*(hi - xb)) then
                    d = p/q
                    u = xb + d
                    ! Never evaluate within tol2 of an end: the value there says nothing new.
                    if (u - lo < tol2 .or. hi - u < tol2) d = sign(tol1, xm - xb)
                    golden = .false.
                end if
            end if

            if (golden) then
                if (xb >= xm) then
                    e = lo - xb
                else
                    e = hi - xb
                end if
                d = GOLD*e
            end if

            ! Never evaluate within tol1 of the best point either.
            if (abs(d) >= tol1) then
                u = xb + d
            else
                u = xb + sign(tol1, d)
            end if

            point(1) = u
            fu = f%eval(point)
            call screen_value(fu, context)
            neval = neval + 1
            if (present(history)) call history%add(point, fu)

            ! ---- narrow the bracket and reorder the three best points ----------------------
            if (fu <= fxb) then
                if (u >= xb) then
                    lo = xb
                else
                    hi = xb
                end if
                v = w
                fv = fw
                w = xb
                fw = fxb
                xb = u
                fxb = fu
            else
                if (u < xb) then
                    lo = u
                else
                    hi = u
                end if
                if (fu <= fw .or. w == xb) then
                    v = w
                    fv = fw
                    w = u
                    fw = fu
                else if (fu <= fv .or. v == xb .or. v == w) then
                    v = u
                    fv = fu
                end if
            end if

        end do search

        x = xb
        fmin = fxb

        if (present(history)) call history_trim(history)

        ! `converged` and `info%converged` are ONE expression, so the short answer and the long one
        ! cannot drift apart, and the flag is set whether or not `info` was asked for.
        if (present(converged)) converged = (status == PF_OPT_OK)
        if (present(info)) then
            info%status = status
            info%converged = (status == PF_OPT_OK)
            info%neval = neval
            info%niter = niter
        end if

    contains

        !> Aborts when the objective returned a NaN or an infinity.
        !!
        !! Its own statement at the ONE evaluation site of this engine, before the value reaches
        !! any comparison: every decision below is an ordered comparison, and nagfor traps on one
        !! with a NaN operand. `ieee_is_finite` rather than the hot-path `v /= v`, because that
        !! form sees a NaN and misses the infinities 5.6 also refuses; the objective call dominates
        !! the cost either way.
        subroutine screen_value(value, ctx)
            real(real64), intent(in)               :: value !! what the objective returned
            character(len=*), intent(in), optional :: ctx   !! caller's call-site text

            if (.not. ieee_is_finite(value)) call optimize_abort("pf_minimize_scalar", &
                "the objective returned a non-finite value", ctx)

        end subroutine screen_value

    end procedure minimize_scalar_obj

    ! The FULLY RESTATED form, not `module procedure minimize_scalar_func`: in the abbreviated
    ! form gfortran 15 gives the `procedure(pf_objective_func)` dummy an implicit interface and
    ! refuses the pointer assignment below with "Explicit interface required for 'f'".
    module subroutine minimize_scalar_func(f, a, b, x, fmin, atol, max_neval, converged, info, history, context)
        implicit none
        procedure(pf_objective_func)                     :: f         !! the objective
        real(real64), intent(in)                         :: a         !! bracket, lower end
        real(real64), intent(in)                         :: b         !! bracket, upper end
        real(real64), intent(out)                        :: x         !! the minimiser found
        real(real64), intent(out)                        :: fmin      !! value at `x`
        real(real64), intent(in), optional               :: atol      !! tolerance on `x`
        integer, intent(in), optional                    :: max_neval !! evaluation budget; at most huge(1)/2
        logical, intent(out), optional                   :: converged !! the run's own rule fired
        type(pf_optimize_info), intent(out), optional    :: info      !! what happened
        type(pf_optimize_history), intent(out), optional :: history   !! every evaluation
        character(len=*), intent(in), optional           :: context   !! call-site text

        type(func_objective) :: obj !! wraps the plain function as an objective object

        obj%fun => f
        call minimize_scalar_obj(obj, a, b, x, fmin, atol=atol, max_neval=max_neval, &
                                 converged=converged, info=info, &
                                 history=history, context=context)

    end subroutine minimize_scalar_func

end submodule parquet_optimize_scalar
