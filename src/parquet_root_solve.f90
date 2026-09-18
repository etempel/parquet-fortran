!> The solver behind `pf_find_root`: the two specifics, every validation and the one
!> `error stop`, the bracket expansion, Brent's method and the evaluation record.
!!
!! **Brent's method as `zeroin.f` writes it** (R. P. Brent, *Algorithms for Minimization without
!! Derivatives*, 1973, chapter 4; Netlib's `zeroin.f` of Forsythe, Malcolm and Moler): bisection
!! guaranteeing convergence, with a secant or inverse-quadratic step accepted only when it lands
!! well inside the bracket and shrinks it faster than bisection would. It is about a hundred lines
!! with no upstream worth tracking, so it is written here rather than vendored, as `fmin` is in
!! `parquet_optimize_scalar.f90`. **Where it departs from `zeroin.f`**, each for a reason stated at
!! the site:
!!
!! 1. The sign test is `(fa > 0) .neqv. (fb > 0)` on two non-zero values, never a product.
!! 2. An interpolation step is used only where every operand it is formed from is finite and in
!!    range -- no infinite value, no length above `2**950`, and `|fa/fc|` at most `2**32` for the
!!    inverse quadratic -- and the step bisects everywhere else. That is what makes an infinite
!!    end a sign rather than a trap, and it keeps every product finite. Inside those ranges the
!!    step is `zeroin.f`'s exactly.
!! 3. The stopping tolerance is `2*eps*|b| + max(tol, rtol*|b|)/2` in place of `2*eps*|b| +
!!    tol/2`, floored at `tiny(1.0_real64)` so that a best point of exactly zero still steps, and
!!    saturating where `rtol*|b|` would pass `huge`.
!! 4. The bracket may be widened first, under the caller's `pf_bracket_expansion`, and no
!!    expansion builds a bracket wider than `WIDTH_CAP`, a few units of `epsilon` short of `huge`,
!!    so every difference of two points in it is finite.
!!
!! **The validation order is the order of the guide page's table**, and each abort's text is what
!! the matching out-of-process scenario in `test/error_scenarios.f90` asserts. Changing a message
!! means changing that scenario in the same commit.
!!
!! **Nothing that can abort is `pure`.** The validator and the abort helper are impure
!! deliberately: a `pure` guard-only subroutine's call is deleted by ifx at `-O0`
!! (`fortran-gotchas.md`). The arithmetic helpers that clamp a value before arithmetic uses it
!! are impure too, because the clamped value is a `volatile` local; see `step_up`.
submodule (parquet_root) parquet_root_solve

    implicit none

    !> The largest finite `real64`.
    real(real64), parameter :: BIG = huge(1.0_real64)
    !> Half of it, exactly: the bound every sum of two halves stays within.
    real(real64), parameter :: HALF_BIG = 0.5_real64*huge(1.0_real64)
    !> Machine epsilon, the unit of Brent's own `2*eps*|b|` stopping term.
    real(real64), parameter :: EPS = epsilon(1.0_real64)
    !> The smallest normal `real64`: the floor under the stopping tolerance, so that a best point
    !! of exactly zero with `tol = 0` still takes a step rather than evaluating itself again.
    real(real64), parameter :: TINY_TOL = tiny(1.0_real64)
    !> A factor eight units of `epsilon` below one. An expansion move is formed from a span cut
    !! this much short of the room it may use, so that the rounding of the few operations forming
    !! it can never carry the new end past its limit.
    real(real64), parameter :: SHRINK = 1.0_real64 - 8.0_real64*epsilon(1.0_real64)
    !> The widest bracket an expansion may build: `huge` cut by `SHRINK`. The end placed at this
    !! distance from the other is itself rounded, and at `huge` exactly that rounding alone can
    !! carry the width past `huge`, so the next `b - a` overflows.
    real(real64), parameter :: WIDTH_CAP = SHRINK*huge(1.0_real64)
    !> Largest half-width, step before last and point spacing an interpolation step is formed at.
    !!
    !! With `|fa/fc|` at most `2**32` the largest product the step forms is about `2**1016`,
    !! short of `huge`'s `2**1024`; a bracket wider than twice this is bisected until it is not,
    !! which costs at most 74 bisections from the widest bracket there is.
    real(real64), parameter :: INTERP_SPAN = 2.0_real64**950
    !> Reciprocal of the largest `|fa/fc|` the inverse quadratic step is formed at. A ratio beyond
    !! it takes a bisection step instead, and interpolation resumes once the values are within it.
    real(real64), parameter :: INTERP_RATIO_INV = 2.0_real64**(-32)

contains

    ! ---- the binding targets, implemented ahead of every call to them -------------------------
    !
    ! nagfor rejects a separate module procedure whose body appears BELOW a call to it in the same
    ! submodule (`code-style.md`), so these sit above the solver that uses them.

    module procedure root_history_append

        real(real64), allocatable :: grown_x(:), grown_f(:)
        integer                   :: cap, want

        cap = 0
        if (allocated(this%f)) cap = size(this%f)

        if (this%n >= cap) then
            ! Geometric growth, from a first block big enough that a short solve never
            ! reallocates. The evaluation budget bounds the final length.
            want = max(16, 2*cap)
            allocate (grown_x(want), grown_f(want))
            if (this%n > 0) then
                grown_x(1:this%n) = this%x(1:this%n)
                grown_f(1:this%n) = this%f(1:this%n)
            end if
            call move_alloc(grown_x, this%x)
            call move_alloc(grown_f, this%f)
        end if

        this%n = this%n + 1
        this%x(this%n) = x
        this%f(this%n) = f

    end procedure root_history_append

    module procedure func_rootfun_eval

        y = this%fp(x)

    end procedure func_rootfun_eval

    ! ---- the two specifics -------------------------------------------------------------------

    module procedure find_root_obj

        type(pf_bracket_expansion) :: grow       !! the growth policy in force
        type(pf_root_info)         :: outcome    !! what happened, filled as the search goes
        real(real64) :: tol_abs, tol_rel         !! the tolerances in force
        real(real64) :: lo, hi, flo, fhi         !! the bracket and the values at its two ends
        real(real64) :: new_lo, new_hi           !! where one expansion try would put the ends
        real(real64) :: w, mid                   !! the bracket's width, finite by construction,
                                                 !! and its midpoint
        real(real64) :: xa, xb, xc, fa, fb, fc   !! Brent's previous, best and contra points
        real(real64) :: d, e, xm, tol1           !! step, step before last, half-width, tolerance
        real(real64) :: s, t, r, p, q            !! the interpolation's ratios, numerator and
                                                 !! denominator
        real(real64) :: ax_cap                   !! largest |x| `rtol*|x|` is formed at
        real(real64) :: xr, fr                   !! the point handed back, and its value
        integer      :: budget                   !! evaluation budget in force
        integer      :: status                   !! the PF_ROOT_* code this call ends on
        logical      :: interp, accept           !! an interpolation may be formed; it passed
        ! Every value below is STORED before arithmetic uses it, which is what `volatile` buys: an
        ! optimiser may split a selection that feeds arithmetic into the arithmetic on each arm,
        ! forming both, and the arm not taken is exactly the overflow the selection avoided.
        ! nagfor's optimised build did that to `min(a, 0) + huge` (`fortran-gotchas.md`).
        real(real64), volatile :: sfa, sfb, sfc  !! fa, fb, fc, or harmless stand-ins
        real(real64), volatile :: sxm, sdx, se   !! xm, xb - xa, e, or harmless stand-ins
        real(real64), volatile :: sq             !! q, or one wherever the step is rejected
        real(real64), volatile :: step           !! the accepted step, or zero
        real(real64), volatile :: rtol_one       !! max(rtol, 1)

        if (present(expand)) grow = expand
        tol_abs = 0.0_real64
        if (present(tol)) tol_abs = tol
        tol_rel = RTOL_FLOOR
        if (present(rtol)) tol_rel = rtol
        budget = DEFAULT_MAX_NEVAL
        if (present(max_neval)) budget = max_neval

        call validate_call(a, b, tol_abs, tol_rel, budget, grow, context)
        ! Validated finite and non-negative above, so `max` never meets a NaN here.
        tol_rel = max(tol_rel, RTOL_FLOOR)
        ! Where `rtol*|x|` would pass `huge`, `|x|` is capped instead (`stop_tolerance`).
        rtol_one = max(tol_rel, 1.0_real64)
        ax_cap = (BIG/rtol_one)*SHRINK

        lo = a
        hi = b
        status = PF_ROOT_OK

        solve: block

            ! ---- the two ends: an exact zero is the answer, whatever else holds -------------
            call evaluate(lo, flo)
            if (flo == 0.0_real64) then
                xr = lo
                fr = flo
                exit solve
            end if
            if (outcome%neval >= budget) then
                status = PF_ROOT_LIMIT
                xr = lo
                fr = flo
                exit solve
            end if
            call evaluate(hi, fhi)
            if (fhi == 0.0_real64) then
                xr = hi
                fr = fhi
                exit solve
            end if

            ! ---- widen the bracket until its two ends differ in sign ------------------------
            !
            ! Each try moves the end or ends the policy names and evaluates each end that moved,
            ! and the loop stops at the FIRST sign change it sees -- in PF_EXPAND_BOTH after the
            ! lower end, before the upper one moves. An end that cannot move is not evaluated
            ! again, and a try in which neither can move ends the expansion as `max_tries` does.
            widen: do while (.not. opposite(flo, fhi))
                if (grow%mode == PF_EXPAND_NONE .or. outcome%nexpand >= grow%max_tries) then
                    status = PF_ROOT_NO_BRACKET
                    call take_the_better_end()
                    exit solve
                end if

                w = hi - lo
                new_lo = lo
                new_hi = hi
                select case (grow%mode)
                case (PF_EXPAND_UP)
                    new_hi = step_up(lo, w, grow%factor, grow%upper_limit, lo)
                case (PF_EXPAND_DOWN)
                    new_lo = step_down(hi, w, grow%factor, grow%lower_limit, hi)
                case (PF_EXPAND_BOTH)
                    ! The midpoint, formed inside the bracket so it cannot overflow. The upper
                    ! end's ceiling is taken from the lower end's NEW position, which is where
                    ! the width it must not stretch past WIDTH_CAP is measured from.
                    mid = lo + 0.5_real64*w
                    new_lo = step_down(mid, 0.5_real64*w, grow%factor, grow%lower_limit, hi)
                    new_hi = step_up(mid, 0.5_real64*w, grow%factor, grow%upper_limit, new_lo)
                end select

                if (.not. (new_lo < lo .or. new_hi > hi)) then
                    status = PF_ROOT_NO_BRACKET
                    call take_the_better_end()
                    exit solve
                end if
                ! A try counts once it evaluates something; the budget running out before it
                ! could is PF_ROOT_LIMIT, not a used-up policy.
                if (outcome%neval >= budget) then
                    status = PF_ROOT_LIMIT
                    call take_the_better_end()
                    exit solve
                end if
                outcome%nexpand = outcome%nexpand + 1

                if (new_lo < lo) then
                    lo = new_lo
                    call evaluate(lo, flo)
                    if (flo == 0.0_real64) then
                        xr = lo
                        fr = flo
                        exit solve
                    end if
                    if (opposite(flo, fhi)) exit widen
                end if
                if (new_hi > hi) then
                    if (outcome%neval >= budget) then
                        status = PF_ROOT_LIMIT
                        call take_the_better_end()
                        exit solve
                    end if
                    hi = new_hi
                    call evaluate(hi, fhi)
                    if (fhi == 0.0_real64) then
                        xr = hi
                        fr = fhi
                        exit solve
                    end if
                end if
            end do widen

            ! ---- Brent's method on [lo, hi], whose ends now differ in sign -----------------
            xa = lo
            fa = flo
            xb = hi
            fb = fhi
            xc = xa
            fc = fa
            d = xb - xa
            e = d
            brent: do
                ! Keep xb the point with the smaller |f|, and xc the one across the root from it.
                if (abs(fc) < abs(fb)) then
                    xa = xb
                    fa = fb
                    xb = xc
                    fb = fc
                    xc = xa
                    fc = fa
                end if
                tol1 = stop_tolerance(xb, tol_abs, tol_rel, ax_cap)
                xm = 0.5_real64*(xc - xb)
                if (abs(xm) <= tol1 .or. fb == 0.0_real64) then
                    xr = xb
                    fr = fb
                    exit solve
                end if
                if (outcome%neval >= budget) then
                    status = PF_ROOT_LIMIT
                    xr = xb
                    fr = fb
                    exit solve
                end if

                ! ---- the interpolation step: the secant when the previous point IS the
                ! contrapoint, the inverse quadratic through all three otherwise, as `zeroin.f`
                ! writes them, accepted only if it lands well inside the bracket and is shorter
                ! than half the step before last.
                !
                ! It is USED only where every operand is finite and in range, and it is FORMED
                ! always, from harmless stand-ins wherever it will not be used: an optimiser may
                ! form a guarded expression before its guard, and an infinite end or a ratio past
                ! `2**32` would then raise the very flag nagfor's `-ieee=stop` ends the process on
                ! (`fortran-gotchas.md`). The stand-ins are stored (the `volatile` locals) so that
                ! no optimiser can pair the selection with the arithmetic and form both arms. No
                ! value here is a NaN -- each was screened when it was evaluated -- so none of the
                ! comparisons can raise IEEE_INVALID; fa and fc are never zero.
                interp = abs(e) >= tol1 .and. abs(fa) > abs(fb)
                interp = interp .and. abs(fa) <= BIG .and. abs(fb) <= BIG .and. abs(fc) <= BIG
                interp = interp .and. abs(e) <= INTERP_SPAN .and. abs(xm) <= INTERP_SPAN
                interp = interp .and. abs(xb - xa) <= INTERP_SPAN
                if (xa /= xc) interp = interp .and. abs(fa)*INTERP_RATIO_INV <= abs(fc)
                sfa = merge(fa, 2.0_real64, interp)
                sfb = merge(fb, 1.0_real64, interp)
                sfc = merge(fc, -4.0_real64, interp)
                sxm = merge(xm, 1.0_real64, interp)
                sdx = merge(xb - xa, 1.0_real64, interp)
                se = merge(e, 1.0_real64, interp)
                s = sfb/sfa
                if (xa == xc) then
                    p = 2.0_real64*sxm*s
                    q = 1.0_real64 - s
                else
                    t = sfa/sfc
                    r = sfb/sfc
                    p = s*(2.0_real64*sxm*t*(t - r) - sdx*(r - 1.0_real64))
                    q = (t - 1.0_real64)*(r - 1.0_real64)*(s - 1.0_real64)
                end if
                if (p > 0.0_real64) then
                    q = -q
                else
                    p = -p
                end if
                accept = interp .and. 2.0_real64*p < 3.0_real64*sxm*q - abs(tol1*q)
                accept = accept .and. p < abs(0.5_real64*se*q)
                ! A rejected step may have q == 0; an accepted one never has (it needs `2p < 0`).
                sq = merge(q, 1.0_real64, accept)
                step = merge(p/sq, 0.0_real64, accept)
                if (accept) then
                    e = d
                    d = step
                else
                    d = xm
                    e = d
                end if

                xa = xb
                fa = fb
                ! Never a step shorter than the tolerance: it would say nothing new.
                if (abs(d) > tol1) then
                    xb = xb + d
                else
                    xb = xb + sign(tol1, xm)
                end if
                call evaluate(xb, fb)
                outcome%niter = outcome%niter + 1
                ! A value of the same sign as fc puts the root between xa and xb instead: xa
                ! becomes the contrapoint, and the steps restart from the new bracket's width.
                if (fb /= 0.0_real64) then
                    if (.not. opposite(fb, fc)) then
                        xc = xa
                        fc = fa
                        d = xb - xa
                        e = d
                    end if
                end if
            end do brent

        end block solve

        x = xr
        outcome%status = status
        outcome%converged = (status == PF_ROOT_OK)
        outcome%froot = fr
        outcome%bracket_lo = lo
        outcome%bracket_hi = hi
        if (present(info)) info = outcome
        if (present(converged)) converged = outcome%converged
        if (present(history)) call trim_history(history)

    contains

        !> Evaluates `f` at `xe`, refuses a NaN, and counts and records the evaluation.
        !!
        !! The NaN test is its own statement, ahead of every comparison the value reaches: an
        !! ordered comparison with a NaN operand raises IEEE_INVALID, fatal under nagfor's default
        !! `-ieee=stop`, and `.and.` does not short-circuit. An infinity passes; its sign is used.
        !! `pf_integrate` reports a NaN integrand value as a status instead; a root finder has no
        !! sign to carry on from, so here it is the caller's function to guard.
        subroutine evaluate(xe, fe)
            real(real64), intent(in)  :: xe !! where to evaluate
            real(real64), intent(out) :: fe !! the function's value there

            fe = f%eval(xe)
            if (ieee_is_nan(fe)) call root_abort("the function returned a NaN", context)
            outcome%neval = outcome%neval + 1
            if (present(history)) call history%append(xe, fe)

        end subroutine evaluate

        !> Hands back the end of the current bracket with the smaller `|f|`, the lower end on a
        !! tie: the answer when no sign change was found, or the budget ran out first.
        subroutine take_the_better_end()

            if (abs(fhi) < abs(flo)) then
                xr = hi
                fr = fhi
            else
                xr = lo
                fr = flo
            end if

        end subroutine take_the_better_end

    end procedure find_root_obj

    ! The FULLY RESTATED form, not `module procedure find_root_func`: in the abbreviated form
    ! gfortran 15 gives the `procedure(pf_root_func)` dummy an implicit interface and refuses the
    ! pointer assignment below (`fortran-gotchas.md`). It calls the object specific rather than
    ! repeating it, so the two forms agree to the bit by construction.
    module subroutine find_root_func(f, a, b, x, expand, tol, rtol, max_neval, converged, info, &
                                     history, context)
        implicit none
        procedure(pf_root_func)                          :: f         !! the function
        real(real64), intent(in)                         :: a         !! bracket, lower end
        real(real64), intent(in)                         :: b         !! bracket, upper end
        real(real64), intent(out)                        :: x         !! the root
        type(pf_bracket_expansion), intent(in), optional :: expand    !! the growth policy
        real(real64), intent(in), optional               :: tol       !! absolute tolerance
        real(real64), intent(in), optional               :: rtol      !! relative tolerance
        integer, intent(in), optional                    :: max_neval !! evaluation budget
        logical, intent(out), optional                   :: converged !! status is OK
        type(pf_root_info), intent(out), optional        :: info      !! what happened
        type(pf_root_history), intent(out), optional     :: history   !! every evaluation
        character(len=*), intent(in), optional           :: context   !! call-site text

        type(func_rootfun) :: wrapped !! the plain function as a function object

        wrapped%fp => f
        call find_root_obj(wrapped, a, b, x, expand, tol, rtol, max_neval, converged, info, &
                           history, context)

    end subroutine find_root_func

    ! ---- validation and the one abort ---------------------------------------------------------

    !> Refuses every call this module cannot answer, in the order the guide page's table lists.
    !!
    !! Each finiteness test is its own statement ahead of the ordered comparison it protects: a
    !! comparison with a NaN operand raises IEEE_INVALID, fatal under nagfor's `-ieee=stop` on
    !! exactly the input being refused, and `.or.` does not short-circuit. Impure deliberately.
    subroutine validate_call(a, b, tol_abs, tol_rel, budget, grow, context)
        real(real64), intent(in)               :: a       !! bracket, lower end
        real(real64), intent(in)               :: b       !! bracket, upper end
        real(real64), intent(in)               :: tol_abs !! resolved `tol`
        real(real64), intent(in)               :: tol_rel !! resolved `rtol`, before its floor
        integer, intent(in)                    :: budget  !! resolved `max_neval`
        type(pf_bracket_expansion), intent(in) :: grow    !! resolved `expand`
        character(len=*), intent(in), optional :: context !! caller's call-site text

        logical :: bad

        bad = .not. (ieee_is_finite(a) .and. ieee_is_finite(b))
        if (.not. bad) bad = (a >= b)
        if (bad) call root_abort("the bracket must satisfy a < b with finite ends", context)
        ! Two finite ends can still have an infinite WIDTH, and every step is measured against
        ! the width. Tested in HALVES rather than by forming `b - a`: the subtraction is the
        ! overflow, and under nagfor's default `-ieee=stop` it would end the process before this
        ! line could refuse it. `b/2 - a/2` is exactly half the width as `b - a` would round it,
        ! so it passes `huge/2` exactly when `b - a` would overflow. NOT `b > huge + min(a, 0)`:
        ! nagfor's optimised build turns `min(a, 0) + huge` into a selection between `a + huge`
        ! and `huge`, forms both, and overflows on a large positive `a`.
        if ((0.5_real64*b) - (0.5_real64*a) > HALF_BIG) &
            call root_abort("the bracket width must be finite", context)

        bad = .not. ieee_is_finite(tol_abs)
        if (.not. bad) bad = (tol_abs < 0.0_real64)
        if (bad) call root_abort("tol must be a non-negative finite number", context)
        bad = .not. ieee_is_finite(tol_rel)
        if (.not. bad) bad = (tol_rel < 0.0_real64)
        if (bad) call root_abort("rtol must be a non-negative finite number", context)
        if (budget < 1) call root_abort("max_neval must be at least 1", context)

        select case (grow%mode)
        case (PF_EXPAND_NONE, PF_EXPAND_UP, PF_EXPAND_DOWN, PF_EXPAND_BOTH)
        case default
            call root_abort("expand%mode must be one of PF_EXPAND_NONE, PF_EXPAND_UP, " &
                            //"PF_EXPAND_DOWN, PF_EXPAND_BOTH", context)
        end select
        bad = .not. ieee_is_finite(grow%factor)
        if (.not. bad) bad = (grow%factor <= 1.0_real64)
        if (bad) call root_abort("expand%factor must be a finite number greater than 1", context)
        if (grow%max_tries < 0) call root_abort("expand%max_tries must not be negative", context)
        ! A NaN limit would pass both comparisons below, and an infinite one would be evaluated at
        ! the first try that reached it, so both are refused before either comparison.
        bad = .not. (ieee_is_finite(grow%lower_limit) .and. ieee_is_finite(grow%upper_limit))
        if (.not. bad) bad = (grow%lower_limit > a .or. grow%upper_limit < b)
        if (bad) call root_abort("the expansion limits must be finite and lie outside the " &
                                 //"initial bracket", context)

    end subroutine validate_call

    !> Aborts with `pf_find_root: <text>`, plus the caller's context when one was given.
    !!
    !! The single `error stop` of this module. Taken under a named `critical` so that one
    !! thread aborts when the call is inside a parallel region: two threads reaching
    !! `ERROR STOP` at once leave the exit status nondeterministic under ifx.
    !! Impure deliberately -- a `pure` guard-only procedure's call is deleted by ifx at `-O0`.
    subroutine root_abort(text, context)
        character(len=*), intent(in)           :: text    !! what went wrong
        character(len=*), intent(in), optional :: context !! caller's call-site text

        character(len=:), allocatable :: msg

        msg = "pf_find_root: "//text
        if (present(context)) then
            if (len_trim(context) > CONTEXT_CAP) then
                msg = msg//" (context: "//context(1:CONTEXT_CAP)//"...)"
            else
                msg = msg//" (context: "//trim(context)//")"
            end if
        end if

        ! One thread aborts, not several: two threads reaching ERROR STOP at once leave the exit
        ! status nondeterministic under ifx (`api-conventions.md`).
        !$omp critical (parquet_root_abort)
        error stop msg
        !$omp end critical (parquet_root_abort)

    end subroutine root_abort

    ! ---- the record ------------------------------------------------------------------------------

    !> Cuts a record down to the `n` entries in use, so that `size(x) == n` on output.
    !!
    !! Every call that returns has evaluated at least once, so the record is allocated here.
    subroutine trim_history(history)
        type(pf_root_history), intent(inout) :: history !! the record to trim

        real(real64), allocatable :: cut_x(:), cut_f(:)
        integer                   :: used

        used = history%n
        if (size(history%f) == used) return
        allocate (cut_x(used), cut_f(used))
        cut_x(1:used) = history%x(1:used)
        cut_f(1:used) = history%f(1:used)
        call move_alloc(cut_x, history%x)
        call move_alloc(cut_f, history%f)

    end subroutine trim_history

    ! ---- the arithmetic ------------------------------------------------------------------------

    !> Do two NON-ZERO values differ in sign?
    !!
    !! `(fa > 0) .neqv. (fb > 0)` is exact for every finite and infinite pair, where the product
    !! `fa*fb < 0` underflows to zero for two small values, overflows for two large ones and is
    !! a NaN with an infinite operand. Neither value is a NaN -- each was screened when it was
    !! evaluated -- so neither comparison can raise IEEE_INVALID.
    pure function opposite(fa, fb) result(differ)
        real(real64), intent(in) :: fa     !! one value, not zero
        real(real64), intent(in) :: fb     !! the other, not zero
        logical                  :: differ !! the two signs differ

        differ = (fa > 0.0_real64) .neqv. (fb > 0.0_real64)

    end function opposite

    !> Brent's stopping tolerance at the best point: `2*eps*|xb| + max(tol, rtol*|xb|)/2`, floored
    !! at `tiny` and saturating short of `huge`.
    !!
    !! The floor keeps a best point of exactly zero stepping when `tol = 0`, where both relative
    !! terms vanish. The saturation caps `|xb|` at `ax_cap`, `(huge/max(rtol, 1))` less a few units
    !! of `epsilon`, so `rtol*|xb|` cannot overflow. For `rtol > 1` the capped tolerance is still
    !! about `huge/2`, no smaller than any half-width it is compared with, so the search ends where
    !! it would have; for `rtol <= 1` the cap binds only within a few units of `epsilon` of `huge`.
    !! The capped `|xb|` is stored before `rtol` multiplies it (see `find_root_obj`), which is why
    !! this is not `pure`.
    function stop_tolerance(xb, tol_abs, tol_rel, ax_cap) result(tol1)
        real(real64), intent(in) :: xb      !! the best point
        real(real64), intent(in) :: tol_abs !! `tol`, finite and non-negative
        real(real64), intent(in) :: tol_rel !! `rtol`, finite and at least `4*epsilon`
        real(real64), intent(in) :: ax_cap  !! largest `|xb|` that `tol_rel` may multiply
        real(real64)             :: tol1    !! the tolerance on the bracket's half-width

        real(real64)           :: ax
        real(real64), volatile :: ax_use !! `min(|xb|, ax_cap)`, stored before it is multiplied

        ax = abs(xb)
        ax_use = min(ax, ax_cap)
        tol1 = max(2.0_real64*EPS*ax + 0.5_real64*max(tol_abs, tol_rel*ax_use), TINY_TOL)

    end function stop_tolerance

    !> Where the upper end goes in one expansion try: `base + factor*span`, cut short at the top.
    !!
    !! The top is `upper_limit`, lowered where need be so that the bracket, whose lower end is
    !! `lo`, stays no wider than `WIDTH_CAP`. Nothing here can overflow in any evaluation order,
    !! and nothing is guarded. `min(lo, 0) + WIDTH_CAP` is finite for every `lo`, and so is the room
    !! between `base` and the top taken as a difference of HALVES, where the plain difference can
    !! overflow on its rounding alone; the move is formed from a span cut just short of that room;
    !! both candidates are finite before one is selected. The two clamped values are STORED
    !! before arithmetic uses them (`volatile`, so this is not `pure`): an optimiser may split a
    !! selection feeding arithmetic into the arithmetic on each arm and form both, and the arm
    !! not taken is exactly the overflow the clamp avoided (`fortran-gotchas.md`).
    function step_up(base, span, factor, upper_limit, lo) result(pos)
        real(real64), intent(in) :: base        !! where the move is measured from, in the bracket
        real(real64), intent(in) :: span        !! the length `factor` multiplies; positive
        real(real64), intent(in) :: factor      !! the growth factor; finite, above 1
        real(real64), intent(in) :: upper_limit !! the caller's limit; finite, not below the bracket
        real(real64), intent(in) :: lo          !! the bracket's lower end, after any move this try
        real(real64)             :: pos         !! the new upper end, at most the top

        real(real64)           :: top, half_room, span_cap
        real(real64), volatile :: lo_neg   !! `min(lo, 0)`, stored before `huge` is added to it
        real(real64), volatile :: span_use !! `min(span, span_cap)`, stored before it is scaled

        lo_neg = min(lo, 0.0_real64)
        top = min(upper_limit, lo_neg + WIDTH_CAP)
        half_room = (0.5_real64*top) - (0.5_real64*base)
        span_cap = 2.0_real64*((half_room/factor)*SHRINK)
        span_use = min(span, span_cap)
        pos = merge(base + factor*span_use, top, span <= span_cap)

    end function step_up

    !> Where the lower end goes in one expansion try: `base - factor*span`, cut short at the
    !! bottom. The mirror of `step_up`, for the same reasons.
    function step_down(base, span, factor, lower_limit, hi) result(pos)
        real(real64), intent(in) :: base        !! where the move is measured from, in the bracket
        real(real64), intent(in) :: span        !! the length `factor` multiplies; positive
        real(real64), intent(in) :: factor      !! the growth factor; finite, above 1
        real(real64), intent(in) :: lower_limit !! the caller's limit; finite, not above the bracket
        real(real64), intent(in) :: hi          !! the bracket's upper end
        real(real64)             :: pos         !! the new lower end, at least the bottom

        real(real64)           :: bottom, half_room, span_cap
        real(real64), volatile :: hi_pos   !! `max(hi, 0)`, stored before `huge` is taken from it
        real(real64), volatile :: span_use !! `min(span, span_cap)`, stored before it is scaled

        hi_pos = max(hi, 0.0_real64)
        bottom = max(lower_limit, hi_pos - WIDTH_CAP)
        half_room = (0.5_real64*base) - (0.5_real64*bottom)
        span_cap = 2.0_real64*((half_room/factor)*SHRINK)
        span_use = min(span, span_cap)
        pos = merge(base - factor*span_use, bottom, span <= span_cap)

    end function step_down

end submodule parquet_root_solve
