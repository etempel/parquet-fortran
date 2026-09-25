!> The Nelder-Mead simplex (`pf_minimize_simplex`) and `pf_simplex_solver%run`.
!!
!! **Ported step for step from `../qfeet/src/minimization.f90`'s `minimize`**, so that its tests
!! and its documented behaviour migrate with it: reflection at `-1`, expansion at `2`, contraction
!! at `0.5`, the shrink onto the best vertex, the value-spread test written as a branch rather than
!! a `merge` (a `merge` evaluates `0/0` before discarding it, raising IEEE invalid under an
!! unoptimised build), the budget tested once per iteration, and the best vertex swapped to the
!! front before returning. The simplex is held one vertex per ROW, as qfeet holds it, so that the
!! two can be compared line by line.
!!
!! What differs from qfeet, and why:
!!
!! - **the names**: `nfeval` and `converged` are `info%neval` and `info%converged`, `max_nfeval` is
!!   `max_neval`, and `minimize` is `pf_minimize_simplex`;
!! - **the full-simplex form is gone.** A caller who wants the search path asks for `history`, which
!!   holds every evaluation in order. It is NOT the final simplex: a trial point this engine
!!   evaluates and then rejects is recorded beside the ones it keeps, and is often the last entry;
!! - **a non-finite value mid-run aborts.** qfeet screens only the starting simplex; every
!!   subsequent decision here is a `minloc`/`maxloc` or an ordered comparison over the values, and
!!   nagfor traps on one with a NaN operand, so the screen sits at the single evaluation site;
!! - **nothing is logged.** Running out of budget is `PF_OPT_LIMIT` in `info`, silently.
submodule (parquet_optimize) parquet_optimize_simplex

    implicit none

contains

    module procedure minimize_simplex_obj

        real(real64), allocatable :: points(:,:)  !! simplex vertices, one per row: (npar+1, npar)
        real(real64), allocatable :: fvalues(:)   !! objective value at each vertex
        real(real64), allocatable :: psum(:)      !! running sum of the simplex coordinates
        real(real64) :: atol_use                  !! absolute tolerance actually in force
        real(real64) :: fdum                      !! scratch value
        real(real64) :: fsave                     !! worst value before a contraction
        real(real64) :: ftry                      !! value at the most recent trial point
        real(real64) :: spread_rel                !! fractional spread of the current values
        integer :: npar                           !! number of variables
        integer :: budget                         !! evaluation budget actually in force
        integer :: i                              !! iterator
        integer :: ilo, ihi, ihi2                 !! best, worst and second-worst vertex
        integer :: neval, niter                   !! evaluations and iterations performed
        integer :: status                         !! the PF_OPT_* code this run ends on

        call refuse_constrained("pf_minimize_simplex", f, context)

        ! ---- validate ---------------------------------------------------------------------
        npar = size(x)
        call validate_size("pf_minimize_simplex", npar, context)

        if (size(step) /= npar) call optimize_abort("pf_minimize_simplex", &
            "step and x must have the same size", context)

        ! A zero step puts two vertices on top of each other, so that coordinate could never move.
        if (any(step == 0.0_real64) .or. any(ieee_is_nan(step))) &
            call optimize_abort("pf_minimize_simplex", "step must not contain zero or NaN", context)

        if (any(ieee_is_nan(x))) call optimize_abort("pf_minimize_simplex", &
            "the start point must not contain NaN", context)

        call validate_tolerance("pf_minimize_simplex", "rtol", rtol, context)
        atol_use = 0.0_real64
        if (present(atol)) then
            call validate_tolerance("pf_minimize_simplex", "atol", atol, context)
            atol_use = atol
        end if

        ! At least one convergence test must be able to fire, or the run can only end by
        ! exhausting the budget -- a caller mistake rather than a way to ask for "as many
        ! evaluations as possible". qfeet refuses the same pair.
        if (rtol + atol_use <= 0.0_real64) call optimize_abort("pf_minimize_simplex", &
            "at least one of rtol and atol must be positive", context)

        budget = SIMPLEX_MAX_NEVAL
        if (present(max_neval)) then
            call validate_budget("pf_minimize_simplex", max_neval, context)
            budget = max_neval
        end if

        if (present(history)) then
            history%n = 0
            allocate(history%x(npar, 0))
            allocate(history%f(0))
        end if

        ! ---- build the starting simplex from the start point and the step -------------------
        allocate(points(npar + 1, npar))
        allocate(fvalues(npar + 1))
        allocate(psum(npar))

        do i = 1, npar + 1
            points(i,:) = x(:)
        end do
        do i = 1, npar
            points(i+1, i) = points(i+1, i) + step(i)
        end do

        do i = 1, npar + 1
            fvalues(i) = f%eval(points(i,:))
            call screen_value(fvalues(i))
            if (present(history)) call history%add(points(i,:), fvalues(i))
        end do
        neval = npar + 1
        niter = 0

        psum(:) = sum(points(:,:), dim=1)
        status = PF_OPT_LIMIT

        simplex: do

            ! Locate the best and worst vertices, then the second worst -- found by temporarily
            ! hiding the worst value behind the best one.
            ilo = minloc(fvalues(:), dim=1)
            ihi = maxloc(fvalues(:), dim=1)
            fdum = fvalues(ihi)
            fvalues(ihi) = fvalues(ilo)
            ihi2 = maxloc(fvalues(:), dim=1)
            fvalues(ihi) = fdum

            ! Fractional spread of the current values; both ends exactly zero counts as converged.
            ! A branch rather than a `merge`, which would evaluate 0/0 before discarding it.
            fdum = abs(fvalues(ihi)) + abs(fvalues(ilo))
            if (fdum > 0.0_real64) then
                spread_rel = 2.0_real64*abs(fvalues(ihi) - fvalues(ilo))/fdum
            else
                spread_rel = 0.0_real64
            end if

            if (spread_rel < rtol .or. abs(fvalues(ihi) - fvalues(ilo)) < atol_use) then
                status = PF_OPT_OK
                exit simplex
            end if

            if (neval >= budget) then
                status = PF_OPT_LIMIT
                exit simplex
            end if

            niter = niter + 1

            ! Reflect the worst vertex through the opposite face.
            ftry = try_point(-1.0_real64)

            if (ftry <= fvalues(ilo)) then

                ! The reflection is the best point so far: try going further.
                ftry = try_point(2.0_real64)

            else if (ftry >= fvalues(ihi2)) then

                ! The reflection is still the worst point: contract towards the face.
                fsave = fvalues(ihi)
                ftry = try_point(0.5_real64)

                if (ftry >= fsave) then

                    ! Contraction did not help either: shrink the whole simplex onto the best
                    ! vertex and re-evaluate every other one.
                    points(:,:) = 0.5_real64*(points(:,:) + spread(points(ilo,:), 1, npar + 1))

                    do i = 1, npar + 1
                        if (i /= ilo) then
                            fvalues(i) = f%eval(points(i,:))
                            call screen_value(fvalues(i))
                            neval = neval + 1
                            if (present(history)) call history%add(points(i,:), fvalues(i))
                        end if
                    end do

                    psum(:) = sum(points(:,:), dim=1)

                end if

            end if

        end do simplex

        ! The spread the run ended on, before the best vertex moves to the front -- the swap
        ! cannot change it, since it is the difference of the two extremes.
        fdum = abs(fvalues(ihi) - fvalues(ilo))
        call swap_best_to_front(ilo)

        x(:) = points(1,:)
        fmin = fvalues(1)

        if (present(history)) call history_trim(history)

        ! `converged` and `info%converged` are ONE expression, so the short answer and the long one
        ! cannot drift apart, and the flag is set whether or not `info` was asked for.
        if (present(converged)) converged = (status == PF_OPT_OK)
        if (present(info)) then
            info%status = status
            info%converged = (status == PF_OPT_OK)
            info%neval = neval
            info%niter = niter
            info%spread = fdum
        end if

    contains

        !> Moves the worst vertex through the opposite face by the factor `fac` and keeps the
        !! trial point if it improves on the worst value.
        !!
        !! `fac` is `-1` to reflect, `2` to expand and `0.5` to contract. Operates on the host's
        !! simplex and counts the evaluation, exactly as qfeet's own `try_point` does. Contained
        !! rather than a module procedure because nothing passes it as an argument, and an
        !! internal procedure passed as one is what flang cannot do.
        function try_point(fac) result(res)
            real(real64), intent(in) :: fac !! how far to move the worst vertex
            real(real64)             :: res !! objective value at the trial point

            real(real64) :: fac1            !! weight of the simplex centroid
            real(real64) :: fac2            !! weight of the worst vertex
            real(real64) :: ptry(npar)      !! the trial point

            fac1 = (1.0_real64 - fac)/real(npar, real64)
            fac2 = fac1 - fac
            ptry(:) = psum(:)*fac1 - points(ihi,:)*fac2

            res = f%eval(ptry)
            call screen_value(res)
            neval = neval + 1
            if (present(history)) call history%add(ptry, res)

            if (res < fvalues(ihi)) then
                fvalues(ihi) = res
                psum(:) = psum(:) - points(ihi,:) + ptry(:)
                points(ihi,:) = ptry(:)
            end if

        end function try_point

        !> Swaps the best vertex into row 1 of the simplex, so `points(1,:)` is the answer.
        subroutine swap_best_to_front(best)
            integer, intent(in) :: best !! row holding the best vertex

            real(real64) :: fswap        !! scratch value
            real(real64) :: pswap(npar)  !! scratch vertex

            fswap = fvalues(1)
            fvalues(1) = fvalues(best)
            fvalues(best) = fswap

            pswap(:) = points(1,:)
            points(1,:) = points(best,:)
            points(best,:) = pswap(:)

        end subroutine swap_best_to_front

        !> Aborts when the objective returned a NaN or an infinity.
        !!
        !! Its own statement at every evaluation site, before the value reaches a comparison:
        !! `minloc`, `maxloc` and the spread test all compare these values, and nagfor traps on a
        !! NaN operand. `ieee_is_finite` rather than `v /= v`, which sees a NaN and misses the
        !! infinities 5.6 also refuses.
        subroutine screen_value(value)
            real(real64), intent(in) :: value !! what the objective returned

            if (.not. ieee_is_finite(value)) call optimize_abort("pf_minimize_simplex", &
                "the objective returned a non-finite value", context)

        end subroutine screen_value

    end procedure minimize_simplex_obj

    ! The FULLY RESTATED form, not `module procedure minimize_simplex_func`: in the abbreviated
    ! form gfortran 15 gives the `procedure(pf_objective_func)` dummy an implicit interface and
    ! refuses the pointer assignment below with "Explicit interface required for 'f'".
    module subroutine minimize_simplex_func(f, x, fmin, step, rtol, atol, max_neval, converged, info, &
                                            history, context)
        implicit none
        procedure(pf_objective_func)                     :: f         !! the objective
        real(real64), intent(inout)                      :: x(:)      !! start in, minimum out
        real(real64), intent(out)                        :: fmin      !! value at `x`
        real(real64), intent(in)                         :: step(:)   !! offset per coordinate
        real(real64), intent(in)                         :: rtol      !! fractional tolerance
        real(real64), intent(in), optional               :: atol      !! absolute tolerance
        integer, intent(in), optional                    :: max_neval !! evaluation budget; at most huge(1)/2
        logical, intent(out), optional                   :: converged !! the run's own rule fired
        type(pf_optimize_info), intent(out), optional    :: info      !! what happened
        type(pf_optimize_history), intent(out), optional :: history   !! every evaluation
        character(len=*), intent(in), optional           :: context   !! call-site text

        type(func_objective) :: obj !! wraps the plain function as an objective object

        obj%fun => f
        call minimize_simplex_obj(obj, x, fmin, step, rtol, atol=atol, max_neval=max_neval, &
                                  converged=converged, info=info, history=history, &
                                  context=context)

    end subroutine minimize_simplex_func

    module procedure simplex_solver_run

        real(real64) :: step(size(x)) !! the per-coordinate step this solver derives from the box

        ! The driver's box sizes the step; the simplex itself is unbounded and may leave it, which
        ! is what `pf_bobyqa_solver` differs in and what the guide page says about both.
        step(:) = self%step_fraction*(upper(:) - lower(:))

        ! `0` is the engine's own default and is passed on by OMISSION, since `pf_minimize_simplex`
        ! refuses a `max_neval` of zero as an argument. A negative value is a caller mistake in
        ! either reading and is refused here, where the object is, rather than silently adjusted.
        if (self%max_neval < 0) call optimize_abort("pf_simplex_solver%run", &
            "max_neval must not be negative")
        if (self%max_neval > 0) then
            call pf_minimize_simplex(f, x, fmin, step, self%rtol, atol=self%atol, &
                                     max_neval=self%max_neval, info=info)
        else
            call pf_minimize_simplex(f, x, fmin, step, self%rtol, atol=self%atol, info=info)
        end if

    end procedure simplex_solver_run

end submodule parquet_optimize_simplex
