!> The driver behind `pf_integrate`: the four specifics, every validation and every `error stop`,
!> the finite integration path, the evaluation budget, the record assembly and the `info` record.
!!
!! The engine in `parquet_integrate_engine.f90` knows nothing about optional arguments, defaults
!! or caller contracts; everything a caller can get wrong is refused here, before the first
!! evaluation, and everything the engine reports is translated here into one `PF_INT_*` code.
!!
!! **The validation order is the order of the guide page's table**, and each abort's text is what
!! the matching out-of-process scenario in `test/error_scenarios.f90` asserts. Changing a message
!! means changing that scenario in the same commit.
!!
!! **Nothing here is `pure`.** The validators abort, and a `pure` guard-only subroutine's call is
!! deleted by ifx at `-O0` (`fortran-gotchas.md`), so every guard is written impure deliberately.
submodule (parquet_integrate) parquet_integrate_driver

    implicit none

contains

    ! ---- the four specifics ------------------------------------------------------------------

    module procedure integrate_obj_rtol

        call integrate_impl(f, a, b, pf_tolerance(rtol=rtol, atol=0.0_real64), max_neval, &
                            converged, info, points, context, log_base, extrapolate, res)

    end procedure integrate_obj_rtol

    module procedure integrate_obj_tol

        call integrate_impl(f, a, b, tol, max_neval, converged, info, points, context, &
                            log_base, extrapolate, res)

    end procedure integrate_obj_tol

    !> Integrand as a plain function, tolerance as a bare `rtol`.
    !!
    !! Written in the fully restated form rather than the abbreviated `module procedure` one: a
    !! dummy PROCEDURE argument in an abbreviated body has an implicit interface under gfortran 15
    !! (`fortran-gotchas.md`).
    module function integrate_func_rtol(f, a, b, rtol, max_neval, converged, info, points, &
                                        context, log_base, extrapolate) result(res)
        procedure(pf_integrand_func)                       :: f           !! the integrand
        real(real64), intent(in)                           :: a           !! lower bound
        real(real64), intent(in)                           :: b           !! upper bound
        real(real64), intent(in)                           :: rtol        !! relative tolerance
        integer, intent(in), optional                      :: max_neval   !! evaluation budget
        logical, intent(out), optional                     :: converged   !! status is OK
        type(pf_integration_info), intent(out), optional   :: info        !! what happened
        type(pf_integration_points), intent(out), optional :: points      !! the record
        character(len=*), intent(in), optional             :: context     !! call-site text
        logical, intent(in), optional                      :: log_base    !! integrate in log x
        logical, intent(in), optional                      :: extrapolate !! epsilon table
        real(real64)                                       :: res         !! the integral

        type(func_integrand) :: wrapped

        wrapped%fp => f
        call integrate_impl(wrapped, a, b, pf_tolerance(rtol=rtol, atol=0.0_real64), max_neval, &
                            converged, info, points, context, log_base, extrapolate, res)

    end function integrate_func_rtol

    !> Integrand as a plain function, tolerance as a `pf_tolerance`. Fully restated for the same
    !! reason as `integrate_func_rtol`.
    module function integrate_func_tol(f, a, b, tol, max_neval, converged, info, points, &
                                       context, log_base, extrapolate) result(res)
        procedure(pf_integrand_func)                       :: f           !! the integrand
        real(real64), intent(in)                           :: a           !! lower bound
        real(real64), intent(in)                           :: b           !! upper bound
        type(pf_tolerance), intent(in)                     :: tol         !! both tolerances
        integer, intent(in), optional                      :: max_neval   !! evaluation budget
        logical, intent(out), optional                     :: converged   !! status is OK
        type(pf_integration_info), intent(out), optional   :: info        !! what happened
        type(pf_integration_points), intent(out), optional :: points      !! the record
        character(len=*), intent(in), optional             :: context     !! call-site text
        logical, intent(in), optional                      :: log_base    !! integrate in log x
        logical, intent(in), optional                      :: extrapolate !! epsilon table
        real(real64)                                       :: res         !! the integral

        type(func_integrand) :: wrapped

        wrapped%fp => f
        call integrate_impl(wrapped, a, b, tol, max_neval, converged, info, points, context, &
                            log_base, extrapolate, res)

    end function integrate_func_tol

    module procedure func_integrand_eval

        f = this%fp(x)

    end procedure func_integrand_eval

    ! ---- the one implementation ---------------------------------------------------------------

    !> Validates the call, integrates, and fills whatever optional outputs were asked for.
    !!
    !! The single place a `pf_integrate` call is carried out; the four specifics differ only in
    !! how they arrive here.
    subroutine integrate_impl(f, a, b, tol, max_neval, converged, info, points, context, &
                              log_base, extrapolate, res)
        class(pf_integrand), intent(inout)                 :: f           !! the integrand
        real(real64), intent(in)                           :: a           !! lower bound
        real(real64), intent(in)                           :: b           !! upper bound
        type(pf_tolerance), intent(in)                     :: tol         !! both tolerances
        integer, intent(in), optional                      :: max_neval   !! evaluation budget
        logical, intent(out), optional                     :: converged   !! status is OK
        type(pf_integration_info), intent(out), optional   :: info        !! what happened
        type(pf_integration_points), intent(out), optional :: points      !! the record
        character(len=*), intent(in), optional             :: context     !! call-site text
        logical, intent(in), optional                      :: log_base    !! integrate in log x
        logical, intent(in), optional                      :: extrapolate !! epsilon table
        real(real64), intent(out)                          :: res         !! the integral

        type(pf_integration_info) :: outcome
        type(engine_work)         :: work
        logical                   :: in_log, use_eps, record
        integer                   :: budget, limit, ier, last, neval
        real(real64)              :: lo, hi, abserr

        in_log = .false.
        if (present(log_base)) in_log = log_base
        use_eps = .false.
        if (present(extrapolate)) use_eps = extrapolate
        record = present(points)
        budget = DEFAULT_MAX_NEVAL
        if (present(max_neval)) budget = max_neval

        call validate_call(a, b, tol, budget, in_log, context)

        ! A zero-width range is the one range with no work in it: no evaluation, no partition and
        ! an empty record rather than an absent one.
        if (a == b) then
            res = 0.0_real64
            if (record) then
                points%n = 0
                allocate (points%x(0), points%w(0), points%f(0))
            end if
            if (present(info)) info = outcome
            if (present(converged)) converged = .true.
            return
        end if

        ! The budget sets the subinterval cap, and the cap is what keeps the count inside it:
        ! `neval = 42*limit - 21`, so `limit = (budget + 21)/42` is the largest partition the
        ! budget pays for. Floored at one, because one rule application always happens.
        limit = (budget + GK_POINTS)/GK_BISECTION
        if (limit < 1) limit = 1

        if (in_log) then
            lo = log(a)
            hi = log(b)
        else
            lo = a
            hi = b
        end if

        work%record = record
        call grow_work(work, min(limit, WORK_START), limit, 0)

        neval = 0
        call qagse(f, lo, hi, tol%atol, tol%rtol, limit, in_log, use_eps, work, res, abserr, &
                   neval, ier, last, outcome%extrapolated, context)

        outcome%status = status_from_ier(ier, context)
        outcome%converged = outcome%status == PF_INT_OK
        outcome%abserr = abserr
        outcome%partition_integral = sum(work%rlist(1:last))
        outcome%neval = neval
        outcome%nsub = last
        outcome%npanels = 1

        if (record) call gather_points(work, last, points)
        if (present(info)) info = outcome
        if (present(converged)) converged = outcome%converged

    end subroutine integrate_impl

    ! ---- validation ---------------------------------------------------------------------------

    !> Refuses every call this module cannot answer, in the order the guide page's table lists.
    !!
    !! Impure deliberately: a `pure` guard-only subroutine's call is deleted by ifx at `-O0`.
    subroutine validate_call(a, b, tol, budget, in_log, context)
        real(real64), intent(in)               :: a       !! lower bound, as the caller gave it
        real(real64), intent(in)               :: b       !! upper bound, as the caller gave it
        type(pf_tolerance), intent(in)         :: tol     !! both tolerances
        integer, intent(in)                    :: budget  !! resolved `max_neval`
        logical, intent(in)                    :: in_log  !! resolved `log_base`
        character(len=*), intent(in), optional :: context !! caller's call-site text

        if (.not. is_finite(tol%rtol) .or. tol%rtol < 0.0_real64) &
            call integrate_abort("rtol must be a finite, non-negative number", context)
        if (.not. is_finite(tol%atol) .or. tol%atol < 0.0_real64) &
            call integrate_abort("atol must be a finite, non-negative number", context)
        if (tol%rtol == 0.0_real64 .and. tol%atol == 0.0_real64) &
            call integrate_abort("at least one of rtol and atol must be positive", context)
        ! QUADPACK's own floor, which it reports as ier = 6; refused here before the engine sees
        ! it, because a status nobody can act on is worse than a message that says what to do.
        if (tol%atol == 0.0_real64 .and. tol%rtol < 50.0_real64*epsilon(1.0_real64)) &
            call integrate_abort("rtol below 50*epsilon needs a positive atol", context)

        if (budget < 1) call integrate_abort("max_neval must be positive", context)
        if (budget > MAX_NEVAL_CEILING) &
            call integrate_abort("max_neval must not exceed huge(1)/42", context)

        if (a /= a .or. b /= b) &
            call integrate_abort("integration bounds must not be NaN", context)
        if (a > b) &
            call integrate_abort("lower bound must not exceed the upper bound", context)

        ! TEMPORARY, for this phase only: `pf_infinity()` exists and is refused, rather than
        ! being silently mis-integrated as a very large finite bound. The outward walk that
        ! answers an infinite range arrives in the next phase, which deletes this refusal and the
        ! `integrate_infinite_bound_unsupported` scenario together.
        if (.not. is_finite(a) .or. .not. is_finite(b)) &
            call integrate_abort("infinite bounds arrive in a later phase", context)

        if (in_log .and. a <= 0.0_real64) &
            call integrate_abort("lower bound must be positive when integrating in log x", context)

    end subroutine validate_call

    !> Is `x` neither a NaN nor an infinity?
    !!
    !! The NaN is screened by self-comparison first, so the magnitude comparison never sees one.
    pure logical function is_finite(x) result(ok)
        real(real64), intent(in) :: x !! value to test

        ok = .false.
        if (x /= x) return
        ok = abs(x) <= huge(1.0_real64)

    end function is_finite

    module procedure integrate_abort

        character(len=:), allocatable :: msg

        msg = "pf_integrate: "//text
        if (present(context)) then
            if (len_trim(context) > CONTEXT_CAP) then
                msg = msg//" (context: "//context(1:CONTEXT_CAP)//"...)"
            else
                msg = msg//" (context: "//trim(context)//")"
            end if
        end if

        ! One thread aborts, not several: two threads reaching ERROR STOP at once leave the exit
        ! status nondeterministic under ifx (`api-conventions.md`).
        !$omp critical (parquet_integrate_abort)
        error stop msg
        !$omp end critical (parquet_integrate_abort)

    end procedure integrate_abort

    ! ---- the outcome ---------------------------------------------------------------------------

    !> Turns the engine's raw QUADPACK `ier` into one of the `PF_INT_*` codes.
    !!
    !! The shift `if (ier > 2) ier = ier - 1` is QUADPACK's own, applied on every one of its exit
    !! paths; the engine returns the code unshifted so that it is applied here exactly once.
    !! Applying it twice, or not at all, reports "no convergence" as "bad integrand" with the
    !! integral itself unchanged.
    integer function status_from_ier(ier, context) result(status)
        integer, intent(in)                    :: ier     !! the engine's raw status
        character(len=*), intent(in), optional :: context !! caller's call-site text

        integer :: shifted

        shifted = ier
        if (shifted > 2) shifted = shifted - 1

        select case (shifted)
        case (0)
            status = PF_INT_OK
        case (1)
            status = PF_INT_LIMIT
        case (2)
            status = PF_INT_ROUNDOFF
        case (3)
            status = PF_INT_BAD_INTEGRAND
        case (4)
            status = PF_INT_NO_CONVERGENCE
        case (5)
            status = PF_INT_DIVERGENT
        case default
            status = PF_INT_OK
            call integrate_abort("internal: the engine returned a status this driver does not " &
                                 // "know", context)
        end select

    end function status_from_ier

    ! ---- the record -----------------------------------------------------------------------------

    !> Copies the final partition's record slots into the caller's `points`, in partition order.
    !!
    !! Slots `1:last` ARE the final partition: a bisected subinterval's slot is overwritten by one
    !! of its children, so nothing superseded survives to be counted twice.
    subroutine gather_points(work, last, points)
        type(engine_work), intent(in)              :: work   !! the engine's work arrays
        integer, intent(in)                        :: last   !! subintervals in the partition
        type(pf_integration_points), intent(inout) :: points !! record to fill

        integer :: i, lo, hi

        points%n = last*GK_POINTS
        allocate (points%x(points%n), points%w(points%n), points%f(points%n))
        do i = 1, last
            lo = (i - 1)*GK_POINTS + 1
            hi = i*GK_POINTS
            points%x(lo:hi) = work%rx(:, i)
            points%w(lo:hi) = work%rw(:, i)
            points%f(lo:hi) = work%rf(:, i)
        end do

    end subroutine gather_points

    module procedure points_append

        type(pf_integration_points) :: merged
        integer                     :: n1, n2

        n1 = this%n
        n2 = other%n
        merged%n = n1 + n2
        allocate (merged%x(merged%n), merged%w(merged%n), merged%f(merged%n))
        if (n1 > 0) then
            merged%x(1:n1) = this%x(1:n1)
            merged%w(1:n1) = this%w(1:n1)
            merged%f(1:n1) = this%f(1:n1)
        end if
        if (n2 > 0) then
            merged%x(n1 + 1:merged%n) = other%x(1:n2)
            merged%w(n1 + 1:merged%n) = other%w(1:n2)
            merged%f(n1 + 1:merged%n) = other%f(1:n2)
        end if

        this%n = merged%n
        call move_alloc(merged%x, this%x)
        call move_alloc(merged%w, this%w)
        call move_alloc(merged%f, this%f)

    end procedure points_append

    ! ---- the work arrays -------------------------------------------------------------------------

    module procedure grow_work

        type(engine_work) :: bigger
        integer           :: cap

        cap = work%capacity
        if (cap < 1) cap = min(WORK_START, max(1, limit))
        do while (cap < want)
            cap = 2*cap
        end do
        if (cap > limit) cap = limit
        if (cap < want) cap = want
        if (cap <= work%capacity) return

        allocate (bigger%alist(cap), bigger%blist(cap), bigger%rlist(cap), bigger%elist(cap))
        allocate (bigger%iord(cap))
        if (last > 0) then
            bigger%alist(1:last) = work%alist(1:last)
            bigger%blist(1:last) = work%blist(1:last)
            bigger%rlist(1:last) = work%rlist(1:last)
            bigger%elist(1:last) = work%elist(1:last)
            bigger%iord(1:last) = work%iord(1:last)
        end if
        call move_alloc(bigger%alist, work%alist)
        call move_alloc(bigger%blist, work%blist)
        call move_alloc(bigger%rlist, work%rlist)
        call move_alloc(bigger%elist, work%elist)
        call move_alloc(bigger%iord, work%iord)

        if (work%record) then
            allocate (bigger%rx(GK_POINTS, cap), bigger%rw(GK_POINTS, cap))
            allocate (bigger%rf(GK_POINTS, cap))
            if (last > 0) then
                bigger%rx(:, 1:last) = work%rx(:, 1:last)
                bigger%rw(:, 1:last) = work%rw(:, 1:last)
                bigger%rf(:, 1:last) = work%rf(:, 1:last)
            end if
            call move_alloc(bigger%rx, work%rx)
            call move_alloc(bigger%rw, work%rw)
            call move_alloc(bigger%rf, work%rf)
        end if

        work%capacity = cap

    end procedure grow_work

end submodule parquet_integrate_driver
