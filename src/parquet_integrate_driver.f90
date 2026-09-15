!> The driver behind `pf_integrate`: the four specifics, every validation and every `error stop`,
!> the finite integration path, the outward walk that answers an infinite range, the evaluation
!> budget, the record assembly and the `info` record.
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
                            converged, info, points, context, log_base, extrapolate, max_panels, &
                            breakpoints, res)

    end procedure integrate_obj_rtol

    module procedure integrate_obj_tol

        call integrate_impl(f, a, b, tol, max_neval, converged, info, points, context, &
                            log_base, extrapolate, max_panels, breakpoints, res)

    end procedure integrate_obj_tol

    !> Integrand as a plain function, tolerance as a bare `rtol`.
    !!
    !! Written in the fully restated form rather than the abbreviated `module procedure` one: a
    !! dummy PROCEDURE argument in an abbreviated body has an implicit interface under gfortran 15
    !! (`fortran-gotchas.md`).
    module function integrate_func_rtol(f, a, b, rtol, max_neval, converged, info, points, &
                                        context, log_base, extrapolate, max_panels, breakpoints) result(res)
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
        integer, intent(in), optional                      :: max_panels  !! walk panel cap
        real(real64), intent(in), optional                 :: breakpoints(:) !! interior cuts
        real(real64)                                       :: res         !! the integral

        type(func_integrand) :: wrapped

        wrapped%fp => f
        call integrate_impl(wrapped, a, b, pf_tolerance(rtol=rtol, atol=0.0_real64), max_neval, &
                            converged, info, points, context, log_base, extrapolate, max_panels, &
                            breakpoints, res)

    end function integrate_func_rtol

    !> Integrand as a plain function, tolerance as a `pf_tolerance`. Fully restated for the same
    !! reason as `integrate_func_rtol`.
    module function integrate_func_tol(f, a, b, tol, max_neval, converged, info, points, &
                                       context, log_base, extrapolate, max_panels, breakpoints) result(res)
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
        integer, intent(in), optional                      :: max_panels  !! walk panel cap
        real(real64), intent(in), optional                 :: breakpoints(:) !! interior cuts
        real(real64)                                       :: res         !! the integral

        type(func_integrand) :: wrapped

        wrapped%fp => f
        call integrate_impl(wrapped, a, b, tol, max_neval, converged, info, points, context, &
                            log_base, extrapolate, max_panels, breakpoints, res)

    end function integrate_func_tol

    module procedure func_integrand_eval

        f = this%fp(x)

    end procedure func_integrand_eval

    ! ---- the workers the module declares, implemented ahead of every call to them --------------
    !
    ! nagfor rejects a separate module procedure whose body appears BELOW a call to it in the same
    ! submodule (`fortran-gotchas.md`), so these three sit above the code that uses them rather
    ! than beside the code they belong with.

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

    ! ---- the one implementation ---------------------------------------------------------------

    !> Validates the call, integrates, and fills whatever optional outputs were asked for.
    !!
    !! The single place a `pf_integrate` call is carried out; the four specifics differ only in
    !! how they arrive here.
    !!
    !! **The record is owned here, not by the path that fills it.** Every path appends its slots
    !! to one `record` that starts allocated and empty, so a walk of several panels and a
    !! `breakpoints` sum of several pieces concatenate the same way a single finite range does,
    !! and a call that recorded nothing still hands back a zero-length record rather than an
    !! absent one.
    subroutine integrate_impl(f, a, b, tol, max_neval, converged, info, points, context, &
                              log_base, extrapolate, max_panels, breakpoints, res)
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
        integer, intent(in), optional                      :: max_panels  !! walk panel cap
        real(real64), intent(in), optional                 :: breakpoints(:) !! interior cuts
        real(real64), intent(out)                          :: res         !! the integral

        type(pf_integration_info)   :: outcome
        type(pf_integration_points) :: record
        type(engine_work)           :: work
        logical                     :: in_log, use_eps, record_wanted
        integer                     :: budget, panel_cap, neval

        in_log = .false.
        if (present(log_base)) in_log = log_base
        use_eps = .false.
        if (present(extrapolate)) use_eps = extrapolate
        record_wanted = present(points)
        budget = DEFAULT_MAX_NEVAL
        if (present(max_neval)) budget = max_neval
        panel_cap = DEFAULT_MAX_PANELS
        if (present(max_panels)) panel_cap = max_panels

        call validate_call(a, b, tol, budget, in_log, present(max_panels), panel_cap, &
                           breakpoints, context)

        record%n = 0
        allocate (record%x(0), record%w(0), record%f(0))

        ! A zero-width range is the one range with no work in it: no evaluation, no partition and
        ! an empty record rather than an absent one.
        if (a == b) then
            res = 0.0_real64
            if (record_wanted) call hand_back_record(record, points)
            if (present(info)) info = outcome
            if (present(converged)) converged = .true.
            return
        end if

        work%record = record_wanted
        neval = 0
        res = 0.0_real64

        if (present(breakpoints)) then
            call integrate_pieces(f, a, b, breakpoints, tol, budget, panel_cap, in_log, use_eps, &
                                  work, record, res, outcome, neval, context)
        else
            call integrate_range(f, a, b, tol, budget, panel_cap, in_log, use_eps, work, record, &
                                 res, outcome, neval, context)
        end if

        outcome%converged = outcome%status == PF_INT_OK
        outcome%neval = neval
        if (present(info)) info = outcome
        if (present(converged)) converged = outcome%converged
        if (record_wanted) call hand_back_record(record, points)

    end subroutine integrate_impl

    !> Moves an assembled record into the caller's `points`, leaving nothing copied.
    subroutine hand_back_record(record, points)
        type(pf_integration_points), intent(inout) :: record !! the assembled record, consumed
        type(pf_integration_points), intent(out)   :: points !! the caller's record

        points%n = record%n
        call move_alloc(record%x, points%x)
        call move_alloc(record%w, points%w)
        call move_alloc(record%f, points%f)

    end subroutine hand_back_record

    ! ---- the pieces a `breakpoints` call is cut into ------------------------------------------

    !> Integrates `[a, b]` as the sequence of pieces the breakpoints cut it into.
    !!
    !! Each piece is integrated on its own, by the path its own bounds select -- a finite piece by
    !! the adaptive engine, a piece reaching an infinity by the outward walk -- and the results,
    !! error estimates, records, panels and subintervals are accumulated in ascending order. With
    !! breakpoints on `(-inf, +inf)` the pieces are `(-inf, p1], [p1, p2], ..., [pn, +inf)`, so
    !! the split at zero the unbroken whole line takes is simply never reached.
    !!
    !! **Each piece is integrated to `atol/n_pieces`** (Q21), not to `atol`. Integrating every
    !! piece to the caller's full `atol` would leave every piece reporting `PF_INT_OK`, the sum
    !! reporting `converged`, and the error up to `n_pieces` times what was asked for -- silently,
    !! and only on the `atol`-dominated call `atol` exists for. `rtol` is NOT divided: each piece
    !! is integrated to `rtol` of itself, which over-delivers on a piece contributing little
    !! rather than under-delivering.
    !!
    !! **The breakpoints are sorted here**, into a local copy, so the caller's array is untouched
    !! and an unsorted list answers the same as a sorted one. QUADPACK's `dqagpe` sorts its own
    !! `points` for the same reason. An insertion sort, because the list is a handful of values a
    !! person typed.
    subroutine integrate_pieces(f, a, b, breakpoints, tol, budget, panel_cap, in_log, use_eps, &
                                work, record, res, outcome, neval, context)
        class(pf_integrand), intent(inout)         :: f              !! the integrand
        real(real64), intent(in)                   :: a              !! lower bound
        real(real64), intent(in)                   :: b              !! upper bound
        real(real64), intent(in)                   :: breakpoints(:) !! interior cuts, any order
        type(pf_tolerance), intent(in)             :: tol            !! both tolerances
        integer, intent(in)                        :: budget         !! resolved `max_neval`
        integer, intent(in)                        :: panel_cap      !! resolved `max_panels`
        logical, intent(in)                        :: in_log         !! integrate in `log x`
        logical, intent(in)                        :: use_eps        !! run the epsilon table
        type(engine_work), intent(inout)           :: work           !! the engine's work arrays
        type(pf_integration_points), intent(inout) :: record         !! the record, appended to
        real(real64), intent(inout)                :: res            !! running result, added to
        type(pf_integration_info), intent(inout)   :: outcome        !! filled in as it goes
        integer, intent(inout)                     :: neval          !! evaluation counter
        character(len=*), intent(in), optional     :: context        !! call-site text

        real(real64), allocatable :: cut(:)
        type(pf_tolerance)        :: piece_tol
        real(real64)              :: lo, hi, key
        integer                   :: npieces, ipiece, i, j

        cut = breakpoints
        do i = 2, size(cut)
            key = cut(i)
            j = i - 1
            do while (j >= 1)
                if (cut(j) <= key) exit
                cut(j + 1) = cut(j)
                j = j - 1
            end do
            cut(j + 1) = key
        end do

        npieces = size(cut) + 1
        piece_tol = pf_tolerance(rtol=tol%rtol, atol=tol%atol/real(npieces, real64))

        do ipiece = 1, npieces
            if (ipiece == 1) then
                lo = a
            else
                lo = cut(ipiece - 1)
            end if
            if (ipiece == npieces) then
                hi = b
            else
                hi = cut(ipiece)
            end if
            ! The budget is carried from one piece to the next, less the one rule application
            ! each piece after this one must be able to pay for: without that reserve an early
            ! piece could spend the whole budget and the pieces beyond it would each still cost
            ! their unavoidable 21 evaluations, putting the total over `max_neval`.
            call integrate_range(f, lo, hi, piece_tol, budget - GK_POINTS*(npieces - ipiece), &
                                 panel_cap, in_log, use_eps, work, record, res, outcome, neval, &
                                 context)
        end do

    end subroutine integrate_pieces

    !> Integrates one range -- the whole call's range, or one piece of it -- by the path its own
    !! bounds select, adding what it finds to the running result.
    subroutine integrate_range(f, a, b, tol, budget, panel_cap, in_log, use_eps, work, record, &
                               res, outcome, neval, context)
        class(pf_integrand), intent(inout)         :: f         !! the integrand
        real(real64), intent(in)                   :: a         !! lower bound
        real(real64), intent(in)                   :: b         !! upper bound
        type(pf_tolerance), intent(in)             :: tol       !! both tolerances
        integer, intent(in)                        :: budget    !! evaluations this range may use
        integer, intent(in)                        :: panel_cap !! resolved `max_panels`
        logical, intent(in)                        :: in_log    !! integrate in `log x`
        logical, intent(in)                        :: use_eps   !! run the epsilon table
        type(engine_work), intent(inout)           :: work      !! the engine's work arrays
        type(pf_integration_points), intent(inout) :: record    !! the record, appended to
        real(real64), intent(inout)                :: res       !! running result, added to
        type(pf_integration_info), intent(inout)   :: outcome   !! filled in as it goes
        integer, intent(inout)                     :: neval     !! evaluation counter
        character(len=*), intent(in), optional     :: context   !! call-site text

        if (is_finite(a) .and. is_finite(b)) then
            call integrate_finite(f, a, b, tol, budget, in_log, use_eps, work, record, res, &
                                  outcome, neval, context)
        else
            call integrate_infinite(f, a, b, tol, budget, panel_cap, use_eps, work, record, res, &
                                    outcome, neval, context)
        end if

    end subroutine integrate_range

    ! ---- the finite range -----------------------------------------------------------------------

    !> Integrates a finite range with one application of the adaptive-bisection engine.
    subroutine integrate_finite(f, a, b, tol, budget, in_log, use_eps, work, record, res, &
                                outcome, neval, context)
        class(pf_integrand), intent(inout)         :: f       !! the integrand
        real(real64), intent(in)                   :: a       !! lower bound, finite
        real(real64), intent(in)                   :: b       !! upper bound, finite
        type(pf_tolerance), intent(in)             :: tol     !! both tolerances
        integer, intent(in)                        :: budget  !! evaluations this range may use
        logical, intent(in)                        :: in_log  !! integrate in `log x`
        logical, intent(in)                        :: use_eps !! run the epsilon table
        type(engine_work), intent(inout)           :: work    !! the engine's work arrays
        type(pf_integration_points), intent(inout) :: record  !! the record, appended to
        real(real64), intent(inout)                :: res     !! running result, added to
        type(pf_integration_info), intent(inout)   :: outcome !! filled in as it goes
        integer, intent(inout)                     :: neval   !! evaluation counter
        character(len=*), intent(in), optional     :: context !! call-site text

        type(pf_integration_points) :: piece_record
        integer                     :: limit, ier, last
        real(real64)                :: lo, hi, res1, abserr, defabs
        logical                     :: extrapolated

        ! The budget sets the subinterval cap, and the cap is what keeps the count inside it:
        ! `neval = 42*limit - 21`, so `limit = (budget + 21)/42` is the largest partition the
        ! budget pays for. What is LEFT of the budget is what counts, so that a second piece of a
        ! `breakpoints` call cannot spend what the first already did. Floored at one, because one
        ! rule application always happens.
        limit = (max(budget - neval, 0) + GK_POINTS)/GK_BISECTION
        if (limit < 1) limit = 1

        if (in_log) then
            lo = log(a)
            hi = log(b)
        else
            lo = a
            hi = b
        end if

        call grow_work(work, min(limit, WORK_START), limit, 0)

        call qagse(f, lo, hi, tol%atol, tol%rtol, limit, in_log, .false., use_eps, work, res1, &
                   abserr, defabs, neval, ier, last, extrapolated, context)

        res = res + res1
        outcome%status = worse_status(outcome%status, status_from_ier(ier, context))
        outcome%extrapolated = outcome%extrapolated .or. extrapolated
        outcome%abserr = outcome%abserr + abserr
        outcome%partition_integral = outcome%partition_integral + sum(work%rlist(1:last))
        outcome%nsub = outcome%nsub + last
        outcome%npanels = outcome%npanels + 1

        if (work%record) then
            call gather_points(work, last, piece_record)
            call record%append(piece_record)
        end if

    end subroutine integrate_finite

    ! ---- the infinite ranges ----------------------------------------------------------------

    !> Integrates a range with at least one infinite bound, by the outward walk of qfeet's
    !! `integrate_inf_obj_tol`.
    !!
    !! The three spellings reduce to one walk over `[a, +infinity)`:
    !!
    !! * `[a, +inf)` is that walk;
    !! * `(-inf, b]` is the walk over `[-b, +inf)` with every evaluation and every recorded
    !!   abscissa mirrored -- the `negate` flag -- which is exact rather than approximate, because
    !!   `integral of f over (-inf, b]` IS `integral of f(-y) over [-b, +inf)`;
    !! * `(-inf, +inf)` is those two, split at zero, with the results, the error estimates, the
    !!   panels and the subintervals summed and `converged` the conjunction.
    !!
    !! **Why not `dqagie`'s change of variable.** QUADPACK answers an infinite range by mapping it
    !! onto `(0, 1]` and bisecting there, which packs everything beyond about `a + 19` into the
    !! first rule's last two abscissae: a unit-width feature at `x = 40` falls between them, every
    !! sampled value is zero, the error estimate is zero, nothing is ever bisected, and the answer
    !! comes back as zero with `converged` true. The outward walk samples 21 points per factor of
    !! e instead, so the same feature is found. What it costs is measured by the benchmark's
    !! `walk` mode.
    subroutine integrate_infinite(f, a, b, tol, budget, panel_cap, use_eps, work, record, res, &
                                  outcome, neval, context)
        class(pf_integrand), intent(inout)         :: f         !! the integrand
        real(real64), intent(in)                   :: a         !! lower bound
        real(real64), intent(in)                   :: b         !! upper bound
        type(pf_tolerance), intent(in)             :: tol       !! both tolerances
        integer, intent(in)                        :: budget    !! evaluations this range may use
        integer, intent(in)                        :: panel_cap !! resolved `max_panels`
        logical, intent(in)                        :: use_eps   !! run the epsilon table
        type(engine_work), intent(inout)           :: work      !! the engine's work arrays
        type(pf_integration_points), intent(inout) :: record    !! the record, appended to
        real(real64), intent(inout)                :: res       !! running result, added to
        type(pf_integration_info), intent(inout)   :: outcome   !! filled in as it goes
        integer, intent(inout)                     :: neval     !! evaluation counter
        character(len=*), intent(in), optional     :: context   !! call-site text

        if (is_finite(a)) then
            call walk_outward(f, a, .false., tol, budget, panel_cap, use_eps, work, record, res, &
                              outcome, neval, context)
        else if (is_finite(b)) then
            call walk_outward(f, -b, .true., tol, budget, panel_cap, use_eps, work, record, res, &
                              outcome, neval, context)
        else
            ! Split at zero and walk both ways. Each half gets the panel cap in full; they share
            ! the evaluation budget, in the order they are walked.
            call walk_outward(f, 0.0_real64, .true., tol, budget, panel_cap, use_eps, work, &
                              record, res, outcome, neval, context)
            call walk_outward(f, 0.0_real64, .false., tol, budget, panel_cap, use_eps, work, &
                              record, res, outcome, neval, context)
        end if

    end subroutine integrate_infinite

    !> Walks outward from `a` to `+infinity`, adding what it finds to the running result.
    !!
    !! Ported from qfeet's `integrate_inf_obj_tol` with the 3-point rule replaced by one `qk21`
    !! application per probe and the full adaptive engine per accepted panel. The structure is
    !! qfeet's and the exit tests are qfeet's: the walk stops when the panel itself has fallen to
    !! round-off, when the tail estimated from the observed decay ratio is inside the tolerance,
    !! or on one of the two caps.
    subroutine walk_outward(f, a, negate, tol, budget, panel_cap, use_eps, work, record, res, &
                            outcome, neval, context)
        class(pf_integrand), intent(inout)         :: f         !! the integrand
        real(real64), intent(in)                   :: a         !! lower bound, in the walk's `y`
        logical, intent(in)                        :: negate    !! the caller's `x` is `-y`
        type(pf_tolerance), intent(in)             :: tol       !! both tolerances
        integer, intent(in)                        :: budget    !! evaluations this range may use
        integer, intent(in)                        :: panel_cap !! resolved `max_panels`
        logical, intent(in)                        :: use_eps   !! run the epsilon table
        type(engine_work), intent(inout)           :: work      !! the engine's work arrays
        type(pf_integration_points), intent(inout) :: record    !! the record, grown per panel
        real(real64), intent(inout)                :: res       !! running result, added to
        type(pf_integration_info), intent(inout)   :: outcome   !! filled in as it goes
        integer, intent(inout)                     :: neval     !! evaluation counter
        character(len=*), intent(in), optional     :: context   !! call-site text

        real(real64) :: aa, bb, u, res1, abserr1, defabs1, resabstot, prev, ratio, tailest, floor
        integer      :: istep, panels
        logical      :: haveprev, in_log, finished

        resabstot = 0.0_real64
        prev = 0.0_real64
        tailest = 0.0_real64
        haveprev = .false.
        finished = .false.
        panels = 1

        ! The first panel is the one the search accepted, in whichever variable the search worked
        ! in; from there the walk is in `log y` whatever the search did.
        call find_start_panel(f, a, negate, budget, neval, aa, bb, in_log, context)
        call run_panel(f, aa, bb, in_log, negate, tol, budget, use_eps, work, record, res1, &
                       abserr1, defabs1, res, outcome, neval, context)
        resabstot = resabstot + defabs1
        outcome%abserr = outcome%abserr + abserr1
        if (in_log) then
            u = bb
        else
            u = log(bb)
        end if

        walk: do istep = 2, panel_cap
            if (neval >= budget) exit walk
            ! Beyond this the abscissa `exp(u)` is no longer a finite number, so the integrand
            ! would be asked for a value at a point that is not one.
            if (u + TAIL_STEP > LOG_X_CEILING) exit walk

            aa = u
            u = u + TAIL_STEP
            call run_panel(f, aa, u, .true., negate, tol, budget, use_eps, work, record, res1, &
                           abserr1, defabs1, res, outcome, neval, context)
            panels = panels + 1
            resabstot = resabstot + defabs1
            outcome%abserr = outcome%abserr + abserr1
            floor = ROUNDOFF_FACTOR*epsilon(1.0_real64)*resabstot

            ! The panel itself has fallen to round-off: nothing further out can contribute, and
            ! there is no tail left to estimate.
            if (abs(res1) <= floor) then
                tailest = 0.0_real64
                finished = .true.
                exit walk
            end if

            ! What is left, if the panels keep decaying at the rate just observed. Two panels are
            ! needed before there is a ratio, which is why the first one never ends the walk.
            if (haveprev .and. abs(res1) < abs(prev)) then
                ratio = abs(res1)/abs(prev)
                tailest = abs(res1)*ratio/(1.0_real64 - ratio)
                if (tailest < max(tol%rtol*abs(res), tol%atol, floor)) then
                    finished = .true.
                    exit walk
                end if
            end if
            prev = res1
            haveprev = .true.
        end do walk

        ! The tail nobody integrated is part of the error, and a walk that stopped on a cap has
        ! not shown that what is left is small.
        outcome%abserr = outcome%abserr + tailest
        outcome%npanels = outcome%npanels + panels
        if (.not. finished) outcome%status = worse_status(outcome%status, PF_INT_LIMIT)

    end subroutine walk_outward

    !> Finds a first panel, ending at `bb`, on which the integrand is not negligible.
    !!
    !! An integrand that is zero, or below round-off, next to its lower bound would otherwise make
    !! the walk start from a panel carrying no information -- and the tail estimate needs two
    !! consecutive panels that are not negligible before it can form a ratio at all. Both the
    !! initial guess and the widening are heuristics: they decide what the walk COSTS, never what
    !! it answers.
    !!
    !! qfeet's `find_start_panel`, with its narrow retry: an integrand falling off far faster than
    !! the first guess assumed is found by a much narrower panel, and trying that before trying
    !! wider ones is what finds a spike sitting just above `a`.
    !!
    !! The probes are one `qk21` application each and their values are discarded -- the panel the
    !! search accepts is integrated again, from scratch, by the adaptive engine. Refining before
    !! the test would let a panel's deeper samples decide the search, which is what made qfeet's
    !! narrow retry nearly unreachable.
    subroutine find_start_panel(f, a, negate, budget, neval, aa, bb, in_log, context)
        class(pf_integrand), intent(inout)     :: f       !! the integrand
        real(real64), intent(in)               :: a       !! lower bound, in the walk's `y`
        logical, intent(in)                    :: negate  !! the caller's `x` is `-y`
        integer, intent(in)                    :: budget  !! evaluations this walk may use
        integer, intent(inout)                 :: neval   !! evaluation counter
        real(real64), intent(out)              :: aa      !! accepted panel's lower bound
        real(real64), intent(out)              :: bb      !! accepted panel's upper bound
        logical, intent(out)                   :: in_log  !! `aa` and `bb` are in `log y`
        character(len=*), intent(in), optional :: context !! call-site text

        real(real64) :: res, abserr, resabs, resasc
        real(real64) :: px(GK_POINTS), pw(GK_POINTS), pv(GK_POINTS)
        integer      :: istep

        if (a > 0.0_real64) then
            ! A positive lower bound: work in log y throughout.
            in_log = .true.
            aa = log(a)
            if (a < START_MIN_A) then
                bb = log(START_MIN_A)
            else
                bb = aa + TAIL_STEP
            end if
            call qk21(f, aa, bb, in_log, negate, res, abserr, resabs, resasc, neval, px, pw, pv, &
                      context)

            if (negligible(res, resabs)) then
                ! Nothing there. Before widening, try a much NARROWER panel: an integrand that
                ! falls off far faster than the first guess assumed lives inside this one.
                if (a < START_MIN_A) then
                    bb = log(a*START_MULT_A)
                else
                    bb = aa + START_ADD_A
                end if
                call qk21(f, aa, bb, in_log, negate, res, abserr, resabs, resasc, neval, px, pw, &
                          pv, context)
            end if

            do istep = 1, MAX_START_STEPS
                if (.not. negligible(res, resabs)) exit
                if (neval >= budget) exit
                if (bb + log(START_WIDEN) > LOG_X_CEILING) exit
                bb = bb + log(START_WIDEN)
                call qk21(f, aa, bb, in_log, negate, res, abserr, resabs, resasc, neval, px, pw, &
                          pv, context)
            end do
        else
            ! A lower bound at or below zero: the first panel has to be done in linear y, since
            ! `log y` does not reach it. The walk continues in log y from wherever this ends.
            in_log = .false.
            aa = a
            bb = START_MIN_A/10.0_real64
            call qk21(f, aa, bb, in_log, negate, res, abserr, resabs, resasc, neval, px, pw, pv, &
                      context)

            do istep = 1, MAX_START_STEPS
                if (.not. negligible(res, resabs)) exit
                if (neval >= budget) exit
                bb = bb*START_WIDEN
                call qk21(f, aa, bb, in_log, negate, res, abserr, resabs, resasc, neval, px, pw, &
                          pv, context)
            end do
        end if

    end subroutine find_start_panel

    !> Integrates one panel and folds what happened into the running result and outcome.
    subroutine run_panel(f, lo, hi, in_log, negate, tol, budget, use_eps, work, record, res1, &
                         abserr1, defabs1, res, outcome, neval, context)
        class(pf_integrand), intent(inout)         :: f       !! the integrand
        real(real64), intent(in)                   :: lo      !! panel's lower bound
        real(real64), intent(in)                   :: hi      !! panel's upper bound
        logical, intent(in)                        :: in_log  !! the bounds are in `log y`
        logical, intent(in)                        :: negate  !! the caller's `x` is `-y`
        type(pf_tolerance), intent(in)             :: tol     !! both tolerances
        integer, intent(in)                        :: budget  !! evaluations this walk may use
        logical, intent(in)                        :: use_eps !! run the epsilon table
        type(engine_work), intent(inout)           :: work    !! the engine's work arrays
        type(pf_integration_points), intent(inout) :: record  !! the record, appended to
        real(real64), intent(out)                  :: res1    !! this panel's integral
        real(real64), intent(out)                  :: abserr1 !! this panel's error estimate
        real(real64), intent(out)                  :: defabs1 !! this panel's integral of `|f|`
        real(real64), intent(inout)                :: res     !! running result, added to
        type(pf_integration_info), intent(inout)   :: outcome !! filled in as it goes
        integer, intent(inout)                     :: neval   !! evaluation counter
        character(len=*), intent(in), optional     :: context !! call-site text

        type(pf_integration_points) :: panel_record
        integer                     :: limit, ier, last
        logical                     :: extrapolated

        ! What is LEFT of the budget sets this panel's subinterval cap, so the walk as a whole
        ! overshoots `max_neval` by at most the one rule application a panel costs even when five
        ! evaluations remain.
        limit = (max(budget - neval, 0) + GK_POINTS)/GK_BISECTION
        if (limit < 1) limit = 1
        call grow_work(work, min(limit, WORK_START), limit, 0)

        ! qfeet's `absdiff` rule, ported: a panel is integrated to the tolerance the whole
        ! INTEGRAL asked for, not to that tolerance of ITSELF. QUADPACK's own test is
        ! `errbnd = max(epsabs, epsrel*|area of this panel|)`, and in a tail the panel's own area
        ! goes to zero, so the surviving requirement is the caller's `atol` -- the same absolute
        ! accuracy demanded of a panel contributing nothing as of the one carrying the integral.
        ! On an oscillatory tail that is where the whole evaluation budget goes, leaving the walk
        ! too few panels to reach the decay it needed to see. Raising `epsabs` to what the running
        ! result makes negligible is never tighter than the caller asked for and never looser than
        ! the tolerance applied to the sum.
        call qagse(f, lo, hi, max(tol%atol, tol%rtol*abs(res)), tol%rtol, limit, in_log, negate, &
                   use_eps, work, res1, abserr1, defabs1, neval, ier, last, extrapolated, context)

        res = res + res1
        outcome%status = worse_status(outcome%status, status_from_ier(ier, context))
        outcome%extrapolated = outcome%extrapolated .or. extrapolated
        outcome%partition_integral = outcome%partition_integral + sum(work%rlist(1:last))
        outcome%nsub = outcome%nsub + last

        if (work%record) then
            call gather_points(work, last, panel_record)
            call record%append(panel_record)
        end if

    end subroutine run_panel

    !> Is a panel's contribution indistinguishable from zero at working precision?
    !!
    !! Measured against the panel's own integral of `|f|` rather than against a fixed constant, so
    !! the test means the same thing whatever the integrand's magnitude, and it is true for an
    !! integrand that is identically zero on the panel. **Sign-agnostic**: without the `abs` a
    !! negative panel is negligible for every magnitude, and the walk steps straight over the
    !! whole of a negative integrand.
    pure logical function negligible(res, resabs) result(isneg)
        real(real64), intent(in) :: res    !! signed integral over the panel
        real(real64), intent(in) :: resabs !! integral of `|f|` over the panel

        isneg = abs(res) <= ROUNDOFF_FACTOR*epsilon(1.0_real64)*resabs

    end function negligible

    !> The worse of two outcomes, for a result assembled from several panels or pieces.
    !!
    !! Ordered by how little the caller can do about it: `PF_INT_OK`, then `PF_INT_LIMIT` (raise a
    !! cap), `PF_INT_ROUNDOFF` (ask for less), `PF_INT_NO_CONVERGENCE`, `PF_INT_DIVERGENT`, and
    !! `PF_INT_BAD_INTEGRAND` last, since a singularity inside the range is a property of the
    !! problem rather than of this call. Only `PF_INT_OK` sets `converged`, so the ordering
    !! decides which non-zero code is reported, never whether one is.
    pure integer function worse_status(left, right) result(status)
        integer, intent(in) :: left  !! one status
        integer, intent(in) :: right !! the other

        if (status_severity(right) > status_severity(left)) then
            status = right
        else
            status = left
        end if

    end function worse_status

    !> How bad a status is, for `worse_status` to compare.
    pure integer function status_severity(status) result(rank)
        integer, intent(in) :: status !! one of the `PF_INT_*` codes

        select case (status)
        case (PF_INT_OK)
            rank = 0
        case (PF_INT_LIMIT)
            rank = 1
        case (PF_INT_ROUNDOFF)
            rank = 2
        case (PF_INT_NO_CONVERGENCE)
            rank = 3
        case (PF_INT_DIVERGENT)
            rank = 4
        case default
            rank = 5
        end select

    end function status_severity

    ! ---- validation ---------------------------------------------------------------------------

    !> Refuses every call this module cannot answer, in the order the guide page's table lists.
    !!
    !! Impure deliberately: a `pure` guard-only subroutine's call is deleted by ifx at `-O0`.
    subroutine validate_call(a, b, tol, budget, in_log, panels_given, panel_cap, breakpoints, &
                             context)
        real(real64), intent(in)               :: a            !! lower bound, as the caller gave it
        real(real64), intent(in)               :: b            !! upper bound, as the caller gave it
        type(pf_tolerance), intent(in)         :: tol          !! both tolerances
        integer, intent(in)                    :: budget       !! resolved `max_neval`
        logical, intent(in)                    :: in_log       !! resolved `log_base`
        logical, intent(in)                    :: panels_given !! `max_panels` was passed
        integer, intent(in)                    :: panel_cap    !! resolved `max_panels`
        real(real64), intent(in), optional     :: breakpoints(:) !! interior cuts, as they came
        character(len=*), intent(in), optional :: context      !! caller's call-site text

        integer :: i, j

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

        if (panel_cap < 1) call integrate_abort("max_panels must be positive", context)
        ! A finite range has no walk, so a `max_panels` on one is a call that does not mean what
        ! it says -- most often an infinite bound that did not survive the caller's arithmetic.
        if (panels_given .and. is_finite(a) .and. is_finite(b)) &
            call integrate_abort("max_panels applies only to an infinite range", context)

        if (a /= a .or. b /= b) &
            call integrate_abort("integration bounds must not be NaN", context)
        if (a > b) &
            call integrate_abort("lower bound must not exceed the upper bound", context)
        ! `a == b` with both infinite is the empty range `[+inf, +inf]`, which has no value; the
        ! finite `a == b` is a zero-width range and returns zero.
        if (a == b .and. .not. is_finite(a)) &
            call integrate_abort("bounds must not both be the same infinity", context)

        if (in_log .and. (.not. is_finite(a) .or. .not. is_finite(b))) &
            call integrate_abort("log_base applies only to a finite range", context)
        if (in_log .and. a <= 0.0_real64) &
            call integrate_abort("lower bound must be positive when integrating in log x", context)

        ! The breakpoints last, in the table's order: finite, then inside, then distinct. The
        ! finiteness pass runs first so that no comparison below ever sees a NaN, and "strictly
        ! inside" is what it says -- a breakpoint ON a bound would make a zero-width piece, and a
        ! caller who wrote one meant something else. Distinctness is checked pairwise rather than
        ! after the sort, because the sort belongs to the integration and this is validation:
        ! the list is a handful of values a person typed, so the pairs cost nothing.
        if (present(breakpoints)) then
            do i = 1, size(breakpoints)
                if (.not. is_finite(breakpoints(i))) &
                    call integrate_abort("breakpoints must be finite", context)
            end do
            do i = 1, size(breakpoints)
                if (breakpoints(i) <= a .or. breakpoints(i) >= b) &
                    call integrate_abort("breakpoints must lie strictly inside the range", context)
            end do
            do i = 1, size(breakpoints)
                do j = i + 1, size(breakpoints)
                    if (breakpoints(i) == breakpoints(j)) &
                        call integrate_abort("breakpoints must be distinct", context)
                end do
            end do
        end if

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


end submodule parquet_integrate_driver
