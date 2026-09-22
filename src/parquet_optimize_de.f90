!> Differential evolution (`pf_minimize_de`): DE/rand/1/bin over a box, seeded, with the
!> population evaluated on `threads=` threads.
!!
!! **The answer is a function of `(seed, arguments, objective)` and of nothing else** -- not of
!! `threads`, not of the schedule, not of which thread finished first. Two rules produce that, and
!! only a test comparing thread counts can see either break:
!!
!!  1. **Every draw is addressed by `(seed, generation, individual)`, and nothing is drawn outside
!!     that scope.** One key per generation (`pf_random_key(seed, g)`), one `pf_random_stream` per
!!     individual seeded from that key inside the per-individual scope. A stream seeded once per
!!     generation and walked across individuals gives perfectly good numbers, reproduces serially,
!!     and diverges under threads -- the failure `test/test_random_omp.f90`'s `test_stream_schedule`
!!     exists for.
!!  2. **Every decision that combines individuals is made serially, or by a rule with no order
!!     dependence.** The selection is element-wise; the best is `minloc`, whose lowest index wins a
!!     tie. A "first thread to find a better value updates the best" pattern, or an OpenMP
!!     `reduction` over reals, would each be a different answer per thread count -- and every one of
!!     those answers is a valid DE run, so nothing but a cross-schedule comparison can fail.
!!
!! What is NOT promised is cross-compiler or cross-version bit equality. The draws are frozen by
!! `pf_random_algorithm` and `pf_random_perm_algorithm`, but `a + F*(b - c)` is an FMA-contractable
!! shape, one ulp in a trial vector can flip one selection, and the paths part from there.
!!
!! **A non-finite value is "outside my domain", not an error.** Unlike the local engines, which
!! abort, this one never selects such a trial, counts it in `info%nonfinite`, and carries a
!! non-finite incumbent as `+Infinity` so that every later comparison is ordered and no NaN reaches
!! a `minloc`, a `maxval` or a `<`. An initial population with no finite value at all is
!! `PF_OPT_NONFINITE`.
submodule (parquet_optimize) parquet_optimize_de

#ifdef _OPENMP
    use omp_lib, only : omp_get_thread_num, omp_get_num_threads
#endif
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_positive_inf

    implicit none

    !> The caller's objective seen through the box: `f(min(upper, max(lower, y)))`.
    !!
    !! **`polish` runs the unbounded simplex, and this is what keeps its answer inside the box.**
    !! Minimising the projected objective is flat outside the box rather than forbidden there, so
    !! the simplex neither walks away down a slope the box excludes nor has to be taught about
    !! bounds; the point that comes back is the projection, which is a point of the box, and the
    !! value is the value there. The alternative -- polishing the objective itself -- answers
    !! outside the box with `PF_OPT_OK`, or walks to `-Infinity` on a linear objective and aborts
    !! inside `pf_minimize_simplex`, an entry point the caller never called.
    type, extends(pf_objective) :: boxed_objective
        class(pf_objective), pointer :: inner => null() !! the caller's own objective
        real(real64), allocatable    :: lower(:)        !! the box, lower corner
        real(real64), allocatable    :: upper(:)        !! the box, upper corner
    contains
        procedure :: eval => boxed_objective_eval !! Value at the point projected into the box.
    end type boxed_objective

contains

    !> Evaluates the caller's objective at `x` projected into the box.
    !!
    !! The projection is `min`/`max` on values the simplex formed from finite vertices, so neither
    !! sees a NaN -- which matters, since both compile to instructions that raise IEEE invalid on
    !! one.
    function boxed_objective_eval(this, x) result(f)
        class(boxed_objective), intent(inout) :: this !! the wrapper, holding the box
        real(real64), intent(in)              :: x(:) !! the point the simplex chose
        real(real64)                          :: f    !! objective value at the projected point

        f = this%inner%eval(min(this%upper, max(this%lower, x)))

    end function boxed_objective_eval

    module procedure minimize_de_obj

        type(objective_slot), allocatable :: slots(:) !! one clone of `f` per thread, or unallocated
        real(real64), allocatable :: pop(:,:)         !! the population, one individual per column
        real(real64), allocatable :: fpop(:)          !! its values; finite or `+Infinity`, never NaN
        real(real64), allocatable :: trial(:,:)       !! this generation's trial vectors
        real(real64), allocatable :: ftrial(:)        !! their values, before the selection screens them
        type(pf_optimize_info) :: polish_info         !! what the optional final simplex did
        real(real64) :: ftol_use                      !! fractional tolerance actually in force
        real(real64) :: atol_use                      !! absolute tolerance actually in force
        real(real64) :: fw                            !! differential weight actually in force
        real(real64) :: cr_use                        !! crossover probability actually in force
        real(real64) :: fhi, flo                      !! worst and best value in the population
        real(real64) :: spread_end                    !! the value spread the run ended on
        real(real64) :: rtol                          !! fractional spread of the current values
        real(real64) :: denom                         !! scale the fractional test divides by
        real(real64) :: fpolish                       !! value the optional final simplex reached
        integer :: npar                               !! number of variables
        integer :: np_use                             !! population size actually in force
        integer :: gen_budget                         !! generation budget actually in force
        integer :: budget                             !! evaluation budget actually in force
        integer :: nt                                 !! team size the population is evaluated on
        integer :: i, t, g                            !! individual, thread, generation
        integer :: ib                                 !! index of the best individual
        integer :: neval, nonfinite                   !! evaluations, and non-finite values seen
        integer :: status                             !! the PF_OPT_* code this run ends on
        logical :: bad                                !! one validator's verdict
        logical :: hit                                !! a convergence test fired
        logical :: keep                               !! this trial wins its selection
        logical :: do_polish                          !! finish with a simplex from the best individual

        call refuse_constrained("pf_minimize_de", f, context)

        ! ---- validate ---------------------------------------------------------------------
        npar = size(x)
        call validate_size("pf_minimize_de", npar, context)
        call validate_box("pf_minimize_de", lower, upper, npar, context)

        np_use = max(20, 10*npar)
        if (present(np)) then
            if (np < DE_MIN_NP) call optimize_abort("pf_minimize_de", &
                "np must be at least 4", context)
            np_use = np
        end if

        ! Finiteness first and separately in both range tests: an ordered comparison with a NaN
        ! operand raises IEEE invalid, fatal under nagfor's `-ieee=stop`, and `.or.` is not
        ! guaranteed to short-circuit -- so the NaN this exists to reject would abort the process
        ! before the abort with the message.
        fw = DE_F_WEIGHT
        if (present(f_weight)) then
            bad = .not. ieee_is_finite(f_weight)
            if (.not. bad) bad = (f_weight <= 0.0_real64 .or. f_weight > 2.0_real64)
            if (bad) call optimize_abort("pf_minimize_de", "f_weight must be in (0, 2]", context)
            fw = f_weight
        end if

        cr_use = DE_CR
        if (present(cr)) then
            bad = .not. ieee_is_finite(cr)
            if (.not. bad) bad = (cr < 0.0_real64 .or. cr > 1.0_real64)
            if (bad) call optimize_abort("pf_minimize_de", "cr must be in [0, 1]", context)
            cr_use = cr
        end if

        ftol_use = DE_FTOL
        if (present(ftol)) then
            call validate_tolerance("pf_minimize_de", "ftol", ftol, context)
            ftol_use = ftol
        end if
        atol_use = 0.0_real64
        if (present(atol)) then
            call validate_tolerance("pf_minimize_de", "atol", atol, context)
            atol_use = atol
        end if

        ! At least one convergence test must be able to fire, or the run can only end by
        ! exhausting a budget -- a caller mistake rather than a way to ask for "as long as
        ! possible". `pf_minimize_simplex` refuses the same pair.
        if (ftol_use + atol_use <= 0.0_real64) call optimize_abort("pf_minimize_de", &
            "at least one of ftol and atol must be positive", context)

        gen_budget = DE_MAX_GEN
        if (present(max_gen)) then
            if (max_gen < 1) call optimize_abort("pf_minimize_de", &
                "max_gen must be positive", context)
            gen_budget = max_gen
        end if

        ! The default budget is the product of two validated counts, formed in `int64` and capped:
        ! `np*(max_gen + 1)` overflows a default integer for a large population, and the overflow
        ! would deliver a negative budget that stops the run at once. **The `+ 1` is added in
        ! `int64` as well**: added before the conversion it overflows at `max_gen = huge(1)`, which
        ! is a budget of zero and a run that ends before its first generation
        ! (`max_gen at huge(1) runs generations rather than stopping at once`).
        budget = int(min(int(np_use, int64)*(int(gen_budget, int64) + 1_int64), &
                         int(huge(1)/2, int64)))
        if (present(max_neval)) then
            call validate_budget("pf_minimize_de", max_neval, context)
            budget = max_neval
        end if

        call resolve_threads("pf_minimize_de", threads, nt, context)

        do_polish = .false.
        if (present(polish)) do_polish = polish

        ! ---- set up ------------------------------------------------------------------------
        allocate(pop(npar, np_use), fpop(np_use))
        allocate(trial(npar, np_use), ftrial(np_use))

        if (present(history)) then
            history%n = 0
            allocate(history%x(npar, 0))
            allocate(history%f(0))
        end if

        ! One clone of the caller's objective per thread, in a SHARED array allocated before any
        ! region and indexed by `omp_get_thread_num() + 1`: never a `private()` copy (gfortran
        ! leaves an allocatable component's descriptor garbage) and never block-local inside the
        ! region (ifx's privatization scaffolding segfaults on such a type). At `threads = 1` the
        ! caller's own object is evaluated instead, so a counter or a cache it keeps is what the
        ! caller reads back.
        if (nt > 1) then
            allocate(slots(nt))
            do t = 1, nt
                allocate(slots(t)%obj, source=f)
            end do
        end if

        call latin_hypercube(seed, lower, upper, pop)
        call evaluate_pass(0_int64, .true.)
        neval = np_use
        g = 0
        nonfinite = count(.not. is_finite_quiet(fpop))

        if (nonfinite == np_use) then

            ! Not one finite value anywhere: there is nothing to minimise and nothing honest to
            ! return but the box's own centre, with `+Infinity` rather than a NaN so a caller who
            ! compares `fmin` is not trapped by the answer.
            status = PF_OPT_NONFINITE
            x(:) = 0.5_real64*(lower(:) + upper(:))
            fmin = ieee_value(1.0_real64, ieee_positive_inf)
            ! The spread is `+Infinity` too, set rather than formed: the difference of two
            ! infinities is a NaN, and forming it raises IEEE invalid, fatal under nagfor.
            spread_end = fmin

        else

            ! A non-finite individual is outside the objective's domain, not an error: carried as
            ! `+Infinity` so that every later `minloc`, `maxval` and `<` is ordered. Without this
            ! the very first `minloc` would compare a NaN, which nagfor traps on.
            where (.not. is_finite_quiet(fpop)) fpop = ieee_value(1.0_real64, ieee_positive_inf)

            ib = minloc(fpop, 1)
            if (present(history)) call history%append(pop(:,ib), fpop(ib))

            status = PF_OPT_LIMIT
            generations: do

                fhi = maxval(fpop)
                flo = minval(fpop)

                if (present(ftarget)) then
                    ! A NaN target can never be met, and comparing against one would raise IEEE
                    ! invalid; `+/-Infinity` compares perfectly well and is left alone.
                    if (.not. ieee_is_nan(ftarget)) then
                        if (flo <= ftarget) then
                            status = PF_OPT_TARGET
                            exit generations
                        end if
                    end if
                end if

                ! An infinite worst value means the population still holds an individual outside
                ! the domain, so it has not converged -- and the fractional test would be Inf/Inf,
                ! a NaN, which the comparison below must never see.
                hit = .false.
                if (ieee_is_finite(fhi)) then
                    denom = abs(fhi) + abs(flo)
                    if (denom > 0.0_real64) then
                        rtol = 2.0_real64*(fhi - flo)/denom
                    else
                        rtol = 0.0_real64
                    end if
                    if (rtol < ftol_use) hit = .true.
                    if ((fhi - flo) < atol_use) hit = .true.
                end if
                if (hit) then
                    status = PF_OPT_OK
                    exit generations
                end if

                if (g >= gen_budget .or. neval >= budget) then
                    status = PF_OPT_LIMIT
                    exit generations
                end if

                g = g + 1
                call evaluate_pass(pf_random_key(seed, int(g, int64)), .false.)
                neval = neval + np_use
                nonfinite = nonfinite + count(.not. is_finite_quiet(ftrial))

                ! Greedy one-to-one selection, element-wise and serial: individual `i` competes
                ! with trial `i` and with nothing else, so no thread ever decides another's fate.
                ! A non-finite trial never wins -- screened here rather than in the comparison,
                ! which would raise IEEE invalid on the NaN.
                do i = 1, np_use
                    keep = is_finite_quiet(ftrial(i))
                    if (keep) keep = (ftrial(i) <= fpop(i))
                    if (keep) then
                        pop(:,i) = trial(:,i)
                        fpop(i) = ftrial(i)
                    end if
                end do

                ib = minloc(fpop, 1)
                if (present(history)) call history%append(pop(:,ib), fpop(ib))

            end do generations

            ! Every exit is taken after `fhi` and `flo` were last formed and before the population
            ! changed again. The selection keeps a finite value finite, so `flo` is finite and
            ! the difference is finite or `+Infinity`.
            spread_end = fhi - flo

            ib = minloc(fpop, 1)
            x(:) = pop(:,ib)
            fmin = fpop(ib)

            if (do_polish) then
                ! The local engine from the best individual, minimising the objective seen through
                ! the box so that its answer is a point of the box. It aborts on a non-finite
                ! value, so `polish=` and an objective with a NaN region inside the box do not
                ! mix; the guide page says so.
                call polish_in_box(f, x, fpolish, polish_info)
                neval = neval + polish_info%neval
                if (fpolish < fmin) then
                    fmin = fpolish
                else
                    x(:) = pop(:,ib)
                end if
            end if

        end if

        ! ---- report --------------------------------------------------------------------------
        if (present(population)) population = pop
        if (present(history)) call history_trim(history)

        if (present(info)) then
            info%status = status
            info%converged = (status == PF_OPT_OK .or. status == PF_OPT_TARGET)
            info%neval = neval
            info%niter = g
            info%nonfinite = nonfinite
            info%spread = spread_end
        end if

    contains

        !> Runs the simplex from the best individual on the objective seen through the box.
        !!
        !! **`obj` is `target` here although the caller's own actual argument is not**: the
        !! language allows that and only makes the pointer undefined once this procedure returns,
        !! which is after the wrapper is gone. It is what lets the polish evaluate the caller's OWN
        !! objective rather than a copy of it, so a counter or a cache the objective keeps records
        !! the polish too, exactly as it records a serial run's generations.
        subroutine polish_in_box(obj, xp, fp, ip)
            class(pf_objective), intent(inout), target :: obj !! the caller's objective
            real(real64), intent(inout)         :: xp(:) !! best individual in, polished point out
            real(real64), intent(out)           :: fp    !! value at the point that comes back
            type(pf_optimize_info), intent(out) :: ip    !! what the simplex did

            type(boxed_objective) :: bx !! `obj` seen through the box

            bx%inner => obj
            bx%lower = lower
            bx%upper = upper
            call pf_minimize_simplex(bx, xp, fp, DE_POLISH_STEP*(upper - lower), ftol_use, &
                                     atol=atol_use, info=ip, context=context)
            ! `fp` is already the value AT this projection -- every evaluation the simplex made was
            ! of the projected point -- so the two come back consistent with each other.
            xp(:) = min(upper, max(lower, xp))

        end subroutine polish_in_box

        !> Evaluates the whole population once, serially or on `nt` threads.
        !!
        !! `initial` selects the pass: the starting population straight into `fpop`, or a
        !! generation's mutation, crossover and trial evaluation into `trial`/`ftrial`. One region
        !! serves both so that the clone indexing and the team-size record have a single site.
        !! `schedule(dynamic)` because one objective's cost can vary by orders of magnitude across
        !! the box; the answer does not depend on the schedule, which is the point of this engine.
        subroutine evaluate_pass(key_g, initial)
            integer(int64), intent(in) :: key_g  !! this generation's key; ignored when `initial`
            logical, intent(in)        :: initial !! evaluate the starting population instead

            integer :: i   !! individual
            integer :: tid !! this thread's slot, 1-based

            if (nt > 1) then

                !$omp parallel default(shared) private(i, tid) num_threads(nt)
                tid = 1
#ifdef _OPENMP
                tid = omp_get_thread_num() + 1
                ! What the region ACHIEVED, taken from inside it: a `threads=` that is validated,
                ! clamped and then never reaches a `num_threads` clause is invisible to every
                ! assertion about the answer, which is bit-identical at any team size.
                if (tid == 1) call record_team_size(omp_get_num_threads())
#endif
                !$omp do schedule(dynamic)
                do i = 1, np_use
                    call work_item(i, key_g, initial, slots(tid)%obj)
                end do
                !$omp end do
                !$omp end parallel

            else

                call record_team_size(1)
                do i = 1, np_use
                    call work_item(i, key_g, initial, f)
                end do

            end if

        end subroutine evaluate_pass

        !> One individual's share of one pass, through the objective this thread owns.
        !!
        !! **The stream is a local of this procedure and is seeded as its first act.** That is the
        !! per-individual scope the reproducibility rule asks for: one call, one stream, addressed
        !! by `(key_g, i)` and by nothing the loop carries. It must never be hoisted above the loop
        !! or put in a `private()` clause.
        !!
        !! Everything it touches of the host is shared and either read-only or written at index `i`
        !! alone, which is what makes host association safe here: a variable the region declares
        !! `private` would be the ORIGINAL, not this thread's copy, so nothing private is reached.
        subroutine work_item(i, key_g, initial, obj)
            integer, intent(in)                :: i       !! which individual
            integer(int64), intent(in)         :: key_g   !! this generation's key
            logical, intent(in)                :: initial !! evaluate the starting population
            class(pf_objective), intent(inout) :: obj     !! this thread's objective

            type(pf_random_stream) :: rng !! this individual's own stream
            real(real64) :: mutant        !! one coordinate of the mutant vector
            integer :: r1, r2, r3         !! the three donors, distinct from each other and from `i`
            integer :: jrand              !! the coordinate crossover always takes from the mutant
            integer :: j                  !! coordinate

            if (initial) then
                fpop(i) = obj%eval(pop(:,i))
                return
            end if

            call rng%seed(key_g, i)
            do
                call rng%int_range(1, np_use, r1)
                if (r1 /= i) exit
            end do
            do
                call rng%int_range(1, np_use, r2)
                if (r2 /= i .and. r2 /= r1) exit
            end do
            do
                call rng%int_range(1, np_use, r3)
                if (r3 /= i .and. r3 /= r1 .and. r3 /= r2) exit
            end do
            call rng%int_range(1, npar, jrand)

            do j = 1, npar
                if (u_or_forced(j, jrand, rng)) then
                    ! The mutant is finite by construction -- three points inside a finite box,
                    ! combined with a weight of at most 2 -- so these comparisons never see a NaN.
                    ! That matters: an ordered comparison raises IEEE invalid on one.
                    !
                    ! **A component outside the box is placed half way between its parent's own
                    ! component and the bound it crossed**, not clipped to the bound. Clipping
                    ! piles individuals onto the boundary, where they are identical and the
                    ! differences that drive the search collapse; the midpoint keeps the
                    ! population's spread and needs no extra draw, so the addressing of
                    ! `(seed, generation, individual)` is untouched.
                    mutant = pop(j,r1) + fw*(pop(j,r2) - pop(j,r3))
                    if (mutant < lower(j)) then
                        mutant = 0.5_real64*(pop(j,i) + lower(j))
                    else if (mutant > upper(j)) then
                        mutant = 0.5_real64*(pop(j,i) + upper(j))
                    end if
                    trial(j,i) = mutant
                else
                    trial(j,i) = pop(j,i)
                end if
            end do

            ftrial(i) = obj%eval(trial(:,i))

        end subroutine work_item

        !> Draws this coordinate's crossover decision: take it from the mutant, or keep the parent's.
        !!
        !! Its own procedure so that the draw happens exactly once per coordinate whichever branch
        !! is taken -- a draw made inside one arm of an `if` would consume a different number of
        !! words per individual and break the addressing the whole engine rests on.
        logical function u_or_forced(j, jrand, rng) result(take)
            integer, intent(in)                  :: j     !! this coordinate
            integer, intent(in)                  :: jrand !! the coordinate always taken from the mutant
            type(pf_random_stream), intent(inout) :: rng  !! this individual's stream

            real(real64) :: u !! the draw

            call rng%uniform(u)
            take = (u < cr_use .or. j == jrand)

        end function u_or_forced

    end procedure minimize_de_obj

    ! The FULLY RESTATED form, not `module procedure minimize_de_func`: in the abbreviated form
    ! gfortran 15 gives the `procedure(pf_objective_func)` dummy an implicit interface and refuses
    ! the pointer assignment below with "Explicit interface required for 'f'".
    module subroutine minimize_de_func(f, lower, upper, seed, x, fmin, np, f_weight, cr, ftol, &
                                       atol, ftarget, max_gen, max_neval, threads, polish, &
                                       info, history, population, context)
        implicit none
        procedure(pf_objective_func)                     :: f          !! the objective
        real(real64), intent(in)                         :: lower(:)   !! the box, lower corner
        real(real64), intent(in)                         :: upper(:)   !! the box, upper corner
        integer(int64), intent(in)                       :: seed       !! the run's seed
        real(real64), intent(out)                        :: x(:)       !! the best point found
        real(real64), intent(out)                        :: fmin       !! value at `x`
        integer, intent(in), optional                    :: np         !! population size
        real(real64), intent(in), optional               :: f_weight   !! differential weight
        real(real64), intent(in), optional               :: cr         !! crossover probability
        real(real64), intent(in), optional               :: ftol       !! fractional tolerance
        real(real64), intent(in), optional               :: atol       !! absolute tolerance
        real(real64), intent(in), optional               :: ftarget    !! stop at this value
        integer, intent(in), optional                    :: max_gen    !! generation budget
        integer, intent(in), optional                    :: max_neval  !! evaluation budget
        integer, intent(in), optional                    :: threads    !! evaluation team size
        logical, intent(in), optional                    :: polish     !! finish with a simplex
        type(pf_optimize_info), intent(out), optional    :: info       !! what happened
        type(pf_optimize_history), intent(out), optional :: history    !! best of each generation
        real(real64), allocatable, intent(out), optional :: population(:,:) !! final population
        character(len=*), intent(in), optional           :: context    !! call-site text

        type(func_objective) :: obj !! wraps the plain function as an objective object

        obj%fun => f
        call minimize_de_obj(obj, lower, upper, seed, x, fmin, np, f_weight, cr, ftol, atol, &
                             ftarget, max_gen, max_neval, threads, polish, info, history, &
                             population, context)

    end subroutine minimize_de_func

end submodule parquet_optimize_de ! GCOVR_EXCL_LINE
