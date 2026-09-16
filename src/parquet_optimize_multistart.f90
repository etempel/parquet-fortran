!> The multistart driver (`pf_minimize_multistart`): a local solver run from each point of a Latin
!> hypercube over a box, with the distinct minima counted.
!!
!! The cheap global method for an objective with a handful of basins. A local engine converges to
!! whichever basin it starts in and reports success for doing so, so enough starts spread evenly
!! over the box find every basin wide enough to catch one. On a function with thousands of local
!! minima it is not a substitute for `pf_minimize_de`, and the guide page says which is which.
!!
!! **The starts depend on `seed`, `nstart` and the box, and on nothing else.** Not on the solver,
!! not on `threads`: two runs differing only in their local engine explore the same basins, which
!! is what makes them comparable. The best is the lowest value with the lowest start index winning
!! a tie, and `nminima` counts by walking the starts in index order -- so nothing here depends on
!! which thread finished first.
!!
!! **The box bounds the starts, not the runs.** `pf_simplex_solver` is unbounded and may walk out
!! of the box; `pf_bobyqa_solver` honours it. The solver object is `intent(in)` throughout, so one
!! instance serves every thread.
!!
!! **The non-finite policy is the solver's, not this driver's.** `pf_simplex_solver` aborts on a
!! NaN or an infinity from the objective, so a NaN region inside the box takes the whole process
!! down -- from inside the team when `threads > 1`, which is the one abort in this module that
!! needs `optimize_abort`'s named `critical`. What this driver does own is the case where a run
!! comes back with a non-finite value anyway: it is never selected, it is counted in
!! `info%nonfinite`, and a run in which no start produced a finite value is `PF_OPT_NONFINITE`.
submodule (parquet_optimize) parquet_optimize_multistart

#ifdef _OPENMP
    use omp_lib, only : omp_get_thread_num, omp_get_num_threads
#endif
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_positive_inf

    implicit none

contains

    module procedure minimize_multistart_obj

        type(objective_slot), allocatable :: slots(:)  !! one clone of `f` per thread
        type(pf_simplex_solver) :: default_solver      !! what `solver=` defaults to
        real(real64), allocatable :: starts(:,:)       !! the Latin hypercube, one start per column
        real(real64), allocatable :: xs(:,:)           !! each start's own minimum
        real(real64), allocatable :: fs(:)             !! its value; finite or `+Infinity`, never NaN
        real(real64), allocatable :: reps(:,:)         !! one representative per distinct minimum
        real(real64), allocatable :: tol(:)            !! the merge radius, per coordinate
        integer, allocatable :: neval_k(:)             !! evaluations each run spent
        integer, allocatable :: status_k(:)            !! the code each run ended on
        real(real64) :: xtol_use                       !! merge radius actually in force, as a fraction
        integer :: npar                                !! number of variables
        integer :: ns                                  !! number of starts actually in force
        integer :: nt                                  !! team size the starts run on
        integer :: k, m, t                             !! start, representative, thread
        integer :: kbest                               !! index of the winning start
        integer :: nmin                                !! distinct minima counted so far
        integer :: nonfinite                           !! runs that came back non-finite
        integer :: status                              !! the PF_OPT_* code this run ends on
        logical :: fresh                               !! this start's minimum is one not yet counted

        call refuse_constrained("pf_minimize_multistart", f, context)

        ! ---- validate ---------------------------------------------------------------------
        npar = size(x)
        call validate_size("pf_minimize_multistart", npar, context)
        call validate_box("pf_minimize_multistart", lower, upper, npar, context)

        ns = max(10, 2*npar)
        if (present(nstart)) then
            if (nstart < 1) call optimize_abort("pf_minimize_multistart", &
                "nstart must be positive", context)
            ns = nstart
        end if

        xtol_use = MULTISTART_XTOL
        if (present(xtol)) then
            call validate_tolerance("pf_minimize_multistart", "xtol", xtol, context)
            xtol_use = xtol
        end if

        call resolve_threads("pf_minimize_multistart", threads, nt, context)

        ! ---- set up ------------------------------------------------------------------------
        allocate(starts(npar, ns), xs(npar, ns), fs(ns))
        allocate(reps(npar, ns), tol(npar))
        allocate(neval_k(ns), status_k(ns))

        ! One clone of the caller's objective per thread, in a SHARED array allocated before the
        ! region and indexed by `omp_get_thread_num() + 1`; never a `private()` copy and never
        ! block-local inside the region. At `threads = 1` the caller's own object is used, so a
        ! counter or a cache it keeps is what the caller reads back afterwards.
        if (nt > 1) then
            allocate(slots(nt))
            do t = 1, nt
                allocate(slots(t)%obj, source=f)
            end do
        end if

        call latin_hypercube(seed, lower, upper, starts)

        ! The default is a fresh `pf_simplex_solver`, whose own defaults are a valid tolerance pair
        ! as they stand -- which the bare `pf_minimize_simplex` call is not, since that one
        ! requires `ftol` positionally.
        if (present(solver)) then
            call run_every_start(solver)
        else
            call run_every_start(default_solver)
        end if

        ! ---- reduce, serially and in start order ---------------------------------------------
        nonfinite = count(.not. ieee_is_finite(fs))

        if (present(history)) then
            history%n = 0
            allocate(history%x(npar, 0))
            allocate(history%f(0))
        end if

        if (nonfinite == ns) then

            ! Not one finite value anywhere: the box's own centre and `+Infinity`, never a NaN.
            status = PF_OPT_NONFINITE
            x(:) = 0.5_real64*(lower(:) + upper(:))
            fmin = ieee_value(1.0_real64, ieee_positive_inf)
            nmin = 0

        else

            ! Carried as `+Infinity` so every comparison below is ordered: a NaN reaching `minloc`
            ! or `maxval` is wrong everywhere and traps under nagfor.
            where (.not. ieee_is_finite(fs)) fs = ieee_value(1.0_real64, ieee_positive_inf)

            ! The lowest value wins, and `minloc` gives the lowest index on a tie -- so a function
            ! with two equal minima always returns the same one, whatever the thread count.
            kbest = minloc(fs, 1)
            x(:) = xs(:,kbest)
            fmin = fs(kbest)

            ! Distinct minima, counted by walking the starts IN INDEX ORDER and merging each into
            ! the first representative it is within `xtol` of, coordinate by coordinate. Walking in
            ! any other order can give a different count on the same data, which is why no thread
            ! does any of this.
            tol(:) = xtol_use*(upper(:) - lower(:))
            nmin = 0
            do k = 1, ns
                if (.not. ieee_is_finite(fs(k))) cycle
                fresh = .true.
                do m = 1, nmin
                    if (all(abs(xs(:,k) - reps(:,m)) <= tol(:))) then
                        fresh = .false.
                        exit
                    end if
                end do
                if (fresh) then
                    nmin = nmin + 1
                    reps(:,nmin) = xs(:,k)
                end if
            end do

            ! One run reaching its own stopping rule is enough for the driver to have converged;
            ! `nlimit` is what says how many did not, and a run that ran out of budget still
            ! contributed its best point to the comparison above.
            if (any(status_k == PF_OPT_OK .or. status_k == PF_OPT_TARGET)) then
                status = PF_OPT_OK
            else
                status = PF_OPT_LIMIT
            end if

        end if

        ! Every start's own minimum, before merging: this is the map of the basins, and a row whose
        ! value is not one of the distinct minima is a run that ran out of budget.
        if (present(history)) then
            do k = 1, ns
                call history%append(xs(:,k), fs(k))
            end do
            call history_trim(history)
        end if

        if (present(info)) then
            info%status = status
            info%converged = (status == PF_OPT_OK)
            info%neval = sum(neval_k)
            info%niter = ns
            info%nonfinite = nonfinite
            info%spread = maxval(fs) - minval(fs)
            info%nminima = nmin
            info%nlimit = count(status_k == PF_OPT_LIMIT)
        end if

    contains

        !> Runs `sv` from every start, serially or on `nt` threads.
        !!
        !! `schedule(dynamic)` because two starts in different basins cost wildly different numbers
        !! of evaluations; the answer does not depend on the schedule, since every start writes only
        !! its own column and nothing is combined until the loop is over.
        subroutine run_every_start(sv)
            class(pf_local_solver), intent(in) :: sv !! the local engine and its options

            integer :: k   !! which start
            integer :: tid !! this thread's slot, 1-based

            if (nt > 1) then

                !$omp parallel default(shared) private(k, tid) num_threads(nt)
                tid = 1
#ifdef _OPENMP
                tid = omp_get_thread_num() + 1
                ! What the region ACHIEVED, taken from inside it: the answer is bit-identical at
                ! any team size, so a `threads=` that never reaches a `num_threads` clause is
                ! invisible to every assertion a test could make about the answer itself.
                if (tid == 1) call record_team_size(omp_get_num_threads())
#endif
                !$omp do schedule(dynamic)
                do k = 1, ns
                    call run_one_start(k, sv, slots(tid)%obj)
                end do
                !$omp end do
                !$omp end parallel

            else

                call record_team_size(1)
                do k = 1, ns
                    call run_one_start(k, sv, f)
                end do

            end if

        end subroutine run_every_start

        !> One local minimisation, from start `k`, through the objective this thread owns.
        !!
        !! Its locals are per-call and so per-thread; everything it touches of the host is shared
        !! and either read-only or written at index `k` alone. Nothing the region declares
        !! `private` is reached from here -- host association inside a region sees the ORIGINAL
        !! variable, not a thread's private copy, so a private one would be a silent race.
        subroutine run_one_start(k, sv, obj)
            integer, intent(in)                :: k   !! which start
            class(pf_local_solver), intent(in) :: sv  !! the local engine and its options
            class(pf_objective), intent(inout) :: obj !! this thread's objective

            type(pf_optimize_info) :: ik !! this run's own outcome
            real(real64) :: xk(npar)     !! this run's point, start in and minimum out
            real(real64) :: fk           !! its value

            xk(:) = starts(:,k)
            call sv%run(obj, xk, fk, lower, upper, ik)
            xs(:,k) = xk(:)
            fs(k) = fk
            neval_k(k) = ik%neval
            status_k(k) = ik%status

        end subroutine run_one_start

    end procedure minimize_multistart_obj

    ! The FULLY RESTATED form, not `module procedure minimize_multistart_func`: in the abbreviated
    ! form gfortran 15 gives the `procedure(pf_objective_func)` dummy an implicit interface and
    ! refuses the pointer assignment below with "Explicit interface required for 'f'".
    module subroutine minimize_multistart_func(f, lower, upper, seed, x, fmin, nstart, solver, &
                                               xtol, threads, info, history, context)
        implicit none
        procedure(pf_objective_func)                     :: f        !! the objective
        real(real64), intent(in)                         :: lower(:) !! the box, lower corner
        real(real64), intent(in)                         :: upper(:) !! the box, upper corner
        integer(int64), intent(in)                       :: seed     !! the run's seed
        real(real64), intent(out)                        :: x(:)     !! the best point found
        real(real64), intent(out)                        :: fmin     !! value at `x`
        integer, intent(in), optional                    :: nstart   !! how many starts
        class(pf_local_solver), intent(in), optional     :: solver   !! the local engine
        real(real64), intent(in), optional               :: xtol     !! merge radius, as a fraction
        integer, intent(in), optional                    :: threads  !! team the starts run on
        type(pf_optimize_info), intent(out), optional    :: info     !! what happened
        type(pf_optimize_history), intent(out), optional :: history  !! every start's minimum
        character(len=*), intent(in), optional           :: context  !! call-site text

        type(func_objective) :: obj !! wraps the plain function as an objective object

        obj%fun => f
        call minimize_multistart_obj(obj, lower, upper, seed, x, fmin, nstart, solver, xtol, &
                                     threads, info, history, context)

    end subroutine minimize_multistart_func

end submodule parquet_optimize_multistart
