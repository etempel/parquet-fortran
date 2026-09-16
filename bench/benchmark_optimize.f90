!> What `parquet_optimize` costs: evaluations per engine per function, what threading `threads=`
!> buys and whether the answer survives it, how DE's three tuning knobs trade against each other,
!> and how many starts the multistart driver needs to find how many basins.
!!
!! **The `threads` mode is a benchmark that also proves the property it measures.** Every arm of the
!! ladder is compared BIT FOR BIT with the serial arm's answer, and the mode exits nonzero when a
!! single value differs. `pf_minimize_de` claims its result is a function of `(seed, arguments,
!! objective)` and not of the thread count; a wall-clock table with no such gate would report a
!! speedup obtained by answering a different question on each arm.
!!
!! **No mode here prints a number the guide page then quotes exactly.** The counts a search makes
!! are whole-search-path quantities: a compiler that contracts `a + F*(b - c)` into a fused
!! multiply-add flips one selection and moves the total. The page carries orders of magnitude,
!! which is what these tables are for. Name the machine, the toolchain and the load average when
!! reporting a figure from here.
program benchmark_optimize

    use parquet_optimize
    use iso_fortran_env, only : real64, int64, error_unit

    implicit none

    !> How costly one `spin` evaluation is, in arbitrary work units; set by `--cost`.
    integer, save :: cost_units = 0

    !> `2 pi`, for Rastrigin.
    real(real64), parameter :: TWO_PI = 2.0_real64*acos(-1.0_real64)

    character(len=32) :: mode
    integer           :: rounds, nstart_max, seeds

    call parse_arguments(mode, rounds, nstart_max, seeds, cost_units)

    select case (trim(mode))
    case ("evals")
        call run_evals(seeds)
    case ("threads")
        call run_threads(rounds)
    case ("params")
        call run_params(seeds)
    case ("starts")
        call run_starts(nstart_max)
    case default
        write (error_unit, '(a)') "benchmark_optimize: unknown mode '"//trim(mode)//"'"
        error stop 1
    end select

contains

    !> Reads the `--key=value` command line, applying each default when the flag is absent.
    subroutine parse_arguments(mode, rounds, nstart_max, seeds, cost)
        character(len=*), intent(out) :: mode       !! which measurement to run
        integer, intent(out)          :: rounds     !! timed rounds; the best of them is kept
        integer, intent(out)          :: nstart_max !! largest start count the `starts` mode tries
        integer, intent(out)          :: seeds      !! seeds averaged over, where a mode averages
        integer, intent(out)          :: cost       !! work units per evaluation in `threads` mode

        character(len=64) :: arg, val
        integer           :: k, eq

        mode = "evals"
        rounds = 3
        nstart_max = 80
        seeds = 5
        cost = 200

        do k = 1, command_argument_count()
            call get_command_argument(k, arg)
            eq = index(arg, "=")
            if (eq == 0) then
                write (error_unit, '(a)') "benchmark_optimize: unrecognised argument '" &
                    //trim(arg)//"'"
                error stop 1
            end if
            val = arg(eq + 1:)
            select case (arg(1:eq - 1))
            case ("--mode")
                mode = trim(val)
            case ("--rounds")
                read (val, *) rounds
            case ("--nstart-max")
                read (val, *) nstart_max
            case ("--seeds")
                read (val, *) seeds
            case ("--cost")
                read (val, *) cost
            case default
                write (error_unit, '(a)') "benchmark_optimize: unknown option '" &
                    //trim(arg(1:eq - 1))//"'"
                write (error_unit, '(a)') "Usage: benchmark_optimize " &
                    //"[--mode=evals|threads|params|starts] [--rounds=N] [--seeds=N] " &
                    //"[--nstart-max=N] [--cost=N]"
                error stop 1
            end select
        end do

        if (rounds < 1) error stop "benchmark_optimize: --rounds must be >= 1"
        if (seeds < 1) error stop "benchmark_optimize: --seeds must be >= 1"
        if (nstart_max < 1) error stop "benchmark_optimize: --nstart-max must be >= 1"
        if (cost < 0) error stop "benchmark_optimize: --cost must be >= 0"

    end subroutine parse_arguments

    ! ---- the objectives ---------------------------------------------------------------------

    !> Rosenbrock's function in any number of variables; minimum `0` at `x = 1`.
    function rosenbrock(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        integer :: i

        f = 0.0_real64
        do i = 1, size(x) - 1
            f = f + (1.0_real64 - x(i))**2 + 100.0_real64*(x(i+1) - x(i)**2)**2
        end do

    end function rosenbrock

    !> Sum of squares about `1`; minimum `0` at `x = 1`.
    function sphere(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = sum((x - 1.0_real64)**2)

    end function sphere

    !> Rastrigin's function; minimum `0` at the origin, with about `11**n` local minima.
    function rastrigin(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = 10.0_real64*size(x) + sum(x**2 - 10.0_real64*cos(TWO_PI*x))

    end function rastrigin

    !> A three-basin function in two variables, for the `starts` mode.
    !!
    !! The six-hump camel: six local minima over `[-3, 3] x [-2, 2]`, two of them global.
    function camel(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = (4.0_real64 - 2.1_real64*x(1)**2 + x(1)**4/3.0_real64)*x(1)**2 + x(1)*x(2) &
            + (-4.0_real64 + 4.0_real64*x(2)**2)*x(2)**2

    end function camel

    !> The sphere, made deliberately expensive per evaluation by `cost_units` of arithmetic.
    !!
    !! The spin is a chain of transcendentals with a data dependence, so no compiler may hoist or
    !! vectorise it away, and its result is FOLDED INTO the answer rather than discarded -- a spin
    !! whose value nothing reads is deleted at `-O2` and the ladder then measures an empty loop.
    !! Folding it in changes the objective, not its minimiser: the added term vanishes at `x = 1`.
    function spin_sphere(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        real(real64) :: burn
        integer :: k

        f = sum((x - 1.0_real64)**2)
        burn = 1.0_real64
        do k = 1, cost_units
            burn = burn + sin(burn)*cos(burn)
        end do
        f = f + f*1.0e-300_real64*burn

    end function spin_sphere

    ! ---- evals ------------------------------------------------------------------------------

    !> Evaluations each engine spends reaching each function's known minimum.
    !!
    !! Averaged over `seeds` seeds for the two seeded engines, because one seed is one sample of a
    !! stochastic search and the spread between seeds is the quantity a reader needs. The local
    !! engines take no seed, so their row is one run from a fixed start and the SPREAD column is
    !! blank rather than zero.
    subroutine run_evals(seeds)
        integer, intent(in) :: seeds !! seeds averaged over, for the seeded engines

        print '(a)', "=== parquet_optimize: evaluations to the known minimum ==="
        print '(a,i0,a)', "seeded engines averaged over ", seeds, " seeds; target 1e-8"
        print '(a)', ""
        print '(a)', "function            n  engine            evals(mean)   evals(min-max)   best f"
        print '(a)', "-------------------------------------------------------------------------------"

        call evals_row("rosenbrock", 2, -5.0_real64, 5.0_real64, seeds)
        call evals_row("sphere", 10, -5.0_real64, 5.0_real64, seeds)
        call evals_row("rosenbrock", 10, -5.0_real64, 5.0_real64, seeds)
        call evals_row("rastrigin", 5, -5.12_real64, 5.12_real64, seeds)
        print '(a)', ""

    end subroutine run_evals

    !> One function's row of the `evals` table: the simplex, DE and the multistart driver.
    subroutine evals_row(name, n, lo1, hi1, seeds)
        character(len=*), intent(in) :: name  !! which objective
        integer, intent(in)          :: n     !! how many variables
        real(real64), intent(in)     :: lo1   !! the box, same in every coordinate
        real(real64), intent(in)     :: hi1   !! the box, same in every coordinate
        integer, intent(in)          :: seeds !! seeds averaged over

        type(pf_optimize_info) :: info
        real(real64) :: x(n), lower(n), upper(n), fmin, best
        integer :: ev(seeds), s, np

        lower = lo1
        upper = hi1

        ! The simplex, from the box's own corner-ish start: one run, no seed.
        x = lo1 + 0.3_real64*(hi1 - lo1)
        call pf_minimize_simplex(pick(name), x, fmin, spread(0.5_real64, 1, n), 0.0_real64, &
                                 atol=1.0e-10_real64, max_neval=200000, info=info)
        print '(a12,i4,2x,a16,i12,a17,es11.3)', name, n, "simplex", info%neval, "  (one start)  ", fmin

        np = max(20, 10*n)
        best = huge(1.0_real64)
        do s = 1, seeds
            call pf_minimize_de(pick(name), lower, upper, int(s, int64), x, fmin, np=np, &
                                ftarget=1.0e-8_real64, max_gen=40000, info=info)
            ev(s) = info%neval
            best = min(best, fmin)
        end do
        print '(a12,i4,2x,a16,i12,i8,a1,i8,es11.3)', name, n, "de", sum(ev)/seeds, &
            minval(ev), "-", maxval(ev), best

        best = huge(1.0_real64)
        do s = 1, seeds
            call pf_minimize_multistart(pick(name), lower, upper, int(s, int64), x, fmin, &
                                        nstart=10*n, info=info)
            ev(s) = info%neval
            best = min(best, fmin)
        end do
        print '(a12,i4,2x,a16,i12,i8,a1,i8,es11.3)', name, n, "multistart", sum(ev)/seeds, &
            minval(ev), "-", maxval(ev), best

    end subroutine evals_row

    ! ---- threads ----------------------------------------------------------------------------

    !> What `threads=` buys on an objective of configurable cost, and whether the answer survives.
    !!
    !! **The checksum gate is the point of this mode.** Every arm's whole final population is
    !! compared bit for bit with the serial arm's; one differing value exits nonzero. A speedup
    !! measured without it could have been bought by answering a different question per arm, which
    !! is exactly the failure the engine's design rules out and nothing else here would notice.
    subroutine run_threads(rounds)
        integer, intent(in) :: rounds !! timed rounds; the best is kept

        integer, parameter :: LADDER(6) = [1, 2, 4, 8, 16, 32]
        type(pf_optimize_info) :: info
        real(real64), allocatable :: reference(:,:), population(:,:)
        real(real64) :: x(8), lower(8), upper(8), fmin, t0, t1, best_time, serial_time
        integer :: k, r, nt, mismatches, total_mismatches

        lower = -5.0_real64
        upper = 5.0_real64

        print '(a)', "=== parquet_optimize: pf_minimize_de across a thread ladder ==="
        print '(a,i0,a,i0,a)', "8 variables, np 96, 200 generations, ", cost_units, &
            " work units per evaluation, best of ", rounds, " rounds"
        print '(a)', ""
        print '(a)', "threads   team     wall(s)   speedup   mismatches"
        print '(a)', "---------------------------------------------------"

        serial_time = 0.0_real64
        total_mismatches = 0

        do k = 1, size(LADDER)
            nt = LADDER(k)
            best_time = huge(1.0_real64)
            do r = 1, rounds
                call cpu_clock(t0)
                call pf_minimize_de(spin_sphere, lower, upper, 42_int64, x, fmin, np=96, &
                                    max_gen=200, threads=nt, info=info, population=population)
                call cpu_clock(t1)
                best_time = min(best_time, t1 - t0)
            end do

            mismatches = 0
            if (k == 1) then
                reference = population
                serial_time = best_time
            else
                mismatches = count(population /= reference)
            end if
            total_mismatches = total_mismatches + mismatches

            print '(i7,i7,f12.4,f10.2,i13)', nt, parquet_debug_optimize_threads_used(), &
                best_time, serial_time/best_time, mismatches
        end do

        print '(a)', ""
        if (total_mismatches /= 0) then
            write (error_unit, '(a,i0,a)') "benchmark_optimize: ", total_mismatches, &
                " population values differed across the thread ladder -- pf_minimize_de's answer" &
                //" must not depend on the thread count"
            error stop 1
        end if
        print '(a)', "every arm reproduced the serial population bit for bit"
        print '(a)', ""

    end subroutine run_threads

    ! ---- params -----------------------------------------------------------------------------

    !> How DE's three knobs trade against each other on a rugged objective.
    subroutine run_params(seeds)
        integer, intent(in) :: seeds !! seeds averaged over

        integer, parameter :: NP_LADDER(4) = [20, 50, 100, 200]
        real(real64), parameter :: F_LADDER(3) = [0.5_real64, 0.8_real64, 1.2_real64]
        real(real64), parameter :: CR_LADDER(4) = [0.1_real64, 0.2_real64, 0.5_real64, 0.9_real64]
        type(pf_optimize_info) :: info
        real(real64) :: x(5), lower(5), upper(5), fmin
        integer :: i, j, m, s, ev, hits

        lower = -5.12_real64
        upper = 5.12_real64

        print '(a)', "=== parquet_optimize: pf_minimize_de knobs on Rastrigin in 5 variables ==="
        print '(a,i0,a)', "evaluations to f < 1e-8, mean over ", seeds, &
            " seeds; 'hits' counts the seeds that reached it"
        print '(a)', ""
        print '(a)', "   np      F     CR   evals(mean)   hits"
        print '(a)', "-------------------------------------------"

        do i = 1, size(NP_LADDER)
            do j = 1, size(F_LADDER)
                do m = 1, size(CR_LADDER)
                    ev = 0
                    hits = 0
                    do s = 1, seeds
                        call pf_minimize_de(rastrigin, lower, upper, int(s, int64), x, fmin, &
                                            np=NP_LADDER(i), f_weight=F_LADDER(j), &
                                            cr=CR_LADDER(m), ftarget=1.0e-8_real64, &
                                            max_gen=3000, info=info)
                        ev = ev + info%neval
                        if (info%status == PF_OPT_TARGET) hits = hits + 1
                    end do
                    print '(i5,f7.1,f7.2,i14,i7)', NP_LADDER(i), F_LADDER(j), CR_LADDER(m), &
                        ev/seeds, hits
                end do
            end do
        end do
        print '(a)', ""

    end subroutine run_params

    ! ---- starts -----------------------------------------------------------------------------

    !> How many starts the multistart driver needs to find how many basins, and at what cost.
    subroutine run_starts(nstart_max)
        integer, intent(in) :: nstart_max !! largest start count to try

        type(pf_optimize_info) :: info
        real(real64) :: x(2), lower(2), upper(2), fmin
        integer :: ns

        print '(a)', "=== parquet_optimize: pf_minimize_multistart, starts against basins ==="
        print '(a)', "the six-hump camel over [-3, 3] x [-2, 2] (six local minima, two global),"
        print '(a)', "then Rastrigin over [-5.12, 5.12]**2 (121 of them), merge radius 1e-2"
        print '(a)', ""
        print '(a)', "function     nstart   nminima   nlimit      evals     best f"
        print '(a)', "------------------------------------------------------------"

        ns = 5
        do while (ns <= nstart_max)
            lower = [-3.0_real64, -2.0_real64]
            upper = [3.0_real64, 2.0_real64]
            call pf_minimize_multistart(camel, lower, upper, 42_int64, x, fmin, nstart=ns, &
                                        xtol=1.0e-2_real64, info=info)
            print '(a12,i9,i10,i9,i11,f11.5)', "camel", ns, info%nminima, info%nlimit, &
                info%neval, fmin
            ns = 2*ns
        end do

        ns = 5
        do while (ns <= nstart_max)
            lower = -5.12_real64
            upper = 5.12_real64
            call pf_minimize_multistart(rastrigin, lower, upper, 42_int64, x, fmin, nstart=ns, &
                                        xtol=1.0e-2_real64, info=info)
            print '(a12,i9,i10,i9,i11,f11.5)', "rastrigin", ns, info%nminima, info%nlimit, &
                info%neval, fmin
            ns = 2*ns
        end do
        print '(a)', ""

    end subroutine run_starts

    ! ---- helpers ----------------------------------------------------------------------------

    !> Wall-clock seconds, from `system_clock`, which counts elapsed time rather than CPU time.
    !!
    !! `cpu_time` sums over every thread, so a perfectly scaling run would show no speedup at all.
    subroutine cpu_clock(t)
        real(real64), intent(out) :: t !! seconds, on an arbitrary origin

        integer(int64) :: ticks, rate

        call system_clock(ticks, rate)
        t = real(ticks, real64)/real(rate, real64)

    end subroutine cpu_clock

    !> Selects one of the module's objectives by name, as a procedure pointer.
    !!
    !! A `select case` returning a pointer rather than four copies of each measurement loop. Every
    !! callback in this library is a module procedure, never an internal one, so these are the
    !! program's own contained procedures reached through a pointer rather than passed directly --
    !! which is the same thing at the call site and keeps the tables to one loop each.
    function pick(name) result(f)
        character(len=*), intent(in) :: name !! the objective's name

        procedure(pf_objective_func), pointer :: f !! the objective

        select case (name)
        case ("rosenbrock")
            f => rosenbrock
        case ("sphere")
            f => sphere
        case ("rastrigin")
            f => rastrigin
        case ("camel")
            f => camel
        case default
            write (error_unit, '(a)') "benchmark_optimize: no objective named '"//name//"'"
            error stop 1
        end select

    end function pick

end program benchmark_optimize
