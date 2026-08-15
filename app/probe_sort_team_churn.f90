!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Reproducer probe for `feature_risks.md` Risk-93: an intermittent hang inside the Fortran sort
!> engine's threaded histogram (`sort_radix_hist_threaded`).
!>
!> **What this is trying to provoke.** `test_merge_size_sweep` (`test/test_sorting.f90`) sorts every
!> size from 2 to 400 at every thread count from 2 to 8 — roughly 2800 sorts, each opening its own
!> `!$omp parallel num_threads(nt)` team, with `nt` cycling. During the Stage 6 flip's test run one
!> such sort sat for over 456 seconds; `sample` put it in libgomp under
!> `_pthread_mutex_firstfit_lock_slow`. The engine's own region there has no lock, no `critical` and
!> no explicit barrier, so the suspicion is libgomp's team pool rather than this library's code.
!>
!> **What it deliberately does NOT assume.** An earlier write-up blamed nesting — a `threads=` honoured
!> inside a caller's parallel region. That was refuted: the `sorting` suite is excluded from
!> test-drive's per-test parallelism (`suite_is_safe_to_parallelize`), so test-drive's
!> `!$omp parallel do ... if (parallel_)` is INACTIVE there and `omp_in_parallel()` is `.false.`
!> inside it. The observed hang had no enclosing active region at all. This probe therefore runs
!> the churn at top level by default, and `--nested` is a separate arm rather than the premise.
!>
!> **Reading the output.** Each round prints its wall-clock time. The pathology is a round that never
!> prints, not a round that is slow — so run it under a timeout and look at which round is missing.
!> `--fixed=T` pins `num_threads` instead of cycling it, which is the one-variable test of the
!> "varying team size churns the pool" hypothesis.
!>
!> Manual tool: never run by `fpm test`. Build with `--profile release`.
program probe_sort_team_churn
#ifdef _OPENMP
    use omp_lib, only : omp_get_wtime, omp_get_max_threads
#endif
    use parquet
    use iso_fortran_env, only : int32, int64, real64, output_unit
    implicit none

    integer(int64) :: n0, n1, t0, t1, rounds, fixed
    logical :: nested
    integer(int64) :: r, n, t, k
    real(real64), allocatable :: v(:)
    integer(int32), allocatable :: perm(:)
    real(real64) :: w0, w1
    integer(int64) :: sorts

    n0 = 2_int64; n1 = 400_int64; t0 = 2_int64; t1 = 8_int64
    rounds = 20_int64; fixed = 0_int64; nested = .false.
    call parse_args()

#ifndef _OPENMP
    write (output_unit, "(a)") "probe_sort_team_churn: built without OpenMP -- nothing to probe."
    stop 0
#endif

    call parquet_debug_use_fortran_sort_engine(.true.)
    ! Without this the engine's own floor (max(32768, 2048*nt)) declines every size below, and the
    ! probe would open no teams at all -- i.e. it would measure nothing while looking healthy.
    call parquet_debug_set_sort_engine_min_rows(2_int64)

    write (output_unit, "(a)") "probe_sort_team_churn"
    write (output_unit, "(a,i0,a,i0,a,i0,a,i0)") "  sizes ", n0, "..", n1, "  threads ", t0, "..", t1
#ifdef _OPENMP
    write (output_unit, "(a,i0)") "  omp_get_max_threads = ", int(omp_get_max_threads(), int64)
#endif
    if (fixed > 0_int64) write (output_unit, "(a,i0)") "  FIXED num_threads = ", fixed
    if (nested) write (output_unit, "(a)") "  arm: inside an ACTIVE !$omp parallel region"
    write (output_unit, "(a)") "  a round that never prints is the hang; a slow round is not."
    flush (output_unit)

    do r = 1_int64, rounds
        sorts = 0_int64
#ifdef _OPENMP
        w0 = omp_get_wtime()
#endif
        if (nested) then
            !$omp parallel default(shared) private(n, t, v, perm, k)
            !$omp do schedule(dynamic)
            do n = n0, n1
                allocate (v(n))
                do k = 1_int64, n
                    v(k) = real(mod(k * 37_int64, n), real64) + 0.5_real64
                end do
                do t = t0, t1
                    call pf_argsort(v, perm, threads=int(merge(fixed, t, fixed > 0_int64), int32))
                end do
                deallocate (v)
            end do
            !$omp end do
            !$omp end parallel
        else
            do n = n0, n1
                allocate (v(n))
                do k = 1_int64, n
                    v(k) = real(mod(k * 37_int64, n), real64) + 0.5_real64
                end do
                do t = t0, t1
                    call pf_argsort(v, perm, threads=int(merge(fixed, t, fixed > 0_int64), int32))
                    sorts = sorts + 1_int64
                end do
                deallocate (v)
            end do
        end if
#ifdef _OPENMP
        w1 = omp_get_wtime()
        write (output_unit, "(a,i0,a,f8.3,a,i0,a)") "  round ", r, ": ", w1 - w0, " s  (", sorts, " sorts)"
#endif
        flush (output_unit)
    end do

    call parquet_debug_set_sort_engine_min_rows(-1_int64)
    write (output_unit, "(a)") "completed without hanging"

contains

    !> Reads `--key=value` flags; anything unrecognised is a hard error rather than a silent default.
    subroutine parse_args()
        character(len=256) :: a !! one raw argument.
        integer :: i            !! argument index.
        !
        do i = 1, command_argument_count()
            call get_command_argument(i, a)
            select case (arg_key(a))
            case ("--n0");     n0 = arg_int(a)
            case ("--n1");     n1 = arg_int(a)
            case ("--t0");     t0 = arg_int(a)
            case ("--t1");     t1 = arg_int(a)
            case ("--rounds"); rounds = arg_int(a)
            case ("--fixed");  fixed = arg_int(a)
            case ("--nested"); nested = .true.
            case default
                write (output_unit, "(a)") "unknown argument: " // trim(a)
                error stop 1
            end select
        end do
    end subroutine parse_args

    !> The part of `--key=value` before the `=`, or the whole flag when it carries no value.
    function arg_key(a) result(k)
        character(len=*), intent(in) :: a !! raw argument.
        character(len=:), allocatable :: k !! the key.
        integer :: p !! position of '='.
        !
        p = index(a, "=")
        if (p > 0) then
            k = trim(a(1:p - 1))
        else
            k = trim(a)
        end if
    end function arg_key

    !> The integer after `--key=`; aborts rather than defaulting when it will not parse.
    function arg_int(a) result(n)
        character(len=*), intent(in) :: a !! raw argument.
        integer(int64) :: n               !! parsed value.
        integer :: p, ios                 !! '=' position and read status.
        !
        p = index(a, "=")
        if (p <= 0) then
            write (output_unit, "(a)") "argument needs a value: " // trim(a)
            error stop 1
        end if
        read (a(p + 1:), *, iostat=ios) n
        if (ios /= 0) then
            write (output_unit, "(a)") "not an integer: " // trim(a)
            error stop 1
        end if
    end function arg_int

end program probe_sort_team_churn
