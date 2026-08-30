!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Answers one question, and the phase plan (`feature_pandas_S4.md`, P5) will not let it be
!! answered by argument: **is `parquet_stats`' moment engine worth threading?**
!!
!! A single O(n) reduction over a resident array may be memory-bandwidth-bound, in which case
!! threading buys nothing and `threads=` would enter nine public signatures for a null result. So
!! this program measures four things, in four modes:
!!
!!   - **`floor`** -- what a bare `s = s + x(i)` loop over the same warm array costs, and what each
!!     library reduction costs above it. That ratio is the whole budget: nothing threaded or
!!     otherwise can take a reduction below the memory floor.
!!   - **`phases`** -- where the engine's time actually goes, by differencing three arms that do
!!     progressively more: `pf_count_valid` (one traversal, no allocation, no compaction), an
!!     allocate-and-fill of an n-element `real64` buffer, and `pf_sum` (two traversals, plus that
!!     allocation and the compaction). CLAUDE.md: instrument phases before optimising.
!!   - **`shapes`** -- the four branches of pass one, which are not one loop but two, and whose
!!     costs differ by more than the threading question is worth.
!!   - **`thread`** -- the ceiling on what threading pass two could ever return, measured on a
!!     replica of that pass over a resident buffer, on a thread ladder.
!!
!! **The replica is validated rather than trusted.** CLAUDE.md is explicit that a benchmark
!! replicating library code is untested code, so `--mode=thread` does not merely time its replica:
!! it asserts the replica's SERIAL arm reproduces `pf_mean` and `pf_variance` **bit for bit** on the
!! same input, and then that its THREADED arm reproduces its own serial arm bit for bit. The first
!! assertion is what makes the timings mean anything -- it proves the loop being timed is the loop
!! the library runs, including `STATS_BLOCK`, which is private to the library and necessarily
!! duplicated here. The second is the reproducibility gate the design demands, demonstrated before
!! a single line of the library is threaded.
!!
!! So a `--mode=thread` run that reports `BIT-EXACT: no` is not a slow result, it is an invalid one,
!! and the program says so and exits nonzero rather than printing a table nobody can use.
!!
!! Rules this program follows, all from CLAUDE.md's benchmarking notes: every buffer is fully
!! written before any timer starts (first-touch page faults otherwise land entirely on whichever arm
!! runs first, which has inverted a comparison in this repository before), the reported figure is
!! the best of several rounds, the keep-it-live checksum is accumulated outside every timed region,
!! and no index arithmetic carries a `mod` with a runtime divisor.
!!
!! Not run by `fpm test`, by design. Run it through its wrapper, which asserts the build is
!! optimised -- an -O0 run here would report the memory floor and the library at the same cost and
!! conclude the engine is free:
!!
!! ```bash
!! bench/benchmark_stats.sh
!! bench/benchmark_stats.sh --mode=thread
!! ```
program benchmark_stats

    use parquet, only : pf_sum, pf_mean, pf_variance, pf_moments, pf_count_valid, &
        pf_iqr, pf_median, pf_sigma_clipped_stats, parquet_debug_stats_sorts, &
        parquet_debug_set_stats_quantile_sort_min
    use iso_fortran_env, only : int32, int64, real64, error_unit, output_unit
#ifdef _OPENMP
    use omp_lib, only : omp_get_wtime, omp_get_max_threads, omp_get_num_procs, omp_set_num_threads
#endif

    implicit none

    !> Elements per block of the library's fixed decomposition.
    !!
    !! `STATS_BLOCK` in `src/parquet_stats_core.f90`, which is private to that submodule and so has
    !! to be duplicated here. **Nothing enforces the copy and nothing needs to**: `--mode=thread`'s
    !! bit-exactness assertion against `pf_variance` fails immediately if the two disagree, because
    !! a different block tree is a different sum in the last bits. That is the staleness check.
    integer(int64), parameter :: BLOCK = 128_int64

    integer(int64) :: nrows
    integer :: rounds, nthreads
    character(len=32) :: mode
    real(real64) :: sink

    nrows = 10000000_int64
    rounds = 5
    nthreads = 0
    mode = "floor"
    sink = 0.0_real64

    call parse_args(mode, nrows, rounds, nthreads)

    write (output_unit, "(a)") "=============================================================================="
    write (output_unit, "(a)") "benchmark_stats -- is the parquet_stats moment engine worth threading?"
    write (output_unit, "(a,a)") "  mode        : ", trim(mode)
    write (output_unit, "(a,i0)") "  nrows       : ", nrows
    write (output_unit, "(a,i0)") "  rounds      : ", rounds
    write (output_unit, "(a,i0)") "  block       : ", BLOCK
#ifdef _OPENMP
    write (output_unit, "(a,i0,a,i0,a)") "  openmp      : yes (max_threads=", omp_get_max_threads(), &
        ", num_procs=", omp_get_num_procs(), ")"
#else
    write (output_unit, "(a)") "  openmp      : NO -- --mode=thread can only report its serial arm"
#endif
    write (output_unit, "(a)") "=============================================================================="
    write (output_unit, "(a)") ""

    select case (trim(mode))
    case ("floor");  call mode_floor(nrows, rounds, sink)
    case ("phases"); call mode_phases(nrows, rounds, sink)
    case ("shapes"); call mode_shapes(nrows, rounds, sink)
    case ("thread"); call mode_thread(nrows, rounds, nthreads, sink)
    case ("library"); call mode_library(nrows, rounds, sink)
    case ("iqr");    call mode_iqr(nrows, rounds, sink)
    case ("clip");   call mode_clip(nrows, rounds, sink)
    case default
        write (error_unit, "(a,a)") "benchmark_stats: unknown --mode=", trim(mode)
        write (error_unit, "(a)") "  known modes: floor, phases, shapes, thread, library"
        stop 2
    end select

    write (output_unit, "(a)") ""
    write (output_unit, "(a,es13.6)") "checksum (keeps every arm live; not a result): ", sink

contains

    ! ==========================================================================================
    ! Mode 1 -- floor: the memory bandwidth reference, and every reduction measured against it
    ! ==========================================================================================

    !> Times a bare accumulation loop, then each public reduction, over one warm array.
    !!
    !! The bare loop is the reference and it is allocation-free, which CLAUDE.md insists on: a
    !! "floor" that allocates measures its own first-touch page faults, and a reference the thing
    !! under test can beat is not a reference at all.
    !!
    !! **The row to read is not the slowest one, it is whether the four library rows AGREE.** Every
    !! entry point calls one engine, so `pf_sum` -- which needs nothing pass two computes -- pays
    !! for the four central moments regardless. If these four rows are equal, that is measured
    !! confirmation of it, and it sizes a saving that has nothing to do with threading.
    subroutine mode_floor(n, rounds, sink)
        integer(int64), intent(in) :: n      !! population size.
        integer, intent(in) :: rounds        !! rounds per arm; the best is kept.
        real(real64), intent(inout) :: sink  !! keep-it-live accumulator.
        real(real64), allocatable :: x(:)
        real(real64) :: t_floor, t_intr, t_cnt, t_sum, t_mean, t_var, t_mom
        real(real64) :: s, m, v, sd, se, sk, ku, lo, hi
        integer(int64) :: nv, i, cnt
        integer :: r
        real(real64) :: t0

        call make_values(x, n)
        write (output_unit, "(a)") "Every arm walks the same warm array once. The floor allocates nothing."
        write (output_unit, "(a)") ""

        t_floor = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            s = 0.0_real64
            do i = 1_int64, n
                s = s + x(i)
            end do
            t_floor = min(t_floor, now() - t0)
            sink = sink + s
        end do

        t_intr = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            s = sum(x)
            t_intr = min(t_intr, now() - t0)
            sink = sink + s
        end do

        t_cnt = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_count_valid(x, cnt)
            t_cnt = min(t_cnt, now() - t0)
            sink = sink + real(cnt, real64)
        end do

        t_sum = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_sum(x, s)
            t_sum = min(t_sum, now() - t0)
            sink = sink + s
        end do

        t_mean = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_mean(x, m)
            t_mean = min(t_mean, now() - t0)
            sink = sink + m
        end do

        t_var = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_variance(x, v)
            t_var = min(t_var, now() - t0)
            sink = sink + v
        end do

        t_mom = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_moments(x, n_valid=nv, mean=m, variance=v, stddev=sd, sem=se, &
                skewness=sk, kurtosis=ku, vmin=lo, vmax=hi)
            t_mom = min(t_mom, now() - t0)
            sink = sink + m + v + sd + se + sk + ku + lo + hi + real(nv, real64)
        end do

        write (output_unit, "(a)") "  arm                       ns/elem   x floor   GB/s"
        write (output_unit, "(a)") "  ------------------------------------------------------"
        call row("serial add loop (floor)", t_floor, t_floor, n)
        call row("intrinsic sum()",         t_intr,  t_floor, n)
        call row("pf_count_valid",          t_cnt,   t_floor, n)
        call row("pf_sum",                  t_sum,   t_floor, n)
        call row("pf_mean",                 t_mean,  t_floor, n)
        call row("pf_variance",             t_var,   t_floor, n)
        call row("pf_moments (all nine)",   t_mom,   t_floor, n)
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  If the last four rows agree, every entry point pays for the full two-pass"
        write (output_unit, "(a)") "  engine -- including pf_sum, which uses nothing pass two computes."
    end subroutine mode_floor

    ! ==========================================================================================
    ! Mode 2 -- phases: where the engine's time goes, by differencing three arms
    ! ==========================================================================================

    !> Sizes the engine's three components without putting a timer on a per-element path.
    !!
    !! A `steady_clock` pair costs 20-25 ns, so a per-element phase timer would cost more than the
    !! phase; CLAUDE.md's answer is to difference arms that do progressively more work instead.
    !! Three arms bracket the engine:
    !!
    !!   - `pf_count_valid` -- one traversal, applying the same exclusion rules, allocating nothing
    !!     and compacting nothing. This is the library-shaped one-pass floor.
    !!   - `alloc + fill` -- `allocate(real64(n))` and write every element. This is what pass one's
    !!     compaction buffer costs before any arithmetic, and on a large population it is not small.
    !!   - `pf_sum` -- both traversals, the allocation and the compaction.
    !!
    !! The residual column is `pf_sum` minus the other two, i.e. roughly what pass two costs. It is
    !! a difference of measurements and therefore an estimate, not an attribution -- read it as a
    !! size, not a budget line.
    subroutine mode_phases(n, rounds, sink)
        integer(int64), intent(in) :: n      !! largest population; the sweep divides down from it.
        integer, intent(in) :: rounds        !! rounds per arm.
        real(real64), intent(inout) :: sink  !! keep-it-live accumulator.
        integer(int64) :: sizes(4), m
        integer :: k

        sizes = [n / 1000_int64, n / 100_int64, n / 10_int64, n]
        write (output_unit, "(a)") "  n            count_valid   alloc+fill        pf_sum      residual"
        write (output_unit, "(a)") "                  ns/elem      ns/elem       ns/elem       ns/elem"
        write (output_unit, "(a)") "  -----------------------------------------------------------------"
        do k = 1, 4
            m = sizes(k)
            if (m < BLOCK) cycle
            call phases_one(m, rounds, sink)
        end do
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  residual = pf_sum - count_valid - (alloc+fill): approximately pass two"
        write (output_unit, "(a)") "  plus the compaction store. A difference of measurements, so a size only."
    end subroutine mode_phases

    !> One row of `--mode=phases`.
    subroutine phases_one(n, rounds, sink)
        integer(int64), intent(in) :: n      !! population size for this row.
        integer, intent(in) :: rounds        !! rounds per arm.
        real(real64), intent(inout) :: sink  !! keep-it-live accumulator.
        real(real64), allocatable :: x(:), buf(:)
        real(real64) :: t_cnt, t_all, t_sum, t0, s, resid
        integer(int64) :: cnt, i
        integer :: r

        call make_values(x, n)

        t_cnt = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_count_valid(x, cnt)
            t_cnt = min(t_cnt, now() - t0)
            sink = sink + real(cnt, real64)
        end do

        ! Allocated and freed inside the timed region on purpose: that is what the engine does per
        ! call, so hoisting it would measure a cost the engine does not have.
        t_all = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            allocate(buf(n))
            do i = 1_int64, n
                buf(i) = x(i)
            end do
            t_all = min(t_all, now() - t0)
            sink = sink + buf(1) + buf(n)
            deallocate(buf)
        end do

        t_sum = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_sum(x, s)
            t_sum = min(t_sum, now() - t0)
            sink = sink + s
        end do

        resid = t_sum - t_cnt - t_all
        write (output_unit, "(2x,i10,4(2x,f12.4))") n, ns(t_cnt, n), ns(t_all, n), ns(t_sum, n), ns(resid, n)
    end subroutine phases_one

    ! ==========================================================================================
    ! Mode 3 -- shapes: pass one is two loops, not one
    ! ==========================================================================================

    !> Times `pf_variance` over the four shapes pass one branches on.
    !!
    !! `stats_engine` has a dedicated fast loop for the common case -- unmasked, unweighted,
    !! NaN-skipping -- and a general loop for everything else. Any threading decision has to be
    !! taken knowing how far apart those are: a gap larger than what threading returns means the
    !! shape a caller passes matters more than the thread count, and the guide page should say so
    !! rather than the signature growing an argument.
    subroutine mode_shapes(n, rounds, sink)
        integer(int64), intent(in) :: n      !! population size.
        integer, intent(in) :: rounds        !! rounds per arm.
        real(real64), intent(inout) :: sink  !! keep-it-live accumulator.
        real(real64), allocatable :: x(:), w(:)
        logical, allocatable :: keep(:)
        real(real64) :: t_plain, t_mask, t_wt, t_prop, t0, v
        integer(int64) :: i
        integer :: r

        call make_values(x, n)
        allocate(keep(n), w(n))
        do i = 1_int64, n
            keep(i) = .true.
            w(i) = 1.0_real64 + real(iand(int(i, int32), 7), real64)
        end do
        keep(1_int64) = .false.          ! one null, so the mask is live rather than degenerate
        sink = sink + w(1) + merge(1.0_real64, 0.0_real64, keep(2_int64))

        t_plain = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_variance(x, v)
            t_plain = min(t_plain, now() - t0)
            sink = sink + v
        end do

        t_mask = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_variance(x, v, is_valid=keep)
            t_mask = min(t_mask, now() - t0)
            sink = sink + v
        end do

        t_wt = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_variance(x, v, weights=w)
            t_wt = min(t_wt, now() - t0)
            sink = sink + v
        end do

        t_prop = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_variance(x, v, skipnan=.false.)
            t_prop = min(t_prop, now() - t0)
            sink = sink + v
        end do

        write (output_unit, "(a)") "  shape                     ns/elem   x plain"
        write (output_unit, "(a)") "  ----------------------------------------------"
        call row2("plain (fast loop)",   t_plain, t_plain, n)
        call row2("is_valid=",           t_mask,  t_plain, n)
        call row2("weights=",            t_wt,    t_plain, n)
        call row2("skipnan=.false.",     t_prop,  t_plain, n)
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  Everything but the first row takes pass one's general branch."
    end subroutine mode_shapes

    ! ==========================================================================================
    ! Mode 4 -- thread: the ceiling, and the bit-exactness gate that makes it meaningful
    ! ==========================================================================================

    !> The P5 question: what could threading pass two return, and is it still bit-exact?
    !!
    !! Times a replica of pass two over a resident, already-compacted buffer -- the best case for
    !! threading, since the replica pays none of pass one's exclusion or compaction -- serially and
    !! then on a thread ladder. Two assertions come first, and a failure of either stops the run:
    !!
    !!   1. the serial replica reproduces `pf_mean` and `pf_variance` **bit for bit**, which is what
    !!      proves it is the library's pass two rather than something that resembles it;
    !!   2. every threaded arm reproduces the serial replica **bit for bit**, which is the
    !!      reproducibility contract, demonstrated before the library is touched.
    subroutine mode_thread(n, rounds, want_threads, sink)
        integer(int64), intent(in) :: n      !! population size.
        integer, intent(in) :: rounds        !! rounds per arm.
        integer, intent(in) :: want_threads  !! 0 = ladder to the machine maximum; else this only.
        real(real64), intent(inout) :: sink  !! keep-it-live accumulator.
        real(real64), allocatable :: x(:)
        real(real64) :: mean_lib, var_lib, mean_rep, var_rep, mean_t, var_t, hi_rep, hi_t
        real(real64) :: t_ser, t_par, t0
        integer :: ladder(8), nl, k, r, t
        logical :: exact_lib, exact_par, all_ok

        call make_values(x, n)
        call pf_mean(x, mean_lib)
        call pf_variance(x, var_lib)
        call replica_moments(x, 1, mean_rep, var_rep, hi_rep)
        exact_lib = (mean_rep == mean_lib) .and. (var_rep == var_lib)

        write (output_unit, "(a)") "Gate 1 -- the replica IS the library's pass two:"
        write (output_unit, "(a,es24.17)") "  pf_mean          : ", mean_lib
        write (output_unit, "(a,es24.17)") "  replica mean     : ", mean_rep
        write (output_unit, "(a,es24.17)") "  pf_variance      : ", var_lib
        write (output_unit, "(a,es24.17)") "  replica variance : ", var_rep
        if (exact_lib) then
            write (output_unit, "(a)") "  BIT-EXACT: yes"
        else
            write (output_unit, "(a)") "  BIT-EXACT: NO"
            write (error_unit, "(a)") ""
            write (error_unit, "(a)") "benchmark_stats: the replica does not reproduce the library bit for bit."
            write (error_unit, "(a)") "  Every timing below would be of a loop the library does not run, so none"
            write (error_unit, "(a)") "  is reported. The usual cause is BLOCK here having drifted from"
            write (error_unit, "(a)") "  STATS_BLOCK in src/parquet_stats_core.f90; the next is a change to pass"
            write (error_unit, "(a)") "  two's re-centring. Fix the replica, do not relax the comparison."
            stop 1
        end if
        write (output_unit, "(a)") ""

        t_ser = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call replica_moments(x, 1, mean_rep, var_rep, hi_rep)
            t_ser = min(t_ser, now() - t0)
            sink = sink + mean_rep + var_rep + hi_rep
        end do

        call thread_ladder(want_threads, ladder, nl)
        write (output_unit, "(a)") "Gate 2 -- every thread count returns the serial answer, bit for bit:"
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  threads   ns/elem   speedup   bit-exact"
        write (output_unit, "(a)") "  ------------------------------------------"
        all_ok = .true.
        do k = 1, nl
            t = ladder(k)
            t_par = huge(0.0_real64)
            do r = 1, rounds
                t0 = now()
                call replica_moments(x, t, mean_t, var_t, hi_t)
                t_par = min(t_par, now() - t0)
                sink = sink + mean_t + var_t + hi_t
            end do
            exact_par = (mean_t == mean_rep) .and. (var_t == var_rep) .and. (hi_t == hi_rep)
            all_ok = all_ok .and. exact_par
            write (output_unit, "(2x,i7,2x,f9.4,2x,f8.2,4x,a)") t, ns(t_par, n), t_ser / t_par, &
                trim(merge("yes", "NO ", exact_par))
        end do
        write (output_unit, "(a)") ""
        write (output_unit, "(a,f9.4,a)") "  serial replica: ", ns(t_ser, n), " ns/elem (pass two alone, no compaction)"
        if (.not. all_ok) then
            write (error_unit, "(a)") "benchmark_stats: a threaded arm did not reproduce the serial answer."
            write (error_unit, "(a)") "  The block decomposition is supposed to make that impossible; the"
            write (error_unit, "(a)") "  speedups above are worthless until it is explained."
            stop 1
        end if
    end subroutine mode_thread

    !> Pass two of `stats_engine`, replicated over a clean resident population.
    !!
    !! Deliberately identical to `src/parquet_stats_core.f90` -- the same block boundaries, the same
    !! `pair_reduce` tree, the same re-centring on `mu + delta` -- because a replica that is merely
    !! similar measures nothing. It is restricted to the unmasked, unweighted, NaN-free case, which
    !! is the only one whose answer can be checked against the library without also replicating
    !! pass one's exclusion rules.
    !!
    !! `threads = 1` runs the identical code with no OpenMP region at all, so the serial arm is not
    !! "the parallel arm on one thread"; that distinction is what makes gate 1 meaningful.
    subroutine replica_moments(x, threads, mean, variance, hi_moments)
        real(real64), intent(in) :: x(:)      !! the population; no nulls, no NaNs, unit weights.
        integer, intent(in) :: threads        !! 1 runs serially; more opens a region over blocks.
        real(real64), intent(out) :: mean     !! the refined mean, `mu + delta`.
        real(real64), intent(out) :: variance !! the sample variance, ddof = 1.
        real(real64), intent(out) :: hi_moments
        !! `q3(1) + q4(1)`, the raw third and fourth accumulations. Nothing reads them for a
        !! result, and that is exactly why they are returned: without an observer the compiler is
        !! free to delete their two lines from the inner loop, which would leave the replica timing
        !! a strictly cheaper loop than the library runs while gate 1 -- which checks only the mean
        !! and the variance -- still reported bit-exact.
        real(real64), allocatable :: px(:), q1(:), q2(:), q3(:), q4(:)
        real(real64) :: mu, delta, wsum, m2, s1, s2, s3, s4, d, dd
        integer(int64) :: n, nb, j, i, lo, hi

        n = size(x, kind=int64)
        nb = (n + BLOCK - 1_int64) / BLOCK
        allocate(px(nb), q1(nb), q2(nb), q3(nb), q4(nb))

        do j = 1_int64, nb
            lo = (j - 1_int64) * BLOCK + 1_int64
            hi = min(j * BLOCK, n)
            s1 = 0.0_real64
            do i = lo, hi
                s1 = s1 + x(i)
            end do
            px(j) = s1
        end do
        call replica_pair_reduce(px, nb)
        wsum = real(n, real64)
        mu = px(1) / wsum

        if (threads > 1) then
#ifdef _OPENMP
            call omp_set_num_threads(threads)
            !$omp parallel do default(shared) private(j, i, lo, hi, s1, s2, s3, s4, d, dd) schedule(static)
            do j = 1_int64, nb
                lo = (j - 1_int64) * BLOCK + 1_int64
                hi = min(j * BLOCK, n)
                s1 = 0.0_real64
                s2 = 0.0_real64
                s3 = 0.0_real64
                s4 = 0.0_real64
                do i = lo, hi
                    d = x(i) - mu
                    dd = d * d
                    s1 = s1 + d
                    s2 = s2 + dd
                    s3 = s3 + dd * d
                    s4 = s4 + dd * dd
                end do
                q1(j) = s1
                q2(j) = s2
                q3(j) = s3
                q4(j) = s4
            end do
            !$omp end parallel do
#endif
        else
            do j = 1_int64, nb
                lo = (j - 1_int64) * BLOCK + 1_int64
                hi = min(j * BLOCK, n)
                s1 = 0.0_real64
                s2 = 0.0_real64
                s3 = 0.0_real64
                s4 = 0.0_real64
                do i = lo, hi
                    d = x(i) - mu
                    dd = d * d
                    s1 = s1 + d
                    s2 = s2 + dd
                    s3 = s3 + dd * d
                    s4 = s4 + dd * dd
                end do
                q1(j) = s1
                q2(j) = s2
                q3(j) = s3
                q4(j) = s4
            end do
        end if

        call replica_pair_reduce(q1, nb)
        call replica_pair_reduce(q2, nb)
        call replica_pair_reduce(q3, nb)
        call replica_pair_reduce(q4, nb)
        delta = q1(1) / wsum
        mean = mu + delta
        m2 = q2(1) - delta * delta * wsum
        ! stats_var's own arithmetic for ddof = 1, reliability weights, w_sum = w_sq = n.
        variance = m2 / (wsum - 1.0_real64 * wsum / wsum)
        hi_moments = q3(1) + q4(1)
    end subroutine replica_moments

    !> `pair_reduce` from `src/parquet_stats_core.f90`, replicated verbatim.
    pure subroutine replica_pair_reduce(a, n)
        real(real64), intent(inout) :: a(:) !! the per-block partials; overwritten.
        integer(int64), intent(in) :: n     !! how many of them are live.
        integer(int64) :: m, k, j

        m = n
        do while (m > 1_int64)
            k = 0_int64
            do j = 1_int64, m - 1_int64, 2_int64
                k = k + 1_int64
                a(k) = a(j) + a(j + 1_int64)
            end do
            if (mod(m, 2_int64) == 1_int64) then
                k = k + 1_int64
                a(k) = a(m)
            end if
            m = k
        end do
    end subroutine replica_pair_reduce

    ! ==========================================================================================
    ! Mode 5 -- library: the shipped procedure, on a thread ladder
    ! ==========================================================================================

    !> What threading actually returns on a whole `pf_variance` call, rather than on pass two alone.
    !!
    !! `--mode=thread` measures a replica over an already-compacted buffer and so reports a
    !! CEILING: it pays none of pass one's exclusion, compaction or allocation, all of which stay
    !! serial. This mode measures the real procedure through its public `threads=` argument, so the
    !! speedup here is what a caller sees. Expect it to be well below the ceiling, and read the two
    !! together -- the gap between them is the serial half of the call.
    !!
    !! Every arm asserts the answer is bit-identical to the serial one. That is the contract, and
    !! checking it on the shipped procedure rather than on a replica is the point of this mode.
    subroutine mode_library(n, rounds, sink)
        integer(int64), intent(in) :: n      !! population size.
        integer, intent(in) :: rounds        !! rounds per arm.
        real(real64), intent(inout) :: sink  !! keep-it-live accumulator.
        real(real64), allocatable :: x(:)
        real(real64) :: v1, vt, t1, tt, t0, t_auto
        integer :: ladder(8), nl, k, r, t
        logical :: all_ok, exact

        call make_values(x, n)
        call pf_variance(x, v1, threads=1)

        t1 = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_variance(x, v1, threads=1)
            t1 = min(t1, now() - t0)
            sink = sink + v1
        end do

        t_auto = huge(0.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_variance(x, vt)
            t_auto = min(t_auto, now() - t0)
            sink = sink + vt
        end do

        call thread_ladder(0, ladder, nl)
        write (output_unit, "(a)") "  pf_variance over the whole call, through the public threads= argument."
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  threads   ns/elem   speedup   bit-exact vs threads=1"
        write (output_unit, "(a)") "  ---------------------------------------------------"
        all_ok = .true.
        do k = 1, nl
            t = ladder(k)
            tt = huge(0.0_real64)
            do r = 1, rounds
                t0 = now()
                call pf_variance(x, vt, threads=t)
                tt = min(tt, now() - t0)
                sink = sink + vt
            end do
            exact = (vt == v1)
            all_ok = all_ok .and. exact
            write (output_unit, "(2x,i7,2x,f9.4,2x,f8.2,6x,a)") t, ns(tt, n), t1 / tt, &
                trim(merge("yes", "NO ", exact))
        end do
        write (output_unit, "(a)") ""
        write (output_unit, "(2x,a,f9.4,a,f6.2,a)") "automatic (no threads=): ", ns(t_auto, n), &
            " ns/elem, ", t1 / t_auto, "x"
        if (.not. all_ok) then
            write (error_unit, "(a)") "benchmark_stats: a threaded pf_variance did not return the serial bits."
            write (error_unit, "(a)") "  That is the module's central contract; the speedups above are moot."
            stop 1
        end if
    end subroutine mode_library

    !> P6-1: does `pf_iqr` do better by SELECTING its two order statistics or by SORTING once?
    !!
    !! It asks for two probabilities and `QUANTILE_SORT_MIN` is 4, so it currently selects -- four
    !! `pf_nth_element` calls against one `pf_argsort`. That constant was placed to make its own
    !! effect visible rather than measured, which is the open question this mode answers.
    !!
    !! The two arms are the SHIPPED procedure with the sort threshold pushed either side of 2, via
    !! the debug override, so both are the real code path and neither is a replica. The answers are
    !! compared bit for bit before any timing is reported: two routes to one quantile that disagree
    !! would make the comparison meaningless whichever won.
    subroutine mode_iqr(n, rounds, sink)
        integer(int64), intent(in) :: n      !! population size.
        integer, intent(in) :: rounds        !! rounds per arm.
        real(real64), intent(inout) :: sink  !! keep-it-live accumulator.
        real(real64), allocatable :: x(:)
        real(real64) :: v_sel, v_sort, t_sel, t_sort, t0
        integer(int64) :: sizes(4), m
        integer :: r, k, np

        sizes = [1000_int64, 100000_int64, 1000000_int64, n]
        write (output_unit, "(a)") "  pf_iqr: SELECT (four pf_nth_element) against SORT (one pf_argsort)."
        write (output_unit, "(a)") "  Both arms are the shipped procedure; only the sort threshold moves."
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  ONE probe (pf_median) and TWO (pf_iqr): the constant has to serve both."
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  probes          n      select ms      sort ms   sort/select   same answer"
        write (output_unit, "(a)") "  --------------------------------------------------------------------------"
        do np = 1, 2
            do k = 1, 4
                m = sizes(k)
                if (m > n) cycle
                call make_values(x, m)

                ! `should_select` is `nprobs < floor_n`, so a floor above the probe count SELECTS
                ! and a floor of 1 always SORTS. Both arms are the shipped procedure.
                call parquet_debug_set_stats_quantile_sort_min(99999_int64)
                call one_probe_or_two(x(1:m), np, v_sel)
                t_sel = huge(0.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call one_probe_or_two(x(1:m), np, v_sel)
                    t_sel = min(t_sel, now() - t0)
                    sink = sink + v_sel
                end do

                call parquet_debug_set_stats_quantile_sort_min(1_int64)
                call one_probe_or_two(x(1:m), np, v_sort)
                t_sort = huge(0.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call one_probe_or_two(x(1:m), np, v_sort)
                    t_sort = min(t_sort, now() - t0)
                    sink = sink + v_sort
                end do
                call parquet_debug_set_stats_quantile_sort_min(-1_int64)

                write (output_unit, "(2x,i6,2x,i9,2x,f11.4,2x,f11.4,2x,f12.2,3x,a)") np, m, &
                    t_sel * 1000.0_real64, t_sort * 1000.0_real64, t_sort / t_sel, &
                    trim(merge("yes", "NO ", v_sel == v_sort))
                if (v_sel /= v_sort) then
                    write (error_unit, "(a)") "benchmark_stats: the two routes disagree; the timings above are moot."
                    stop 1
                end if
            end do
        end do
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  sort/select > 1 means selecting is the faster choice, i.e. the shipped default is right."
    end subroutine mode_iqr

    !> `pf_median` for one probe, `pf_iqr` for two -- the two probe counts the constant serves.
    subroutine one_probe_or_two(x, nprobs, res)
        real(real64), intent(in) :: x(:)      !! the population.
        integer, intent(in) :: nprobs         !! 1 or 2.
        real(real64), intent(out) :: res      !! the answer, kept so the call cannot be elided.
        if (nprobs == 1) then
            call pf_median(x, res)
        else
            call pf_iqr(x, res)
        end if
    end subroutine one_probe_or_two

    !> P8-3: what does `stdfunc="mad_std"` cost, against the default `"std"`?
    !!
    !! The clip runs on ONE ordering of the values, whatever the round count -- but `"mad_std"`
    !! orders the DEVIATIONS once per round as well, because those are a different population whose
    !! order the values' order does not imply. That is honest and asserted, and it puts the
    !! `mad_std` path back at O(k n log n), which is the shape the interval property was chosen to
    !! avoid. This mode sizes it, so the decision to keep it or to look for a rank-selection form
    !! rests on a number.
    subroutine mode_clip(n, rounds, sink)
        integer(int64), intent(in) :: n      !! population size.
        integer, intent(in) :: rounds        !! rounds per arm.
        real(real64), intent(inout) :: sink  !! keep-it-live accumulator.
        real(real64), allocatable :: x(:)
        real(real64) :: m1, md1, sd1, m2, md2, sd2, t_std, t_mad, t0, sg
        integer(int64) :: sizes(4), m, s_std, s_mad, before
        integer :: r, k, np

        sizes = [1000_int64, 100000_int64, 1000000_int64, n]
        write (output_unit, "(a)") '  pf_sigma_clipped_stats: stdfunc="std" against stdfunc="mad_std".'
        write (output_unit, "(a)") "  `sorts` is parquet_debug_stats_sorts() over one call: the orderings each arm pays for."
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  sigma=3 CONVERGES in one round on this population; sigma=0.5 keeps clipping, which"
        write (output_unit, "(a)") "  is the only case the O(k n log n) question is about."
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  sigma          n        std ms     mad_std ms       ratio   sorts std   sorts mad"
        write (output_unit, "(a)") "  -------------------------------------------------------------------------------"
        do np = 1, 2
            sg = 3.0_real64
            if (np == 2) sg = 0.5_real64
            do k = 1, 4
                m = sizes(k)
                if (m > n) cycle
                call make_values(x, m)

                call pf_sigma_clipped_stats(x(1:m), m1, md1, sd1, sigma=sg)
                t_std = huge(0.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call pf_sigma_clipped_stats(x(1:m), m1, md1, sd1, sigma=sg)
                    t_std = min(t_std, now() - t0)
                    sink = sink + m1
                end do
                before = parquet_debug_stats_sorts()
                call pf_sigma_clipped_stats(x(1:m), m1, md1, sd1, sigma=sg)
                s_std = parquet_debug_stats_sorts() - before

                call pf_sigma_clipped_stats(x(1:m), m2, md2, sd2, sigma=sg, stdfunc="mad_std")
                t_mad = huge(0.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call pf_sigma_clipped_stats(x(1:m), m2, md2, sd2, sigma=sg, stdfunc="mad_std")
                    t_mad = min(t_mad, now() - t0)
                    sink = sink + m2
                end do
                before = parquet_debug_stats_sorts()
                call pf_sigma_clipped_stats(x(1:m), m2, md2, sd2, sigma=sg, stdfunc="mad_std")
                s_mad = parquet_debug_stats_sorts() - before

                write (output_unit, "(2x,f5.1,2x,i9,2x,f12.4,2x,f13.4,2x,f11.2,2x,i11,2x,i11)") &
                    sg, m, t_std * 1000.0_real64, t_mad * 1000.0_real64, t_mad / t_std, &
                    s_std, s_mad
            end do
        end do
        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "  A `sorts mad` that grows with the round count is the O(k n log n) shape; one that stays at"
        write (output_unit, "(a)") "  `sorts std` would mean the deviations are not being re-ordered and this question is closed."
    end subroutine mode_clip

    ! ==========================================================================================
    ! Shared helpers
    ! ==========================================================================================

    !> Builds the population, and WRITES every element before any timer starts.
    !!
    !! Values are spread over several orders of magnitude and both signs so that no arm can be
    !! helped by a degenerate distribution, and are produced by a cheap recurrence rather than a
    !! random generator, which would put an unrelated library on the measurement.
    subroutine make_values(x, n)
        real(real64), allocatable, intent(out) :: x(:) !! the population.
        integer(int64), intent(in) :: n                !! how many elements.
        integer(int64) :: i
        real(real64) :: v

        allocate(x(n))
        v = 0.5_real64
        do i = 1_int64, n
            v = 4.0_real64 * v * (1.0_real64 - v)
            if (v <= 0.0_real64 .or. v >= 1.0_real64) v = 0.3141592653589793_real64
            x(i) = (v - 0.5_real64) * 1000.0_real64
        end do
    end subroutine make_values

    !> The thread ladder: 1, then doubling to the machine's maximum, or one explicit count.
    subroutine thread_ladder(want, ladder, nl)
        integer, intent(in) :: want       !! 0 for the ladder; otherwise this one count.
        integer, intent(out) :: ladder(:) !! the counts to time.
        integer, intent(out) :: nl        !! how many are live.
        integer :: hi, t

        if (want > 0) then
            ladder(1) = want
            nl = 1
            return
        end if
#ifdef _OPENMP
        hi = min(omp_get_max_threads(), omp_get_num_procs())
#else
        hi = 1
#endif
        nl = 0
        t = 1
        do while (t <= hi .and. nl < size(ladder))
            nl = nl + 1
            ladder(nl) = t
            t = t * 2
        end do
        if (nl < size(ladder)) then
            if (ladder(nl) /= hi) then
                nl = nl + 1
                ladder(nl) = hi
            end if
        end if
    end subroutine thread_ladder

    !> Nanoseconds per element.
    pure function ns(t, n) result(res)
        real(real64), intent(in) :: t   !! seconds for the whole arm.
        integer(int64), intent(in) :: n !! elements walked.
        real(real64) :: res             !! nanoseconds per element.
        res = t * 1.0e9_real64 / real(n, real64)
    end function ns

    !> One `--mode=floor` row: cost per element, ratio to the floor, and effective read bandwidth.
    subroutine row(label, t, t_ref, n)
        character(len=*), intent(in) :: label !! the arm's name.
        real(real64), intent(in) :: t         !! its best time, seconds.
        real(real64), intent(in) :: t_ref     !! the floor's best time, seconds.
        integer(int64), intent(in) :: n       !! elements walked.
        write (output_unit, "(2x,a24,f9.4,2x,f8.2,2x,f7.2)") label, ns(t, n), t / t_ref, &
            real(n, real64) * 8.0_real64 / t / 1.0e9_real64
    end subroutine row

    !> One `--mode=shapes` row: cost per element and ratio to the plain shape.
    subroutine row2(label, t, t_ref, n)
        character(len=*), intent(in) :: label !! the shape's name.
        real(real64), intent(in) :: t         !! its best time, seconds.
        real(real64), intent(in) :: t_ref     !! the plain shape's best time, seconds.
        integer(int64), intent(in) :: n       !! elements walked.
        write (output_unit, "(2x,a24,f9.4,2x,f8.2)") label, ns(t, n), t / t_ref
    end subroutine row2

    !> Wall-clock seconds since an arbitrary origin.
    function now() result(t)
        real(real64) :: t !! seconds; only differences are meaningful.
#ifndef _OPENMP
        integer(int64) :: c, r
#endif
        !
#ifdef _OPENMP
        t = omp_get_wtime()
#else
        call system_clock(count=c, count_rate=r)
        t = real(c, real64) / real(r, real64)
#endif
    end function now

    !> Reads `--mode=`, `--nrows=`, `--rounds=` and `--threads=`.
    subroutine parse_args(mode, nrows, rounds, nthreads)
        character(len=*), intent(inout) :: mode  !! which mode to run.
        integer(int64), intent(inout) :: nrows   !! population size.
        integer, intent(inout) :: rounds         !! rounds per arm.
        integer, intent(inout) :: nthreads       !! 0 for the ladder.
        character(len=256) :: arg
        integer :: k, ios
        integer(int64) :: v

        do k = 1, command_argument_count()
            call get_command_argument(k, arg)
            if (arg(1:7) == "--mode=") then
                mode = trim(arg(8:))
            else if (arg(1:8) == "--nrows=") then
                read (arg(9:), *, iostat=ios) v
                if (ios == 0 .and. v > 0_int64) nrows = v
            else if (arg(1:9) == "--rounds=") then
                read (arg(10:), *, iostat=ios) v
                if (ios == 0 .and. v > 0_int64) rounds = int(v)
            else if (arg(1:10) == "--threads=") then
                read (arg(11:), *, iostat=ios) v
                if (ios == 0 .and. v > 0_int64) nthreads = int(v)
            end if
        end do
    end subroutine parse_args

end program benchmark_stats
