!> TEMPORARY probe: how far does an LSD radix over (key, row) pairs actually parallelise on this
!! machine, and which decomposition scales best?
!!
!! Not part of the library, and it deliberately does NOT call the library: it reimplements the
!! engine's inner loop over synthetic data so that the scaling question can be asked without first
!! threading `src/parquet_sorting_engine.f90`. Every arm sorts the same input and every parallel arm
!! is checked against the serial one for an exact match, so a figure is only printed for an arm that
!! computed the right answer.
!!
!! Designs compared -- see feature_sort_parallel.md:
!!   A  LSD, per-thread-per-bucket offsets, one synchronised scatter per digit.
!!   B  one parallel MSD split on the top digit, then the remaining digits per bucket, threads
!!      taking whole buckets independently with no further synchronisation.
!!   B16  the same, splitting on TWO digits (65536 buckets), to test whether the bucket count is
!!      the scaling ceiling. It is not -- it is 5x worse at 128 threads.
!!
!! Key shapes (`--narrow`, `--lowcard`, `--prefix`, default wide). The last two are NOT the same
!! test: `lowcard` has few distinct keys overall, while `prefix` has plenty of variation sitting
!! below a split digit that carries only four values -- the shape a string column with a common
!! stem produces, and Design B's predicted worst case.
!!
!! **Two rules this file has already broken once each, so do not undo either:**
!!   1. **Ping-pong by PARITY, never by swapping the buffers.** A swap is O(n) per pass, the engine
!!      does not do it (`move_alloc`), and Design B never called it -- so it was charged to the
!!      baseline and to A while B escaped, inflating every ratio here. See radix_serial's header
!!      and feature_sort_parallel.md section 2.4.
!!   2. **Rebuild the per-thread histogram every pass.** Reusing one across passes gives a silently
!!      wrong permutation from two threads upward. See lsd_pass_par.
!!
!! Kept in the repository rather than deleted with the investigation: the scaling questions have to
!! be re-askable whenever the engine or the machine changes.
program probe_radix_parallel
    use iso_fortran_env, only : int32, int64, real64, output_unit
    use omp_lib
    implicit none
    !
    integer(int64) :: n            !! rows.
    integer :: reps                !! timed repetitions; the best is kept.
    integer :: maxthreads          !! highest thread count on the ladder.
    character(len=8) :: shape_in   !! "wide", "narrow" or "lowcard".
    logical :: firsttouch          !! spread the buffers' pages across NUMA nodes before filling.
    integer :: ncols               !! >0 runs the NESTED sweep instead of the single-sort ladders.
    integer(int64), allocatable :: k0(:), r0(:)      !! the pristine input.
    integer(int64), allocatable :: ka(:), ra(:)      !! the working pair.
    integer(int64), allocatable :: kb(:), rb(:)      !! the scatter partner.
    integer(int64), allocatable :: kref(:), rref(:)  !! the serial answer.
    integer(int64), allocatable :: vals(:)           !! raw key VALUES, before the image transform.
    integer(int32), allocatable :: p32(:)            !! the narrowed permutation, as narrow_perm makes.
    integer :: tl(14), nt, i, got
    real(real64) :: tser, tpar, tpar2, tfull
    !
    call read_args(n, reps, maxthreads, shape_in, firsttouch, ncols)
    call omp_set_dynamic(.false.)
    if (ncols > 0) then
        call nested_sweep(n, ncols, reps, maxthreads, shape_in)
        stop
    end if
    allocate(k0(n), r0(n), ka(n), ra(n), kb(n), rb(n), kref(n), rref(n), vals(n), p32(n))
    ! Linux places a page on the node of the thread that FIRST TOUCHES it. Allocating and filling
    ! serially therefore puts every buffer on one NUMA node, and on a two-socket machine every
    ! thread on the far socket then reads across the interconnect. Touching the buffers from many
    ! threads first spreads them, which is the in-code stand-in for `numactl --interleave=all`
    ! (not installed on this machine). This is the only difference the flag makes.
    if (firsttouch) call spread_pages(k0, r0, ka, ra, kb, rb, n)
    call make_input(k0, r0, n, shape_in)
    vals(1:n) = k0(1:n)
    !
    write (output_unit, '(a)') "=============================================================="
    write (output_unit, '(a)') "probe_radix_parallel -- LSD radix over (key,row), scaling ladder"
    write (output_unit, '(a)') "=============================================================="
    write (output_unit, '(a,i0,a,i0,a,l1)') "  n = ", n, "   reps = ", reps, &
        "   key shape = " // trim(shape_in) // "   first-touch spread = ", firsttouch
    write (output_unit, '(a,i0,a,i0)') "  omp_get_max_threads() = ", omp_get_max_threads(), &
        "   omp_get_num_procs() = ", omp_get_num_procs()
    write (output_unit, '(a,f6.1,a)') "  working set = ", real(n, real64) * 32.0_real64 / 1048576.0_real64, " MiB"
    write (output_unit, '(a)') ""
    !
    ! The serial reference, and the baseline every ratio below is taken against.
    call time_arm(k0, r0, ka, ra, kb, rb, n, reps, 0, 0, tser)
    kref = ka
    rref = ra
    write (output_unit, '(a)') "  design  threads     ms      ns/elem   speedup  per-core   check"
    write (output_unit, '(a,i7,f9.3,f11.3,f10.2,f10.3,a)') "  serial ", 1, tser * 1.0e3_real64, &
        tser * 1.0e9_real64 / real(n, real64), 1.0_real64, 1.0_real64, "   --"
    ! **The tie-down.** A benchmark that reimplements library code is untested code, so one of its
    ! rows must reproduce a figure measured independently on the real thing or it is three confident
    ! numbers about nothing. The serial arm above is the histogram plus the scatter passes, which
    ! feature_sort_parallel.md section 3 measures inside the engine itself at 2.22 + 12.56 = 14.78
    ! ns/element for a full-range int64 key at n = 1e6 on machine B (its 16.59 radix total less the
    ! 1.23 tier split and 0.59 image build, neither of which this probe does). A large gap means
    ! this probe is measuring something the engine does not -- which is exactly what happened before
    ! the buffer swap was removed, when this arm read 26.19 against that 14.78. See section 2.4.
    ! Only at the size and shape the reference figure was taken at: the serial arm's cost per element
    ! changes with the working set (610 MiB at n = 2e7 against 30 MiB at n = 1e6), so quoting the
    ! comparison at another n would be the kind of confidently wrong number this check exists to stop.
    if (trim(shape_in) == "wide" .and. n == 1000000_int64) write (output_unit, '(a,f7.2,a)') &
        "  tie-down: on machine B (ifx, n = 1e6, wide) the engine's own histogram + scatter cost " // &
        "is 14.78 ns/elem; this arm reads ", tser * 1.0e9_real64 / real(n, real64), &
        " -- a large gap invalidates every ratio below."
    !
    tl = [1, 2, 4, 8, 16, 32, 64, 96, 128, 192, 256, 384, 512, 768]
    do i = 1, size(tl)
        nt = tl(i)
        if (nt > maxthreads) cycle
        got = team_size(nt)
        if (got /= nt) cycle
        call time_arm(k0, r0, ka, ra, kb, rb, n, reps, nt, 1, tpar)
        call report("A     ", nt, tpar, tser, n, ka, ra, kref, rref)
    end do
    write (output_unit, '(a)') ""
    do i = 1, size(tl)
        nt = tl(i)
        if (nt > maxthreads) cycle
        got = team_size(nt)
        if (got /= nt) cycle
        call time_arm(k0, r0, ka, ra, kb, rb, n, reps, nt, 2, tpar)
        call report("B     ", nt, tpar, tser, n, ka, ra, kref, rref)
    end do
    write (output_unit, '(a)') ""
    do i = 1, size(tl)
        nt = tl(i)
        if (nt > maxthreads) cycle
        got = team_size(nt)
        if (got /= nt) cycle
        call time_arm(k0, r0, ka, ra, kb, rb, n, reps, nt, 3, tpar)
        call report("B16   ", nt, tpar, tser, n, ka, ra, kref, rref)
    end do
    !
    ! Whole-path arms: the image build and the int32 narrowing -- the phases that sit either side of
    ! the radix and make up the Amdahl tail of section 4 -- with design B between them. The two
    ! differ ONLY in whether those two phases are threaded, so the gap between them is exactly what
    ! composing the tail with B is worth.
    write (output_unit, '(a)') ""
    call time_path(vals, ka, ra, kb, rb, p32, n, reps, 1, .false., tfull)
    write (output_unit, '(a,i7,f9.3,f11.3,f10.2,a)') "  path0 ", 1, tfull * 1.0e3_real64, &
        tfull * 1.0e9_real64 / real(n, real64), 1.0_real64, "   serial reference"
    do i = 1, size(tl)
        nt = tl(i)
        if (nt > maxthreads) cycle
        got = team_size(nt)
        if (got /= nt) cycle
        call time_path(vals, ka, ra, kb, rb, p32, n, reps, nt, .false., tpar)
        call report_path("pathS ", nt, tpar, tfull, n)
        call time_path(vals, ka, ra, kb, rb, p32, n, reps, nt, .true., tpar2)
        call report_path("pathP ", nt, tpar2, tfull, n)
    end do
    !
contains
    !
    subroutine read_args(n, reps, maxthreads, shape_in, firsttouch, ncols)
        integer(int64), intent(out) :: n
        integer, intent(out) :: reps, maxthreads
        character(len=*), intent(out) :: shape_in
        logical, intent(out) :: firsttouch
        integer, intent(out) :: ncols
        character(len=64) :: a
        integer :: i
        !
        n = 1000000_int64
        reps = 5
        maxthreads = 64
        shape_in = "wide"
        firsttouch = .false.
        ncols = 0
        do i = 1, command_argument_count()
            call get_command_argument(i, a)
            if (a(1:8) == "--narrow") then
                shape_in = "narrow"
            else if (a(1:9) == "--lowcard") then
                shape_in = "lowcard"
            else if (a(1:8) == "--prefix") then
                shape_in = "prefix"
            else if (a(1:12) == "--firsttouch") then
                firsttouch = .true.
            else if (a(1:7) == "--cols=") then
                read (a(8:), *) ncols
            else if (a(1:4) == "--n=") then
                read (a(5:), *) n
            else if (a(1:7) == "--reps=") then
                read (a(8:), *) reps
            else if (a(1:10) == "--threads=") then
                read (a(11:), *) maxthreads
            end if
        end do
    end subroutine read_args
    !
    !> The NESTED regime: `nc` independent columns of `n` rows each, an outer parallel loop over the
    !! columns and design B inside each one. Sweeps how a fixed thread budget should be SPLIT between
    !! the two levels -- the question single-sort ladders cannot answer.
    !!
    !! Realistic in shape: a table sort hands every column the same row count, the columns are fully
    !! independent, and the outer loop has no synchronisation at all where the inner one has a barrier
    !! pair plus a dynamically scheduled bucket loop.
    subroutine nested_sweep(n, nc, reps, budget, shape_in)
        integer(int64), intent(in) :: n     !! rows PER COLUMN.
        integer, intent(in) :: nc           !! columns.
        integer, intent(in) :: reps         !! timed repetitions; the best is kept.
        integer, intent(in) :: budget       !! total threads to spend across both levels.
        character(len=*), intent(in) :: shape_in
        !
        integer(int64), allocatable :: g0(:,:), h0(:,:)     !! pristine keys and rows, per column.
        integer(int64), allocatable :: ga(:,:), ha(:,:)     !! the working pair.
        integer(int64), allocatable :: gb(:,:), hb(:,:)     !! the scatter partner.
        integer(int64), allocatable :: gref(:,:), href(:,:) !! the serial answer.
        integer :: ol(6), o, outer, inner, i, c
        !> The column count, re-read from the ALLOCATED array rather than trusted from the dummy, and
        !! used in place of `nc` in every expression below.
        !!
        !! **`volatile` is load-bearing here and is not decoration.** gfortran 14.2.1 at -O2/-O3
        !! derives a bogus value range for `nc` in this procedure and folds it to 1 inside every
        !! expression -- while still printing it correctly, allocating the right shape and sorting the
        !! right number of columns. The damage was entirely silent: every ns/element figure came out
        !! divided by `n` instead of `n*nc`, and `min(ol(i), nc)` collapsed to 1 for every entry, so
        !! the outer ladder below skipped five of its six configurations and the sweep reduced to a
        !! single data point that still looked like a valid run. Reading the count back from the
        !! array descriptor defeats it, and `volatile` stops the same range propagation folding the
        !! read itself. ifx 2026.1.1 is unaffected, as is gfortran at -O0/-O1 or with -fno-tree-vrp.
        !! See feature_sort_report.md section 7.
        integer, volatile :: ncol
        real(real64) :: t0, t1, best, tbase
        integer :: rep
        logical :: ok
        !
        allocate(g0(n, nc), h0(n, nc), ga(n, nc), ha(n, nc), gb(n, nc), hb(n, nc))
        allocate(gref(n, nc), href(n, nc))
        ncol = int(size(g0, 2))
        if (ncol /= nc) write (output_unit, '(a,i0,a,i0,a)') &
            "  *** COMPILER DEFECT: column count reads ", nc, " as a dummy argument but ", ncol, &
            " from the allocated array. Using the latter; rebuild with -fno-tree-vrp."
        do c = 1, ncol
            call make_input(g0(:, c), h0(:, c), n, shape_in)
            ! Decorrelate the columns so no two are the same sort.
            g0(:, c) = ieor(g0(:, c), int(c, int64) * 6364136223846793005_int64)
        end do
        !
        write (output_unit, '(a)') "=============================================================="
        write (output_unit, '(a)') "probe_radix_parallel -- NESTED sweep (outer columns x inner B)"
        write (output_unit, '(a)') "=============================================================="
        write (output_unit, '(a,i0,a,i0,a,i0)') "  columns = ", ncol, "   rows/column = ", n, &
            "   total elements = ", n * int(ncol, int64)
        write (output_unit, '(a,i0,a,i0)') "  thread budget = ", budget, &
            "   omp_get_num_procs() = ", omp_get_num_procs()
        write (output_unit, '(a,f7.1,a)') "  scratch if every column is live at once = ", &
            real(n, real64) * real(ncol, real64) * 32.0_real64 / 1073741824.0_real64, " GiB"
        write (output_unit, '(a)') ""
        !
        ! Nested regions are INACTIVE by default (max_active_levels = 1), so an inner num_threads()
        ! request silently yields a team of one. This is the line that enables the whole regime.
        call omp_set_max_active_levels(2)
        !
        ! Reference: one column at a time, design B serial inside.
        call run_nested(g0, h0, ga, ha, gb, hb, n, ncol, 1, 1, reps, tbase)
        gref = ga
        href = ha
        write (output_unit, '(a)') "  outer  inner  total     ms     ns/elem  speedup  check"
        write (output_unit, '(a,i5,i7,i7,f9.2,f10.3,f9.2,a)') "  ", 1, 1, 1, tbase * 1.0e3_real64, &
            tbase * 1.0e9_real64 / real(n * int(ncol, int64), real64), 1.0_real64, "   --"
        !
        ol = [1, 2, 4, 8, 16, 32]
        do i = 1, size(ol)
            outer = min(ol(i), ncol)
            if (i > 1) then
                if (min(ol(i - 1), ncol) == outer) cycle
            end if
            inner = max(1, budget / outer)
            if (outer * inner > 2 * omp_get_num_procs()) cycle
            call run_nested(g0, h0, ga, ha, gb, hb, n, ncol, outer, inner, reps, best)
            ok = all(ga(1:n, 1:ncol) == gref(1:n, 1:ncol)) .and. all(ha(1:n, 1:ncol) == href(1:n, 1:ncol))
            write (output_unit, '(a,i5,i7,i7,f9.2,f10.3,f9.2,a)') "  ", outer, inner, outer * inner, &
                best * 1.0e3_real64, best * 1.0e9_real64 / real(n * int(ncol, int64), real64), &
                tbase / best, merge("   OK  ", "  WRONG", ok)
        end do
        !
        ! The same budget spent ENTIRELY at one level or the other, for the two extremes.
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "  -- the two pure strategies at the same budget --"
        call run_nested(g0, h0, ga, ha, gb, hb, n, ncol, 1, budget, reps, best)
        write (output_unit, '(a,i5,i7,i7,f9.2,f10.3,f9.2,a)') "  ", 1, budget, budget, &
            best * 1.0e3_real64, best * 1.0e9_real64 / real(n * int(ncol, int64), real64), &
            tbase / best, "   inner only"
        call run_nested(g0, h0, ga, ha, gb, hb, n, ncol, ncol, 1, reps, best)
        write (output_unit, '(a,i5,i7,i7,f9.2,f10.3,f9.2,a)') "  ", ncol, 1, ncol, &
            best * 1.0e3_real64, best * 1.0e9_real64 / real(n * int(ncol, int64), real64), &
            tbase / best, "   outer only"
    end subroutine nested_sweep
    !
    !> One timed configuration of the nested sweep.
    subroutine run_nested(g0, h0, ga, ha, gb, hb, n, nc, outer, inner, reps, best)
        integer(int64), intent(in) :: g0(:,:), h0(:,:)
        integer(int64), intent(inout) :: ga(:,:), ha(:,:), gb(:,:), hb(:,:)
        integer(int64), intent(in) :: n
        integer, intent(in) :: nc, outer, inner, reps
        real(real64), intent(out) :: best
        real(real64) :: t0, t1
        integer :: rep, c
        !
        best = huge(1.0_real64)
        do rep = 0, reps
            ga(1:n, 1:nc) = g0(1:n, 1:nc)
            ha(1:n, 1:nc) = h0(1:n, 1:nc)
            t0 = omp_get_wtime()
            !$omp parallel do num_threads(outer) default(shared) private(c) schedule(dynamic)
            do c = 1, nc
                if (inner <= 1) then
                    call radix_serial(ga(:, c), ha(:, c), gb(:, c), hb(:, c), n)
                else
                    call radix_msd_bucket(ga(:, c), ha(:, c), gb(:, c), hb(:, c), n, inner, .false.)
                end if
            end do
            !$omp end parallel do
            t1 = omp_get_wtime()
            if (rep > 0 .and. t1 - t0 < best) best = t1 - t0
        end do
    end subroutine run_nested
    !
    !> Writes every buffer from many threads so that Linux's first-touch policy distributes the
    !! pages over both NUMA nodes rather than putting all of them on the master's node.
    subroutine spread_pages(k0, r0, ka, ra, kb, rb, n)
        integer(int64), intent(out) :: k0(:), r0(:), ka(:), ra(:), kb(:), rb(:)
        integer(int64), intent(in) :: n
        integer(int64) :: j
        !
        !$omp parallel do num_threads(omp_get_num_procs()) default(shared) private(j) schedule(static)
        do j = 1_int64, n
            k0(j) = 0_int64
            r0(j) = 0_int64
            ka(j) = 0_int64
            ra(j) = 0_int64
            kb(j) = 0_int64
            rb(j) = 0_int64
        end do
        !$omp end parallel do
    end subroutine spread_pages
    !
    !> How many threads a request for `nt` actually gets. Both designs derive their chunk bounds
    !! from the REQUESTED count, so a short team would leave rows unprocessed; this reports it
    !! rather than letting the equality check call it a wrong answer.
    function team_size(nt) result(got)
        integer, intent(in) :: nt
        integer :: got
        !
        got = 0
        !$omp parallel num_threads(nt) default(shared)
        !$omp master
        got = omp_get_num_threads()
        !$omp end master
        !$omp end parallel
        if (got /= nt) write (output_unit, '(a,i0,a,i0,a)') "  (requested ", nt, &
            " threads, runtime gave ", got, " -- row skipped)"
    end function team_size
    !
    !> xorshift64, so the same seed gives the same data on every compiler.
    subroutine make_input(k, r, n, shape_in)
        integer(int64), intent(out) :: k(:), r(:)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: shape_in
        integer(int64) :: s, i
        integer(int64) :: prefix_top !! the two constant top bytes of the "prefix" shape.
        !
        ! Built rather than written as a literal so the two bytes are readable: 104 = "h", 116 = "t".
        prefix_top = ior(ishft(104_int64, 56), ishft(116_int64, 48))
        s = 88172645463325252_int64
        do i = 1_int64, n
            s = ieor(s, ishft(s, 13))
            s = ieor(s, ishft(s, -7))
            s = ieor(s, ishft(s, 17))
            select case (trim(shape_in))
            case ("narrow")
                ! A 32-bit value range -- the shape S4 in feature_sort_improvements.md creates, and
                ! the shape most real integer columns already have.
                k(i) = iand(s, 4294967295_int64)
            case ("lowcard")
                ! 1024 distinct values: a category code, a status flag, a small identifier. The
                ! shape that tests whether a bucket decomposition can stay load balanced. 1024 is
                ! the number that matters, not a round 1000: it puts exactly FOUR distinct values
                ! in digit 1, which is the split digit, and four buckets is the whole of section
                ! 6.3's argument.
                k(i) = iand(s, 1023_int64)
            case ("prefix")
                ! A SHARED-PREFIX key: the packed 8-byte prefix image of a string column whose
                ! values begin alike -- URLs, paths, identifiers with a common stem. Two constant
                ! top bytes ("ht"), then a third carrying only four distinct stems, then five bytes
                ! that vary freely.
                !
                ! This is Design B's predicted worst case and the shape `lowcard` does NOT model:
                ! lowcard tests low CARDINALITY (few distinct keys overall), this tests a low-
                ! cardinality SPLIT DIGIT sitting above plenty of variation below it. The most
                ! significant varying digit is 5 and it offers four buckets, so at most four
                ! threads have anything to do however many are asked for -- while the per-bucket
                ! work below it stays large. Strings are in scope for the parallel engine, so this
                ! is the arm that decides between Design A, a deeper prefix split, and recursive
                ! re-splitting.
                k(i) = ior(prefix_top, ior(ishft(iand(ishft(s, -50), 3_int64), 40), &
                    iand(s, 1099511627775_int64)))
            case default
                k(i) = s
            end select
            r(i) = i
        end do
    end subroutine make_input
    !
    !> Best-of-`reps` wall time for one arm. `nt == 0` means the serial reference.
    subroutine time_arm(k0, r0, ka, ra, kb, rb, n, reps, nt, design, best)
        integer(int64), intent(in) :: k0(:), r0(:)
        integer(int64), intent(inout) :: ka(:), ra(:), kb(:), rb(:)
        integer(int64), intent(in) :: n
        integer, intent(in) :: reps, nt
        integer, intent(in) :: design !! 0 serial, 1 = A, 2 = B (8-bit split), 3 = B16.
        real(real64), intent(out) :: best
        real(real64) :: t0, t1
        integer :: rep
        !
        best = huge(1.0_real64)
        do rep = 0, reps
            ka(1:n) = k0(1:n)
            ra(1:n) = r0(1:n)
            t0 = omp_get_wtime()
            select case (design)
            case (0)
                call radix_serial(ka, ra, kb, rb, n)
            case (1)
                call radix_lsd_parallel(ka, ra, kb, rb, n, nt)
            case (2)
                call radix_msd_bucket(ka, ra, kb, rb, n, nt, .false.)
            case default
                call radix_msd_bucket(ka, ra, kb, rb, n, nt, .true.)
            end select
            t1 = omp_get_wtime()
            ! Round 0 is a warm-up: it pays the first-touch page faults on every buffer.
            if (rep > 0 .and. t1 - t0 < best) best = t1 - t0
        end do
    end subroutine time_arm
    !
    !> Times image build -> design B -> int32 narrowing. `par_tail` threads the first and last.
    subroutine time_path(vals, ka, ra, kb, rb, p32, n, reps, nt, par_tail, best)
        integer(int64), intent(in) :: vals(:)
        integer(int64), intent(inout) :: ka(:), ra(:), kb(:), rb(:)
        integer(int32), intent(inout) :: p32(:)
        integer(int64), intent(in) :: n
        integer, intent(in) :: reps, nt
        logical, intent(in) :: par_tail
        real(real64), intent(out) :: best
        integer(int64), parameter :: SIGNBIT = -huge(0_int64) - 1_int64
        real(real64) :: t0, t1
        integer(int64) :: j
        integer :: rep
        !
        best = huge(1.0_real64)
        do rep = 0, reps
            t0 = omp_get_wtime()
            if (par_tail) then
                !$omp parallel do num_threads(nt) default(shared) private(j) schedule(static)
                do j = 1_int64, n
                    ka(j) = ieor(vals(j), SIGNBIT)
                    ra(j) = j
                end do
                !$omp end parallel do
            else
                do j = 1_int64, n
                    ka(j) = ieor(vals(j), SIGNBIT)
                    ra(j) = j
                end do
            end if
            if (nt <= 1) then
                call radix_serial(ka, ra, kb, rb, n)
            else
                call radix_msd_bucket(ka, ra, kb, rb, n, nt, .false.)
            end if
            if (par_tail) then
                !$omp parallel do num_threads(nt) default(shared) private(j) schedule(static)
                do j = 1_int64, n
                    p32(j) = int(ra(j), int32)
                end do
                !$omp end parallel do
            else
                do j = 1_int64, n
                    p32(j) = int(ra(j), int32)
                end do
            end if
            t1 = omp_get_wtime()
            if (rep > 0 .and. t1 - t0 < best) best = t1 - t0
        end do
    end subroutine time_path
    !
    subroutine report_path(tag, nt, tpar, tser, n)
        character(len=*), intent(in) :: tag
        integer, intent(in) :: nt
        real(real64), intent(in) :: tpar, tser
        integer(int64), intent(in) :: n
        !
        write (output_unit, '(a,i7,f9.3,f11.3,f10.2,f10.3)') "  " // tag, nt, tpar * 1.0e3_real64, &
            tpar * 1.0e9_real64 / real(n, real64), tser / tpar, (tser / tpar) / real(nt, real64)
    end subroutine report_path
    !
    subroutine report(tag, nt, tpar, tser, n, ka, ra, kref, rref)
        character(len=*), intent(in) :: tag
        integer, intent(in) :: nt
        real(real64), intent(in) :: tpar, tser
        integer(int64), intent(in) :: n
        integer(int64), intent(in) :: ka(:), ra(:), kref(:), rref(:)
        character(len=6) :: ok
        !
        ok = "  OK  "
        if (any(ka(1:n) /= kref(1:n)) .or. any(ra(1:n) /= rref(1:n))) ok = " WRONG"
        ! `per-core` is the speedup divided by the thread count -- the efficiency fraction. It is
        ! printed because the supported configurations include a SMALL team on a large machine (a
        ! user's own outer parallel region handing each sort perhaps 8 threads), and a headline
        ! speedup at 64 threads says nothing about that regime. A design that is 46x at 64 threads
        ! and one that is 46x at 64 threads after scaling badly to 8 look identical without it.
        write (output_unit, '(a,i7,f9.3,f11.3,f10.2,f10.3,a)') "  " // tag, nt, tpar * 1.0e3_real64, &
            tpar * 1.0e9_real64 / real(n, real64), tser / tpar, &
            (tser / tpar) / real(nt, real64), "  " // ok
    end subroutine report
    !
    !> The MSD bucket of `v`: one digit, or two combined into a 16-bit index.
    pure function bucket_of(v, dhi, dlo, wide) result(b)
        integer(int64), intent(in) :: v
        integer, intent(in) :: dhi, dlo
        logical, intent(in) :: wide
        integer(int64) :: b
        if (wide) then
            b = ishft(dig(v, dhi), 8) + dig(v, dlo)
        else
            b = dig(v, dhi)
        end if
    end function bucket_of
    !
    !> Byte `p` of `v`, as an unsigned 0..255.
    pure function dig(v, p) result(b)
        integer(int64), intent(in) :: v
        integer, intent(in) :: p
        integer(int64) :: b
        b = iand(ishft(v, -8 * p), 255_int64)
    end function dig
    !
    ! ---- Serial reference: the shape src/parquet_sorting_engine.f90 uses today ------------------
    !
    !> **This routine PING-PONGS BY PARITY and must never go back to swapping the buffers.**
    !!
    !! It previously exchanged the contents of both pairs after every executed pass, with a comment
    !! claiming the cost was "counted in every arm equally, so it cannot bias a ratio". That was
    !! wrong twice over. The library does not do it at all -- `sort_radix_permutation` ping-pongs
    !! with three `move_alloc` calls, which move no data -- and Design B never called it either,
    !! because `lsd_range` already ping-pongs by parity. So the swap was charged to the BASELINE and
    !! to Design A while Design B escaped it, which inflated every speedup in this probe and B's
    !! most. It came to ~1.43 ns/element/pass, 64 bytes/element of traffic per pass, 512 bytes over
    !! eight passes that the engine never moves -- enough to make this probe's serial arm 1.58x the
    !! engine's own measured radix. See feature_sort_parallel.md section 2.4.
    !!
    !! `move_alloc` is not available here because these are dummy arrays, but it is not needed:
    !! tracking which pair is live costs one logical and one branch per pass. The final copy back is
    !! conditional and happens at most once; the engine avoids even that by letting its last pass
    !! write straight into the output.
    subroutine radix_serial(ka, ra, kb, rb, n)
        integer(int64), intent(inout) :: ka(:), ra(:), kb(:), rb(:)
        integer(int64), intent(in) :: n
        integer(int64) :: hist(0:255, 0:7)
        integer(int64) :: j, b, u
        integer :: p
        logical :: in_b !! .true. when the live data is in the kb/rb pair.
        !
        ! All eight histograms in one read, exactly as the engine does. This is the GLOBAL histogram
        ! and it stays valid across passes because the multiset of key values never changes; only a
        ! PER-THREAD histogram goes stale after a reordering (section 6.1).
        hist = 0_int64
        do j = 1_int64, n
            u = ka(j)
            do p = 0, 7
                b = iand(u, 255_int64)
                hist(b, p) = hist(b, p) + 1_int64
                u = ishft(u, -8)
            end do
        end do
        in_b = .false.
        do p = 0, 7
            ! Any element answers the pass-skip question: if every row agrees on digit p then
            ! element 1 speaks for all of them, and if they do not then the count differs from n
            ! whichever element is read. It has to come from the LIVE pair, though.
            if (in_b) then
                b = dig(kb(1), p)
            else
                b = dig(ka(1), p)
            end if
            if (hist(b, p) == n) cycle
            if (in_b) then
                call lsd_pass_serial(kb, rb, ka, ra, n, p, hist(:, p))
            else
                call lsd_pass_serial(ka, ra, kb, rb, n, p, hist(:, p))
            end if
            in_b = .not. in_b
        end do
        if (in_b) then
            do j = 1_int64, n
                ka(j) = kb(j)
                ra(j) = rb(j)
            end do
        end if
    end subroutine radix_serial
    !
    !> One serial stable LSD pass over digit `p`, reading `(sk, sr)` and writing `(dk, dr)`.
    !! The caller swaps the argument order to flip parity, so there is one copy of this loop
    !! rather than one per parity.
    subroutine lsd_pass_serial(sk, sr, dk, dr, n, p, cnt)
        integer(int64), intent(in) :: sk(:), sr(:)      !! the live pair.
        integer(int64), intent(inout) :: dk(:), dr(:)   !! the partner pair, overwritten.
        integer(int64), intent(in) :: n
        integer, intent(in) :: p                        !! which byte.
        integer(int64), intent(in) :: cnt(0:255)        !! this digit's global histogram.
        integer(int64) :: off(0:255), j, b, t
        !
        t = 1_int64
        do b = 0_int64, 255_int64
            off(b) = t
            t = t + cnt(b)
        end do
        do j = 1_int64, n
            b = dig(sk(j), p)
            dk(off(b)) = sk(j)
            dr(off(b)) = sr(j)
            off(b) = off(b) + 1_int64
        end do
    end subroutine lsd_pass_serial
    !
    ! ---- Design A: LSD, per-thread-per-bucket offsets -------------------------------------------
    !
    !> Every digit is a synchronised pass: each thread counts its own contiguous chunk, a serial
    !! prefix sum over (bucket, thread) hands every (bucket, thread) pair its own output cursor, and
    !! every thread then scatters its chunk into slots no other thread can touch.
    !!
    !! **Stability is by construction and is what makes the answer bit-identical to the serial one**:
    !! ordering the cursors by bucket first and thread second means that within a bucket, thread 0's
    !! rows precede thread 1's, and within a thread the input order is preserved.
    subroutine radix_lsd_parallel(ka, ra, kb, rb, n, nt)
        integer(int64), intent(inout) :: ka(:), ra(:), kb(:), rb(:)
        integer(int64), intent(in) :: n
        integer, intent(in) :: nt
        integer(int64), allocatable :: hist(:,:) !! (bucket, thread), REBUILT every pass.
        integer(int64), allocatable :: cur(:,:)  !! (bucket, thread) output cursor.
        integer(int64) :: j
        integer :: p
        logical :: in_b  !! .true. when the live data is in the kb/rb pair.
        logical :: moved !! .false. when the pass was skipped, so parity does not flip.
        !
        ! Two dimensions, not three: only the current digit's counts are ever live, because they
        ! are rebuilt every pass anyway (see lsd_pass_par). The 3-D form allocated eight slabs and
        ! used one.
        allocate(hist(0:255, 0:nt-1), cur(0:255, 0:nt-1))
        !
        ! Parity, not swapping -- see radix_serial's header for what this cost when it was a swap.
        in_b = .false.
        do p = 0, 7
            if (in_b) then
                call lsd_pass_par(kb, rb, ka, ra, n, nt, p, hist, cur, moved)
            else
                call lsd_pass_par(ka, ra, kb, rb, n, nt, p, hist, cur, moved)
            end if
            if (moved) in_b = .not. in_b
        end do
        if (in_b) then
            !$omp parallel do num_threads(nt) default(shared) private(j) schedule(static)
            do j = 1_int64, n
                ka(j) = kb(j)
                ra(j) = rb(j)
            end do
            !$omp end parallel do
        end if
    end subroutine radix_lsd_parallel
    !
    !> One parallel stable LSD pass over digit `p`, reading `(sk, sr)` and writing `(dk, dr)`.
    !! `moved` reports whether it scattered at all, so the caller knows whether to flip parity.
    subroutine lsd_pass_par(sk, sr, dk, dr, n, nt, p, hist, cur, moved)
        integer(int64), intent(in) :: sk(:), sr(:)          !! the live pair.
        integer(int64), intent(inout) :: dk(:), dr(:)       !! the partner pair, overwritten.
        integer(int64), intent(in) :: n
        integer, intent(in) :: nt, p
        integer(int64), intent(inout) :: hist(0:,0:), cur(0:,0:) !! (bucket, thread); scratch.
        logical, intent(out) :: moved
        integer(int64) :: j, b, t, u, lo, hi
        integer :: tid
        !
        ! **The per-thread histogram must be rebuilt EVERY pass, and this is the single most
        ! important structural difference from the serial code.** Serially all eight histograms
        ! can be built in one read, because the multiset of values never changes. Here they
        ! cannot: after a pass, thread t's contiguous chunk holds a DIFFERENT set of rows than it
        ! held when a global histogram was built, so a per-thread count taken before the
        ! reordering describes the wrong rows. Building them up front and reusing them across
        ! passes produces a silently wrong permutation from two threads upward -- confirmed by
        ! this probe's own equality check, which is why the check is here.
        !
        ! The consequence is a real cost: a parallel LSD pass is TWO synchronised phases (count,
        ! then scatter) and reads the key array twice, where a serial pass is one phase and one
        ! read. That, not the buffer handling, is why Design A starts behind the serial reference.
        !$omp parallel num_threads(nt) default(shared) private(tid, lo, hi, j, b)
        tid = omp_get_thread_num()
        lo = 1_int64 + (n * int(tid, int64)) / int(nt, int64)
        hi = (n * int(tid + 1, int64)) / int(nt, int64)
        hist(:, tid) = 0_int64
        do j = lo, hi
            b = dig(sk(j), p)
            hist(b, tid) = hist(b, tid) + 1_int64
        end do
        !$omp end parallel
        !
        ! The pass-skip test, unchanged: a digit every row agrees on cannot reorder anything.
        b = dig(sk(1), p)
        u = 0_int64
        do t = 0_int64, int(nt - 1, int64)
            u = u + hist(b, t)
        end do
        if (u == n) then
            moved = .false.
            return
        end if
        !
        ! Serial prefix over (bucket, thread) -- **BUCKET major, THREAD minor, and that ordering is
        ! what makes the answer stable**: within a bucket, thread 0's rows precede thread 1's, and
        ! within a thread the input order survives. 256*nt entries -- 16384 at nt = 64, against n
        ! rows, so it is not a scaling term until n gets small.
        u = 1_int64
        do b = 0_int64, 255_int64
            do t = 0_int64, int(nt - 1, int64)
                cur(b, t) = u
                u = u + hist(b, t)
            end do
        end do
        !
        !$omp parallel num_threads(nt) default(shared) private(tid, lo, hi, j, b)
        tid = omp_get_thread_num()
        lo = 1_int64 + (n * int(tid, int64)) / int(nt, int64)
        hi = (n * int(tid + 1, int64)) / int(nt, int64)
        do j = lo, hi
            b = dig(sk(j), p)
            dk(cur(b, tid)) = sk(j)
            dr(cur(b, tid)) = sr(j)
            cur(b, tid) = cur(b, tid) + 1_int64
        end do
        !$omp end parallel
        moved = .true.
    end subroutine lsd_pass_par
    !
    ! ---- Design B: one MSD split, then whole buckets in parallel --------------------------------
    !
    !> One synchronised MSD pass on the TOP digit puts every row in its final bucket; each bucket is
    !! then an independent problem that one thread finishes alone, with no further synchronisation
    !! and a working set small enough to stay in cache.
    !!
    !! Correct for the same reason A is: the MSD split is stable and orders the buckets against each
    !! other, and each bucket's own sort is a stable LSD over the remaining digits.
    subroutine radix_msd_bucket(ka, ra, kb, rb, n, nt, wide_split)
        integer(int64), intent(inout) :: ka(:), ra(:), kb(:), rb(:)
        integer(int64), intent(in) :: n
        integer, intent(in) :: nt
        logical, intent(in) :: wide_split
        !! .true. splits on TWO digits at once -- 65536 buckets instead of 256. An 8-bit split caps
        !! the available parallelism at 256 independent tasks however many threads are asked for,
        !! which is the ceiling this exists to test.
        integer(int64), allocatable :: hcnt(:,:), cur(:,:), bstart(:), bend(:)
        integer(int64), allocatable :: hall(:,:,:)
        integer(int64) :: j, b, t, u, lo, hi
        integer :: tid, ib, p, dsplit, dlow, nbk
        logical :: in_a, wide
        !
        allocate(hall(0:255, 0:7, 0:nt-1))
        !
        ! **Choosing the split digit is what makes or breaks this design.** Splitting on digit 7
        ! unconditionally gives ONE bucket for any key whose top byte is constant -- which is every
        ! narrow-range integer column, i.e. most real ones -- and one bucket means no parallelism at
        ! all. The split must be on the MOST SIGNIFICANT DIGIT THAT VARIES: every digit above it is
        ! constant, so ordering by it and then by the digits below it is still the correct total
        ! order, and it is the highest digit that can separate anything.
        !
        ! This histogram is over the ORIGINAL data and is valid because nothing has moved yet. It
        ! cannot be reused after a pass -- see the note in radix_lsd_parallel.
        !$omp parallel num_threads(nt) default(shared) private(tid, lo, hi, j, b, u, p)
        tid = omp_get_thread_num()
        lo = 1_int64 + (n * int(tid, int64)) / int(nt, int64)
        hi = (n * int(tid + 1, int64)) / int(nt, int64)
        hall(:, :, tid) = 0_int64
        do j = lo, hi
            u = ka(j)
            do p = 0, 7
                b = iand(u, 255_int64)
                hall(b, p, tid) = hall(b, p, tid) + 1_int64
                u = ishft(u, -8)
            end do
        end do
        !$omp end parallel
        !
        dsplit = -1
        do p = 7, 0, -1
            b = dig(ka(1), p)
            u = 0_int64
            do t = 0_int64, int(nt - 1, int64)
                u = u + hall(b, p, t)
            end do
            if (u /= n) then
                dsplit = p
                exit
            end if
        end do
        if (dsplit < 0) return   ! every digit constant: all keys equal, nothing to reorder.
        !
        wide = wide_split .and. dsplit >= 1
        nbk = 256
        dlow = dsplit
        if (wide) then
            nbk = 65536
            dlow = dsplit - 1
        end if
        allocate(hcnt(0:nbk-1, 0:nt-1), cur(0:nbk-1, 0:nt-1), bstart(0:nbk-1), bend(0:nbk-1))
        !
        ! A combined two-digit bucket is not a marginal of the per-digit histograms, so it needs its
        ! own counting pass. One extra read of the key array, against 256x more independent tasks.
        !$omp parallel num_threads(nt) default(shared) private(tid, lo, hi, j, b)
        tid = omp_get_thread_num()
        lo = 1_int64 + (n * int(tid, int64)) / int(nt, int64)
        hi = (n * int(tid + 1, int64)) / int(nt, int64)
        hcnt(:, tid) = 0_int64
        do j = lo, hi
            b = bucket_of(ka(j), dsplit, dlow, wide)
            hcnt(b, tid) = hcnt(b, tid) + 1_int64
        end do
        !$omp end parallel
        !
        u = 1_int64
        do b = 0_int64, int(nbk - 1, int64)
            bstart(b) = u
            do t = 0_int64, int(nt - 1, int64)
                cur(b, t) = u
                u = u + hcnt(b, t)
            end do
            bend(b) = u - 1_int64
        end do
        !
        !$omp parallel num_threads(nt) default(shared) private(tid, lo, hi, j, b)
        tid = omp_get_thread_num()
        lo = 1_int64 + (n * int(tid, int64)) / int(nt, int64)
        hi = (n * int(tid + 1, int64)) / int(nt, int64)
        do j = lo, hi
            b = bucket_of(ka(j), dsplit, dlow, wide)
            kb(cur(b, tid)) = ka(j)
            rb(cur(b, tid)) = ra(j)
            cur(b, tid) = cur(b, tid) + 1_int64
        end do
        !$omp end parallel
        !
        ! Every bucket is now independent. `dynamic` because bucket sizes are data, not a promise.
        !$omp parallel do num_threads(nt) default(shared) private(ib, in_a) schedule(dynamic)
        do ib = 0, nbk - 1
            if (bend(ib) <= bstart(ib)) cycle
            call lsd_range(kb, rb, ka, ra, bstart(ib), bend(ib), dlow - 1, in_a)
            ! `lsd_range` leaves its answer in whichever buffer the pass parity ended on; normalise
            ! every bucket into kb/rb so the caller has one place to look.
            if (in_a) then
                kb(bstart(ib):bend(ib)) = ka(bstart(ib):bend(ib))
                rb(bstart(ib):bend(ib)) = ra(bstart(ib):bend(ib))
            end if
        end do
        !$omp end parallel do
        !
        !$omp parallel do num_threads(nt) default(shared) private(j) schedule(static)
        do j = 1_int64, n
            ka(j) = kb(j)
            ra(j) = rb(j)
        end do
        !$omp end parallel do
    end subroutine radix_msd_bucket
    !
    !> Serial stable LSD over digits 0..6 of `k(lo:hi)`, ping-ponging into `ks(lo:hi)`.
    !! `in_s` reports whether the answer ended up in the scratch pair.
    subroutine lsd_range(k, r, ks, rs, lo, hi, dtop, in_s)
        integer(int64), intent(inout) :: k(:), r(:), ks(:), rs(:)
        integer(int64), intent(in) :: lo, hi
        integer, intent(in) :: dtop
        logical, intent(out) :: in_s
        integer(int64) :: cnt(0:255), j, b, t, m
        integer :: p
        !
        in_s = .false.
        m = hi - lo + 1_int64
        do p = 0, dtop
            cnt = 0_int64
            if (in_s) then
                do j = lo, hi
                    b = dig(ks(j), p)
                    cnt(b) = cnt(b) + 1_int64
                end do
            else
                do j = lo, hi
                    b = dig(k(j), p)
                    cnt(b) = cnt(b) + 1_int64
                end do
            end if
            if (maxval(cnt) == m) cycle
            t = lo
            do b = 0_int64, 255_int64
                j = cnt(b)
                cnt(b) = t
                t = t + j
            end do
            if (in_s) then
                do j = lo, hi
                    b = dig(ks(j), p)
                    k(cnt(b)) = ks(j)
                    r(cnt(b)) = rs(j)
                    cnt(b) = cnt(b) + 1_int64
                end do
            else
                do j = lo, hi
                    b = dig(k(j), p)
                    ks(cnt(b)) = k(j)
                    rs(cnt(b)) = r(j)
                    cnt(b) = cnt(b) + 1_int64
                end do
            end if
            in_s = .not. in_s
        end do
    end subroutine lsd_range
    !
end program probe_radix_parallel
