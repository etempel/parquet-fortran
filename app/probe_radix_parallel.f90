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
!!   C  design A over a NARROW key (32-bit range), i.e. four active digits instead of eight.
!!
!! Delete with the investigation.
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
    write (output_unit, '(a)') "  design  threads     ms      ns/elem   speedup   check"
    write (output_unit, '(a,i7,f9.3,f11.3,f10.2,a)') "  serial ", 1, tser * 1.0e3_real64, &
        tser * 1.0e9_real64 / real(n, real64), 1.0_real64, "   --"
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
        real(real64) :: t0, t1, best, tbase
        integer :: rep
        logical :: ok
        !
        allocate(g0(n, nc), h0(n, nc), ga(n, nc), ha(n, nc), gb(n, nc), hb(n, nc))
        allocate(gref(n, nc), href(n, nc))
        do c = 1, nc
            call make_input(g0(:, c), h0(:, c), n, shape_in)
            ! Decorrelate the columns so no two are the same sort.
            g0(:, c) = ieor(g0(:, c), int(c, int64) * 6364136223846793005_int64)
        end do
        !
        write (output_unit, '(a)') "=============================================================="
        write (output_unit, '(a)') "probe_radix_parallel -- NESTED sweep (outer columns x inner B)"
        write (output_unit, '(a)') "=============================================================="
        write (output_unit, '(a,i0,a,i0,a,i0)') "  columns = ", nc, "   rows/column = ", n, &
            "   total elements = ", n * int(nc, int64)
        write (output_unit, '(a,i0,a,i0)') "  thread budget = ", budget, &
            "   omp_get_num_procs() = ", omp_get_num_procs()
        write (output_unit, '(a,f7.1,a)') "  scratch if every column is live at once = ", &
            real(n, real64) * real(nc, real64) * 32.0_real64 / 1073741824.0_real64, " GiB"
        write (output_unit, '(a)') ""
        !
        ! Nested regions are INACTIVE by default (max_active_levels = 1), so an inner num_threads()
        ! request silently yields a team of one. This is the line that enables the whole regime.
        call omp_set_max_active_levels(2)
        !
        ! Reference: one column at a time, design B serial inside.
        call run_nested(g0, h0, ga, ha, gb, hb, n, nc, 1, 1, reps, tbase)
        gref = ga
        href = ha
        write (output_unit, '(a)') "  outer  inner  total     ms     ns/elem  speedup  check"
        write (output_unit, '(a,i5,i7,i7,f9.2,f10.3,f9.2,a)') "  ", 1, 1, 1, tbase * 1.0e3_real64, &
            tbase * 1.0e9_real64 / real(n * int(nc, int64), real64), 1.0_real64, "   --"
        !
        ol = [1, 2, 4, 8, 16, 32]
        do i = 1, size(ol)
            outer = min(ol(i), nc)
            if (i > 1) then
                if (min(ol(i - 1), nc) == outer) cycle
            end if
            inner = max(1, budget / outer)
            if (outer * inner > 2 * omp_get_num_procs()) cycle
            call run_nested(g0, h0, ga, ha, gb, hb, n, nc, outer, inner, reps, best)
            ok = all(ga(1:n, 1:nc) == gref(1:n, 1:nc)) .and. all(ha(1:n, 1:nc) == href(1:n, 1:nc))
            write (output_unit, '(a,i5,i7,i7,f9.2,f10.3,f9.2,a)') "  ", outer, inner, outer * inner, &
                best * 1.0e3_real64, best * 1.0e9_real64 / real(n * int(nc, int64), real64), &
                tbase / best, merge("   OK  ", "  WRONG", ok)
        end do
        !
        ! The same budget spent ENTIRELY at one level or the other, for the two extremes.
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "  -- the two pure strategies at the same budget --"
        call run_nested(g0, h0, ga, ha, gb, hb, n, nc, 1, budget, reps, best)
        write (output_unit, '(a,i5,i7,i7,f9.2,f10.3,f9.2,a)') "  ", 1, budget, budget, &
            best * 1.0e3_real64, best * 1.0e9_real64 / real(n * int(nc, int64), real64), &
            tbase / best, "   inner only"
        call run_nested(g0, h0, ga, ha, gb, hb, n, nc, nc, 1, reps, best)
        write (output_unit, '(a,i5,i7,i7,f9.2,f10.3,f9.2,a)') "  ", nc, 1, nc, &
            best * 1.0e3_real64, best * 1.0e9_real64 / real(n * int(nc, int64), real64), &
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
        !
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
                ! 1000 distinct values: a category code, a status flag, a small identifier. The
                ! shape that tests whether a bucket decomposition can stay load balanced.
                k(i) = iand(s, 1023_int64)
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
        write (output_unit, '(a,i7,f9.3,f11.3,f10.2)') "  " // tag, nt, tpar * 1.0e3_real64, &
            tpar * 1.0e9_real64 / real(n, real64), tser / tpar
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
        write (output_unit, '(a,i7,f9.3,f11.3,f10.2,a)') "  " // tag, nt, tpar * 1.0e3_real64, &
            tpar * 1.0e9_real64 / real(n, real64), tser / tpar, "  " // ok
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
    subroutine radix_serial(ka, ra, kb, rb, n)
        integer(int64), intent(inout) :: ka(:), ra(:), kb(:), rb(:)
        integer(int64), intent(in) :: n
        integer(int64) :: hist(0:255, 0:7), off(0:255)
        integer(int64) :: j, b, t, u
        integer :: p
        !
        hist = 0_int64
        do j = 1_int64, n
            u = ka(j)
            do p = 0, 7
                b = iand(u, 255_int64)
                hist(b, p) = hist(b, p) + 1_int64
                u = ishft(u, -8)
            end do
        end do
        do p = 0, 7
            b = dig(ka(1), p)
            if (hist(b, p) == n) cycle
            t = 1_int64
            do b = 0_int64, 255_int64
                off(b) = t
                t = t + hist(b, p)
            end do
            do j = 1_int64, n
                b = dig(ka(j), p)
                kb(off(b)) = ka(j)
                rb(off(b)) = ra(j)
                off(b) = off(b) + 1_int64
            end do
            call swap_halves(ka, ra, kb, rb, n)
        end do
    end subroutine radix_serial
    !
    !> Exchange the contents of the two pairs. A copy rather than `move_alloc` because these are
    !! dummy arrays here; the library uses `move_alloc` and pays nothing. Counted in every arm
    !! equally, so it cannot bias a ratio.
    subroutine swap_halves(ka, ra, kb, rb, n)
        integer(int64), intent(inout) :: ka(:), ra(:), kb(:), rb(:)
        integer(int64), intent(in) :: n
        integer(int64) :: j, x
        !
        do j = 1_int64, n
            x = ka(j)
            ka(j) = kb(j)
            kb(j) = x
            x = ra(j)
            ra(j) = rb(j)
            rb(j) = x
        end do
    end subroutine swap_halves
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
        integer(int64), allocatable :: hist(:,:,:) !! (bucket, digit, thread)
        integer(int64), allocatable :: cur(:,:)    !! (bucket, thread) output cursor
        integer(int64) :: j, b, t, u, lo, hi
        integer :: p, tid
        !
        allocate(hist(0:255, 0:7, 0:nt-1), cur(0:255, 0:nt-1))
        !
        do p = 0, 7
            ! **The per-thread histogram must be rebuilt EVERY pass, and this is the single most
            ! important structural difference from the serial code.** Serially all eight histograms
            ! can be built in one read of `ka`, because the multiset of values never changes. Here
            ! they cannot: after a pass, thread t's contiguous chunk holds a DIFFERENT set of rows
            ! than it held when a global histogram was built, so a per-thread count taken before the
            ! reordering describes the wrong rows. Building them up front and reusing them across
            ! passes produces a silently wrong permutation from two threads upward -- confirmed by
            ! this probe's own equality check, which is why the check is here.
            !
            ! The consequence is a real cost: a parallel LSD pass is TWO synchronised phases (count,
            ! then scatter) and reads `ka` twice, where a serial pass is one phase and one read.
            !$omp parallel num_threads(nt) default(shared) private(tid, lo, hi, j, b)
            tid = omp_get_thread_num()
            lo = 1_int64 + (n * int(tid, int64)) / int(nt, int64)
            hi = (n * int(tid + 1, int64)) / int(nt, int64)
            hist(:, p, tid) = 0_int64
            do j = lo, hi
                b = dig(ka(j), p)
                hist(b, p, tid) = hist(b, p, tid) + 1_int64
            end do
            !$omp end parallel
            !
            ! The pass-skip test, unchanged: a digit every row agrees on cannot reorder anything.
            b = dig(ka(1), p)
            u = 0_int64
            do t = 0_int64, int(nt - 1, int64)
                u = u + hist(b, p, t)
            end do
            if (u == n) cycle
            !
            ! Serial prefix over (bucket, thread). 256*nt entries -- 16384 at nt = 64, against n
            ! rows, so it is not a scaling term until n gets small.
            u = 1_int64
            do b = 0_int64, 255_int64
                do t = 0_int64, int(nt - 1, int64)
                    cur(b, t) = u
                    u = u + hist(b, p, t)
                end do
            end do
            !
            !$omp parallel num_threads(nt) default(shared) private(tid, lo, hi, j, b)
            tid = omp_get_thread_num()
            lo = 1_int64 + (n * int(tid, int64)) / int(nt, int64)
            hi = (n * int(tid + 1, int64)) / int(nt, int64)
            do j = lo, hi
                b = dig(ka(j), p)
                kb(cur(b, tid)) = ka(j)
                rb(cur(b, tid)) = ra(j)
                cur(b, tid) = cur(b, tid) + 1_int64
            end do
            !$omp end parallel
            call swap_halves_par(ka, ra, kb, rb, n, nt)
        end do
    end subroutine radix_lsd_parallel
    !
    subroutine swap_halves_par(ka, ra, kb, rb, n, nt)
        integer(int64), intent(inout) :: ka(:), ra(:), kb(:), rb(:)
        integer(int64), intent(in) :: n
        integer, intent(in) :: nt
        integer(int64) :: j, x, y
        !
        !$omp parallel do num_threads(nt) default(shared) private(j, x, y) schedule(static)
        do j = 1_int64, n
            x = ka(j)
            ka(j) = kb(j)
            kb(j) = x
            y = ra(j)
            ra(j) = rb(j)
            rb(j) = y
        end do
        !$omp end parallel do
    end subroutine swap_halves_par
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
