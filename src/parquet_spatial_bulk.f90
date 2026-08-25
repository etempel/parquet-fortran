!> The bulk query families: every point's neighbours at once, threaded.
!!
!! **These are the only entry points that may rebuild the index**, and they do it before opening the
!! parallel region. A query that can mutate the index is not read-only, so two threads querying
!! concurrently could both decide to rebuild and race on the same arrays -- a corrupted heap rather
!! than a wrong number. The prevention is a design rule and not a mechanism, which is exactly why
!! "let single queries rebuild too" must not be added later without one.
!!
!! **Each family is two passes over the same walk**: count, prefix-sum, fill. That is what allocates
!! the result exactly once at exactly the right size, and it is why `%count_all_within` is not a
!! special case but simply the first pass on its own.
!!
!! The sweep runs in STORED order rather than the caller's row order and writes each answer to the
!! row it belongs to. Stored order means consecutive query points sit in the same or neighbouring
!! cells, so the cells a query walks are usually already in cache from the previous one.
submodule (parquet_spatial) parquet_spatial_bulk
    implicit none

contains

    !> How many threads a bulk query should open, after every cap and the affinity clamp.
    module procedure spatial_threads
#ifdef _OPENMP
        use omp_lib, only: omp_get_max_threads, omp_in_parallel
#endif

        n = 1
        if (present(threads)) then
            if (threads < 1) error stop "pf_spatial_index: threads= must be >= 1"
            n = threads
        else
#ifdef _OPENMP
            n = omp_get_max_threads()
            ! Inside someone else's region the default is serial: `omp_get_max_threads` reads an
            ! ICV rather than the current team size, so it answers 8 inside an 8-thread region and
            ! a missing check means 8x8 threads. An explicit `threads=` is still honoured, which is
            ! what makes this a DEFAULT rather than the kind of refusal that would fire across the
            ! whole test suite.
            if (omp_in_parallel()) n = 1
#endif
            if (cfg_spatial_threads > 0) n = min(n, cfg_spatial_threads)
        end if
        ! One clamp, in one place, for every subsystem: `omp_get_max_threads` is what the
        ! environment asked for and `omp_get_num_procs` is what the affinity mask allows, and a
        ! team opened on more threads than there are processors time-shares them.
        n = parquet_clamp_to_affinity(n, "spatial")
        if (n < 1) n = 1
    end procedure spatial_threads

    !> Rebuilds when the radii a bulk query is about to use would choose a very different cell.
    module procedure spatial_maybe_rebuild
        real(real64) :: r_cur, r_new, h_cur, h_new, s2, s3

        if (.not. self%built_ok) error stop &
            "pf_spatial_index: this index has not been built; call %build first"
        if (self%npts < 2_int64) return
        if (self%r2sum <= 0.0_real64) return
        if (dbg_cell > 0.0_real64) return
        r_cur = self%r3sum / self%r2sum
        s2 = self%r2sum + sum(radii * radii)
        s3 = self%r3sum + sum(radii * radii * radii)
        r_new = s3 / s2
        ! Model against model, not model against the cell in use: the cell in use came from the
        ! PROBE and may legitimately sit a factor from what the model would have said, so comparing
        ! the two would trigger a rebuild on a perfectly well-tuned index.
        h_cur = spatial_model_cell(self%dims_eff, self%rho, r_cur)
        h_new = spatial_model_cell(self%dims_eff, self%rho, r_new)
        if (h_new <= spatial_rebuild_factor * h_cur .and. &
            h_new * spatial_rebuild_factor >= h_cur) return
        call spatial_fold_radii(self, radii, .true.)
        call spatial_retune(self)
        dbg_rebuilds = dbg_rebuilds + 1_int64
        ! Once per index, not once per query: the message exists to tell the caller their radius=
        ! hint was wrong, and repeating it per query would bury that under itself.
        if (.not. self%warned .and. cfg_spatial_rebuild_warning .and. .not. parquet_output_is_suppressed()) then
            self%warned = .true.
            ! A sky index accumulates chords; the caller asked in degrees and must read degrees.
            if (self%metric_id == PF_METRIC_SKY) then
                r_cur = 2.0_real64 * asin(min(0.5_real64 * r_cur, 1.0_real64)) * spatial_rad2deg
                r_new = 2.0_real64 * asin(min(0.5_real64 * r_new, 1.0_real64)) * spatial_rad2deg
            end if
            call parquet_emit_warning("pf_spatial_index: rebuilt for a query radius far from the one " // &
                "it was built for (built " // num_text(r_cur) // ", now " // num_text(r_new) // &
                ", cell " // num_text(self%cell_side) // "). Pass a better radius= to %build, or " // &
                "parquet_set_spatial_rebuild_warning(.false.) to silence this.")
        end if
    end procedure spatial_maybe_rebuild

    !> Every point's neighbours as CSR.
    module procedure spatial_all_within_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64), allocatable :: counts(:)
        integer(int64) :: n, t, i, u, m, s0, e0, total
        real(real64) :: p(3), r
        integer :: nt, nr
        logical :: direct

        call spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads)
        allocate (offsets(n + 1_int64))
        allocate (counts(max(n, 1_int64)))
        if (n == 0_int64) then
            offsets(1) = 1_int64
            allocate (neighbours(0))
            return
        end if
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) private(t, i, u, m, p, r)
        do t = 1_int64, n
            i = self%idx(t)
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = radii(1)
            if (nr > 1) r = radii(i)
            call spatial_scan(self, p, r, m)
            counts(i) = m
        end do
        !$omp end parallel do
        offsets(1) = 1_int64
        do i = 1_int64, n
            offsets(i + 1_int64) = offsets(i) + counts(i)
        end do
        total = offsets(n + 1_int64) - 1_int64
        allocate (neighbours(total))
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) private(t, i, u, m, p, r, s0, e0)
        do t = 1_int64, n
            i = self%idx(t)
            s0 = offsets(i)
            e0 = offsets(i + 1_int64) - 1_int64
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = radii(1)
            if (nr > 1) r = radii(i)
            call spatial_scan(self, p, r, m, out64=neighbours(s0:e0))
        end do
        !$omp end parallel do
    end procedure spatial_all_within_worker

    !> How many neighbours each point has, in the caller's row order.
    module procedure spatial_count_all_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64) :: n, t, i, u, m
        real(real64) :: p(3), r
        integer :: nt, nr
        logical :: direct

        call spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads)
        allocate (counts(n))
        if (n == 0_int64) return
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) private(t, i, u, m, p, r)
        do t = 1_int64, n
            i = self%idx(t)
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = radii(1)
            if (nr > 1) r = radii(i)
            call spatial_scan(self, p, r, m)
            counts(i) = m
        end do
        !$omp end parallel do
    end procedure spatial_count_all_worker

    !> Every neighbouring pair exactly once, with `i < j` in the caller's row numbering.
    module procedure spatial_pairs_within_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64), allocatable :: counts(:), heads(:), keys(:), ord(:), rank_row(:)
        integer(int64) :: n, t, i, u, m, s0, e0, total, k
        real(real64) :: p(3), r
        integer :: nt, nr
        logical :: direct

        call spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads)
        if (n == 0_int64) then
            allocate (ii(0), jj(0))
            return
        end if
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        call pair_order_keys(self, radii, nr, n, nt, keys)
        allocate (counts(n), heads(n + 1_int64))
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) private(t, i, u, m, p, r)
        do t = 1_int64, n
            i = self%idx(t)
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = radii(1)
            if (nr > 1) r = radii(i)
            ! `min_key` makes the walk report only points ranked above this one, so each pair is
            ! produced by exactly one of its two endpoints and there is nothing to de-duplicate.
            call spatial_scan(self, p, r, m, min_key=keys(t), keys=keys)
            counts(t) = m
        end do
        !$omp end parallel do
        ! Prefix over STORED order here, unlike the CSR forms: the output is a flat pair list with
        ! no per-row addressing, so the cheapest consistent order is the one the sweep runs in.
        heads(1) = 1_int64
        do t = 1_int64, n
            heads(t + 1_int64) = heads(t) + counts(t)
        end do
        total = heads(n + 1_int64) - 1_int64
        allocate (ii(total), jj(total))
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) private(t, i, u, m, p, r, s0, e0, k)
        do t = 1_int64, n
            i = self%idx(t)
            s0 = heads(t)
            e0 = heads(t + 1_int64) - 1_int64
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = radii(1)
            if (nr > 1) r = radii(i)
            call spatial_scan(self, p, r, m, out64=jj(s0:e0), min_key=keys(t), keys=keys)
            ! `i` is the endpoint that did the SEARCHING, which under a per-point radius is the one
            ! with the larger ball and so not necessarily the lower row. Order each pair here, so
            ! both forms carry the same contract: every unordered pair once, always with i < j.
            do k = s0, e0
                if (jj(k) > i) then
                    ii(k) = i
                else
                    ii(k) = jj(k)
                    jj(k) = i
                end if
            end do
        end do
        !$omp end parallel do
    end procedure spatial_pairs_within_worker

    !> Gives every point the ORDER KEY that decides which endpoint of a pair reports it.
    !>
    !> **A pair belongs in the output when EITHER ball reaches the other**, `d <= max(r_a, r_b)` --
    !> and only the endpoint with the larger radius is guaranteed to walk far enough to see its
    !> partner at all, so that is the one that has to do the reporting. Ranking the points by
    !> DESCENDING radius and emitting only upward through the rank therefore produces every
    !> qualifying pair exactly once, from the one endpoint whose own sweep can find it.
    !>
    !> Ties can fall either way and it does not matter: any permutation gives every point a
    !> distinct rank, which is the whole of what the emit-once rule needs.
    !>
    !> A single radius needs no sort — the balls are all the same size, so both endpoints see each
    !> other and any total order will do. The caller's row index is the one that keeps the output
    !> in the order it has always been in.
    subroutine pair_order_keys(self, radii, nr, n, nt, keys)
        type(pf_spatial_index), intent(in) :: self !! the index about to be swept.
        real(real64), intent(in) :: radii(:) !! one radius, or one per point in row order.
        integer, intent(in) :: nr !! `size(radii)`.
        integer(int64), intent(in) :: n !! how many points the index holds.
        integer, intent(in) :: nt !! the team size, for the sort.
        integer(int64), allocatable, intent(out) :: keys(:) !! order key per STORED position.
        integer(int64), allocatable :: ord(:), rank_row(:)
        integer(int64) :: t, k

        allocate (keys(n))
        if (nr <= 1) then
            keys = self%idx
            return
        end if
        call pf_argsort(radii, ord, descending=.true., threads=nt)
        allocate (rank_row(n))
        do k = 1_int64, n
            rank_row(ord(k)) = k
        end do
        deallocate (ord)
        ! Into STORED order, so the sweep reads a key with the same locality as a coordinate
        ! instead of gathering one per candidate. Freeing `ord` first keeps the peak at two of
        ! these arrays rather than three.
        do t = 1_int64, n
            keys(t) = rank_row(self%idx(t))
        end do
    end subroutine pair_order_keys

    !> Validates a bulk call's radius list, rebuilds if it disagrees badly with the build, and
    !> resolves the team size. The three things every bulk family does identically, before any of
    !> them opens a parallel region.
    subroutine spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads)
        type(pf_spatial_index), intent(inout), target :: self !! the index about to be swept.
        real(real64), intent(in) :: radii(:) !! one radius, or one per point, in the index's own units.
        integer, intent(in) :: expect_metric !! the metric the caller's radii were stated in.
        integer(int64), intent(out) :: n !! how many points the index holds.
        integer, intent(out) :: nr !! `size(radii)`, so the sweep can pick per-row or scalar.
        integer, intent(out) :: nt !! the team size to open.
        integer, intent(in), optional :: threads !! an explicit request; absent resolves automatically.

        if (.not. self%built_ok) error stop &
            "pf_spatial_index: this index has not been built; call %build first"
        ! **Both directions, from one place.** By the time a worker runs, its radii have already
        ! been converted into the index's own units, so nothing downstream can tell degrees from
        ! chords -- which is exactly the confusion this catches. Every bulk binding therefore
        ! declares which metric it converted FROM, and a mismatch aborts here rather than
        ! producing a plausible answer to a question nobody asked.
        if (self%metric_id /= expect_metric) then
            if (expect_metric == PF_METRIC_SKY) error stop &
                "pf_spatial_index: this is a Euclidean index; use the plain bulk forms, not the _sky ones"
            error stop &
                "pf_spatial_index: this index was built with %build_sky; use the _sky bulk forms, " // &
                "which take an angular radius in degrees"
        end if
        n = self%npts
        nr = size(radii)
        if (nr /= 1 .and. int(nr, kind=int64) /= n) error stop &
            "pf_spatial_index: radius must be one value or one per point"
        if (any(radii < 0.0_real64)) error stop "pf_spatial_index: every radius must be >= 0"
        call spatial_maybe_rebuild(self, radii)
        nt = spatial_threads(threads)
        dbg_threads_used = nt
    end subroutine spatial_bulk_setup

    !> A short decimal rendering of a real, for a message.
    function num_text(v) result(text)
        real(real64), intent(in) :: v !! the value to render.
        character(len=:), allocatable :: text !! the rendered value, trimmed.
        character(len=32) :: buf

        write (buf, '(g0.6)') v
        text = trim(adjustl(buf))
    end function num_text

end submodule parquet_spatial_bulk ! GCOVR_EXCL_LINE
