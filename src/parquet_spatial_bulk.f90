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
        real(real64) :: p(3), r, rin
        integer :: nt, nr, nri
        logical :: direct, want_sort

        call spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads, radii_inner, nri)
        want_sort = .false.
        if (present(sorted)) want_sort = sorted
        allocate (offsets(n + 1_int64))
        allocate (counts(max(n, 1_int64)))
        if (n == 0_int64) then
            offsets(1) = 1_int64
            allocate (neighbours(0))
            return
        end if
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) private(t, i, u, m, p, r, rin)
        do t = 1_int64, n
            i = self%idx(t)
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = radii(1)
            if (nr > 1) r = radii(i)
            rin = inner_at(radii_inner, nri, i)
            call spatial_scan(self, p, r, m, r_inner=rin)
            counts(i) = m
        end do
        !$omp end parallel do
        offsets(1) = 1_int64
        do i = 1_int64, n
            offsets(i + 1_int64) = offsets(i) + counts(i)
        end do
        total = offsets(n + 1_int64) - 1_int64
        allocate (neighbours(total))
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) &
        !$omp     private(t, i, u, m, p, r, rin, s0, e0)
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
            rin = inner_at(radii_inner, nri, i)
            call spatial_scan(self, p, r, m, out64=neighbours(s0:e0), r_inner=rin, sorted=want_sort)
        end do
        !$omp end parallel do
    end procedure spatial_all_within_worker

    !> The inner radius that applies to row `i`, as a plain value.
    !>
    !> Absent is reported as zero rather than through a flag, because `d >= 0` is what every point
    !> already satisfies -- so the ordinary ball and the annulus run the same code and the walk
    !> needs no second path. See `spatial_scan`.
    real(real64) function inner_at(radii_inner, nri, i) result(rin)
        real(real64), intent(in), optional :: radii_inner(:) !! the caller's inner radii, if any.
        integer, intent(in) :: nri !! `size(radii_inner)`, or 0 when it was absent.
        integer(int64), intent(in) :: i !! the caller's row index.

        rin = 0.0_real64
        if (nri < 1) return
        if (nri == 1) then
            rin = radii_inner(1)
        else
            rin = radii_inner(i)
        end if
    end function inner_at

    !> How many neighbours each point has, in the caller's row order.
    module procedure spatial_count_all_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64) :: n, t, i, u, m
        real(real64) :: p(3), r, rin
        integer :: nt, nr, nri
        logical :: direct

        call spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads, radii_inner, nri)
        allocate (counts(n))
        if (n == 0_int64) return
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) private(t, i, u, m, p, r, rin)
        do t = 1_int64, n
            i = self%idx(t)
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = radii(1)
            if (nr > 1) r = radii(i)
            rin = inner_at(radii_inner, nri, i)
            call spatial_scan(self, p, r, m, r_inner=rin)
            counts(i) = m
        end do
        !$omp end parallel do
    end procedure spatial_count_all_worker

    !> Every neighbouring pair exactly once, with `i < j` in the caller's row numbering.
    module procedure spatial_pairs_within_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64), allocatable :: counts(:), heads(:), keys(:)
        integer(int64) :: n, t, i, u, m, s0, e0, total, k
        real(real64) :: p(3), r, rin
        integer :: nt, nr
        logical :: direct

        ! Scalar only, and validated against the OUTER radii through the same path every other
        ! bulk form uses -- which is what makes a per-point outer radius with a scalar inner one
        ! check against the smallest of them rather than against nothing. `nri` is not asked for:
        ! the value is already to hand, and this sweep applies it to every point alike.
        rin = 0.0_real64
        if (present(r_inner)) then
            rin = r_inner
            call spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads, [rin])
        else
            call spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads)
        end if
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
            call spatial_scan(self, p, r, m, min_key=keys(t), keys=keys, r_inner=rin)
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
            call spatial_scan(self, p, r, m, out64=jj(s0:e0), min_key=keys(t), keys=keys, r_inner=rin)
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
    subroutine spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads, radii_inner, nri)
        type(pf_spatial_index), intent(inout), target :: self !! the index about to be swept.
        real(real64), intent(in) :: radii(:) !! one radius, or one per point, in the index's own units.
        integer, intent(in) :: expect_metric !! the metric the caller's radii were stated in.
        integer(int64), intent(out) :: n !! how many points the index holds.
        integer, intent(out) :: nr !! `size(radii)`, so the sweep can pick per-row or scalar.
        integer, intent(out) :: nt !! the team size to open.
        integer, intent(in), optional :: threads !! an explicit request; absent resolves automatically.
        real(real64), intent(in), optional :: radii_inner(:) !! inner radii, when the call asked for an annulus.
        integer, intent(out), optional :: nri !! `size(radii_inner)`, or 0 when it was absent.
        integer :: nin
        logical :: bad

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
        if (present(nri)) nri = 0
        if (present(radii_inner)) then
            nin = size(radii_inner)
            ! `nri` is optional on its own: a sweep that applies one inner radius to every point
            ! has no use for the size, and asking for it would leave a dead store behind.
            if (present(nri)) nri = nin
            if (nin /= 1 .and. int(nin, kind=int64) /= n) error stop &
                "pf_spatial_index: the inner radius must be one value or one per point"
            if (any(radii_inner < 0.0_real64)) error stop &
                "pf_spatial_index: every inner radius must be >= 0"
            ! Compared against whichever shape the outer radii came in: a scalar inner radius has
            ! to clear the SMALLEST outer one, and a scalar outer radius has to be cleared by the
            ! largest inner one, or some row would be asked for an annulus turned inside out.
            if (nin == nr) then
                bad = any(radii_inner > radii)
            else if (nin == 1) then
                bad = radii_inner(1) > minval(radii)
            else
                bad = maxval(radii_inner) > radii(1)
            end if
            if (bad) error stop &
                "pf_spatial_index: every inner radius must not exceed the outer radius for that point"
        end if
        call spatial_maybe_rebuild(self, radii)
        nt = spatial_threads(threads)
        dbg_threads_used = nt
    end subroutine spatial_bulk_setup

    !> The distance from every point to its `k`-th nearest OTHER point.
    module procedure spatial_kth_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64), allocatable :: rows(:)
        real(real64), allocatable :: ds(:)
        integer(int64) :: n, t, i, u, q, pos, kk
        real(real64) :: p(3), rseed
        integer :: nt
        logical :: direct

        if (.not. self%built_ok) error stop &
            "pf_spatial_index%kth_distance: this index has not been built; call %build first"
        if (self%metric_id /= expect_metric) then
            if (expect_metric == PF_METRIC_SKY) error stop &
                "pf_spatial_index%kth_distance_sky: this is a Euclidean index; use %kth_distance"
            error stop "pf_spatial_index%kth_distance: this index was built with %build_sky; " // &
                "use %kth_distance_sky, which answers in degrees"
        end if
        n = self%npts
        if (k < 1_int64) error stop "pf_spatial_index%kth_distance: k must be >= 1"
        ! Checked up front rather than reported per point: `k` is a scalar and `n` is known, so a
        ! request that cannot be answered is a mistake in the call and not a property of one row.
        if (k > n - 1_int64) error stop &
            "pf_spatial_index%kth_distance: k must be at most %size()-1, since a point is not its own neighbour"
        nt = spatial_threads(threads)
        dbg_threads_used = nt
        allocate (dist(n))
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        kk = k + 1_int64
        !$omp parallel num_threads(nt) default(shared) &
        !$omp     private(t, i, u, q, pos, p, rseed, rows, ds)
        ! Each thread carries its own converged radius from one query to the next. The sweep runs
        ! in STORED order, so consecutive points are spatially adjacent and the previous radius is
        ! a good guess -- which changes only how many rounds the expansion takes, never an answer.
        rseed = -1.0_real64
        !$omp do schedule(static)
        do t = 1_int64, n
            i = self%idx(t)
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            call spatial_shell_search(self, p, kk, rseed, rows, ds, "kth_distance")
            ! Self is excluded by IDENTITY, not by distance: two coincident points are both at
            ! distance zero and only one of them is this row. Removing entry `pos` from a sorted
            ! list of `k+1` leaves the k-th other point at `k+1` when `pos` was inside the first
            ! `k`, and at `k` otherwise -- which also covers self not being in the list at all,
            ! possible only when more than `k+1` points share these coordinates and every
            ! candidate distance is therefore zero.
            pos = kk + 1_int64
            do q = 1_int64, kk
                if (rows(q) == i) then
                    pos = q
                    exit
                end if
            end do
            if (pos <= k) then
                dist(i) = ds(kk)
            else
                dist(i) = ds(k)
            end if
        end do
        !$omp end do
        !$omp end parallel
    end procedure spatial_kth_worker

    !> Labels the connected components of an undirected graph given as an edge list.
    !>
    !> **Serial, deliberately.** The edge pass is memory-bound and every concurrent union-find
    !> worth having is substantially subtler than this problem justifies; `%pairs_within`, which is
    !> what usually produces the edge list, is already threaded and dominates the runtime.
    module procedure spatial_components_worker
        integer(int64), allocatable :: parent(:), csize(:), lab(:)
        integer(int64) :: e, a, b, ra, rb, v, root, nc, nedge
        integer(int64) :: ms

        ! The default is 1, the strict graph-theoretic reading in which every vertex belongs to
        ! some component. A group finder that wants singletons dropped says `min_size = 2` at the
        ! call, where the choice is visible; making that the default would have this routine answer
        ! a domain question a general graph utility has no business deciding.
        ms = 1_int64
        if (present(min_size)) ms = int(min_size, kind=int64)
        if (ms < 1_int64) error stop "pf_connected_components: min_size must be >= 1"
        nedge = size(i, kind=int64)
        if (size(j, kind=int64) /= nedge) error stop &
            "pf_connected_components: the two endpoint arrays must be the same length"
        if (nvert < 0_int64) error stop "pf_connected_components: nvert must be >= 0"
        allocate (labels(max(nvert, 0_int64)))
        if (nvert == 0_int64) then
            if (present(ncomp)) ncomp = 0_int64
            if (present(sizes)) allocate (sizes(0))
            return
        end if
        labels = 0_int64
        allocate (parent(nvert), csize(nvert), lab(nvert))
        do v = 1_int64, nvert
            parent(v) = v
            csize(v) = 1_int64
            lab(v) = 0_int64
        end do
        do e = 1_int64, nedge
            a = i(e)
            b = j(e)
            if (a < 1_int64 .or. a > nvert .or. b < 1_int64 .or. b > nvert) error stop &
                "pf_connected_components: every edge endpoint must be a vertex in 1..nvert"
            ra = uf_find(parent, a)
            rb = uf_find(parent, b)
            if (ra == rb) cycle
            ! Union by size, which is what keeps the trees shallow. Which root wins is an internal
            ! choice and deliberately does NOT decide the labels -- see the numbering pass below.
            if (csize(ra) < csize(rb)) then
                parent(ra) = rb
                csize(rb) = csize(rb) + csize(ra)
            else
                parent(rb) = ra
                csize(ra) = csize(ra) + csize(rb)
            end if
        end do
        ! **Numbering by ascending vertex of first appearance, and that is a contract.** Left to
        ! the union-find's own roots the numbering would depend on union-by-size tie-breaking, so
        ! the same catalogue could come back with differently numbered groups on another compiler.
        nc = 0_int64
        do v = 1_int64, nvert
            root = uf_find(parent, v)
            if (csize(root) < ms) cycle
            if (lab(root) == 0_int64) then
                nc = nc + 1_int64
                lab(root) = nc
            end if
            labels(v) = lab(root)
        end do
        if (present(ncomp)) ncomp = nc
        if (present(sizes)) then
            allocate (sizes(nc))
            do v = 1_int64, nvert
                root = uf_find(parent, v)
                if (lab(root) == 0_int64) cycle
                sizes(lab(root)) = csize(root)
            end do
        end if
    end procedure spatial_components_worker

    !> The union-find root of `v`, with full path compression.
    integer(int64) function uf_find(parent, v) result(r)
        integer(int64), intent(inout) :: parent(:) !! the forest; compressed in place.
        integer(int64), intent(in) :: v !! the vertex to look up.
        integer(int64) :: w, nxt

        r = v
        do while (parent(r) /= r)
            r = parent(r)
        end do
        w = v
        do while (parent(w) /= r)
            nxt = parent(w)
            parent(w) = r
            w = nxt
        end do
    end function uf_find

    !> A short decimal rendering of a real, for a message.
    function num_text(v) result(text)
        real(real64), intent(in) :: v !! the value to render.
        character(len=:), allocatable :: text !! the rendered value, trimmed.
        character(len=32) :: buf

        write (buf, '(g0.6)') v
        text = trim(adjustl(buf))
    end function num_text

end submodule parquet_spatial_bulk ! GCOVR_EXCL_LINE
