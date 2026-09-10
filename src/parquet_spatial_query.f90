!> The cell walk: one scan every query family goes through, and the work counter the probe uses.
!!
!! **Cells along x at a fixed (j, k) are consecutive linear indices and `start` is a prefix sum, so
!! a whole x-run is ONE contiguous range of stored points.** The inner loop therefore needs two
!! lookups per (j, k) pair rather than two per cell -- same points tested, a third of the index
!! arithmetic. That is free and is why it is done here rather than left as an optimisation.
!!
!! **The periodic walk is a separate loop nest, deliberately.** Folding it into the free walk with a
!! test would put a branch on the hottest path in the module to serve the rarer case; the minimum
!! image itself is branch-free (`wrap` is 0 on a free axis, so the correction term vanishes without
!! a comparison), but which cells to visit genuinely differs.
submodule (parquet_spatial) parquet_spatial_query
    implicit none

contains

    !> The cell range a ball of radius `r` reaches along one FREE axis, as a start and a count.
    !>
    !> Reports `cnt = 0` when the ball misses the grid entirely, which is what lets the caller
    !> return without walking anything.
    subroutine axis_span_free(pd, r, lod, inv, nc, a, cnt)
        real(real64), intent(in) :: pd !! the query point's coordinate on this axis.
        real(real64), intent(in) :: r !! the search radius.
        real(real64), intent(in) :: lod !! the grid origin on this axis.
        real(real64), intent(in) :: inv !! 1/cell on this axis.
        integer(int64), intent(in) :: nc !! cells along this axis.
        integer(int64), intent(out) :: a !! first cell index, 0-based.
        integer(int64), intent(out) :: cnt !! how many cells; 0 when the ball misses the grid.
        real(real64) :: t0, t1
        integer(int64) :: b

        a = 0_int64
        cnt = 0_int64
        ! Clamped BEFORE the conversion, not after: a query far from the grid gives a real value
        ! that does not fit in int64, and `floor` on it is undefined rather than merely large.
        t0 = (pd - r - lod) * inv
        t1 = (pd + r - lod) * inv
        if (t1 < 0.0_real64) return
        if (t0 > real(nc, kind=real64)) return
        if (t0 < 0.0_real64) then
            a = 0_int64
        else
            a = floor(t0, kind=int64)
        end if
        if (t1 > real(nc - 1_int64, kind=real64)) then
            b = nc - 1_int64
        else
            b = floor(t1, kind=int64)
        end if
        if (b > nc - 1_int64) b = nc - 1_int64
        if (a > b) return
        cnt = b - a + 1_int64
    end subroutine axis_span_free

    !> The cell range a ball reaches along one PERIODIC axis, as an unwrapped start and a count.
    !>
    !> The count is capped at the axis length: without that cap a radius at the `L/2` limit can name
    !> `nc + 1` cells, and the extra one is a cell already visited -- so every point in it would be
    !> reported twice.
    subroutine axis_span_wrapped(pd, r, lod, inv, nc, a, cnt)
        real(real64), intent(in) :: pd !! the query point's coordinate on this axis.
        real(real64), intent(in) :: r !! the search radius.
        real(real64), intent(in) :: lod !! the grid origin on this axis.
        real(real64), intent(in) :: inv !! 1/cell on this axis.
        integer(int64), intent(in) :: nc !! cells along this axis.
        integer(int64), intent(out) :: a !! first cell index, possibly negative; wrap it before use.
        integer(int64), intent(out) :: cnt !! how many cells, never more than `nc`.
        integer(int64) :: reach, centre

        reach = ceiling(r * inv, kind=int64)
        centre = modulo(floor((pd - lod) * inv, kind=int64), nc)
        cnt = 2_int64 * reach + 1_int64
        if (cnt >= nc) then
            a = 0_int64
            cnt = nc
        else
            a = centre - reach
        end if
    end subroutine axis_span_wrapped

    !> The HEALPix candidate walk: one disc, its contiguous RING runs, and the same point test.
    !>
    !> **Structurally identical to the 3D walk's inner loop**, which is what makes the two
    !> comparable at all: a run of RING pixels is one contiguous slice of the bucketed point array,
    !> `start(first+1)` to `start(first+len+1)-1`, exactly the two-lookup shape the cell walk uses
    !> for its x-runs. About six or seven such runs replace the cell walk's `cnt(2)*cnt(3)`
    !> iterations.
    !>
    !> **`inclusive = .true.` is MANDATORY and is not an optimisation to revisit.** The default
    !> mode returns the pixels whose CENTRE lies in the disc; used as a candidate filter that
    !> silently loses points -- a point near the edge of a pixel whose centre falls just outside
    !> the disc is a real neighbour that never gets distance-tested. The inclusive mode returns
    !> the overlap superset, every pixel whose area meets the disc, which is the only correct
    !> candidate set. It costs about 57% more in the disc walk, and every candidate it adds is
    !> rejected by the exact distance test below, so the ANSWER is unchanged either way -- which
    !> is precisely why removing it would be a silent wrong answer rather than a visible one.
    subroutine scan_healpix(self, p, r, m, cap, r2, r2in, minkey, want_min, direct, &
                            has32, has64, hasd, usework, xs, ys, zs, out32, out64, dist, &
                            keys, dwork, want_bnd, uslf, vslf, bnd_u, bnd_v)
        type(pf_spatial_index), intent(in) :: self !! the index to search.
        real(real64), intent(in) :: p(3) !! the query direction; need not be normalised.
        real(real64), intent(in) :: r !! the search radius, as a chord in unit-vector space.
        integer(int64), intent(inout) :: m !! running count of qualifying points; the TRUE count.
        integer(int64), intent(in) :: cap !! how many results the caller's buffers can hold.
        real(real64), intent(in) :: r2 !! `r*r`, the squared acceptance bound.
        real(real64), intent(in) :: r2in !! squared inner radius, or 0 for a plain ball.
        integer(int64), intent(in) :: minkey !! accept only points whose key exceeds this.
        logical, intent(in) :: want_min !! whether `minkey`/`keys` are in play.
        logical, intent(in) :: direct !! whether the coordinates are in bucket order.
        logical, intent(in) :: has32 !! whether an int32 output buffer was given.
        logical, intent(in) :: has64 !! whether an int64 output buffer was given.
        logical, intent(in) :: hasd !! whether a distance buffer was given.
        logical, intent(in) :: usework !! whether distances go into `dwork` for ordering.
        ! **`contiguous` is load-bearing, not decoration.** These arrive as contiguous pointers,
        ! and an assumed-shape dummy without the attribute forces the compiler to assume a stride
        ! it must load per element -- which is the difference between a vectorised distance loop
        ! and a scalar one, on the hottest loop in the backend.
        real(real64), intent(in), contiguous :: xs(:) !! stored x, in bucket order or the caller's.
        real(real64), intent(in), contiguous :: ys(:) !! stored y.
        real(real64), intent(in), contiguous :: zs(:) !! stored z.
        integer(int32), intent(inout), optional :: out32(:) !! caller's row indices, int32.
        integer(int64), intent(inout), optional :: out64(:) !! caller's row indices, int64.
        real(real64), intent(inout), optional :: dist(:) !! distance to each reported point.
        integer(int64), intent(in), optional :: keys(:) !! order key per stored position.
        real(real64), intent(inout), optional :: dwork(:) !! distances kept for ordering.
        logical, intent(in) :: want_bnd !! whether a per-pair acceptance bound is in play.
        real(real64), intent(in) :: uslf !! the searcher's own `u` term.
        real(real64), intent(in) :: vslf !! the searcher's own `v` term.
        real(real64), intent(in), optional :: bnd_u(:) !! `u` term per stored position.
        real(real64), intent(in), optional :: bnd_v(:) !! `v` term per stored position.
        ! A stack buffer, sized so that the allocating path below is unreachable for any disc a
        ! tuned index actually issues: a disc spans about `2r/resol` rings and at most two runs
        ! per ring, and the resolution is chosen to sit just under the radius. It is
        ! `%nearest_sky`'s expanding shell that can outgrow it, by design, since that widens the
        ! radius until it has enough neighbours. 8 kB per call and per thread.
        integer(int64), parameter :: runs_stack = 512_int64
        integer(int64), target :: runs(2, runs_stack)
        integer(int64), allocatable, target :: big_runs(:,:)
        integer(int64), pointer :: rp(:,:)
        integer(int64) :: nruns, k, s0, e0, t, row, first, count, buf
        real(real64) :: ang, dx, dy, dz, d2, p1, p2, p3, bnd

        p1 = p(1)
        p2 = p(2)
        p3 = p(3)
        ! `chord = 2*sin(theta/2)` inverts exactly over [0, 180 degrees], and the clamp turns a
        ! chord past the sphere's diameter into a half-turn rather than a NaN from `asin`.
        ang = 2.0_real64 * asin(min(1.0_real64, 0.5_real64 * r))
        ! A section rather than the whole array, so that a test can narrow the buffer and reach
        ! the allocating path -- which no fixture a test can build would otherwise touch. The
        ! section is contiguous, so nothing is copied and the default path is one comparison.
        buf = runs_stack
        if (dbg_run_buf > 0_int64) buf = min(dbg_run_buf, runs_stack)
        call pf_query_disc_runs(self%nside_v, p, ang, runs(:, 1:buf), nruns, inclusive=.true.)
        rp => runs
        if (nruns > buf) then
            ! `nruns` is the TRUE count whatever the buffer held, so the exact size is known and
            ! one re-query with an exact allocation finishes it -- the same count-then-fill shape
            ! `pf_query_disc_alloc` uses internally, and the reason `pf_query_disc_runs` reports a
            ! true count rather than a truncation flag. A pointer rather than a second copy of the
            ! walk below, so the two paths cannot diverge and the loop is written once.
            allocate (big_runs(2, nruns))
            call pf_query_disc_runs(self%nside_v, p, ang, big_runs, nruns, inclusive=.true.)
            rp => big_runs
        end if

        ! **The run loop and both point loops are written out here rather than factored into a
        ! contained procedure**, deliberately and for the same reason the 3D walk below writes its
        ! four out: a per-run call keeps the compiler from holding the query point and the squared
        ! radius in registers across runs, and a disc has enough runs for that to be measurable.
        do k = 1_int64, nruns
            ! Buckets are 1-based over pixels, so pixel `q` is bucket `q + 1` and a run of
            ! `count` pixels is one slice spanning `count` buckets. Two array reads, no search --
            ! structurally the same shape the 3D walk uses for its x-runs, which is what makes the
            ! two backends comparable at all.
            first = rp(1, k) + 1_int64
            count = rp(2, k)
            s0 = self%start(first)
            e0 = self%start(first + count) - 1_int64
            if (e0 < s0) cycle
            !$omp atomic update
            dbg_pixels_visited = dbg_pixels_visited + count
            ! The direct/borrowed fork is at the RUN rather than inside the point loop, for the
            ! reason the 3D walk's own comment gives: testing it per point would slow the default
            ! path to serve the `copy=.false.` one.
            if (direct) then
                do t = s0, e0
                    dx = xs(t) - p1
                    dy = ys(t) - p2
                    dz = zs(t) - p3
                    d2 = dx * dx + dy * dy + dz * dz
                    if (d2 <= r2) then
                        if (d2 < r2in) cycle
                        if (want_min) then
                            if (keys(t) <= minkey) cycle
                        end if
                        if (want_bnd) then
                            bnd = uslf * bnd_v(t) + vslf * bnd_u(t)
                            if (d2 > bnd * bnd) cycle
                        end if
                        row = self%idx(t)
                        m = m + 1_int64
                        if (m <= cap) then
                            if (has32) out32(m) = int(row, kind=int32)
                            if (has64) out64(m) = row
                            if (hasd) dist(m) = sqrt(d2)
                            if (usework) dwork(m) = sqrt(d2)
                        end if
                    end if
                end do
            else
                do t = s0, e0
                    row = self%idx(t)
                    dx = xs(row) - p1
                    dy = ys(row) - p2
                    dz = zs(row) - p3
                    d2 = dx * dx + dy * dy + dz * dz
                    if (d2 <= r2) then
                        if (d2 < r2in) cycle
                        if (want_min) then
                            if (keys(t) <= minkey) cycle
                        end if
                        if (want_bnd) then
                            bnd = uslf * bnd_v(t) + vslf * bnd_u(t)
                            if (d2 > bnd * bnd) cycle
                        end if
                        m = m + 1_int64
                        if (m <= cap) then
                            if (has32) out32(m) = int(row, kind=int32)
                            if (has64) out64(m) = row
                            if (hasd) dist(m) = sqrt(d2)
                            if (usework) dwork(m) = sqrt(d2)
                        end if
                    end if
                end do
            end if
        end do
    end subroutine scan_healpix

    !> Walks the cells a ball of radius `r` about `p` can reach and reports what it finds.
    module procedure spatial_scan
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64) :: nc(3), a(3), cnt(3)
        integer(int64) :: ii, jj, kk, jc, kc, base, s0, e0, t, cap, row, minkey, ntest
        integer(int64) :: run_lo(2), run_hi(2), ilo, ihi
        real(real64) :: r2, r2in, dx, dy, dz, d2, p1, p2, p3
        real(real64) :: w1, w2, w3, wi1, wi2, wi3
        real(real64) :: bnd, uslf, vslf
        real(real64) :: q1, q2, q3, dself, lself, bpself, blself, dj, sd, ddif, ex, ey, ez, dpv, dlv, dv
        real(real64), allocatable :: dwork(:)
        logical :: has32, has64, hasd, want_min, direct, want_sort, usework, want_bnd
        logical :: want_los, hasdp, hasdl, ok, want_tie, own, oth
        integer :: d, nrun, ir, lrule

        m = 0_int64
        if (.not. self%built_ok) error stop &
            "pf_spatial_index: this index has not been built; call %build first"
        if (.not. (r >= 0.0_real64)) error stop &
            "pf_spatial_index: the search radius must be >= 0 and not NaN"
        ! The radius was already screened; the POINT was not, and it reaches the same arithmetic.
        ! One check here covers %within, %count_within and both sky forms, which arrive as a
        ! vector built from their own (ra, dec).
        call spatial_check_finite(p, "pf_spatial_index", "query point coordinate")
        if (self%npts == 0_int64) return
        has32 = present(out32)
        has64 = present(out64)
        hasd = present(dist)
        want_min = present(min_key)
        minkey = 0_int64
        if (want_min) minkey = min_key
        ! Defensive: the two are one mechanism and both call sites pass them together, so this is
        ! unreachable through any public entry point and no fixture can build a case for it.
        if (want_min .and. .not. present(keys)) error stop & ! GCOVR_EXCL_LINE
            "pf_spatial_index: min_key needs keys" ! GCOVR_EXCL_LINE
        ! The per-pair acceptance bound, keyed off the ARRAY rather than a flag so that the four
        ! travel as one mechanism the way `min_key` and `keys` do. The self terms are lifted into
        ! locals here because the accept test reads them once per candidate and an optional dummy
        ! is not something a compiler will keep in a register across the cell walk.
        want_bnd = present(bnd_u)
        uslf = 0.0_real64
        vslf = 0.0_real64
        if (want_bnd) then
            ! Defensive, exactly as above: every caller passes all four together. ! GCOVR_EXCL_LINE
            if (.not. (present(bnd_v) .and. present(bnd_u_self) .and. &
                present(bnd_v_self))) error stop & ! GCOVR_EXCL_LINE
                "pf_spatial_index: bnd_u needs bnd_v and both self terms" ! GCOVR_EXCL_LINE
            uslf = bnd_u_self
            vslf = bnd_v_self
        end if
        ! The line-of-sight accept, keyed on `los_rule` so the whole group travels as one mechanism
        ! the way `min_key`/`keys` and `bnd_*` do; the searcher's terms are lifted into locals for
        ! the reason the bound's are.
        want_los = present(los_rule)
        hasdp = present(dperp)
        hasdl = present(dpar)
        lrule = 0
        q1 = 0.0_real64
        q2 = 0.0_real64
        q3 = 0.0_real64
        dself = 1.0_real64
        lself = 0.0_real64
        bpself = 0.0_real64
        blself = 0.0_real64
        dpv = 0.0_real64
        dlv = 0.0_real64
        if (want_los) then
            ! Defensive, exactly as above: both workers pass the group complete. ! GCOVR_EXCL_LINE
            if (.not. (present(los_q) .and. present(los_d) .and. present(los_l) .and. present(los_bp) &
                .and. present(los_bl) .and. present(los_ls))) error stop & ! GCOVR_EXCL_LINE
                "pf_spatial_index: los_rule needs the searcher's terms and los_ls" ! GCOVR_EXCL_LINE
            lrule = los_rule
            if (lrule /= 0) then
                if (.not. (present(los_bps) .and. present(los_bls))) error stop & ! GCOVR_EXCL_LINE
                    "pf_spatial_index: a combine rule needs los_bps and los_bls" ! GCOVR_EXCL_LINE
            end if
            q1 = los_q(1)
            q2 = los_q(2)
            q3 = los_q(3)
            dself = los_d
            lself = los_l
            bpself = los_bp
            blself = los_bl
        else if (hasdp .or. hasdl) then
            error stop "pf_spatial_index: dperp and dpar are line-of-sight outputs and need los_rule" ! GCOVR_EXCL_LINE
        end if
        ! The union's emit-once tiebreak (see the interface): under it the rank does not screen the
        ! candidates, so the rank test is switched off here and the rank read inside the accept.
        want_tie = .false.
        if (want_los .and. want_min) then
            if (present(los_tiebreak)) want_tie = los_tiebreak .and. lrule == PF_LINK_MAX
        end if
        if (want_tie) want_min = .false.
        ntest = 0_int64
        if (has32 .and. self%npts > int(huge(0_int32), kind=int64)) error stop &
            "pf_spatial_index%within: this index holds more rows than an int32 buffer can name; use an int64 one"
        cap = 0_int64
        if (has32 .or. has64 .or. hasd .or. hasdp .or. hasdl) cap = huge(0_int64)
        if (has32) cap = min(cap, size(out32, kind=int64))
        if (has64) cap = min(cap, size(out64, kind=int64))
        if (hasd) cap = min(cap, size(dist, kind=int64))
        if (hasdp) cap = min(cap, size(dperp, kind=int64))
        if (hasdl) cap = min(cap, size(dpar, kind=int64))
        ! An absent inner radius is stored as zero rather than behind a flag, so the accept test
        ! is one comparison against a squared bound in both cases: `d2 < 0` is never true, so the
        ! plain ball pays a compare it can never fail and needs no second code path.
        r2in = 0.0_real64
        if (present(r_inner)) then
            if (.not. (r_inner >= 0.0_real64)) error stop &
                "pf_spatial_index: the inner radius must be >= 0 and not NaN"
            if (r_inner > r) error stop &
                "pf_spatial_index: the inner radius must not exceed the outer radius"
            r2in = r_inner * r_inner
        end if
        want_sort = .false.
        if (present(sorted)) want_sort = sorted
        ! Ordering needs the distances the walk computes, so when the caller did not ask for them
        ! they go into a buffer of our own. Sizing it to `cap` is exact: nothing past `cap` is
        ! written, and nothing past `cap` can be ordered.
        usework = want_sort .and. .not. hasd .and. cap > 0_int64 .and. cap < huge(0_int64)
        if (usework) allocate (dwork(cap))

        nc = self%grid_n
        ! An index that owns its coordinates holds them in cell order; one that borrows them must
        ! reach every coordinate through the permutation instead. See the run loops below.
        direct = self%owns
        r2 = r * r
        p1 = p(1)
        p2 = p(2)
        p3 = p(3)
        call spatial_storage(self, xs, ys, zs)

        ! ---- HEALPix ----
        !
        ! **The branch is here and nowhere else, which is the whole point.** Every sky operation --
        ! `%within_sky`, `%all_within_sky`, `%pairs_within_sky`, `%count_all_within_sky`,
        ! `%nearest_sky`, `%kth_distance_sky` -- reaches this one procedure, so one branch gives
        ! all six the backend and none of them can be forgotten. It sits AFTER the shared setup
        ! and returns through the shared `scan_finish`, so the annulus test, the `min_key`/`keys`
        ! ranking `%pairs_within_sky` depends on, the `cap` truncation that makes `m` the TRUE
        ! count, and the `sorted=` ordering contract are all the same code on both backends. A
        ! second copy of any of those would be a second place for the two to disagree.
        if (self%backend_id == PF_SKY_HEALPIX) then
            call scan_healpix(self, p, r, m, cap, r2, r2in, minkey, want_min, direct, &
                              has32, has64, hasd, usework, xs, ys, zs, out32, out64, dist, &
                              keys, dwork, want_bnd, uslf, vslf, bnd_u, bnd_v)
            call scan_finish(want_sort, m, cap, dist, dwork, out32, out64)
            return
        end if

        if (.not. self%periodic_on) then
            do d = 1, 3
                call axis_span_free(p(d), r, self%lo(d), self%cell_inv(d), nc(d), a(d), cnt(d))
                if (cnt(d) == 0_int64) return
            end do
            ilo = a(1)
            ihi = a(1) + cnt(1) - 1_int64
            do kk = a(3), a(3) + cnt(3) - 1_int64
                do jj = a(2), a(2) + cnt(2) - 1_int64
                    base = 1_int64 + nc(1) * (jj + nc(2) * kk)
                    s0 = self%start(base + ilo)
                    e0 = self%start(base + ihi + 1_int64) - 1_int64
                    ! The fork is at the RUN, never inside the point loop: an index that owns its
                    ! coordinates has them in cell order, so stored position `t` addresses them
                    ! directly, while a `copy=.false.` index must reach them through the
                    ! permutation. Testing that per point would slow the default path to serve the
                    ! borrowed one -- which is exactly the trade `copy=` exists to let a caller
                    ! make, in the other direction.
                    if (direct) then
                        do t = s0, e0
                            dx = xs(t) - p1
                            dy = ys(t) - p2
                            dz = zs(t) - p3
                            d2 = dx * dx + dy * dy + dz * dz
                            if (d2 <= r2) then
                                if (d2 < r2in) cycle
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                if (want_bnd) then
                                    bnd = uslf * bnd_v(t) + vslf * bnd_u(t)
                                    if (d2 > bnd * bnd) cycle
                                end if
                                if (want_los) then
                                    ! The cylinder test, formed without cancellation: `D_j - D_i`
                                    ! is `(2 q.s + |s|^2) / (D_i + D_j)` rather than the difference
                                    ! of two thousand-unit distances, and the unit vectors' difference
                                    ! is `(s D_i - q (D_j - D_i)) / (D_i D_j)`, never `2 - 2 cos`.
                                    ! Keep the two copies of this block in step.
                                    ntest = ntest + 1_int64
                                    dj = self%d_s(t)
                                    sd = dself + dj
                                    ddif = (2.0_real64 * (q1 * dx + q2 * dy + q3 * dz) + d2) / sd
                                    ex = dx * dself - q1 * ddif
                                    ey = dy * dself - q2 * ddif
                                    ez = dz * dself - q3 * ddif
                                    dpv = sqrt(ex * ex + ey * ey + ez * ez) * (0.5_real64 * sd / (dself * dj))
                                    dlv = abs(los_ls(t) - lself)
                                    select case (lrule)
                                    case (0)
                                        ok = dpv <= bpself .and. dlv <= blself
                                    case (PF_LINK_MAX)
                                        own = dpv <= bpself .and. dlv <= blself
                                        oth = dpv <= los_bps(t) .and. dlv <= los_bls(t)
                                        if (want_tie) then
                                            ok = own .and. (.not. oth .or. keys(t) > minkey)
                                        else
                                            ok = own .or. oth
                                        end if
                                    case (PF_LINK_MIN)
                                        ok = dpv <= bpself .and. dlv <= blself .and. &
                                             dpv <= los_bps(t) .and. dlv <= los_bls(t)
                                    case (PF_LINK_MEAN)
                                        ok = 2.0_real64 * dpv <= bpself + los_bps(t) .and. &
                                             2.0_real64 * dlv <= blself + los_bls(t)
                                    case default
                                        ok = dpv <= bpself + los_bps(t) .and. dlv <= blself + los_bls(t)
                                    end select
                                    if (.not. ok) cycle
                                end if
                                row = self%idx(t)
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd .or. usework) then
                                        ! The reported measure: the distance, or for a line-of-sight
                                        ! query the normalised one, 1 on the cylinder's surface.
                                        dv = sqrt(d2)
                                        if (want_los) then
                                            dv = dpv / bpself
                                            if (dlv / blself > dv) dv = dlv / blself
                                        end if
                                        if (hasd) dist(m) = dv
                                        if (usework) dwork(m) = dv
                                    end if
                                    if (hasdp) dperp(m) = dpv
                                    if (hasdl) dpar(m) = dlv
                                end if
                            end if
                        end do
                    else
                        do t = s0, e0
                            row = self%idx(t)
                            dx = xs(row) - p1
                            dy = ys(row) - p2
                            dz = zs(row) - p3
                            d2 = dx * dx + dy * dy + dz * dz
                            if (d2 <= r2) then
                                if (d2 < r2in) cycle
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                if (want_bnd) then
                                    bnd = uslf * bnd_v(t) + vslf * bnd_u(t)
                                    if (d2 > bnd * bnd) cycle
                                end if
                                if (want_los) then
                                    ! The same block as the direct loop's; see the comment there.
                                    ntest = ntest + 1_int64
                                    dj = self%d_s(t)
                                    sd = dself + dj
                                    ddif = (2.0_real64 * (q1 * dx + q2 * dy + q3 * dz) + d2) / sd
                                    ex = dx * dself - q1 * ddif
                                    ey = dy * dself - q2 * ddif
                                    ez = dz * dself - q3 * ddif
                                    dpv = sqrt(ex * ex + ey * ey + ez * ez) * (0.5_real64 * sd / (dself * dj))
                                    dlv = abs(los_ls(t) - lself)
                                    select case (lrule)
                                    case (0)
                                        ok = dpv <= bpself .and. dlv <= blself
                                    case (PF_LINK_MAX)
                                        own = dpv <= bpself .and. dlv <= blself
                                        oth = dpv <= los_bps(t) .and. dlv <= los_bls(t)
                                        if (want_tie) then
                                            ok = own .and. (.not. oth .or. keys(t) > minkey)
                                        else
                                            ok = own .or. oth
                                        end if
                                    case (PF_LINK_MIN)
                                        ok = dpv <= bpself .and. dlv <= blself .and. &
                                             dpv <= los_bps(t) .and. dlv <= los_bls(t)
                                    case (PF_LINK_MEAN)
                                        ok = 2.0_real64 * dpv <= bpself + los_bps(t) .and. &
                                             2.0_real64 * dlv <= blself + los_bls(t)
                                    case default
                                        ok = dpv <= bpself + los_bps(t) .and. dlv <= blself + los_bls(t)
                                    end select
                                    if (.not. ok) cycle
                                end if
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd .or. usework) then
                                        dv = sqrt(d2)
                                        if (want_los) then
                                            dv = dpv / bpself
                                            if (dlv / blself > dv) dv = dlv / blself
                                        end if
                                        if (hasd) dist(m) = dv
                                        if (usework) dwork(m) = dv
                                    end if
                                    if (hasdp) dperp(m) = dpv
                                    if (hasdl) dpar(m) = dlv
                                end if
                            end if
                        end do
                    end if
                end do
            end do
            call scan_finish(want_sort, m, cap, dist, dwork, out32, out64, dperp, dpar)
            if (present(ntested)) ntested = ntested + ntest
            return
        end if

        ! ---- Periodic ----
        if (any(r > 0.5_real64 * self%wrap(1:self%ncoord))) error stop &
            "pf_spatial_index: a periodic search radius must not exceed half the box on any axis"
        do d = 1, 3
            if (self%wrap(d) > 0.0_real64) then
                call axis_span_wrapped(p(d), r, self%lo(d), self%cell_inv(d), nc(d), a(d), cnt(d))
            else
                call axis_span_free(p(d), r, self%lo(d), self%cell_inv(d), nc(d), a(d), cnt(d))
                if (cnt(d) == 0_int64) return
            end if
        end do
        ! The x sweep is one contiguous run, or two when it crosses the seam. Splitting it here
        ! keeps the prefix-sum trick that makes the inner loop two lookups per (j, k).
        if (self%wrap(1) > 0.0_real64) then
            ilo = modulo(a(1), nc(1))
            ihi = ilo + cnt(1) - 1_int64
            if (ihi <= nc(1) - 1_int64) then
                nrun = 1
                run_lo(1) = ilo
                run_hi(1) = ihi
            else
                nrun = 2
                run_lo(1) = ilo
                run_hi(1) = nc(1) - 1_int64
                run_lo(2) = 0_int64
                run_hi(2) = ihi - nc(1)
            end if
        else
            nrun = 1
            run_lo(1) = a(1)
            run_hi(1) = a(1) + cnt(1) - 1_int64
        end if
        w1 = self%wrap(1)
        w2 = self%wrap(2)
        w3 = self%wrap(3)
        wi1 = self%wrap_inv(1)
        wi2 = self%wrap_inv(2)
        wi3 = self%wrap_inv(3)
        do kk = a(3), a(3) + cnt(3) - 1_int64
            kc = kk
            if (self%wrap(3) > 0.0_real64) kc = modulo(kk, nc(3))
            do jj = a(2), a(2) + cnt(2) - 1_int64
                jc = jj
                if (self%wrap(2) > 0.0_real64) jc = modulo(jj, nc(2))
                base = 1_int64 + nc(1) * (jc + nc(2) * kc)
                do ir = 1, nrun
                    s0 = self%start(base + run_lo(ir))
                    e0 = self%start(base + run_hi(ir) + 1_int64) - 1_int64
                    if (direct) then
                        do t = s0, e0
                            ! Minimum image, branch-free: `wrap` and `wrap_inv` are both 0 on a
                            ! free axis, so the correction is `0 * anint(0)` there and vanishes.
                            dx = xs(t) - p1
                            dx = dx - w1 * anint(dx * wi1)
                            dy = ys(t) - p2
                            dy = dy - w2 * anint(dy * wi2)
                            dz = zs(t) - p3
                            dz = dz - w3 * anint(dz * wi3)
                            d2 = dx * dx + dy * dy + dz * dz
                            if (d2 <= r2) then
                                if (d2 < r2in) cycle
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                if (want_bnd) then
                                    bnd = uslf * bnd_v(t) + vslf * bnd_u(t)
                                    if (d2 > bnd * bnd) cycle
                                end if
                                row = self%idx(t)
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd) dist(m) = sqrt(d2)
                                    if (usework) dwork(m) = sqrt(d2)
                                end if
                            end if
                        end do
                    else
                        do t = s0, e0
                            row = self%idx(t)
                            dx = xs(row) - p1
                            dx = dx - w1 * anint(dx * wi1)
                            dy = ys(row) - p2
                            dy = dy - w2 * anint(dy * wi2)
                            dz = zs(row) - p3
                            dz = dz - w3 * anint(dz * wi3)
                            d2 = dx * dx + dy * dy + dz * dz
                            if (d2 <= r2) then
                                if (d2 < r2in) cycle
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                if (want_bnd) then
                                    bnd = uslf * bnd_v(t) + vslf * bnd_u(t)
                                    if (d2 > bnd * bnd) cycle
                                end if
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd) dist(m) = sqrt(d2)
                                    if (usework) dwork(m) = sqrt(d2)
                                end if
                            end if
                        end do
                    end if
                end do
            end do
        end do
        call scan_finish(want_sort, m, cap, dist, dwork, out32, out64)
    end procedure spatial_scan

    !> Applies `sorted=` at the end of a ball walk, from whichever buffer holds the distances.
    !>
    !> Two exits reach this rather than one, because the free and periodic walks are separate loop
    !> nests -- see this file's header for why they are kept apart.
    subroutine scan_finish(want_sort, m, cap, dist, dwork, out32, out64, dperp, dpar)
        logical, intent(in) :: want_sort !! whether the caller asked for an ordered result.
        integer(int64), intent(in) :: m !! the true count, which may exceed the buffers.
        integer(int64), intent(in) :: cap !! how many entries were actually written.
        real(real64), intent(inout), optional :: dist(:) !! the caller's distance buffer, if any.
        real(real64), intent(inout), optional :: dwork(:) !! our own, when the caller wanted none.
        integer(int32), intent(inout), optional :: out32(:) !! int32 rows, permuted with the keys.
        integer(int64), intent(inout), optional :: out64(:) !! int64 rows, permuted with the keys.
        real(real64), intent(inout), optional :: dperp(:) !! transverse separations, permuted with the rows.
        real(real64), intent(inout), optional :: dpar(:) !! parallel separations, permuted with the rows.
        integer(int64) :: nfill

        if (.not. want_sort) return
        nfill = min(m, cap)
        if (nfill < 2_int64) return
        if (present(dist)) then
            call spatial_order_by_dist(nfill, dist, out32=out32, out64=out64, dperp=dperp, dpar=dpar)
        else if (present(dwork)) then
            ! An unallocated `dwork` arrives here ABSENT (F2018 15.5.2.12), which is exactly the
            ! "the caller wanted no distances and no ordering either" case.
            call spatial_order_by_dist(nfill, dwork, out32=out32, out64=out64, dperp=dperp, dpar=dpar)
        end if
    end subroutine scan_finish

    module procedure spatial_scan_axis
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64) :: nc(3), sa(3), sc(3), ia, alo, acnt, jj, kk, base, s0, e0, t, cap, row, minkey
        integer(int64) :: ilo, ihi, nfill, ntest
        real(real64) :: dv(3), c0(3), c1(3)
        real(real64) :: dd, ddinv, rmax, dr, w0, w1, ta, tb, tlo, thi, rloc, ctr, half
        real(real64) :: vx, vy, vz, b1, b2, b3, qx, qy, qz, wx, wy, wz, tp, d2, rad
        real(real64) :: q1, q2, q3, e1, e2, e3, dself, lself, bpself, blself, dj, sd, ddif, sx, sy, sz, s2
        real(real64) :: ex, ey, ez, dpv, dlv, dm
        real(real64), allocatable :: dwork(:)
        logical :: has32, has64, hasd, direct, hasap, hasat, want_sort, usework
        logical :: want_min, want_los, want_tie, hasdp, hasdl, ok, own, oth
        integer :: k, da, nd, lrule

        m = 0_int64
        if (.not. self%built_ok) error stop "pf_spatial_index%" // what // &
            ": this index has not been built; call %build first"
        ! Refused rather than approximated. Under the minimum image an axis longer than the box
        ! wraps onto itself, so a point can be near the shape through more than one image and the
        ! ball search's `r <= L/2` guard has no equivalent here. See the guide page.
        if (self%periodic_on) error stop "pf_spatial_index%" // what // &
            ": an axis-shaped query is not supported on a periodic index; only %within is"
        if (.not. (r1 >= 0.0_real64) .or. .not. (r2 >= 0.0_real64)) error stop &
            "pf_spatial_index%" // what // ": every radius must be >= 0 and not NaN"
        ! Both ends, for the same reason %within screens its centre: the axis is divided by its
        ! own squared length and the parameter is then clamped into [0, 1] with `min`/`max`.
        call spatial_check_finite(p1, "pf_spatial_index%" // what, "axis endpoint coordinate")
        call spatial_check_finite(p2, "pf_spatial_index%" // what, "axis endpoint coordinate")
        if (self%npts == 0_int64) return
        has32 = present(out32)
        has64 = present(out64)
        hasd = present(dist)
        hasap = present(axis_point)
        hasat = present(axis_t)
        hasdp = present(dperp)
        hasdl = present(dpar)
        nd = self%ncoord
        if (hasap) then
            if (size(axis_point, 1) /= nd) error stop "pf_spatial_index%" // what // &
                ": axis_point's first extent must equal %ndim(), the rank the index was built with"
        end if
        want_min = present(min_key)
        minkey = 0_int64
        if (want_min) minkey = min_key
        ! Defensive, as `spatial_scan`'s: the line-of-sight worker passes each group complete, so
        ! none of these is reachable through a public entry point.
        if (want_min .and. .not. present(keys)) error stop & ! GCOVR_EXCL_LINE
            "pf_spatial_index%" // what // ": min_key needs keys" ! GCOVR_EXCL_LINE
        ! The line-of-sight accept, as `spatial_scan` sets it up; `e1..e3` is the EMITTER, which the
        ! separations are measured from -- `p1` is the padded axis's near end and would be wrong.
        want_los = present(los_rule)
        want_tie = .false.
        lrule = 0
        q1 = 0.0_real64
        q2 = 0.0_real64
        q3 = 0.0_real64
        e1 = 0.0_real64
        e2 = 0.0_real64
        e3 = 0.0_real64
        dself = 1.0_real64
        lself = 0.0_real64
        bpself = 0.0_real64
        blself = 0.0_real64
        dpv = 0.0_real64
        dlv = 0.0_real64
        ntest = 0_int64
        if (want_los) then
            if (.not. (present(los_q) .and. present(los_p) .and. present(los_d) .and. present(los_l) .and. & ! GCOVR_EXCL_LINE
                present(los_bp) .and. present(los_bl) .and. present(los_ls))) error stop & ! GCOVR_EXCL_LINE
                "pf_spatial_index%" // what // ": los_rule needs the emitter's terms and los_ls" ! GCOVR_EXCL_LINE
            lrule = los_rule
            if (lrule /= 0) then
                if (.not. (present(los_bps) .and. present(los_bls))) error stop & ! GCOVR_EXCL_LINE
                    "pf_spatial_index%" // what // ": a combine rule needs los_bps and los_bls" ! GCOVR_EXCL_LINE
            end if
            q1 = los_q(1)
            q2 = los_q(2)
            q3 = los_q(3)
            e1 = los_p(1)
            e2 = los_p(2)
            e3 = los_p(3)
            dself = los_d
            lself = los_l
            bpself = los_bp
            blself = los_bl
            ! The union's emit-once tiebreak, exactly as `spatial_scan` switches it: the rank no
            ! longer screens, and is read inside the accept instead.
            if (want_min) then
                if (present(los_tiebreak)) want_tie = los_tiebreak .and. lrule == PF_LINK_MAX
            end if
            if (want_tie) want_min = .false.
        else if (hasdp .or. hasdl) then
            error stop "pf_spatial_index%" // what // & ! GCOVR_EXCL_LINE
                ": dperp and dpar are line-of-sight outputs and need los_rule" ! GCOVR_EXCL_LINE
        end if
        if (has32 .and. self%npts > int(huge(0_int32), kind=int64)) error stop &
            "pf_spatial_index%" // what // &
            ": this index holds more rows than an int32 buffer can name; use an int64 one"
        cap = 0_int64
        if (has32 .or. has64 .or. hasd .or. hasap .or. hasat .or. hasdp .or. hasdl) cap = huge(0_int64)
        if (has32) cap = min(cap, size(out32, kind=int64))
        if (has64) cap = min(cap, size(out64, kind=int64))
        if (hasd) cap = min(cap, size(dist, kind=int64))
        if (hasap) cap = min(cap, size(axis_point, 2, kind=int64))
        if (hasat) cap = min(cap, size(axis_t, kind=int64))
        if (hasdp) cap = min(cap, size(dperp, kind=int64))
        if (hasdl) cap = min(cap, size(dpar, kind=int64))
        want_sort = .false.
        if (present(sorted)) want_sort = sorted
        usework = want_sort .and. .not. hasd .and. cap > 0_int64 .and. cap < huge(0_int64)
        if (usework) allocate (dwork(cap))

        dv = p2 - p1
        dd = dv(1) * dv(1) + dv(2) * dv(2) + dv(3) * dv(3)
        rmax = max(r1, r2)
        ! A zero-length axis has no direction to project onto, so all three shapes collapse to the
        ! same ball. Documented on each binding rather than left to be discovered.
        if (.not. (dd > 0.0_real64)) then
            ! Unreachable from the line-of-sight worker, whose axis always has positive length by ! GCOVR_EXCL_LINE
            ! the slack it adds; refused, because the ball below would measure from `p1`, not the ! GCOVR_EXCL_LINE
            ! emitter. ! GCOVR_EXCL_LINE
            if (want_los) error stop "pf_spatial_index%" // what // & ! GCOVR_EXCL_LINE
                ": a line-of-sight axis must have positive length" ! GCOVR_EXCL_LINE
            call spatial_scan(self, p1, rmax, m, out32=out32, out64=out64, dist=dist, sorted=sorted)
            ! Every returned point has the SAME foot -- `p1` is the whole segment -- so the axis
            ! outputs are constant and any ordering the ball applied leaves them correct. That is
            ! also why they may be filled to their own extent here rather than to a common cap.
            if (hasap) then
                do k = 1, int(min(m, size(axis_point, 2, kind=int64)))
                    axis_point(1:nd, k) = p1(1:nd)
                end do
            end if
            if (hasat) axis_t(1:int(min(m, size(axis_t, kind=int64)))) = 0.0_real64
            return
        end if
        ddinv = 1.0_real64 / dd
        dr = r2 - r1
        nc = self%grid_n
        direct = self%owns
        call spatial_storage(self, xs, ys, zs)
        vx = dv(1)
        vy = dv(2)
        vz = dv(3)
        b1 = p1(1)
        b2 = p1(2)
        b3 = p1(3)

        ! The axis travels furthest along this one, so a slab across it is a thin cross-section of
        ! the shape rather than a long smear -- which is the whole reason this is not a walk over
        ! the region's own bounding box.
        da = 1
        if (abs(dv(2)) > abs(dv(da))) da = 2
        if (abs(dv(3)) > abs(dv(da))) da = 3

        ctr = 0.5_real64 * (p1(da) + p2(da))
        half = 0.5_real64 * abs(dv(da)) + rmax
        call axis_span_free(ctr, half, self%lo(da), self%cell_inv(da), nc(da), alo, acnt)
        if (acnt == 0_int64) return

        do ia = alo, alo + acnt - 1_int64
            w0 = self%lo(da) + real(ia, kind=real64) * self%cell(da)
            w1 = w0 + self%cell(da)
            ! Which part of the axis can serve this slab: the slab widened by the largest radius,
            ! mapped back through the dominant component -- which cannot be zero, that being what
            ! `da` was chosen for -- and then held inside [0, 1]. Ordering the two before clamping
            ! is what makes a descending axis and a slab past either end all come out right.
            ta = (w0 - rmax - p1(da)) / dv(da)
            tb = (w1 + rmax - p1(da)) / dv(da)
            tlo = min(max(min(ta, tb), 0.0_real64), 1.0_real64)
            thi = min(max(max(ta, tb), 0.0_real64), 1.0_real64)
            c0 = p1 + tlo * dv
            c1 = p1 + thi * dv
            ! The radius is linear in the parameter, so its largest value over this slab's range is
            ! at one of the two ends. Taking it per slab rather than `max(r1, r2)` throughout is
            ! what keeps a steep cone's narrow end from walking the wide end's cells.
            rloc = max(r1 + tlo * dr, r1 + thi * dr)
            sa(da) = ia
            sc(da) = 1_int64
            do k = 1, 3
                if (k == da) cycle
                ctr = 0.5_real64 * (c0(k) + c1(k))
                half = 0.5_real64 * abs(c1(k) - c0(k)) + rloc
                call axis_span_free(ctr, half, self%lo(k), self%cell_inv(k), nc(k), sa(k), sc(k))
            end do
            if (sc(1) == 0_int64 .or. sc(2) == 0_int64 .or. sc(3) == 0_int64) cycle
            ilo = sa(1)
            ihi = sa(1) + sc(1) - 1_int64
            do kk = sa(3), sa(3) + sc(3) - 1_int64
                do jj = sa(2), sa(2) + sc(2) - 1_int64
                    base = 1_int64 + nc(1) * (jj + nc(2) * kk)
                    s0 = self%start(base + ilo)
                    e0 = self%start(base + ihi + 1_int64) - 1_int64
                    ! Forked at the RUN exactly as the ball walk is, and for the same reason: an
                    ! index that owns its coordinates has them in cell order, a borrowed one does
                    ! not, and testing that per point would charge the default path for it.
                    if (direct) then
                        do t = s0, e0
                            qx = xs(t) - b1
                            qy = ys(t) - b2
                            qz = zs(t) - b3
                            tp = (qx * vx + qy * vy + qz * vz) * ddinv
                            if (clamp) then
                                tp = min(max(tp, 0.0_real64), 1.0_real64)
                            else if (tp < 0.0_real64 .or. tp > 1.0_real64) then
                                cycle
                            end if
                            wx = qx - tp * vx
                            wy = qy - tp * vy
                            wz = qz - tp * vz
                            d2 = wx * wx + wy * wy + wz * wz
                            rad = r1 + tp * dr
                            if (d2 <= rad * rad) then
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                if (want_los) then
                                    ! The exact cylinder criterion, the geometric test above being
                                    ! only the pre-filter: the same block as `spatial_scan`'s, on
                                    ! separations from the EMITTER, never from the axis's near end.
                                    ! Keep the three copies in step.
                                    ntest = ntest + 1_int64
                                    sx = xs(t) - e1
                                    sy = ys(t) - e2
                                    sz = zs(t) - e3
                                    s2 = sx * sx + sy * sy + sz * sz
                                    dj = self%d_s(t)
                                    sd = dself + dj
                                    ddif = (2.0_real64 * (q1 * sx + q2 * sy + q3 * sz) + s2) / sd
                                    ex = sx * dself - q1 * ddif
                                    ey = sy * dself - q2 * ddif
                                    ez = sz * dself - q3 * ddif
                                    dpv = sqrt(ex * ex + ey * ey + ez * ez) * (0.5_real64 * sd / (dself * dj))
                                    dlv = abs(los_ls(t) - lself)
                                    select case (lrule)
                                    case (0)
                                        ok = dpv <= bpself .and. dlv <= blself
                                    case (PF_LINK_MAX)
                                        own = dpv <= bpself .and. dlv <= blself
                                        oth = dpv <= los_bps(t) .and. dlv <= los_bls(t)
                                        if (want_tie) then
                                            ok = own .and. (.not. oth .or. keys(t) > minkey)
                                        else
                                            ok = own .or. oth
                                        end if
                                    case (PF_LINK_MIN)
                                        ok = dpv <= bpself .and. dlv <= blself .and. &
                                             dpv <= los_bps(t) .and. dlv <= los_bls(t)
                                    case (PF_LINK_MEAN)
                                        ok = 2.0_real64 * dpv <= bpself + los_bps(t) .and. &
                                             2.0_real64 * dlv <= blself + los_bls(t)
                                    case default
                                        ok = dpv <= bpself + los_bps(t) .and. dlv <= blself + los_bls(t)
                                    end select
                                    if (.not. ok) cycle
                                end if
                                row = self%idx(t)
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd .or. usework) then
                                        ! The distance to the axis, or for a line-of-sight query
                                        ! the normalised measure, 1 on the cylinder's surface.
                                        dm = sqrt(d2)
                                        if (want_los) then
                                            dm = dpv / bpself
                                            if (dlv / blself > dm) dm = dlv / blself
                                        end if
                                        if (hasd) dist(m) = dm
                                        if (usework) dwork(m) = dm
                                    end if
                                    if (hasap) axis_point(1:nd, m) = p1(1:nd) + tp * dv(1:nd)
                                    if (hasat) axis_t(m) = tp
                                    if (hasdp) dperp(m) = dpv
                                    if (hasdl) dpar(m) = dlv
                                end if
                            end if
                        end do
                    else
                        do t = s0, e0
                            row = self%idx(t)
                            qx = xs(row) - b1
                            qy = ys(row) - b2
                            qz = zs(row) - b3
                            tp = (qx * vx + qy * vy + qz * vz) * ddinv
                            if (clamp) then
                                tp = min(max(tp, 0.0_real64), 1.0_real64)
                            else if (tp < 0.0_real64 .or. tp > 1.0_real64) then
                                cycle
                            end if
                            wx = qx - tp * vx
                            wy = qy - tp * vy
                            wz = qz - tp * vz
                            d2 = wx * wx + wy * wy + wz * wz
                            rad = r1 + tp * dr
                            if (d2 <= rad * rad) then
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                if (want_los) then
                                    ! The same block as the direct loop's; see the comment there.
                                    ntest = ntest + 1_int64
                                    sx = xs(row) - e1
                                    sy = ys(row) - e2
                                    sz = zs(row) - e3
                                    s2 = sx * sx + sy * sy + sz * sz
                                    dj = self%d_s(t)
                                    sd = dself + dj
                                    ddif = (2.0_real64 * (q1 * sx + q2 * sy + q3 * sz) + s2) / sd
                                    ex = sx * dself - q1 * ddif
                                    ey = sy * dself - q2 * ddif
                                    ez = sz * dself - q3 * ddif
                                    dpv = sqrt(ex * ex + ey * ey + ez * ez) * (0.5_real64 * sd / (dself * dj))
                                    dlv = abs(los_ls(t) - lself)
                                    select case (lrule)
                                    case (0)
                                        ok = dpv <= bpself .and. dlv <= blself
                                    case (PF_LINK_MAX)
                                        own = dpv <= bpself .and. dlv <= blself
                                        oth = dpv <= los_bps(t) .and. dlv <= los_bls(t)
                                        if (want_tie) then
                                            ok = own .and. (.not. oth .or. keys(t) > minkey)
                                        else
                                            ok = own .or. oth
                                        end if
                                    case (PF_LINK_MIN)
                                        ok = dpv <= bpself .and. dlv <= blself .and. &
                                             dpv <= los_bps(t) .and. dlv <= los_bls(t)
                                    case (PF_LINK_MEAN)
                                        ok = 2.0_real64 * dpv <= bpself + los_bps(t) .and. &
                                             2.0_real64 * dlv <= blself + los_bls(t)
                                    case default
                                        ok = dpv <= bpself + los_bps(t) .and. dlv <= blself + los_bls(t)
                                    end select
                                    if (.not. ok) cycle
                                end if
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd .or. usework) then
                                        dm = sqrt(d2)
                                        if (want_los) then
                                            dm = dpv / bpself
                                            if (dlv / blself > dm) dm = dlv / blself
                                        end if
                                        if (hasd) dist(m) = dm
                                        if (usework) dwork(m) = dm
                                    end if
                                    if (hasap) axis_point(1:nd, m) = p1(1:nd) + tp * dv(1:nd)
                                    if (hasat) axis_t(m) = tp
                                    if (hasdp) dperp(m) = dpv
                                    if (hasdl) dpar(m) = dlv
                                end if
                            end if
                        end do
                    end if
                end do
            end do
        end do
        if (want_sort) then
            nfill = min(m, cap)
            if (nfill >= 2_int64) then
                if (hasd) then
                    call spatial_order_by_dist(nfill, dist, out32=out32, out64=out64, &
                        axis_point=axis_point, axis_t=axis_t, dperp=dperp, dpar=dpar)
                else if (usework) then
                    call spatial_order_by_dist(nfill, dwork, out32=out32, out64=out64, &
                        axis_point=axis_point, axis_t=axis_t, dperp=dperp, dpar=dpar)
                end if
            end if
        end if
        if (present(ntested)) ntested = ntested + ntest
    end procedure spatial_scan_axis

    !> Orders a query's results by increasing distance, ties broken by ascending row index.
    module procedure spatial_order_by_dist
        integer(int64), allocatable :: perm(:), tmp64(:)
        integer(int32), allocatable :: tmp32(:)
        real(real64), allocatable :: tmpr(:), tmpp(:,:)
        integer(int64) :: k, e
        integer :: nd

        if (nfill < 2_int64) return
        ! Serial by construction: this runs per query, and a bulk sweep calls it from inside its
        ! own parallel region, where a nested team would be the caller's business rather than ours.
        call pf_argsort(d(1:nfill), perm, threads=1)
        ! The sort is stable, so an exact tie comes back in the order the WALK produced -- which is
        ! cell order, and cell order depends on the tuned cell size, which depends on the machine.
        ! Re-ordering each tied run by ascending row index is what makes `sorted=` mean the same
        ! thing everywhere. Exact ties are rare, so this is a scan and almost never a swap.
        if (present(out32) .or. present(out64)) then
            k = 1_int64
            do while (k < nfill)
                e = k
                do while (e < nfill)
                    if (d(perm(e + 1_int64)) /= d(perm(k))) exit
                    e = e + 1_int64
                end do
                if (e > k) call sort_run_by_row(perm(k:e), out32, out64)
                k = e + 1_int64
            end do
        end if
        allocate (tmpr(nfill))
        do k = 1_int64, nfill
            tmpr(k) = d(perm(k))
        end do
        d(1:nfill) = tmpr
        if (present(out32)) then
            allocate (tmp32(nfill))
            do k = 1_int64, nfill
                tmp32(k) = out32(perm(k))
            end do
            out32(1:nfill) = tmp32
        end if
        if (present(out64)) then
            allocate (tmp64(nfill))
            do k = 1_int64, nfill
                tmp64(k) = out64(perm(k))
            end do
            out64(1:nfill) = tmp64
        end if
        if (present(axis_t)) then
            do k = 1_int64, nfill
                tmpr(k) = axis_t(perm(k))
            end do
            axis_t(1:nfill) = tmpr
        end if
        if (present(dperp)) then
            do k = 1_int64, nfill
                tmpr(k) = dperp(perm(k))
            end do
            dperp(1:nfill) = tmpr
        end if
        if (present(dpar)) then
            do k = 1_int64, nfill
                tmpr(k) = dpar(perm(k))
            end do
            dpar(1:nfill) = tmpr
        end if
        if (present(axis_point)) then
            nd = size(axis_point, 1)
            allocate (tmpp(nd, nfill))
            do k = 1_int64, nfill
                tmpp(:, k) = axis_point(:, perm(k))
            end do
            axis_point(:, 1:nfill) = tmpp
        end if
    end procedure spatial_order_by_dist

    !> Sorts one run of tied positions into ascending row order, in place.
    !>
    !> An insertion sort because a run of EXACTLY equal distances is nearly always one element and
    !> essentially never long; the loop above only calls this when a run has at least two.
    subroutine sort_run_by_row(run, out32, out64)
        integer(int64), intent(inout) :: run(:) !! positions into the result buffers.
        integer(int32), intent(in), optional :: out32(:) !! int32 rows, when that is the buffer in use.
        integer(int64), intent(in), optional :: out64(:) !! int64 rows, when that is the buffer in use.
        integer(int64) :: a, b, hold, key

        do a = 2_int64, size(run, kind=int64)
            hold = run(a)
            key = row_at(hold, out32, out64)
            b = a - 1_int64
            do while (b >= 1_int64)
                if (row_at(run(b), out32, out64) <= key) exit
                run(b + 1_int64) = run(b)
                b = b - 1_int64
            end do
            run(b + 1_int64) = hold
        end do
    end subroutine sort_run_by_row

    !> The caller's row index sitting at result position `k`, from whichever buffer holds it.
    integer(int64) function row_at(k, out32, out64) result(row)
        integer(int64), intent(in) :: k !! the result position.
        integer(int32), intent(in), optional :: out32(:) !! int32 rows.
        integer(int64), intent(in), optional :: out64(:) !! int64 rows.

        if (present(out64)) then
            row = out64(k)
        else
            row = int(out32(k), kind=int64)
        end if
    end function row_at

    !> Counts the cells a ball would visit and the points it would distance-test.
    module procedure spatial_scan_work
        integer(int64) :: a(3), cnt(3), jj, kk, jc, kc, base, ilo, ihi
        integer(int64) :: run_lo(2), run_hi(2)
        integer :: d, nrun, ir

        do d = 1, 3
            if (wrap(d) > 0.0_real64) then
                call axis_span_wrapped(p(d), r, lo(d), cell_inv(d), nc(d), a(d), cnt(d))
            else
                call axis_span_free(p(d), r, lo(d), cell_inv(d), nc(d), a(d), cnt(d))
                if (cnt(d) == 0_int64) return
            end if
        end do
        if (wrap(1) > 0.0_real64) then
            ilo = modulo(a(1), nc(1))
            ihi = ilo + cnt(1) - 1_int64
            if (ihi <= nc(1) - 1_int64) then
                nrun = 1
                run_lo(1) = ilo
                run_hi(1) = ihi
            else
                nrun = 2
                run_lo(1) = ilo
                run_hi(1) = nc(1) - 1_int64
                run_lo(2) = 0_int64
                run_hi(2) = ihi - nc(1)
            end if
        else
            nrun = 1
            run_lo(1) = a(1)
            run_hi(1) = a(1) + cnt(1) - 1_int64
        end if
        cells = cells + cnt(1) * cnt(2) * cnt(3)
        ! Points are counted from the prefix sums, so this is O(cells along y and z) rather than
        ! O(cells) -- which is what makes evaluating a candidate cheap enough to do three times.
        do kk = a(3), a(3) + cnt(3) - 1_int64
            kc = kk
            if (wrap(3) > 0.0_real64) kc = modulo(kk, nc(3))
            do jj = a(2), a(2) + cnt(2) - 1_int64
                jc = jj
                if (wrap(2) > 0.0_real64) jc = modulo(jj, nc(2))
                base = 1_int64 + nc(1) * (jc + nc(2) * kc)
                do ir = 1, nrun
                    pts = pts + start(base + run_hi(ir) + 1_int64) - start(base + run_lo(ir))
                end do
            end do
        end do
    end procedure spatial_scan_work

    !> The `kk` nearest points to `p`, by a ball that expands until it holds enough of them.
    module procedure spatial_shell_search
        integer(int64) :: cnt, cnt2, rounds
        real(real64) :: r, rcap, cd, grow, ext, ss
        integer :: dm, ax

        if (kk < 1_int64) error stop "pf_spatial_index%" // what // ": k must be >= 1"
        dm = max(self%dims_eff, 1)

        ! How far the ball may ever grow, and the three metrics answer it differently. A periodic
        ! ball beyond half the box is undefined rather than imprecise, so that is a hard stop and a
        ! failure to reach `kk` inside it is an error. The other two caps are simply "everything is
        ! already inside", so reaching them cannot leave the search short.
        if (self%periodic_on) then
            rcap = huge(0.0_real64)
            do ax = 1, self%ncoord
                if (self%wrap(ax) > 0.0_real64) rcap = min(rcap, 0.5_real64 * self%wrap(ax))
            end do
        else if (self%metric_id == PF_METRIC_SKY) then
            ! The chord of 180 degrees, NOT the 90-degree limit %within_sky enforces: that limit
            ! judges a radius the caller chose, and this one is derived.
            rcap = 2.0_real64
        else
            ss = 0.0_real64
            do ax = 1, self%ncoord
                ext = max(abs(p(ax) - self%lo(ax)), abs(p(ax) - self%hi(ax)))
                ss = ss + ext * ext
            end do
            rcap = sqrt(ss)
        end if

        ! Where to start. A forced start wins outright (that is what it is for); otherwise a radius
        ! carried over from a neighbouring query, which is what makes the bulk sweep cheap; failing
        ! both, the radius that would hold `kk+1` points at the density the tuner already measured.
        if (dbg_shell_start > 0.0_real64) then
            r = dbg_shell_start
        else if (r_seed > 0.0_real64) then
            r = r_seed
        else if (self%rho > 0.0_real64) then
            select case (dm)
            case (1)
                cd = 2.0_real64
            case (2)
                cd = 3.141592653589793_real64
            case default
                cd = 4.1887902047863905_real64
            end select
            r = spatial_shell_safety * (real(kk + 1_int64, kind=real64) / (cd * self%rho)) &
                ** (1.0_real64 / real(dm, kind=real64))
        else
            r = self%cell_side ! GCOVR_EXCL_LINE
        end if
        if (.not. (r > 0.0_real64)) r = 1.0_real64
        if (r > rcap) r = rcap

        rounds = 0_int64
        do
            call spatial_scan(self, p, r, cnt)
            rounds = rounds + 1_int64
            if (cnt >= kk) exit
            if (r >= rcap) exit
            ! Rescaled from what the ball actually held rather than doubled: the count grows as
            ! r**dm, so this lands close in one further round where a fixed factor takes several.
            grow = (real(kk, kind=real64) / real(max(cnt, 1_int64), kind=real64)) &
                ** (1.0_real64 / real(dm, kind=real64))
            r = r * max(grow, 1.0_real64) * spatial_shell_grow
            if (r > rcap) r = rcap
        end do
        if (cnt < kk) then
            if (self%periodic_on) error stop "pf_spatial_index%" // what // &
                ": a periodic index cannot answer for this k -- half the box does not hold that " // &
                "many neighbours, and a periodic ball beyond half the box is undefined"
            ! Unreachable: on a free or sky index the cap encloses every point, so the last scan
            ! saw all npts and the caller has already clamped kk to that.
            error stop "pf_spatial_index%" // what // ": the expanding ball did not reach k" ! GCOVR_EXCL_LINE
        end if

        if (.not. allocated(rows)) then
            allocate (rows(max(cnt, 1_int64)))
        else if (size(rows, kind=int64) < cnt) then
            deallocate (rows)
            allocate (rows(cnt))
        end if
        if (.not. allocated(dists)) then
            allocate (dists(max(cnt, 1_int64)))
        else if (size(dists, kind=int64) < cnt) then
            deallocate (dists)
            allocate (dists(cnt))
        end if
        ! The second pass is what makes the result exact: the ball at `r` holds every point closer
        ! than the kk-th, so ordering what it returned and keeping the first kk is the answer.
        call spatial_scan(self, p, r, cnt2, out64=rows(1:cnt), dist=dists(1:cnt), sorted=.true.)
        if (cnt2 /= cnt) error stop "pf_spatial_index%" // what // & ! GCOVR_EXCL_LINE
            ": the index changed between the two passes of one query" ! GCOVR_EXCL_LINE
        r_seed = r
        !$omp atomic
        dbg_shell_rounds = dbg_shell_rounds + rounds
    end procedure spatial_shell_search

end submodule parquet_spatial_query ! GCOVR_EXCL_LINE
