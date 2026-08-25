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

    !> Walks the cells a ball of radius `r` about `p` can reach and reports what it finds.
    module procedure spatial_scan
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64) :: nc(3), a(3), cnt(3)
        integer(int64) :: ii, jj, kk, jc, kc, base, s0, e0, t, cap, row, minkey
        integer(int64) :: run_lo(2), run_hi(2), ilo, ihi
        real(real64) :: r2, dx, dy, dz, d2, p1, p2, p3
        real(real64) :: w1, w2, w3, wi1, wi2, wi3
        logical :: has32, has64, hasd, want_min, direct
        integer :: d, nrun, ir

        m = 0_int64
        if (.not. self%built_ok) error stop &
            "pf_spatial_index: this index has not been built; call %build first"
        if (.not. (r >= 0.0_real64)) error stop &
            "pf_spatial_index: the search radius must be >= 0 and not NaN"
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
        if (has32 .and. self%npts > int(huge(0_int32), kind=int64)) error stop &
            "pf_spatial_index%within: this index holds more rows than an int32 buffer can name; use an int64 one"
        cap = 0_int64
        if (has32 .or. has64 .or. hasd) cap = huge(0_int64)
        if (has32) cap = min(cap, size(out32, kind=int64))
        if (has64) cap = min(cap, size(out64, kind=int64))
        if (hasd) cap = min(cap, size(dist, kind=int64))

        nc = self%grid_n
        ! An index that owns its coordinates holds them in cell order; one that borrows them must
        ! reach every coordinate through the permutation instead. See the run loops below.
        direct = self%owns
        r2 = r * r
        p1 = p(1)
        p2 = p(2)
        p3 = p(3)
        call spatial_storage(self, xs, ys, zs)

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
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                row = self%idx(t)
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd) dist(m) = sqrt(d2)
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
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd) dist(m) = sqrt(d2)
                                end if
                            end if
                        end do
                    end if
                end do
            end do
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
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                row = self%idx(t)
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd) dist(m) = sqrt(d2)
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
                                if (want_min) then
                                    if (keys(t) <= minkey) cycle
                                end if
                                m = m + 1_int64
                                if (m <= cap) then
                                    if (has32) out32(m) = int(row, kind=int32)
                                    if (has64) out64(m) = row
                                    if (hasd) dist(m) = sqrt(d2)
                                end if
                            end if
                        end do
                    end if
                end do
            end do
        end do
    end procedure spatial_scan

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

end submodule parquet_spatial_query ! GCOVR_EXCL_LINE
