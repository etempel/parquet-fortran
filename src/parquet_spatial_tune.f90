!> Choosing the cell size: a cost model for a bracket, then a deterministic probe to pick from it.
!!
!! **The model alone is not good enough and that was measured, not feared.** Solving
!! `dT/dh = 0` on `T(h) ~ c_cell*((2r+h)/h)^3 + c_point*(2r+h)^3*rho` gives
!! `h = kappa * r * N_r^(-1/4)`, and back-solving `kappa` from the best cell of each of the gate's
!! four swept fixtures gives 2.95, 3.14, 4.51 and 1.68 -- a spread of 2.7. Shipping the fitted
!! centre costs +42.9% on the clustered fixture, which is most of the margin the whole engine
!! decision was made on.
!!
!! **So the model supplies a BRACKET and a work-count probe picks the winner.** For each candidate
!! the probe counts the cells a query would visit and the points it would distance-test -- no
!! timing, so the answer is reproducible run to run and machine to machine -- and ranks them by
!! `A*cells + B*points`. Measured over 18 overlapping sweep triples, `A/B = 2` picks the timed
!! winner 15 times out of 18 with a mean penalty of 1.17%, and ANY ratio from 0.5 to 16 stays under
!! 3.2%. A constant that tolerates being wrong by 8x replaced one that had to be right within 1.3x.
!!
!! **Evaluating a candidate needs only the counting half of a build** -- the cell index, the counts
!! and the prefix sum -- never the sort, the scatter or the coordinate reorder. That is what makes
!! measuring three candidates cheaper than one extra build rather than two.
submodule (parquet_spatial) parquet_spatial_tune
    implicit none

    !> Most candidates one tuning run may evaluate: the three-point bracket, a widening step at
    !> each end, and the two refinement points either side of the winner.
    integer, parameter :: max_candidates = 3 + 2 * spatial_probe_widen + 2

contains

    !> `r_eff = sum(r^3) / sum(r^2)`, the one radius a whole list collapses to.
    !>
    !> Summing the per-radius cost model over a set and solving `dT/dh = 0` leaves exactly this
    !> cubic-over-quadratic moment ratio, which reduces to `r` when the list has one entry. Weights
    !> come free if the relative frequency of each radius is ever known: weight both sums.
    module procedure spatial_effective_radius
        real(real64) :: s2, s3

        s2 = sum(radii * radii)
        s3 = sum(radii * radii * radii)
        r = 1.0_real64
        if (s2 > 0.0_real64) r = s3 / s2
    end procedure spatial_effective_radius

    !> The cost model's cell size.
    module procedure spatial_model_cell
        real(real64), parameter :: four_thirds_pi = 4.18879020478639098_real64
        real(real64) :: nr

        if (.not. (rho > 0.0_real64)) then
            h = r_eff
            return
        end if
        ! h^(1+ndim) = kappa^4 * r_eff / ((4/3) pi rho). The exponent carries the dimension --
        ! 1/4 in 3D, 1/3 in 2D, 1/2 in 1D -- and falls out of the SAME formula because `rho` is a
        ! density in the cloud's own dimension. There is no 2D branch anywhere for that reason.
        nr = spatial_kappa ** 4 * r_eff / (four_thirds_pi * rho)
        h = nr ** (1.0_real64 / real(1 + ndim, kind=real64))
        if (.not. (h > 0.0_real64)) h = r_eff
    end procedure spatial_model_cell

    !> Measures the local density from the median occupied cell of a provisional coarse grid.
    module procedure spatial_measure_density
        integer(int64), allocatable :: cnt(:), occ(:), perm(:)
        integer(int64) :: nc(3), ncells, i, c, nocc, med
        real(real64) :: inv(3), ext(3), vol, h0, h_eff
        integer :: d

        ext = self%hi - self%lo
        ndim = count(ext > 0.0_real64)
        rho = 0.0_real64
        if (self%npts <= 0_int64) then
            ndim = max(ndim, 1)
            return
        end if
        if (ndim == 0) then
            ! Every point coincident: any cell size answers, and the grid is 1x1x1 whatever this
            ! returns. Report a 3D density so the model stays on its ordinary branch.
            ndim = 3
            rho = real(self%npts, kind=real64)
            return
        end if
        vol = 1.0_real64
        do d = 1, 3
            if (ext(d) > 0.0_real64) vol = vol * ext(d)
        end do
        ! A DEGENERATE axis must be left out of both the product and the root, or a flat cloud --
        ! which is what every 2D index is -- gives `(0/n)^(1/3) == 0` and the provisional bucketing
        ! is undefined. The density estimator downstream of this is self-correcting; this step is
        ! not, which is why the guard is here and explicit.
        h0 = (vol / real(self%npts, kind=real64)) ** (1.0_real64 / real(ndim, kind=real64))
        h0 = h0 * 4.0_real64
        if (.not. (h0 > 0.0_real64)) then
            rho = real(self%npts, kind=real64) / max(vol, tiny(1.0_real64))
            return
        end if
        call spatial_bucket_counts(self, h0, h_eff, nc, inv, cnt)
        ncells = size(cnt, kind=int64)
        nocc = count(cnt > 0_int64, kind=int64)
        if (nocc <= 0_int64) then
            rho = real(self%npts, kind=real64) / max(vol, tiny(1.0_real64))
            return
        end if
        allocate (occ(nocc))
        c = 0_int64
        do i = 1_int64, ncells
            if (cnt(i) > 0_int64) then
                c = c + 1_int64
                occ(c) = cnt(i)
            end if
        end do
        deallocate (cnt)
        ! The MEDIAN over occupied cells, never `n / bounding-box volume`: the box average
        ! describes nowhere for clumped data (most points sit far above it) and understates a
        ! sparse geometry, and the sky case -- a zero-thickness shell inside its bounding cube --
        ! is the sharpest instance of the same problem rather than a new one.
        call pf_argsort(occ, perm)
        med = occ(perm((nocc + 1_int64) / 2_int64))
        ! `h_eff`, not `h0`: the counting pass applies the same cell-count clamp %build does, so
        ! the cell whose volume this divides by is the one the counts were actually taken on.
        rho = real(med, kind=real64) / (h_eff ** ndim)
    end procedure spatial_measure_density

    !> Chooses the cell side: model for a bracket, deterministic probe to pick from it.
    module procedure spatial_choose_cell
#ifdef _OPENMP
        use omp_lib, only: omp_in_parallel
#endif
        real(real64) :: cand(max_candidates), work(max_candidates)
        real(real64) :: rho, r_eff, hm
        integer :: ndim, k, best, ncand, widened_lo, widened_hi
        logical :: in_region

        call spatial_measure_density(self, ndim, rho)
        self%rho = rho
        self%dims_eff = ndim
        r_eff = spatial_effective_radius(radii)
        hm = spatial_model_cell(ndim, rho, r_eff)
        h = hm
        dbg_probe_count = 0_int64

        in_region = .false.
#ifdef _OPENMP
        in_region = omp_in_parallel()
#endif
        ! Every thread would run its own probe to reach the same answer, multiplying the cost by
        ! the team size. This is a COST argument only: with a deterministic proxy there is no
        ! longer a correctness reason, because nothing is being timed.
        if (in_region) return
        if (self%npts < 2_int64) return

        ncand = 3
        cand(1) = hm / spatial_probe_step
        cand(2) = hm
        cand(3) = hm * spatial_probe_step
        do k = 1, ncand
            work(k) = spatial_probe_cost(self, cand(k), radii)
        end do
        widened_lo = 0
        widened_hi = 0
        do
            best = minloc(work(1:ncand), dim=1)
            ! An endpoint winner means the true optimum may lie beyond the bracket -- the gate's
            ! wedge fixture picked the low end -- so step out once more rather than accept it.
            if (best == 1 .and. widened_lo < spatial_probe_widen) then
                widened_lo = widened_lo + 1
                cand(2:ncand + 1) = cand(1:ncand)
                work(2:ncand + 1) = work(1:ncand)
                ncand = ncand + 1
                cand(1) = cand(2) / spatial_probe_step
                work(1) = spatial_probe_cost(self, cand(1), radii)
            else if (best == ncand .and. widened_hi < spatial_probe_widen) then
                widened_hi = widened_hi + 1
                ncand = ncand + 1
                cand(ncand) = cand(ncand - 1) * spatial_probe_step
                work(ncand) = spatial_probe_cost(self, cand(ncand), radii)
            else
                exit
            end if
        end do
        best = minloc(work(1:ncand), dim=1)
        ! REFINE once, at sqrt(2) either side of the winner. The bracket steps by 2, so its answer
        ! carries up to a half-step of granularity even when its ranking is perfect -- measured at
        ! 12.9% on the sparse wedge fixture, where the probe correctly picked the best of its own
        ! three candidates and the true optimum lay between two of them. Two more candidates cost
        ! about a third of one extra build.
        hm = cand(best)
        ncand = ncand + 1
        cand(ncand) = hm / spatial_refine_step
        work(ncand) = spatial_probe_cost(self, cand(ncand), radii)
        ncand = ncand + 1
        cand(ncand) = hm * spatial_refine_step
        work(ncand) = spatial_probe_cost(self, cand(ncand), radii)
        best = minloc(work(1:ncand), dim=1)
        h = cand(best)
        dbg_probe_count = int(ncand, kind=int64)
    end procedure spatial_choose_cell

    !> The proxy cost of one candidate cell size: `A*cells_visited + B*points_tested`, summed over
    !> a deterministic draw of probe queries.
    !>
    !> Needs only the counting half of a build, so it is far cheaper than building the candidate.
    function spatial_probe_cost(self, h, radii) result(w)
        type(pf_spatial_index), intent(in), target :: self !! the index whose points are probed.
        real(real64), intent(in) :: h !! the candidate cell side.
        real(real64), intent(in) :: radii(:) !! the radii probe queries are drawn from.
        real(real64) :: w !! the proxy cost; larger is worse.
        integer(int64) :: cells, pts

        call spatial_probe_counts(self, h, radii, cells, pts)
        w = spatial_work_a * real(cells, kind=real64) + spatial_work_b * real(pts, kind=real64)
    end function spatial_probe_cost

    !> Counts the cells and points a deterministic draw of probe queries would touch at cell `h`.
    subroutine spatial_probe_counts(self, h, radii, cells, pts)
        type(pf_spatial_index), intent(in), target :: self !! the index whose points are probed.
        real(real64), intent(in) :: h !! the candidate cell side.
        real(real64), intent(in) :: radii(:) !! the radii probe queries are drawn from.
        integer(int64), intent(out) :: cells !! cells the probe queries would visit in total.
        integer(int64), intent(out) :: pts !! points they would distance-test in total.
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64), allocatable :: cnt(:), start(:)
        integer(int64) :: nc(3), ncells, i, c, pos, stride
        real(real64) :: inv(3), p(3), r, h_eff
        integer :: t, nq, nr

        call spatial_bucket_counts(self, h, h_eff, nc, inv, cnt)
        ncells = size(cnt, kind=int64)
        allocate (start(ncells + 1_int64))
        start(1) = 1_int64
        do c = 1_int64, ncells
            start(c + 1_int64) = start(c) + cnt(c)
        end do
        deallocate (cnt)
        call spatial_storage(self, xs, ys, zs)
        cells = 0_int64
        pts = 0_int64
        nq = spatial_probe_points
        if (int(nq, kind=int64) > self%npts) nq = int(self%npts)
        nr = size(radii)
        ! A UNIFORM draw over stored rows. For a self-join the query points ARE the data, so this
        ! is exact rather than a proxy; and for a clustered cloud a uniform draw over rows is
        ! already density-weighted, which is the correct weighting rather than a bias to correct.
        ! Stepping by a golden-ratio stride keeps it deterministic and needs no generator -- which
        ! matters, because importing one would put another module in every consumer's build.
        stride = max(1_int64, int(0.6180339887498949_real64 * real(self%npts, kind=real64), kind=int64))
        pos = 0_int64
        do t = 1, nq
            i = pos + 1_int64
            p(1) = xs(i)
            p(2) = ys(i)
            p(3) = zs(i)
            r = radii(1 + mod(t - 1, nr))
            call spatial_scan_work(nc, self%lo, inv, start, self%wrap, p, r, cells, pts)
            pos = modulo(pos + stride, self%npts)
        end do
    end subroutine spatial_probe_counts

    !> The two counts the probe ranks by, for any cell size, exposed for re-fitting `A/B`.
    module procedure parquet_debug_spatial_work

        cells = 0_int64
        points = 0_int64
        if (.not. index%built_ok) return
        call spatial_probe_counts(index, h, [radius], cells, points)
    end procedure parquet_debug_spatial_work

    !> Counts how many points fall in each cell of a candidate grid, applying the same cell-count
    !> clamp `%build` would.
    subroutine spatial_bucket_counts(self, h, h_eff, nc, inv, cnt)
        type(pf_spatial_index), intent(in), target :: self !! the index whose points are counted.
        real(real64), intent(in) :: h !! the candidate cell side.
        real(real64), intent(out) :: h_eff !! the side actually used, after the cell-count clamp.
        integer(int64), intent(out) :: nc(3) !! cells along each axis, after any clamp.
        real(real64), intent(out) :: inv(3) !! 1/cell per axis, for the grid the counts describe.
        integer(int64), allocatable, intent(out) :: cnt(:) !! points in each cell.
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        real(real64) :: hh, cellv(3), rc, ratio
        integer(int64) :: ncells, maxc, i, c, ii(3)
        integer :: d, it

        hh = h
        maxc = max(1_int64, int(spatial_max_cells_per_point * real(self%npts, kind=real64), kind=int64))
        do it = 1, 64
            call spatial_grid_dims(self%lo, self%hi, self%wrap, hh, nc, cellv, inv)
            rc = real(nc(1), kind=real64) * real(nc(2), kind=real64) * real(nc(3), kind=real64)
            if (rc <= real(maxc, kind=real64)) exit
            ratio = (rc / real(maxc, kind=real64)) ** (1.0_real64 / 3.0_real64)
            hh = hh * max(1.001_real64, ratio)
        end do
        h_eff = hh
        ncells = nc(1) * nc(2) * nc(3)
        allocate (cnt(ncells))
        cnt = 0_int64
        call spatial_storage(self, xs, ys, zs)
        do i = 1_int64, self%npts
            ii(1) = floor((xs(i) - self%lo(1)) * inv(1), kind=int64)
            ii(2) = floor((ys(i) - self%lo(2)) * inv(2), kind=int64)
            ii(3) = floor((zs(i) - self%lo(3)) * inv(3), kind=int64)
            do d = 1, 3
                if (self%wrap(d) > 0.0_real64) then
                    ii(d) = modulo(ii(d), nc(d))
                else
                    ii(d) = min(max(ii(d), 0_int64), nc(d) - 1_int64)
                end if
            end do
            c = 1_int64 + ii(1) + nc(1) * (ii(2) + nc(2) * ii(3))
            cnt(c) = cnt(c) + 1_int64
        end do
    end subroutine spatial_bucket_counts

end submodule parquet_spatial_tune ! GCOVR_EXCL_LINE
