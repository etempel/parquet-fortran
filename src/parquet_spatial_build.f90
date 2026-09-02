!> Building, rebuilding and the grid itself: everything that turns coordinate arrays into cells.
!!
!! The grid is deliberately DENSE -- `start` has one entry per cell whether or not the cell holds
!! anything -- because a query visits 27 or more cells and an occupied-cells-only representation
!! costs a lookup at each of them. The gate's sparse-wedge fixture, data filling a few per cent of
!! its bounding box, is what said that is affordable: the cell-count clamp coarsens the grid before
!! the array gets large, and an empty cell is skipped in a couple of instructions.
!!
!! **Points outside a periodic box are never moved.** `modulo` in `spatial_cell_of` puts such a
!! point in the cell of its image and the minimum-image distance test answers about that image, so
!! "wrap silently" holds by construction rather than by mutating the caller's data -- which is also
!! what lets `copy=.false.` and periodic boundaries be used together.
submodule (parquet_spatial) parquet_spatial_build
    implicit none

contains

    !> Aims three local pointers at wherever the coordinates actually live.
    !>
    !> One pointer assignment per query, and nothing in the inner loop knows whether the index owns
    !> its coordinates or borrows them -- which is the whole reason `copy=` costs no branch.
    module procedure spatial_storage

        if (associated(self%px)) then
            xs => self%px
        else
            xs => self%xs
        end if
        if (associated(self%py)) then
            ys => self%py
        else
            ys => self%ys
        end if
        if (associated(self%pz)) then
            zs => self%pz
        else
            zs => self%zs
        end if
    end procedure spatial_storage

    !> The cell a point falls in, 1-based, for the index's current grid.
    module procedure spatial_cell_of
        integer(int64) :: ii(3)
        real(real64) :: q(3)
        integer :: d

        q = [px, py, pz]
        do d = 1, 3
            ii(d) = floor((q(d) - self%lo(d)) * self%cell_inv(d), kind=int64)
            if (self%wrap(d) > 0.0_real64) then
                ii(d) = modulo(ii(d), self%grid_n(d))
            else
                ii(d) = min(max(ii(d), 0_int64), self%grid_n(d) - 1_int64)
            end if
        end do
        c = 1_int64 + ii(1) + self%grid_n(1) * (ii(2) + self%grid_n(2) * ii(3))
    end procedure spatial_cell_of

    !> Cells per axis, the per-axis cell side and its reciprocal, for one candidate cell size.
    module procedure spatial_grid_dims
        integer :: d
        real(real64) :: t

        do d = 1, 3
            if (wrap(d) > 0.0_real64) then
                ! A periodic axis must be tiled EXACTLY, or the seam cell is a different width and
                ! every query crossing it is wrong by a fraction of a cell -- silently. So the cell
                ! count comes first and the side follows from it.
                t = wrap(d) / h
                if (t > 1.0e15_real64) t = 1.0e15_real64
                nc(d) = max(1_int64, int(t, kind=int64))
                cell(d) = wrap(d) / real(nc(d), kind=real64)
            else
                t = (hi(d) - lo(d)) / h + 1.0_real64
                if (t > 1.0e15_real64) t = 1.0e15_real64
                nc(d) = max(1_int64, int(t, kind=int64))
                cell(d) = h
            end if
            cell_inv(d) = 1.0_real64 / cell(d)
        end do
    end procedure spatial_grid_dims

    !> Fixes the grid for cell side `h`, coarsening when the cell count would leave the counting path.
    module procedure spatial_set_grid
        real(real64) :: hh, rc, ratio, cellv(3), inv(3)
        integer(int64) :: nc(3), maxc
        integer :: it

        if (present(coarsened)) coarsened = .false.
        hh = h
        if (.not. (hh > 0.0_real64)) error stop "pf_spatial_index: the cell side must be > 0"
        ! The ceiling is points-per-cell, not a copy of cubesort's cells-per-point budget: below
        ! 0.3*n cells the bucketing keeps pf_argsort's serial counting fast path, and cubesort's
        ! own rule (8*n cells) is 27x past that and would never see it.
        maxc = max(1_int64, int(spatial_max_cells_per_point * real(self%npts, kind=real64), kind=int64))
        do it = 1, 64
            call spatial_grid_dims(self%lo, self%hi, self%wrap, hh, nc, cellv, inv)
            ! Multiplied in real64 first: three axes of up to 10^15 cells overflow int64, and the
            ! overflow would be a NEGATIVE cell count that passes every comparison below it.
            rc = real(nc(1), kind=real64) * real(nc(2), kind=real64) * real(nc(3), kind=real64)
            if (rc <= real(maxc, kind=real64)) exit
            ratio = (rc / real(maxc, kind=real64)) ** (1.0_real64 / 3.0_real64)
            hh = hh * max(1.001_real64, ratio)
            if (present(coarsened)) coarsened = .true.
        end do
        self%cell_side = hh
        self%grid_n = nc
        self%cell = cellv
        self%cell_inv = inv
        self%n_cells = nc(1) * nc(2) * nc(3)
    end procedure spatial_set_grid

    !> Fixes the HEALPix resolution for a query chord. See the interface for the contract.
    module procedure spatial_set_nside
        integer(int64) :: ns, ns_cap, maxc

        if (present(coarsened)) coarsened = .false.
        ! The same buckets-per-point ceiling the 3D grid obeys, and for the same reason: the bucket
        ! index IS the bucketing sort's key, so it is a KEY RANGE that keeps `pf_argsort` on its
        ! counting fast path, not a memory budget. `npix = 12*nside**2 <= 0.3*npts` therefore caps
        ! nside at `sqrt(0.025*npts)`, rounded DOWN to a power of two.
        !
        ! **The cap costs HEALPix far less than it costs the 3D grid**, which is the whole reason
        ! this backend exists: a sphere occupies a zero-thickness shell of the grid's bounding
        ! cube, so around 95% of its cells can never hold a point and 0.3 cells per point buys
        ! about 0.014 OCCUPIED ones. Every HEALPix pixel is on the sphere, so 0.3 buys 0.3.
        maxc = max(1_int64, int(spatial_max_cells_per_point * real(self%npts, kind=real64), kind=int64))
        ! Written as a loop with the test INSIDE, because `.and.` does not short-circuit in
        ! Fortran: with the ceiling test as a second operand, `12*(2*nside)**2` would still be
        ! evaluated at the ceiling and overflow int64. This way `2*ns_cap` never exceeds the
        ! ceiling, so the product stays under `huge(0_int64)`.
        ns_cap = 1_int64
        do while (ns_cap < spatial_max_nside)
            if (12_int64 * (2_int64 * ns_cap) * (2_int64 * ns_cap) > maxc) exit
            ns_cap = 2_int64 * ns_cap
        end do
        ns = max(1_int64, min(want, ns_cap))
        ! **nside 1 can violate the cap, and must**: 12 pixels is the coarsest pixelisation that
        ! exists, so a handful of points cannot have fewer buckets than that. The 3D grid makes the
        ! same concession with its own `max(1_int64, ...)`, and a sort over 12 keys is not where a
        ! fast path matters.
        if (present(coarsened)) coarsened = ns < want
        self%nside_v = ns
        self%npix_v = 12_int64 * ns * ns
        self%n_cells = self%npix_v
        ! The 3D grid's own description is zeroed rather than left stale, so `%cell_size`,
        ! `%cell_sides` and `%grid` report a value no valid grid ever has instead of describing a
        ! grid this index does not have. `%cells` still answers, because a pixel IS a bucket.
        self%cell_side = 0.0_real64
        self%cell = 0.0_real64
        self%cell_inv = 0.0_real64
        self%grid_n = 0_int64
    end procedure spatial_set_nside

    !> Buckets the stored points into the current grid, filling `start` and `idx`.
    module procedure spatial_bucket
        integer(int64), allocatable :: cid(:), perm(:), offs(:), newstart(:), newidx(:)
        real(real64), allocatable :: tmp(:)
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64) :: n, i, g, c, run, ipix
        real(real64) :: v(3)
        integer :: d

        n = self%npts
        call spatial_storage(self, xs, ys, zs)
        allocate(cid(n))
        if (self%backend_id == PF_SKY_HEALPIX) then
            ! **The ONLY thing a backend changes on the build side.** Everything below -- the sort,
            ! the CSR scatter, the composed permutation, the reorder into bucket order -- is
            ! shared, because a pixel index and a flattened cell index are both just a bucket
            ! number. `v` is a named local rather than `[xs(i), ys(i), zs(i)]` inline: an array
            ! constructor passed to an explicit-shape dummy makes ifx build a temporary and warn
            ! about it once per call under `-check arg_temp_created`.
            !
            ! **No declination frame is involved and none can be**, which is what makes this
            ! simpler than it looks from `parquet_healpix`'s own RA/Dec surface. `%build_sky` has
            ! already turned `(ra, dec)` into unit vectors with `vz = sin(dec)`, and a vector names
            ! a direction outright -- so the mirrored convention that `pf_healpix_grid` needs a
            ! `frame=` to disambiguate cannot arise here at all.
            do i = 1_int64, n
                v(1) = xs(i)
                v(2) = ys(i)
                v(3) = zs(i)
                call pf_vec2pix_ring(self%nside_v, v, ipix)
                cid(i) = ipix + 1_int64
            end do
        else
            do i = 1_int64, n
                cid(i) = spatial_cell_of(self, xs(i), ys(i), zs(i))
            end do
        end if
        call pf_argsort(cid, perm, group_offsets=offs)
        ! group_offsets gives one entry per DISTINCT cell present, not one per grid cell, so the
        ! dense array is scattered from it -- O(occupied + cells) rather than a second O(n) pass.
        allocate(newstart(self%n_cells + 1_int64))
        newstart = 0_int64
        do g = 1_int64, size(offs, kind=int64) - 1_int64
            c = cid(perm(offs(g)))
            run = offs(g + 1_int64) - offs(g)
            newstart(c + 1_int64) = run
        end do
        deallocate(cid, offs)
        newstart(1) = 1_int64
        do c = 1_int64, self%n_cells
            newstart(c + 1_int64) = newstart(c + 1_int64) + newstart(c)
        end do
        call move_alloc(newstart, self%start)
        ! The caller's row index of stored point k. On a rebuild the points are already permuted,
        ! so the two permutations COMPOSE -- taking `perm` as the answer would silently renumber
        ! every row to its position in the previous grid.
        allocate(newidx(n))
        ! COMPOSE only when the coordinates were reordered too. A `copy=.false.` index leaves them
        ! in the caller's order, so its `idx` is always a permutation OF ROWS and `perm` is the new
        ! one outright -- composing there would renumber every row through a permutation that was
        ! never applied to anything.
        if (self%owns .and. allocated(self%idx)) then
            do i = 1_int64, n
                newidx(i) = self%idx(perm(i))
            end do
        else
            do i = 1_int64, n
                newidx(i) = perm(i)
            end do
        end if
        call move_alloc(newidx, self%idx)
        if (self%owns) then
            ! Reordering the coordinates into cell order is what makes a query scan a contiguous
            ! run of memory, and the gate attributes the grid's whole query advantage over
            ! nanoflann to exactly this -- nanoflann keeps a permutation and never applies it.
            do d = 1, 3
                allocate(tmp(n))
                select case (d)
                case (1)
                    do i = 1_int64, n
                        tmp(i) = self%xs(perm(i))
                    end do
                    call move_alloc(tmp, self%xs)
                case (2)
                    do i = 1_int64, n
                        tmp(i) = self%ys(perm(i))
                    end do
                    call move_alloc(tmp, self%ys)
                case default
                    do i = 1_int64, n
                        tmp(i) = self%zs(perm(i))
                    end do
                    call move_alloc(tmp, self%zs)
                end select
            end do
        end if
    end procedure spatial_bucket

    !> Releases every array and returns `self` to its default, unbuilt state.
    module procedure spatial_clear_worker

        self%npts = 0_int64
        self%grid_n = 1_int64
        self%n_cells = 0_int64
        self%ncoord = 3
        self%dims_eff = 3
        self%metric_id = PF_METRIC_EUCLIDEAN
        self%backend_id = PF_SKY_GRID3D
        self%nside_v = 0_int64
        self%npix_v = 0_int64
        self%built_ok = .false.
        self%owns = .true.
        self%periodic_on = .false.
        self%warned = .false.
        self%lo = 0.0_real64
        self%hi = 0.0_real64
        self%cell = 1.0_real64
        self%cell_inv = 1.0_real64
        self%cell_side = 1.0_real64
        self%wrap = 0.0_real64
        self%wrap_inv = 0.0_real64
        self%rho = 0.0_real64
        self%r2sum = 0.0_real64
        self%r3sum = 0.0_real64
        if (allocated(self%xs)) deallocate (self%xs)
        if (allocated(self%ys)) deallocate (self%ys)
        if (allocated(self%zs)) deallocate (self%zs)
        if (allocated(self%start)) deallocate (self%start)
        if (allocated(self%idx)) deallocate (self%idx)
        nullify (self%px)
        nullify (self%py)
        nullify (self%pz)
    end procedure spatial_clear_worker

    !> Folds a radius list into the index's two accumulators.
    !>
    !> `r_eff = sum(r^3)/sum(r^2)` needs no list, which is what makes a union two additions rather
    !> than a growing array -- and what makes an index alternating between two distant radii settle
    !> on a cell serving both instead of rebuilding on every switch.
    module procedure spatial_fold_radii

        if (.not. union) then
            self%r2sum = 0.0_real64
            self%r3sum = 0.0_real64
        end if
        self%r2sum = self%r2sum + sum(radii * radii)
        self%r3sum = self%r3sum + sum(radii * radii * radii)
    end procedure spatial_fold_radii

    !> Re-chooses the cell size for the recorded radii and re-buckets the stored points.
    module procedure spatial_retune
        real(real64) :: h, r_eff
        integer(int64) :: ns

        r_eff = 1.0_real64
        if (self%r2sum > 0.0_real64) r_eff = self%r3sum / self%r2sum
        if (self%periodic_on) then
            if (any(r_eff > 0.5_real64 * self%wrap(1:self%ncoord))) error stop &
                "pf_spatial_index: a periodic search radius must not exceed half the box on any axis"
        end if
        if (self%backend_id == PF_SKY_HEALPIX) then
            ! `%rebuild_for` re-pixelates a HEALPix index exactly as it re-tunes a grid one -- the
            ! same operation in the other backend's vocabulary -- and never changes which backend
            ! the index has.
            if (dbg_nside > 0_int64) then
                ns = dbg_nside
                dbg_probe_count = 0_int64
            else if (self%npts > 0_int64) then
                call spatial_choose_nside(self, [r_eff], ns)
            else
                ns = 1_int64
                dbg_probe_count = 0_int64
            end if
            call spatial_set_nside(self, ns)
            call spatial_bucket(self)
            return
        end if
        if (dbg_cell > 0.0_real64) then
            h = dbg_cell
            dbg_probe_count = 0_int64
        else if (self%npts > 0_int64) then
            call spatial_choose_cell(self, [r_eff], h)
        else
            h = 1.0_real64
        end if
        call spatial_set_grid(self, h)
        call spatial_bucket(self)
    end procedure spatial_retune

    !> Aborts unless every element of `a` is a finite number -- see the interface in
    !! parquet_spatial.f90 for why a non-finite coordinate cannot be carried.
    module procedure spatial_check_finite
        if (size(a, kind=int64) == 0_int64) return
        if (all(abs(a) <= huge(0.0_real64))) return
        error stop what // ": every " // argname // " must be a finite number (no NaN, no infinity)"
    end procedure spatial_check_finite

    !> Builds `self` over the caller's coordinates.
    module procedure spatial_build_worker
        integer(int64) :: n, ns
        integer :: nd, back
        logical :: docopy, coarsened
        real(real64) :: h, rmax
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)

        n = size(x, kind=int64)
        if (size(y, kind=int64) /= n) error stop "pf_spatial_index%build: x and y must be the same length"
        if (present(z)) then
            if (size(z, kind=int64) /= n) error stop "pf_spatial_index%build: x and z must be the same length"
        end if
        if (size(radii) < 1) error stop "pf_spatial_index%build: radius= must name at least one radius"
        ! `.not. all(> 0)` rather than `any(<= 0)`: a NaN answers .false. to BOTH comparisons, so
        ! the `any` form let a NaN radius through -- to `maxval(radii)` and the tuner, where it
        ! traps under nagfor. %build_sky's own per-radius check was already written this way.
        if (.not. all(radii > 0.0_real64)) error stop "pf_spatial_index%build: every radius= must be > 0"
        call spatial_check_finite(x, "pf_spatial_index%build", "x coordinate")
        call spatial_check_finite(y, "pf_spatial_index%build", "y coordinate")
        if (present(z)) call spatial_check_finite(z, "pf_spatial_index%build", "z coordinate")
        if (present(cell)) then
            if (.not. (cell > 0.0_real64)) error stop "pf_spatial_index%build: cell= must be > 0"
        end if
        nd = 2
        if (present(z)) nd = 3
        if (present(box_lo) .neqv. present(box_hi)) error stop &
            "pf_spatial_index%build: periodic boundaries need both box_lo= and box_hi="
        if (present(box_lo)) then
            if (size(box_lo) /= nd .or. size(box_hi) /= nd) error stop &
                "pf_spatial_index%build: box_lo=/box_hi= must have one entry per coordinate"
        end if
        docopy = .true.
        if (present(copy)) docopy = copy
        back = PF_SKY_GRID3D
        if (present(backend)) back = backend
        if (back == PF_SKY_HEALPIX .and. present(cell)) error stop &
            "pf_spatial_index%build_sky: cell= describes the 3D grid's cell in unit-vector space " // &
            "and has no meaning for backend=PF_SKY_HEALPIX; use nside= instead"
        if (present(nside)) then
            if (back /= PF_SKY_HEALPIX) error stop &
                "pf_spatial_index%build_sky: nside= is the HEALPix resolution and needs " // &
                "backend=PF_SKY_HEALPIX; the 3D grid is tuned with cell="
            if (nside < 1_int64 .or. nside > spatial_max_nside) error stop &
                "pf_spatial_index%build_sky: nside= must be a power of two in 1 .. 2**29"
            if (iand(nside, nside - 1_int64) /= 0_int64) error stop &
                "pf_spatial_index%build_sky: nside= must be a power of two in 1 .. 2**29"
        end if

        call spatial_clear_worker(self)
        self%npts = n
        self%ncoord = nd
        self%owns = docopy
        ! Set BEFORE the grid choice and the bucketing, both of which branch on it, and after
        ! `spatial_clear_worker`, which resets it.
        self%backend_id = back
        self%r2sum = sum(radii * radii)
        self%r3sum = sum(radii * radii * radii)

        if (docopy) then
            allocate (self%xs(n), self%ys(n), self%zs(n))
            self%xs = x
            self%ys = y
            if (present(z)) then
                self%zs = z
            else
                self%zs = 0.0_real64
            end if
        else
            ! A `contiguous` pointer may only aim at a contiguous target, and a non-contiguous
            ! actual would have been copied into a temporary that dies at the end of this call --
            ! so refusing is the only safe answer, and it must be refused rather than detected
            ! later, because nothing later can detect it.
            if (.not. is_contiguous(x) .or. .not. is_contiguous(y)) error stop &
                "pf_spatial_index%build: copy=.false. needs contiguous x and y (pass whole arrays, not strided sections)"
            self%px => x
            self%py => y
            if (present(z)) then
                if (.not. is_contiguous(z)) error stop &
                    "pf_spatial_index%build: copy=.false. needs a contiguous z"
                self%pz => z
            else
                allocate (self%zs(n))
                self%zs = 0.0_real64
            end if
        end if
        call spatial_storage(self, xs, ys, zs)

        if (present(box_lo)) then
            self%periodic_on = .true.
            self%lo = 0.0_real64
            self%hi = 0.0_real64
            self%lo(1:nd) = box_lo
            self%hi(1:nd) = box_hi
            if (any(self%hi(1:nd) <= self%lo(1:nd))) error stop &
                "pf_spatial_index%build: box_hi= must be strictly above box_lo= on every axis"
            self%wrap = 0.0_real64
            self%wrap_inv = 0.0_real64
            self%wrap(1:nd) = self%hi(1:nd) - self%lo(1:nd)
            self%wrap_inv(1:nd) = 1.0_real64 / self%wrap(1:nd)
            rmax = maxval(radii)
            ! Beyond half the box the minimum image is ambiguous -- a point can be its own
            ! neighbour through two images -- so the answer is not inaccurate, it does not exist.
            ! This is the one periodic guard that survives "wrap silently", because it is not
            ! about bad input.
            if (any(rmax > 0.5_real64 * self%wrap(1:nd))) error stop &
                "pf_spatial_index%build: a periodic search radius must not exceed half the box on any axis"
        else if (n > 0_int64) then
            self%lo(1) = minval(xs)
            self%hi(1) = maxval(xs)
            self%lo(2) = minval(ys)
            self%hi(2) = maxval(ys)
            self%lo(3) = minval(zs)
            self%hi(3) = maxval(zs)
        end if

        if (back == PF_SKY_HEALPIX) then
            if (present(nside)) then
                ns = nside
                dbg_probe_count = 0_int64
            else if (dbg_nside > 0_int64) then
                ns = dbg_nside
                dbg_probe_count = 0_int64
            else if (n > 0_int64) then
                call spatial_choose_nside(self, radii, ns)
            else
                ns = 1_int64
                dbg_probe_count = 0_int64
            end if
            call spatial_set_nside(self, ns, coarsened)
            ! **Said out loud only when the caller ASKED for a resolution**, exactly as the grid's
            ! coarsening warning fires only for an explicit `cell=`. An explicit `nside=` is a
            ! statement that the caller has measured something, so silently overriding it would
            ! hide the one case where they need to know; an automatically chosen resolution being
            ! capped is the ordinary state of any index with more radius than points, and warning
            ! about it would fire on essentially every small index for no action the caller can
            ! take.
            if (present(nside) .and. coarsened .and. .not. parquet_output_is_suppressed()) then
                call parquet_emit_warning("pf_spatial_index%build_sky: nside= was coarsened from " // &
                    int_text(ns) // " to " // int_text(self%nside_v) // " to keep the pixel count " // &
                    "under 0.3 per point, which is what keeps the bucketing on the counting fast path")
            end if
        else
            if (present(cell)) then
                h = cell
                dbg_probe_count = 0_int64
            else if (dbg_cell > 0.0_real64) then
                h = dbg_cell
                dbg_probe_count = 0_int64
            else if (n > 0_int64) then
                call spatial_choose_cell(self, radii, h)
            else
                h = 1.0_real64
                dbg_probe_count = 0_int64
            end if
            call spatial_set_grid(self, h, coarsened)
            ! An explicit cell is a statement that the caller has measured something, so coarsening
            ! it must SAY so rather than happen in silence.
            if (present(cell) .and. coarsened .and. .not. parquet_output_is_suppressed()) then
                call parquet_emit_warning("pf_spatial_index%build: cell= was coarsened from " // &
                    real_text(cell) // " to " // real_text(self%cell_side) // " to keep the grid under " // &
                    "0.3 cells per point, which is what keeps the bucketing on the counting fast path")
            end if
        end if
        call spatial_bucket(self)
        self%built_ok = .true.
    end procedure spatial_build_worker

    module procedure spatial_build_sky_worker
        real(real64), allocatable :: vx(:), vy(:), vz(:), chords(:)
        real(real64) :: cd, rr
        integer(int64) :: n, i
        integer :: k, back

        back = PF_SKY_GRID3D
        if (present(backend)) back = backend
        if (back /= PF_SKY_GRID3D .and. back /= PF_SKY_HEALPIX) error stop &
            "pf_spatial_index%build_sky: backend= must be PF_SKY_GRID3D or PF_SKY_HEALPIX"
        n = size(ra, kind=int64)
        if (size(dec, kind=int64) /= n) error stop &
            "pf_spatial_index%build_sky: ra and dec must be the same length"
        if (size(radii_deg) < 1) error stop &
            "pf_spatial_index%build_sky: radius_deg= must name at least one radius"
        do k = 1, size(radii_deg)
            if (.not. (radii_deg(k) > 0.0_real64)) error stop &
                "pf_spatial_index%build_sky: every radius_deg= must be > 0"
            if (radii_deg(k) > spatial_max_sky_deg) error stop &
                "pf_spatial_index%build_sky: an angular radius above 90 degrees is not a " // &
                "neighbour search; the ball then covers most of the sky and the grid has nothing to prune"
        end do
        do i = 1_int64, n
            if (.not. (abs(dec(i)) <= 90.0_real64)) error stop &
                "pf_spatial_index%build_sky: every dec must lie in [-90, 90] degrees and not be NaN"
        end do
        ! `dec` is screened by the range test above; `ra` has no range to be in -- any value is
        ! folded into one turn -- so it needs its own. It cannot be left to the walk either:
        ! `cos(Inf)` raises IEEE_INVALID on the spot, and a NaN `ra` reaches the bounds scan below.
        call spatial_check_finite(ra, "pf_spatial_index%build_sky", "ra")

        allocate (vx(n), vy(n), vz(n))
        do i = 1_int64, n
            cd = cos(dec(i) * spatial_deg2rad)
            vx(i) = cd * cos(ra(i) * spatial_deg2rad)
            vy(i) = cd * sin(ra(i) * spatial_deg2rad)
            vz(i) = sin(dec(i) * spatial_deg2rad)
        end do
        allocate (chords(size(radii_deg)))
        do k = 1, size(radii_deg)
            chords(k) = 2.0_real64 * sin(0.5_real64 * radii_deg(k) * spatial_deg2rad)
        end do

        ! From here it is an ordinary 3D Euclidean build -- same tuner, same bucketing, same walk.
        ! `copy` is not offered and not passed: the coordinates are the unit vectors computed just
        ! above, which are local to this routine, so they must be copied whatever a caller wants.
        !
        ! The density estimate is what makes this work at all. Unit vectors occupy a thin shell of
        ! the [-1, 1]^3 box, so a bounding-box density would be meaningless -- the tuner takes the
        ! MEDIAN OCCUPIED CELL instead, and the sphere is the sharpest instance of the problem that
        ! choice already solves rather than a new one.
        call spatial_build_worker(self, vx, vy, vz, chords, cell, backend=back, &
                                  nside=nside)
        self%metric_id = PF_METRIC_SKY
    end procedure spatial_build_sky_worker

    !> Rebuilds only when the coordinates differ from what the index already holds.
    module procedure spatial_rebuild_worker
        integer(int64) :: n, k, i
        logical :: changed
        integer :: nd

        if (present(rebuilt)) rebuilt = .false.
        if (.not. self%built_ok) error stop &
            "pf_spatial_index%rebuild: this index has not been built; call %build first"
        if (.not. self%owns) error stop &
            "pf_spatial_index%rebuild: an index built with copy=.false. holds no copy to compare against"
        if (self%metric_id /= PF_METRIC_EUCLIDEAN) error stop &
            "pf_spatial_index%rebuild: this index was built with %build_sky; rebuild it with %build_sky"
        nd = 2
        if (present(z)) nd = 3
        if (nd /= self%ncoord) error stop &
            "pf_spatial_index%rebuild: z must be supplied exactly as it was to %build"
        n = self%npts
        changed = size(x, kind=int64) /= n .or. size(y, kind=int64) /= n
        if (.not. changed .and. present(z)) changed = size(z, kind=int64) /= n
        if (.not. changed) then
            ! Element-wise and exact, never a checksum. Both are O(n); a hash admits collisions,
            ! and a collision here means silently answering about the OLD positions, which is the
            ! wrong-answer failure this whole mechanism exists to prevent. The scan exits at the
            ! first difference, so "something changed" is fast and only "nothing changed" is a
            ! full pass -- against a rebuild that costs several.
            do k = 1_int64, n
                i = self%idx(k)
                if (self%xs(k) /= x(i) .or. self%ys(k) /= y(i)) then
                    changed = .true.
                    exit
                end if
                if (present(z)) then
                    if (self%zs(k) /= z(i)) then
                        changed = .true.
                        exit
                    end if
                end if
            end do
        end if
        if (.not. changed) then
            if (present(radii)) call spatial_fold_radii(self, radii, .true.)
            return
        end if
        if (present(rebuilt)) rebuilt = .true.
        if (size(x, kind=int64) /= size(y, kind=int64)) error stop &
            "pf_spatial_index%rebuild: x and y must be the same length"
        n = size(x, kind=int64)
        if (present(z)) then
            if (size(z, kind=int64) /= n) error stop "pf_spatial_index%rebuild: x and z must be the same length"
        end if
        call spatial_check_finite(x, "pf_spatial_index%rebuild", "x coordinate")
        call spatial_check_finite(y, "pf_spatial_index%rebuild", "y coordinate")
        if (present(z)) call spatial_check_finite(z, "pf_spatial_index%rebuild", "z coordinate")
        self%npts = n
        if (allocated(self%idx)) deallocate (self%idx)
        if (allocated(self%xs)) deallocate (self%xs)
        if (allocated(self%ys)) deallocate (self%ys)
        if (allocated(self%zs)) deallocate (self%zs)
        allocate (self%xs(n), self%ys(n), self%zs(n))
        self%xs = x
        self%ys = y
        if (present(z)) then
            self%zs = z
        else
            self%zs = 0.0_real64
        end if
        if (present(radii)) call spatial_fold_radii(self, radii, .true.)
        if (.not. self%periodic_on .and. n > 0_int64) then
            self%lo(1) = minval(self%xs)
            self%hi(1) = maxval(self%xs)
            self%lo(2) = minval(self%ys)
            self%hi(2) = maxval(self%ys)
            self%lo(3) = minval(self%zs)
            self%hi(3) = maxval(self%zs)
        end if
        call spatial_retune(self)
    end procedure spatial_rebuild_worker

    !> Re-tunes the cell size for `radii` over the points already stored.
    module procedure spatial_rebuild_for_worker

        if (.not. self%built_ok) error stop &
            "pf_spatial_index%rebuild_for: this index has not been built; call %build first"
        if (size(radii) < 1) error stop "pf_spatial_index%rebuild_for: name at least one radius"
        ! `.not. all(...)` rather than `any(...)`: a NaN answers .false. to both comparisons,
        ! so the `any` form let one through to the tuner's own min/max. See %build's copy.
        if (.not. all(radii > 0.0_real64)) error stop "pf_spatial_index%rebuild_for: every radius must be > 0"
        call spatial_fold_radii(self, radii, union)
        call spatial_retune(self)
    end procedure spatial_rebuild_for_worker

    !> A decimal rendering of an integer, for a message.
    function int_text(v) result(text)
        integer(int64), intent(in) :: v !! the value to render.
        character(len=:), allocatable :: text !! the rendered value, trimmed.
        character(len=32) :: buf

        write (buf, '(i0)') v
        text = trim(adjustl(buf))
    end function int_text

    !> A short decimal rendering of a real, for a message.
    function real_text(v) result(text)
        real(real64), intent(in) :: v !! the value to render.
        character(len=:), allocatable :: text !! the rendered value, trimmed.
        character(len=32) :: buf

        write (buf, '(g0.6)') v
        text = trim(adjustl(buf))
    end function real_text

end submodule parquet_spatial_build ! GCOVR_EXCL_LINE
