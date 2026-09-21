!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `pf_kde_grid`: the set-up, the deposit, the merge, the interpolated queries, the accessors and
!> the printer; and the adaptive rule both forms read -- the pilot `pf_kde%fit` builds, the table
!> a pilot is copied into, and the look-up that turns it into each point's bandwidth.
!!
!! **Every point deposits exactly its weight.** A point's kernel is evaluated at every cell centre
!! it reaches -- with its mirror images under `"reflect"` -- and those values are scaled to sum to
!! the share of the point's mass that lies inside `[xmin, xmax]`. The shares below `xmin` and
!! above `xmax` come from the kernel's distribution function and go to two scalars, `w_below` and
!! `w_above`: the grid knows how much weight lies beyond each end of its range, not where. Where a
!! kernel lies wholly inside the range -- the usual case -- both shares are zero, the in-range
!! share is exactly one, and the scaling is the plain discrete normalisation
!! `w / (step * sum K_i)`, so the midpoint rule's residual never reaches the total. The boundary
!! correction is the same division: every share is formed over the support, so a kernel crossing
!! a bound is renormalised onto the cells inside it, and under `"reflect"` the images' values are
!! part of the sum being normalised.
!!
!! **A kernel narrower than a cell** can fall between two centres and reach none; its in-range
!! share then goes whole into the cell containing the point.
!!
!! **The queries read one piecewise-linear density.** `%pdf` is linear between neighbouring
!! centres and constant over the outer half-cells, so its integral over the range is exactly the
!! midpoint sum `step * sum(acc)` -- the trapezoid rule over that interpolant and the midpoint rule
!! over the cells are the same number -- and `%cdf` integrates it piece by piece, a quadratic
!! between two centres, which `%quantile` solves. Each query forms the running sum it needs per
!! call and writes nothing. `%sample` inverts the same integral over the cells alone, from one
!! uniform per draw at `(pf_random_key(seed, KDE_GRID_FAMILY_LABEL), pf_random_key(stream, k))`, so
!! its draws follow `%pdf` inside the range and none lands on weight the grid counted beyond it.
!!
!! **`%add(threads=)` partitions by index.** Each thread of the team takes a contiguous share of
!! the survivors and deposits it into a private partial grid, and the partial grids are added to
!! the grid in thread order afterwards. The partition depends on the survivor count and the team
!! size alone, so one team size answers the same bits every time, and two team sizes group the
!! additions differently and differ by rounding. The pilot `pf_kde%fit` builds is cut into pieces
!! the sample and the pilot decide instead, which any team takes in turn, so that the fit answers
!! the same bits at every `threads=`.
!!
!! **The adaptive rule reads its pilot through one table and one look-up.** A pilot's cells are
!! copied into a `kde_adapt`, and a point's bandwidth is `h * (p(x)/g)**(-alpha)` with `p` the
!! table interpolated exactly as `%pdf` interpolates a grid -- the same procedure, `interp_cells` --
!! and `log g` the mean of `log p` over the pilot's own density. `pf_kde%fit` and `%init(pilot=)`
!! both build the table here and both read it here, so one pilot gives both forms the same bits.
submodule (parquet_kde) parquet_kde_grid

    implicit none

    !> The fewest survivors each thread of `%add`'s team is given. Below it, opening a team costs
    !! more than the deposit it would share out; a team is also never wider than the deposit's
    !! work over the grid's size, since each thread zeroes and then adds one whole partial grid.
    integer(int64), parameter :: KDE_ADD_MIN_PER_THREAD = 1024_int64

contains

    ! ==========================================================================================
    ! The binned method's workers; above every call to them, as nagfor requires
    ! ==========================================================================================

    !> Resolves everything the binned method fixes at `%init`: the bandwidth classes, the pad and
    !! the transform's length, with the two refusals of that geometry. Called after `set_geometry`
    !! and the rule, both of which it reads, and before `empty_grid`, which allocates from it.
    !!
    !! **Every refusal is an `%init` refusal**, which is the good place for one: the binned method
    !! decides nothing from the data, so a grid that cannot be transformed says so before the caller
    !! has streamed a file.
    module procedure kde_binned_setup

        real(real64) :: hmax, span, need, cap
        integer :: j

        self%method_code = KDE_METHOD_BINNED
        call class_ladder(self, hlo, hhi, hmax)
        ! The pad: far enough that the cosine basis's own reflection at the array's ends cannot
        ! reach a cell the caller asked for. Under `"reflect"` the mirrored sample is binned into
        ! the array too, and a mirror sits up to one reach beyond its bound, so the pad is doubled.
        span = KDE_RADIUS(self%kernel_code)*hmax
        if (self%boundary_code == KDE_BOUNDARY_REFLECT) span = 2.0_real64*span
        ! The aligned reflecting grid needs no pad at all: the type II cosine transform is
        ! half-sample even about the array's own ends, which are then exactly the two bounds, so
        ! the basis IS the correction. It needs a legal transform length, which `ncells` is only
        ! when it is a power of two.
        self%pad = 0
        if (aligned_reflect(self)) then
            self%ntr = self%nc
            self%nclass = size(self%hclass)
            return
        end if
        need = span/self%dx
        cap = real(KDE_BINNED_L_MAX, real64)*real(self%nc, real64)
        ! Formed in real64 and compared before any conversion: `span/dx` can be far past the largest
        ! integer, and `int()` of that is undefined rather than large.
        if (.not. (2.0_real64*(need + 2.0_real64) + real(self%nc, real64) <= cap)) call binned_too_long(self, entry)
        j = int(ceiling(need)) + 1
        need = real(self%nc, real64) + 2.0_real64*real(j, real64)
        if (.not. (need <= cap)) call binned_too_long(self, entry)
        self%pad = j
        self%ntr = pf_next_pow2(self%nc + 2*j)
        if (.not. (real(self%ntr, real64) <= cap)) call binned_too_long(self, entry)
        self%nclass = size(self%hclass)

    end procedure kde_binned_setup
    !> The transform array's cell centres, `L` of them: the grid's own centres extended by `pad`
    !! cells at each end, so that centre `pad + i` is cell `i`'s. The one spelling of the binned
    !! method's geometry, read by the binning, the read-back and the pad sums alike.
    module procedure kde_padded_centres

        integer :: k

        do k = 1, self%ntr
            cb(k) = centre_at(self%x0, self%dx, k - self%pad)
        end do

    end procedure kde_padded_centres
    !> Bins the points `x(jlo:jhi)` into `bins`, one column per bandwidth class, and adds what lies
    !! beyond the transform array altogether to `below` and `above`.
    !!
    !! **The position split is `pf_bin_linear`'s**, not a copy of it: its `volatile` residual makes
    !! the two shares sum to the weight exactly, and its comment forbids simplifying that. The
    !! BANDWIDTH split is this routine's, and it is the same trick in `log h`: a point a fraction
    !! `f` of the way from class `c` to class `c+1` gives `w (1 - f)` to one and `w f` to the other,
    !! so the bucketing error falls as the square of the class count where rounding to the nearest
    !! class would leave it falling as the count itself.
    module procedure kde_bin_range

        real(real64), allocatable :: gx(:), gw(:), mass(:)
        real(real64) :: wj, lstep, u, f, lo_c
        integer(int64) :: j, g
        integer :: c, cj, k, nk
        logical :: split

        nk = self%ntr
        allocate(gx(jhi - jlo + 1_int64), gw(jhi - jlo + 1_int64), mass(nk))
        split = self%nclass > 1
        lstep = 1.0_real64
        lo_c = 0.0_real64
        if (split) then
            lo_c = log(self%hclass(1))
            lstep = log(self%hclass(self%nclass)/self%hclass(1))/real(self%nclass - 1, real64)
        end if
        do c = 1, self%nclass
            g = 0_int64
            do j = jlo, jhi
                wj = 1.0_real64
                if (weighted) wj = w(j)
                ! Beyond the array's first or last centre the point's kernel cannot reach a cell
                ! the caller asked for -- the pad is one reach and half a cell wider than that --
                ! so its whole weight is counted at that end rather than binned.
                if (x(j) < cb(1)) then
                    if (c == 1) then
                        if (self%pad == 0) then
                            ! With no pad the array's edge IS the bound, and the half cell beyond
                            ! the first centre lies inside the support: the point lands whole on
                            ! that centre, which is where the reflecting basis puts it.
                            bins(1, 1) = bins(1, 1) + wj
                        else
                            below = below + wj
                        end if
                    end if
                    cycle
                end if
                if (x(j) > cb(nk)) then
                    if (c == 1) then
                        if (self%pad == 0) then
                            bins(nk, 1) = bins(nk, 1) + wj
                        else
                            above = above + wj
                        end if
                    end if
                    cycle
                end if
                f = 1.0_real64
                if (split) then
                    u = (log(hb(1_int64 + (j - 1_int64)*hstride)) - lo_c)/lstep
                    if (.not. (u > 0.0_real64)) u = 0.0_real64
                    if (u > real(self%nclass - 1, real64)) u = real(self%nclass - 1, real64)
                    cj = int(u) + 1
                    if (cj >= self%nclass) cj = self%nclass - 1
                    ! The share this class takes: `1 - f` for the class below the point, `f` for the
                    ! one above, and nothing at all for every other class.
                    if (c == cj) then
                        f = 1.0_real64 - (u - real(cj - 1, real64))
                    else if (c == cj + 1) then
                        f = u - real(cj - 1, real64)
                    else
                        cycle
                    end if
                    if (f == 0.0_real64) cycle
                end if
                g = g + 1_int64
                gx(g) = x(j)
                gw(g) = wj*f
            end do
            if (g == 0_int64) cycle
            call pf_bin_linear(gx(1:g), cb, mass, weights=gw(1:g))
            do k = 1, nk
                bins(k, c) = bins(k, c) + mass(k)
            end do
        end do

    end procedure kde_bin_range
    !> Fills the cells from the bins: the one operation of the binned method that cannot stream.
    !!
    !! Per bandwidth class: transform the bins with `pf_dct`, multiply coefficient `k` by the
    !! kernel's own transform at `pi k/(L step)`, invert with `pf_idct`, and apply that class's
    !! per-cell boundary correction. The classes are summed, the reflecting images are folded in,
    !! and the interior is read back as the cells while the pad becomes the two beyond-range
    !! counters.
    !!
    !! **The coefficient at zero frequency is multiplied by exactly one**, every kernel's transform
    !! being one there, so the convolution conserves the binned weight to the bit: the cells and
    !! the two counters still sum to what was deposited.
    !!
    !! `parquet_transform`'s header warns that the transform of a padded sequence is not a padded
    !! transform and its coefficients mean something else. That warning is about READING the
    !! coefficients, which the ISJ rule does and this does not: here they are multiplied and
    !! immediately inverted, and the padding is there precisely so that the periodic images the
    !! transform implies stay `pad` cells away from the data.
    module procedure kde_binned_transform

        real(real64), allocatable :: cb(:), co(:), wk(:), lam(:), s0(:), val(:), fold(:)
        real(real64), allocatable :: mu(:), sc(:), s1(:)
        type(kde_corr_kernel) :: cf
        real(real64) :: rc, v
        integer :: nl, np, c, k, i
        logical :: linear

        nl = self%ntr
        np = self%pad
        linear = self%boundary_code == KDE_BOUNDARY_LINEAR
        allocate(cb(nl), co(nl), wk(nl), lam(nl), s0(nl), val(nl))
        ! The local linear correction needs a SECOND convolution, against the odd kernel `u K(u)`:
        ! its equivalent kernel is `(a2 - a1 u) K(u)/(a0 a2 - a1**2)`, and the `a1 u` term is a sum
        ! the cosine transform cannot give. The odd kernel's transform is a sine, so that sum comes
        ! back through `pf_idst`.
        if (linear) allocate(mu(nl), sc(nl), s1(nl))
        call kde_padded_centres(self, cb)
        val = 0.0_real64
        do c = 1, self%nclass
            call kde_dct_filter(self%kernel_code, self%hclass(c), self%dx, lam)
            call pf_dct(self%bins(:, c), co, context="pf_kde_grid%finish")
            do k = 1, nl
                wk(k) = co(k)*lam(k)
            end do
            call pf_idct(wk, s0, context="pf_kde_grid%finish")
            if (linear) then
                call kde_dst_filter(self%kernel_code, self%hclass(c), self%dx, mu)
                ! The shift the guide page writes out: cosine coefficient `k` carries frequency
                ! `k - 1` and sine coefficient `k` carries frequency `k`, so the spectrum moves
                ! down one position and the top frequency has no sine coefficient at all.
                do k = 1, nl - 1
                    sc(k) = co(k + 1)*mu(k + 1)
                end do
                sc(nl) = 0.0_real64
                call pf_idst(sc, s1, context="pf_kde_grid%finish")
            end if
            if (kde_is_corrected(self%boundary_code)) then
                ! The correction is per CELL: the moments are the CENTRE's own, formed at this
                ! class's bandwidth, so a cell inside a zone gets the weight the exact form gives
                ! it there. `"renormalise"` divides by `a0` alone; `"linear"` combines the two
                ! convolutions, `(a2 + a1 c) S0 - a1 S1` over the moments' determinant, which is
                ! `kde_corr_value` summed over the points.
                rc = 1.0_real64/self%hclass(c)
                do k = 1, nl
                    if (outside_support(self, cb(k))) then
                        s0(k) = 0.0_real64
                    else if (.not. plain_at(self, cb(k), rc)) then
                        cf = kde_corr_factor(self%boundary_code, self%kernel_code, self%has_lower, &
                            self%lo, self%has_upper, self%hi, cb(k), rc)
                        if (linear) then
                            s0(k) = cf%dinv*((cf%a + cf%b*cf%c)*s0(k) - cf%b*s1(k))
                        else
                            s0(k) = s0(k)*cf%dinv
                        end if
                    end if
                end do
            end if
            do k = 1, nl
                val(k) = val(k) + s0(k)
            end do
        end do

        ! ---- the reflecting images, where the basis did not already supply them ----
        ! With no pad the array's own ends ARE the bounds and the cosine basis has reflected about
        ! them already, to every order. With a pad the images are read in explicitly, and they are
        ! GATHERED rather than scattered: the reflection estimator is `S(x) + S(2 lo - x) +
        ! S(2 hi - x)` inside the support, so a cell takes the value the plain estimate has at its
        ! mirror. Scattering each outside cell onto its mirror instead conserves the weight exactly
        ! but answers HALF at a cell sitting on the bound, whose mirror is itself -- and the exact
        ! deposit doubles there, its image kernel landing on the point's own.
        if (self%boundary_code == KDE_BOUNDARY_REFLECT .and. np > 0) then
            allocate(fold(nl))
            do k = 1, nl
                if (outside_support(self, cb(k))) then
                    fold(k) = 0.0_real64
                    cycle
                end if
                v = val(k)
                if (self%has_lower) v = v + gather_at(self, cb, val, 2.0_real64*self%lo - cb(k))
                if (self%has_upper) v = v + gather_at(self, cb, val, 2.0_real64*self%hi - cb(k))
                fold(k) = v
            end do
            do k = 1, nl
                val(k) = fold(k)
            end do
        end if

        ! ---- the cells, and the two beyond-range counters ----
        do i = 1, self%nc
            self%acc(i) = val(np + i)/self%dx
        end do
        do k = 1, np
            v = val(k)
            if (v == 0.0_real64) cycle
            if (outside_support(self, cb(k))) cycle
            self%w_below = self%w_below + v
        end do
        do k = np + self%nc + 1, nl
            v = val(k)
            if (v == 0.0_real64) cycle
            if (outside_support(self, cb(k))) cycle
            self%w_above = self%w_above + v
        end do
        ! The bins have become the cells and are not read again; a `%clear` gives them back.
        deallocate(self%bins)

    end procedure kde_binned_transform

    ! ==========================================================================================
    ! The adaptive rule, for both forms; above every call to it, as nagfor requires
    ! ==========================================================================================

    module procedure kde_adapt_set

        integer :: i
        real(real64) :: p, s1, s2, tp

        a%on = .true.
        a%alpha = alpha
        a%has_bmax = has_bmax
        a%bmax = 0.0_real64
        if (has_bmax) a%bmax = bmax
        a%nc = pilot%nc
        a%x0 = pilot%x0
        a%x1 = pilot%x1
        a%dx = pilot%dx
        a%wt = pilot%w_total
        a%unreadable = grid_poisoned(pilot)
        allocate(a%acc(pilot%nc))
        if (kde_is_corrected(pilot%boundary_code)) then
            ! Under `"linear"` the pilot's cells can be negative and its mass is not its weight:
            ! the table takes the CLIPPED, normalised densities, which is what `%density` answers
            ! there, so the adaptive rule never reads a negative cell.
            tp = total_mass(pilot)
            a%wt = 1.0_real64
            if (.not. (tp > 0.0_real64)) then
                a%unreadable = .true.
                tp = 1.0_real64
            end if
            do i = 1, pilot%nc
                a%acc(i) = cell_acc(pilot, i)/tp
            end do
        else
            do i = 1, pilot%nc
                a%acc(i) = pilot%acc(i)
            end do
        end if
        ! A position-weighted checksum: two tables holding the same values in a different order
        ! differ in it, which a plain sum would not. Formed before the early return, so that even an
        ! unreadable table carries one.
        a%digest = 0.0_real64
        do i = 1, a%nc
            a%digest = a%digest + a%acc(i)*real(i, real64)
        end do
        a%logg = 0.0_real64
        a%pmin = 0.0_real64
        a%pmax = 0.0_real64
        if (a%unreadable) return

        ! `log g` is the mean of `log p` over the pilot's own density: the population form of the
        ! geometric mean of the pilot at the sample points, which needs no second pass over them.
        ! It is taken over the density the cells hold, so weight the pilot counted beyond its range
        ! does not pull it towards zero.
        s1 = 0.0_real64
        s2 = 0.0_real64
        a%pmin = huge(1.0_real64)
        do i = 1, a%nc
            if (.not. (a%acc(i) > 0.0_real64)) cycle
            p = cell_value(a%acc(i), a%wt)
            if (.not. (p > 0.0_real64)) cycle
            s1 = s1 + p
            s2 = s2 + p*log(p)
            if (p < a%pmin) a%pmin = p
            if (p > a%pmax) a%pmax = p
        end do
        ! A pilot with no positive density in any cell -- nothing added, only weight beyond its
        ! range, or cells too small against the total to register -- has nothing to read: a data
        ! condition, answered with NaN by every grid built on it rather than by an abort.
        if (.not. (s1 > 0.0_real64)) then
            a%unreadable = .true.
            a%pmin = 0.0_real64
            a%pmax = 0.0_real64
            return
        end if
        a%logg = s2/s1

    end procedure kde_adapt_set

    module procedure kde_adapt_bandwidths

        integer(int64) :: i
        real(real64) :: elim

        ! The exponent above which `h*exp(e)` would overflow: two logarithms of loop-invariant
        ! values, so they are formed once here rather than per point. The same number, so the
        ! same bandwidths.
        elim = log(huge(1.0_real64)) - log(h) - 1.0_real64
        do i = 1_int64, size(x, kind=int64)
            hb(i) = rule_bandwidth(a, h, elim, x(i))
        end do

    end procedure kde_adapt_bandwidths

    module procedure kde_build_pilot

        real(real64) :: a, b, lim, reach, one(1), hb(1)
        integer(int64) :: m
        integer :: nc

        m = size(x, kind=int64)
        ok = .false.
        ! The range: the reach beyond the extreme points, held inside half the largest number at
        ! either end so that its width is finite, then clipped to the support. Every bound is
        ! tested before it is formed: the overflow itself would stop a program under nagfor.
        lim = 0.5_real64*huge(1.0_real64)
        a = -lim
        b = lim
        if (h <= lim/KDE_PILOT_REACH) then
            reach = KDE_PILOT_REACH*h
            if (x(1) > reach - lim) a = x(1) - reach
            if (x(m) < lim - reach) b = x(m) + reach
        end if
        if (has_lower) a = max(a, lo)
        if (has_upper) b = min(b, hi)
        ! Under `"linear"` the pilot is a grid like any other and keeps R1 and R2: its range starts
        ! at a bound that was given, even where no kernel reaches that bound's zone, so that
        ! `%pilot`'s copy passes `%init(pilot=)`'s coverage check; and where one bound is given its
        ! FREE edge lies at least a reach from it, which `x(m) + KDE_PILOT_REACH*h` need not.
        if (boundary_code == KDE_BOUNDARY_LINEAR) then
            if (has_lower) a = lo
            if (has_upper) b = hi
            if (has_lower .neqv. has_upper) then
                reach = KDE_RADIUS(kernel_code)*h
                if (has_lower) then
                    if (b - a < reach .and. a <= lim - reach) b = a + reach
                else
                    if (b - a < reach .and. b >= reach - lim) a = b - reach
                end if
            end if
        end if
        if (.not. (a < b)) return
        ! The cells: a quarter of a bandwidth wide, clamped, unless a test forces their number. A
        ! range too many bandwidths wide for the count is recognised before the count is formed.
        if (kde_pilot_cells_forced > 0) then
            nc = kde_pilot_cells_forced
        else if ((b - a)/(real(KDE_PILOT_MAX_CELLS, real64)/KDE_PILOT_PER_H) >= h) then
            nc = KDE_PILOT_MAX_CELLS
        else
            nc = int(ceiling(KDE_PILOT_PER_H*((b - a)/h)))
            nc = min(KDE_PILOT_MAX_CELLS, max(KDE_PILOT_MIN_CELLS, nc))
        end if
        call set_geometry(g, nc, a, b, h, kernel_code, boundary_code, has_lower, lo, has_upper, hi)
        g%adapt = kde_adapt()
        call empty_grid(g)
        g%cnt_all = m
        g%cnt_valid = m
        g%w_total = w_total
        hb(1) = h
        if (weighted) then
            call deposit(g, x, w, .true., hb, 0_int64, m, threads, pilot_parts(g, m))
        else
            one(1) = 1.0_real64
            call deposit(g, x, one, .false., hb, 0_int64, m, threads, pilot_parts(g, m))
        end if
        ! The pilot is read as a density from here on -- by `kde_adapt_set`, and by any caller who
        ! takes a copy through `pf_kde%pilot` -- so it leaves this procedure closed.
        call grid_finish(g)
        ok = .true.

    end procedure kde_build_pilot

    ! ==========================================================================================
    ! Set-up and accumulation
    ! ==========================================================================================

    module procedure grid_init

        character(len=*), parameter :: EP = "pf_kde_grid%init"
        real(real64) :: step, lo, hi, a_alpha, a_bmax
        integer :: kcode, bcode, mcode
        logical :: has_lo, has_hi
        type(kde_adapt) :: rule

        if (ncells < 1) call kde_abort(EP, "ncells must be positive")
        if (.not. ieee_is_finite(xmin)) call kde_abort(EP, "xmin and xmax must be finite")
        if (.not. ieee_is_finite(xmax)) call kde_abort(EP, "xmin and xmax must be finite")
        if (.not. (xmin < xmax)) call kde_abort(EP, "xmin must be below xmax")
        ! Halved before they are subtracted, so that a range wider than the largest number is
        ! refused rather than overflowing; below that, the width is the plain quotient.
        if (.not. (0.5_real64*xmax - 0.5_real64*xmin <= 0.5_real64*huge(1.0_real64))) &
            call kde_abort(EP, "the cell width (xmax - xmin)/ncells must be a finite, positive number")
        step = (xmax - xmin)/real(ncells, real64)
        if (.not. (step > 0.0_real64)) &
            call kde_abort(EP, "the cell width (xmax - xmin)/ncells must be a finite, positive number")
        if (.not. kde_positive_finite(bandwidth)) &
            call kde_abort(EP, "bandwidth must be a finite, positive number")
        call kde_resolve_setup(EP, kernel, lower, upper, boundary, kcode, has_lo, lo, has_hi, hi, bcode)
        ! The module's one admission rule, which `pf_kde%fit` applies to every bandwidth a rule or a
        ! caller produces: normal, and reaching a finite distance. A grid's bandwidth is always a
        ! number the caller passed, so failing it is a caller's mistake and aborts.
        if (.not. kde_bandwidth_usable(kcode, bandwidth)) call kde_abort(EP, &
            "bandwidth must be a normal positive number whose kernel reach is finite")
        ! Every cell centre inside the support, so that no cell can hold weight the support
        ! excludes and the queries never straddle a bound.
        if (has_lo) then
            if (xmin < lo) call kde_abort(EP, "the grid's range must lie inside the support")
        end if
        if (has_hi) then
            if (xmax > hi) call kde_abort(EP, "the grid's range must lie inside the support")
        end if
        ! Under `"linear"` weight beyond the range is counted by the PLAIN kernel's mass there,
        ! which is exact only where the correction is not acting. Two rules keep that true, at the
        ! cost of refusing two configurations the other corrections accept. `"renormalise"` needs
        ! neither: its beyond-range weight is the CORRECTED term's own integral (`deposit_corrected`),
        ! so a range anywhere inside the support counts exactly.
        if (bcode == KDE_BOUNDARY_LINEAR) then
            ! R1: a bounded side's edge is the bound itself, so a zone lies wholly inside the range.
            if (has_lo) then
                if (xmin /= lo) call kde_abort(EP, 'under boundary="' // trim(boundary_token(bcode)) // &
                    '" the grid''s range must start at lower and end at upper')
            end if
            if (has_hi) then
                if (xmax /= hi) call kde_abort(EP, 'under boundary="' // trim(boundary_token(bcode)) // &
                    '" the grid''s range must start at lower and end at upper')
            end if
            ! R2: with a FREE edge, the range is at least one reach wide, so that edge lies outside
            ! the bound's zone. With both bounds no edge is free and the rule does not apply.
            if (has_lo .neqv. has_hi) then
                if (xmax - xmin < KDE_RADIUS(kcode)*bandwidth) call kde_abort(EP, &
                    'under boundary="' // trim(boundary_token(bcode)) // &
                    '" the grid must be at least one kernel reach wide')
            end if
        end if

        ! ---- the adaptive rule ----
        if (present(alpha) .or. present(bandwidth_max)) then
            if (.not. present(pilot)) call kde_abort(EP, "alpha= and bandwidth_max= need pilot=")
        end if
        call check_adaptive_settings(EP, alpha, bandwidth_max, a_alpha, a_bmax)
        if (present(pilot)) then
            ! A pilot must be a grid, and must cover the range, so that no point inside it reads a
            ! pilot that was never built there. What it holds is data: one with no density to read,
            ! a kept NaN or nothing in its cells, makes this grid answer NaN, quietly.
            if (.not. pilot%initialised) call kde_abort(EP, "pilot must be an initialised grid")
            if (.not. pilot%finished) call kde_abort(EP, "pilot must be a finished grid; call %finish on it")
            if (pilot%x0 > xmin .or. pilot%x1 < xmax) call kde_abort(EP, "the pilot must cover this grid's range")
            ! The table is read out of the pilot before this grid is touched.
            call kde_adapt_set(rule, pilot, a_alpha, present(bandwidth_max), a_bmax)
        end if

        ! ---- the method ----
        mcode = KDE_METHOD_EXACT
        if (present(method)) call kde_resolve_method(EP, method, mcode)

        call set_geometry(self, ncells, xmin, xmax, bandwidth, kcode, bcode, has_lo, lo, has_hi, hi)
        call move_adapt(rule, self%adapt)
        if (mcode == KDE_METHOD_BINNED) call kde_binned_setup(self, EP)
        call empty_grid(self)

    end procedure grid_init

    module procedure grid_add_f64_r1

        character(len=*), parameter :: EP = "pf_kde_grid%add"
        real(real64), allocatable :: keep_x(:), keep_w(:), hb(:)
        real(real64) :: one(1), v, wsum, hj
        integer(int64) :: nv, nnull, nnan, nout, m, i, stride
        logical :: saw_nan, weighted

        call require_initialised(self, EP)
        call require_unfinished(self, EP)
        if (present(threads)) then
            if (threads < 1) call kde_abort(EP, "threads must be positive")
        end if

        ! ---- the population: the family's exclusions, then the support, as `pf_kde%fit` ----
        call stats_compact(x, EP, is_valid, weights, skipnan, keep_x, keep_w, nv, nnull, nnan, &
            saw_nan)
        weighted = present(weights)
        nout = 0_int64
        m = 0_int64
        do i = 1_int64, nv
            v = keep_x(i)
            ! A NaN kept by `skipnan = .false.` is counted and poisons the grid; it must not reach
            ! an ordered comparison.
            if (v == v) then
                if (kde_outside(self%has_lower, self%lo, self%has_upper, self%hi, v)) then
                    nout = nout + 1_int64
                    cycle
                end if
            end if
            m = m + 1_int64
            keep_x(m) = v
            if (weighted) keep_w(m) = keep_w(i)
        end do
        self%cnt_all = self%cnt_all + size(x, kind=int64)
        self%cnt_valid = self%cnt_valid + m
        self%cnt_null = self%cnt_null + nnull
        self%cnt_nan = self%cnt_nan + nnan
        self%cnt_out = self%cnt_out + nout
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        if (present(n_outside)) n_outside = nout
        if (weighted) then
            wsum = 0.0_real64
            do i = 1_int64, m
                wsum = wsum + keep_w(i)
            end do
        else
            wsum = real(m, real64)
        end if
        self%w_total = self%w_total + wsum
        if (saw_nan) self%poisoned = .true.
        ! A poisoned grid answers NaN whatever arrives later, so nothing more is deposited.
        if (self%poisoned .or. m == 0_int64) then
            call close_if_asked(self, finish)
            return
        end if

        ! ---- each point's bandwidth: its own when adaptive, the one for all otherwise ----
        if (self%adapt%on) then
            allocate(hb(m))
            call kde_adapt_bandwidths(self%adapt, self%h, keep_x(1:m), hb)
            ! A bandwidth the rule could only overflow or underflow to leaves no estimate to
            ! answer with: the grid is poisoned, as by a kept NaN.
            do i = 1_int64, m
                if (.not. kde_bandwidth_usable(self%kernel_code, hb(i))) then
                    self%poisoned = .true.
                    call close_if_asked(self, finish)
                    return
                end if
            end do
            ! R3: under `"linear"` with a free edge, a point whose own reach exceeds the range's
            ! width would put corrected weight beyond that edge, where the beyond-range count is the
            ! plain kernel's mass. The grid poisons itself rather than count it wrongly, as it does
            ! for a bandwidth the rule cannot represent; `bandwidth_max` is the remedy.
            if (self%boundary_code == KDE_BOUNDARY_LINEAR .and. (self%has_lower .neqv. self%has_upper)) then
                do i = 1_int64, m
                    if (KDE_RADIUS(self%kernel_code)*hb(i) > self%x1 - self%x0) then
                        self%reach_poisoned = .true.
                        call close_if_asked(self, finish)
                        return
                    end if
                end do
            end if
            stride = 1_int64
        else
            allocate(hb(1))
            hb(1) = self%h
            stride = 0_int64
        end if

        ! ---- a per-point divisor that underflowed ----
        ! A kernel far wider than a two-sided support keeps a mass that underflows to zero, and the
        ! deposit divides the point's shares by it: the grid poisons itself rather than spread a NaN
        ! over its cells and return having deposited nothing while counting the point as valid.
        ! `"reflect"` is the only correction with a per-point divisor, and only where a kernel is
        ! wider than the range between the bounds -- the case the doubly reflected terms omit mass
        ! in. Checked here, serially, rather than inside the threaded deposit.
        if (self%boundary_code == KDE_BOUNDARY_REFLECT .and. self%has_lower .and. self%has_upper) then
            do i = 1_int64, m
                hj = hb(1_int64 + (i - 1_int64)*stride)
                if (KDE_RADIUS(self%kernel_code)*hj <= self%hi - self%lo) cycle
                if (.not. kde_positive_finite(images_cdf(self, keep_x(i), hj, self%hi) &
                        - images_cdf(self, keep_x(i), hj, self%lo))) then
                    self%poisoned = .true.
                    call close_if_asked(self, finish)
                    return
                end if
            end do
        end if

        ! ---- the deposit, or the binning that stands in for it ----
        if (self%method_code == KDE_METHOD_BINNED) then
            if (weighted) then
                call bin_points(self, keep_x, keep_w, .true., hb, stride, m, threads)
            else
                one(1) = 1.0_real64
                call bin_points(self, keep_x, one, .false., hb, stride, m, threads)
            end if
        else if (weighted) then
            call deposit(self, keep_x, keep_w, .true., hb, stride, m, threads)
        else
            one(1) = 1.0_real64
            call deposit(self, keep_x, one, .false., hb, stride, m, threads)
        end if
        call close_if_asked(self, finish)

    end procedure grid_add_f64_r1

    module procedure grid_add_f64_r0

        real(real64) :: xs(1)
        logical, allocatable :: vs(:)
        real(real64), allocatable :: ws(:)

        ! One point is an array of one. A scalar argument that was not given stays an unallocated
        ! array, which reaches the array form as absent.
        xs(1) = x
        if (present(is_valid)) then
            allocate(vs(1))
            vs(1) = is_valid
        end if
        if (present(weights)) then
            allocate(ws(1))
            ws(1) = weights
        end if
        call grid_add_f64_r1(self, xs, vs, ws, skipnan, n_null, n_nan, n_outside, finish=finish)

    end procedure grid_add_f64_r0

    module procedure grid_add_f32_r1

        real(real64), allocatable :: xw(:)

        allocate(xw(size(x, kind=int64)))
        xw = real(x, real64)
        call grid_add_f64_r1(self, xw, is_valid, weights, skipnan, n_null, n_nan, n_outside, threads, finish)

    end procedure grid_add_f32_r1

    module procedure grid_add_col

        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)

        ! The column's validity becomes `is_valid`: unallocated when it has no null, and so absent.
        call kde_widen_column("pf_kde_grid%add", x, is_valid, wide, mask)
        call grid_add_f64_r1(self, wide, mask, weights, skipnan, n_null, n_nan, n_outside, threads, finish)

    end procedure grid_add_col

    module procedure grid_add_f32_r0

        call grid_add_f64_r0(self, real(x, real64), is_valid, weights, skipnan, n_null, n_nan, n_outside, finish)

    end procedure grid_add_f32_r0

    module procedure grid_merge

        character(len=*), parameter :: EP = "pf_kde_grid%merge"
        integer :: i, j

        call require_initialised(self, EP)
        call require_unfinished(self, EP)
        if (.not. other%initialised) call kde_abort(EP, "the other grid has not been initialised")
        if (self%nc /= other%nc) call kde_abort(EP, "the two grids differ in cells")
        if (self%x0 /= other%x0 .or. self%x1 /= other%x1) call kde_abort(EP, "the two grids differ in range")
        if (self%h /= other%h) call kde_abort(EP, "the two grids differ in bandwidth")
        if (self%kernel_code /= other%kernel_code) call kde_abort(EP, "the two grids differ in kernel")
        if (.not. same_support(self, other)) call kde_abort(EP, "the two grids differ in support")
        if (self%boundary_code /= other%boundary_code) call kde_abort(EP, "the two grids differ in boundary")
        if (self%method_code /= other%method_code) call kde_abort(EP, "the two grids differ in method")
        if (.not. same_rule(self%adapt, other%adapt)) call kde_abort(EP, "the two grids differ in pilot")

        do i = 1, self%nc
            self%acc(i) = self%acc(i) + other%acc(i)
        end do
        ! Under `"binned"` the accumulation is in the bins, not the cells, until `%finish` runs.
        ! Binning is linear, so two bin arrays add exactly as two deposits do; the geometry checked
        ! above and the shared pilot fix the ladder, so the two shapes cannot disagree.
        if (self%method_code == KDE_METHOD_BINNED) then
            do j = 1, self%nclass
                do i = 1, self%ntr
                    self%bins(i, j) = self%bins(i, j) + other%bins(i, j)
                end do
            end do
        end if
        self%w_below = self%w_below + other%w_below
        self%w_above = self%w_above + other%w_above
        self%w_total = self%w_total + other%w_total
        self%cnt_all = self%cnt_all + other%cnt_all
        self%cnt_valid = self%cnt_valid + other%cnt_valid
        self%cnt_null = self%cnt_null + other%cnt_null
        self%cnt_nan = self%cnt_nan + other%cnt_nan
        self%cnt_out = self%cnt_out + other%cnt_out
        self%poisoned = self%poisoned .or. other%poisoned
        self%reach_poisoned = self%reach_poisoned .or. other%reach_poisoned
        call close_if_asked(self, finish)

    end procedure grid_merge

    module procedure grid_finish

        call require_initialised(self, "pf_kde_grid%finish")
        ! A second `%finish` is a no-op: a caller who cannot tell whether a helper already closed
        ! the grid may call it again.
        if (self%finished) return
        ! Under `"binned"` the cells hold nothing until here: the accumulation is in the bins, and
        ! this is the one operation of the method that cannot stream, needing the last point to
        ! have arrived. A poisoned grid answers NaN whatever the cells hold, so nothing is
        ! transformed for it.
        if (self%method_code == KDE_METHOD_BINNED .and. .not. grid_poisoned(self)) call kde_binned_transform(self)
        ! Everything a query would otherwise rebuild per call, computed once here. The grid cannot
        ! change between `%finish` and a query -- `%add` and `%merge` are shut, and `%clear` is what
        ! reopens it -- so the stored values are always current. This is what turns `%density` from
        ! `O(cells**2)` into `O(cells)` and a bulk query from `O(cells)` a point into a constant.
        self%mass_total = accumulated_mass(self)
        call fill_running_mass(self)
        self%finished = .true.

    end procedure grid_finish

    module procedure grid_is_finished
        res = self%finished
    end procedure grid_is_finished

    ! ==========================================================================================
    ! Queries
    ! ==========================================================================================

    module procedure grid_density

        character(len=*), parameter :: EP = "pf_kde_grid%density"
        logical :: norm
        integer :: i

        call require_finished(self, EP)
        if (size(f, kind=int64) /= int(self%nc, int64)) call kde_abort(EP, "f must have one element per cell")
        if (present(x)) then
            if (size(x, kind=int64) /= int(self%nc, int64)) call kde_abort(EP, "x must have one element per cell")
            call fill_centres(self, x)
        end if
        norm = .true.
        if (present(normalise)) norm = normalise
        if (grid_poisoned(self)) then
            f = ieee_value(1.0_real64, ieee_quiet_nan)
        else if (.not. norm) then
            ! The accumulation as deposited, signed: under `"linear"` a cell can hold a negative
            ! value, and this is where a caller sees it.
            do i = 1, self%nc
                f(i) = self%acc(i)
            end do
        else if (total_mass(self) > 0.0_real64) then
            do i = 1, self%nc
                f(i) = cell_density(self, i)
            end do
        else
            ! Nothing accumulated: an empty estimate is zero everywhere, as an empty histogram is.
            f = 0.0_real64
        end if

    end procedure grid_density

    module procedure grid_pdf_r0

        call require_finished(self, "pf_kde_grid%pdf")
        f = pdf_value(self, x)

    end procedure grid_pdf_r0

    module procedure grid_pdf_r1

        character(len=*), parameter :: EP = "pf_kde_grid%pdf"
        integer(int64) :: i, n
        integer :: team

        call require_finished(self, EP)
        n = size(x, kind=int64)
        if (size(f, kind=int64) /= n) call kde_abort(EP, "f must have one element per point of x")
        call kde_query_team(EP, threads, n, KDE_GRID_QUERY_WORK, team)
        if (team <= 1) then
            kde_team_used = 1
            do i = 1_int64, n
                f(i) = pdf_value(self, x(i))
            end do
            return
        end if
        !$omp parallel num_threads(team) default(shared) private(i)
        call kde_record_team()
        !$omp do schedule(static)
        do i = 1_int64, n
            f(i) = pdf_value(self, x(i))
        end do
        !$omp end do
        !$omp end parallel

    end procedure grid_pdf_r1

    module procedure grid_cdf_r0

        call require_finished(self, "pf_kde_grid%cdf")
        p = cdf_value(self, x)

    end procedure grid_cdf_r0

    module procedure grid_cdf_r1

        character(len=*), parameter :: EP = "pf_kde_grid%cdf"
        integer(int64) :: i, n
        integer :: team

        call require_finished(self, EP)
        n = size(x, kind=int64)
        if (size(p, kind=int64) /= n) call kde_abort(EP, "p must have one element per point of x")
        call kde_query_team(EP, threads, n, KDE_GRID_QUERY_WORK, team)
        if (team <= 1) then
            kde_team_used = 1
            do i = 1_int64, n
                p(i) = cdf_value(self, x(i))
            end do
            return
        end if
        !$omp parallel num_threads(team) default(shared) private(i)
        call kde_record_team()
        !$omp do schedule(static)
        do i = 1_int64, n
            p(i) = cdf_value(self, x(i))
        end do
        !$omp end do
        !$omp end parallel

    end procedure grid_cdf_r1

    module procedure grid_quantile_r0

        call require_finished(self, "pf_kde_grid%quantile")
        call check_probability(p)
        x = quantile_value(self, p)

    end procedure grid_quantile_r0

    module procedure grid_quantile_r1

        character(len=*), parameter :: EP = "pf_kde_grid%quantile"
        integer(int64) :: i, n
        integer :: team

        call require_finished(self, EP)
        n = size(p, kind=int64)
        if (size(x, kind=int64) /= n) call kde_abort(EP, "x must have one element per element of p")
        call kde_query_team(EP, threads, n, KDE_GRID_QUERY_WORK, team)
        ! Every probability is checked before any is answered, and serially, so that an abort is
        ! taken outside the team.
        do i = 1_int64, n
            call check_probability(p(i))
        end do
        if (team <= 1) then
            kde_team_used = 1
            do i = 1_int64, n
                x(i) = quantile_value(self, p(i))
            end do
            return
        end if
        !$omp parallel num_threads(team) default(shared) private(i)
        call kde_record_team()
        !$omp do schedule(static)
        do i = 1_int64, n
            x(i) = quantile_value(self, p(i))
        end do
        !$omp end do
        !$omp end parallel

    end procedure grid_quantile_r1

    module procedure grid_sample_s32

        integer(int64) :: s

        s = 0_int64
        if (present(stream)) s = int(stream, int64)
        call sample_fill(self, v, seed, s, threads)

    end procedure grid_sample_s32

    module procedure grid_sample_s64

        call sample_fill(self, v, seed, stream, threads)

    end procedure grid_sample_s64

    ! ==========================================================================================
    ! Accessors
    ! ==========================================================================================

    module procedure grid_centres
        call require_initialised(self, "pf_kde_grid%grid")
        if (size(x, kind=int64) /= int(self%nc, int64)) &
            call kde_abort("pf_kde_grid%grid", "x must have one element per cell")
        call fill_centres(self, x)
    end procedure grid_centres

    module procedure grid_ncells
        call require_initialised(self, "pf_kde_grid%ncells")
        n = self%nc
    end procedure grid_ncells

    module procedure grid_step
        call require_initialised(self, "pf_kde_grid%step")
        s = self%dx
    end procedure grid_step

    module procedure grid_bandwidth
        call require_initialised(self, "pf_kde_grid%bandwidth")
        h = self%h
    end procedure grid_bandwidth

    module procedure grid_kernel_name
        call require_initialised(self, "pf_kde_grid%kernel")
        select case (self%kernel_code)
        case (KDE_EPANECHNIKOV)
            name = "epanechnikov"
        case (KDE_BSPLINE)
            name = "bspline"
        case (KDE_BOX)
            name = "box"
        case default
            name = "gaussian"
        end select
    end procedure grid_kernel_name

    module procedure grid_method_name
        call require_initialised(self, "pf_kde_grid%method")
        name = trim(method_token(self%method_code))
    end procedure grid_method_name

    module procedure grid_bounds
        call require_initialised(self, "pf_kde_grid%bounds")
        lo = ieee_value(1.0_real64, ieee_negative_inf)
        hi = ieee_value(1.0_real64, ieee_positive_inf)
        if (self%has_lower) lo = self%lo
        if (self%has_upper) hi = self%hi
    end procedure grid_bounds

    module procedure grid_n
        call require_initialised(self, "pf_kde_grid%n")
        n = self%cnt_all
    end procedure grid_n

    module procedure grid_n_valid
        call require_initialised(self, "pf_kde_grid%n_valid")
        n = self%cnt_valid
    end procedure grid_n_valid

    module procedure grid_n_null
        call require_initialised(self, "pf_kde_grid%n_null")
        n = self%cnt_null
    end procedure grid_n_null

    module procedure grid_n_nan
        call require_initialised(self, "pf_kde_grid%n_nan")
        n = self%cnt_nan
    end procedure grid_n_nan

    module procedure grid_n_outside
        call require_initialised(self, "pf_kde_grid%n_outside")
        n = self%cnt_out
    end procedure grid_n_outside

    module procedure grid_sum_weights
        call require_initialised(self, "pf_kde_grid%sum_weights")
        s = self%w_total
    end procedure grid_sum_weights

    module procedure grid_is_initialised
        res = self%initialised
    end procedure grid_is_initialised

    module procedure grid_is_adaptive
        call require_initialised(self, "pf_kde_grid%is_adaptive")
        res = self%adapt%on
    end procedure grid_is_adaptive

    module procedure grid_print

        integer :: u
        character(len=:), allocatable :: token

        ! Solicited output, so `verbosity = "silent"` governs it as it governs `pf_kde%print`.
        if (parquet_output_is_suppressed()) return
        u = parquet_message_unit()
        if (present(unit)) u = unit
        if (.not. self%initialised) then
            write(u, '(a)') "pf_kde_grid: not initialised (call %init first)"
            return
        end if
        write(u, '(a)') "pf_kde_grid"
        ! Every row's label in the one thirteen-character column, `kde_label`'s.
        if (self%finished) then
            write(u, '(2x,a,a)') kde_label("state"), "finished (the queries answer)"
        else
            write(u, '(2x,a,a)') kde_label("state"), "accumulating (%finish before querying)"
        end if
        write(u, '(2x,a,i0)') kde_label("cells"), self%nc
        write(u, '(2x,a,es24.16e3)') kde_label("xmin"), self%x0
        write(u, '(2x,a,es24.16e3)') kde_label("xmax"), self%x1
        write(u, '(2x,a,es24.16e3)') kde_label("step"), self%dx
        call grid_kernel_name(self, token)
        write(u, '(2x,a,a)') kde_label("kernel"), token
        write(u, '(2x,a,es24.16e3)') kde_label("bandwidth"), self%h
        write(u, '(2x,a,i0)') kde_label("n"), self%cnt_all
        write(u, '(2x,a,i0)') kde_label("n_valid"), self%cnt_valid
        write(u, '(2x,a,i0)') kde_label("n_null"), self%cnt_null
        write(u, '(2x,a,i0)') kde_label("n_nan"), self%cnt_nan
        write(u, '(2x,a,i0)') kde_label("n_outside"), self%cnt_out
        write(u, '(2x,a,es24.16e3)') kde_label("sum_weights"), self%w_total
        if (self%has_lower) write(u, '(2x,a,es24.16e3)') kde_label("lower"), self%lo
        if (self%has_upper) write(u, '(2x,a,es24.16e3)') kde_label("upper"), self%hi
        write(u, '(2x,a,a)') kde_label("boundary"), trim(boundary_token(self%boundary_code))
        write(u, '(2x,a,a)') kde_label("method"), trim(method_token(self%method_code))
        if (self%method_code == KDE_METHOD_BINNED .and. self%nclass > 1) &
            write(u, '(2x,a,i0)') kde_label("h classes"), self%nclass
        if (self%adapt%on) then
            write(u, '(2x,a,es24.16e3)') kde_label("alpha"), self%adapt%alpha
            if (self%adapt%has_bmax) write(u, '(2x,a,es24.16e3)') kde_label("bandwidth_max"), self%adapt%bmax
            write(u, '(2x,a,i0,a,es24.16e3,a,es24.16e3)') kde_label("pilot"), self%adapt%nc, " cells over", &
                self%adapt%x0, " to", self%adapt%x1
        end if
        if (self%poisoned) then
            ! A kept NaN, a pilot with no density to read or a bandwidth the adaptive rule could not
            ! represent.
            write(u, '(2x,a,a)') kde_label("poisoned"), "every query answers NaN"
        end if
        if (self%reach_poisoned) write(u, '(2x,a,a)') kde_label("poisoned"), &
            'a kernel wider than the grid crossed a bound under boundary="linear"'
        if (.not. grid_poisoned(self)) then
            if (.not. (total_mass(self) > 0.0_real64)) &
                write(u, '(2x,a,a)') kde_label("empty"), "nothing accumulated; %density answers zeros"
        end if

    end procedure grid_print

    module procedure grid_clear

        if (.not. self%initialised) return
        call empty_grid(self)

    end procedure grid_clear

    ! ==========================================================================================
    ! Private helpers
    ! ==========================================================================================

    !> Aborts unless the grid has been initialised. Impure deliberately, like every guard here.
    subroutine require_initialised(self, entry)
        class(pf_kde_grid), intent(in) :: self  !! the grid
        character(len=*), intent(in)   :: entry !! the binding, for the message

        if (.not. self%initialised) call kde_abort(entry, "the grid has not been initialised")

    end subroutine require_initialised

    !> Aborts unless the grid is still accumulating: `%add` and `%merge` are shut after `%finish`,
    !! and `%clear` is what reopens them. Impure deliberately, like every guard here.
    subroutine require_unfinished(self, entry)
        class(pf_kde_grid), intent(in) :: self  !! the grid
        character(len=*), intent(in)   :: entry !! the binding, for the message

        if (self%finished) call kde_abort(entry, &
            "the grid has been finished; call %clear to accumulate into it again")

    end subroutine require_unfinished

    !> Aborts unless the grid has been finished, which is when the queries open.
    subroutine require_finished(self, entry)
        class(pf_kde_grid), intent(in) :: self  !! the grid
        character(len=*), intent(in)   :: entry !! the binding, for the message

        call require_initialised(self, entry)
        if (.not. self%finished) call kde_abort(entry, &
            "the grid has not been finished; call %finish before querying it")

    end subroutine require_finished

    !> Closes the accumulation when the caller asked for it with `finish=.true.`, which is the
    !! one-line form of `%finish` on the last `%add` or `%merge`.
    subroutine close_if_asked(self, finish)
        class(pf_kde_grid), intent(inout) :: self   !! the grid
        logical, intent(in), optional     :: finish !! the caller's request

        if (.not. present(finish)) return
        if (finish) call grid_finish(self)

    end subroutine close_if_asked

    !> Aborts unless `p` is a probability. The NaN is screened first, as its own test.
    subroutine check_probability(p)
        real(real64), intent(in) :: p !! the caller's probability

        if (p /= p) call kde_abort("pf_kde_grid%quantile", "p must lie in [0, 1]")
        if (p < 0.0_real64 .or. p > 1.0_real64) call kde_abort("pf_kde_grid%quantile", "p must lie in [0, 1]")

    end subroutine check_probability

    !> Zeroes the accumulation and every count, keeping the geometry and the settings. A grid whose
    !! pilot has no density to read starts poisoned: its adaptive rule has nothing to read.
    subroutine empty_grid(self)
        class(pf_kde_grid), intent(inout) :: self !! the grid, initialised

        self%acc = 0.0_real64
        self%finished = .false.
        self%mass_total = 0.0_real64
        if (allocated(self%cum)) deallocate(self%cum)
        ! The bins are what `%add` fills under `"binned"` and `%finish` transforms away, so `%clear`
        ! has to give them back: a cleared grid accumulates again exactly as a new one does.
        if (self%method_code == KDE_METHOD_BINNED) then
            if (allocated(self%bins)) deallocate(self%bins)
            allocate(self%bins(self%ntr, self%nclass))
            self%bins = 0.0_real64
        end if
        self%poisoned = self%adapt%on .and. self%adapt%unreadable
        self%reach_poisoned = .false.
        self%w_total = 0.0_real64
        self%w_below = 0.0_real64
        self%w_above = 0.0_real64
        self%cnt_all = 0_int64
        self%cnt_valid = 0_int64
        self%cnt_null = 0_int64
        self%cnt_nan = 0_int64
        self%cnt_out = 0_int64

    end subroutine empty_grid

    !> A boundary correction's own token, as `%init` took it: the one spelling `%print` reports and
    !! the refusals name, so that a message and a printed row cannot drift from each other or from
    !! `kde_resolve_boundary`'s vocabulary.
    pure function boundary_token(bcode) result(name)
        integer, intent(in) :: bcode !! the correction's code
        character(len=16)   :: name  !! its token, blank-padded

        select case (bcode)
        case (KDE_BOUNDARY_RENORMALISE)
            name = "renormalise"
        case (KDE_BOUNDARY_REFLECT)
            name = "reflect"
        case (KDE_BOUNDARY_LINEAR)
            name = "linear"
        case default
            name = "none (unbounded)"
        end select

    end function boundary_token

    !> A method's own token, as `%init` took it: the one spelling `%method` answers, `%print`
    !! reports and the refusals name, so that none of the three can drift from
    !! `kde_resolve_method`'s vocabulary.
    pure function method_token(mcode) result(name)
        integer, intent(in) :: mcode !! the method's code
        character(len=8)    :: name  !! its token, blank-padded

        select case (mcode)
        case (KDE_METHOD_BINNED)
            name = "binned"
        case default
            name = "exact"
        end select

    end function method_token

    !> `.true.` when two grids have the same support: the same bounds given, at the same values.
    pure function same_support(a, b) result(res)
        class(pf_kde_grid), intent(in) :: a   !! one grid
        class(pf_kde_grid), intent(in) :: b   !! the other
        logical                        :: res !! their supports agree

        res = (a%has_lower .eqv. b%has_lower) .and. (a%has_upper .eqv. b%has_upper)
        if (.not. res) return
        if (a%has_lower) res = a%lo == b%lo
        if (.not. res) return
        if (a%has_upper) res = a%hi == b%hi

    end function same_support

    !> `.true.` when two adaptive rules are the same: both off, or both on with the same `alpha`
    !! and cap and a pilot the same cell for cell.
    pure function same_rule(a, b) result(res)
        type(kde_adapt), intent(in) :: a   !! one rule
        type(kde_adapt), intent(in) :: b   !! the other
        logical                     :: res !! they agree

        integer :: i

        res = a%on .eqv. b%on
        if (.not. (res .and. a%on)) return
        res = .false.
        if (a%alpha /= b%alpha) return
        if (a%has_bmax .neqv. b%has_bmax) return
        if (a%has_bmax) then
            if (a%bmax /= b%bmax) return
        end if
        if (a%nc /= b%nc .or. a%x0 /= b%x0 .or. a%x1 /= b%x1 .or. a%wt /= b%wt) return
        if (a%unreadable .neqv. b%unreadable) return
        ! Two tables that differ almost always differ here, and then no cell is looked at. A match
        ! is not trusted: a checksum can collide, so the cells are still walked.
        if (a%digest /= b%digest) return
        do i = 1, a%nc
            if (a%acc(i) /= b%acc(i)) return
        end do
        res = .true.

    end function same_rule

    !> The adaptive rule's settings `%init` checks, in the canonical order, with their defaults
    !! filled in: `alpha` in `[0, 1]`, 0.5 when absent, and `bandwidth_max` a finite positive
    !! number, when given.
    subroutine check_adaptive_settings(entry, alpha, bandwidth_max, a_alpha, a_bmax)
        character(len=*), intent(in)       :: entry         !! the binding, for the message
        real(real64), intent(in), optional :: alpha         !! the caller's sensitivity
        real(real64), intent(in), optional :: bandwidth_max !! the caller's cap
        real(real64), intent(out)          :: a_alpha       !! the sensitivity to use
        real(real64), intent(out)          :: a_bmax        !! the cap, or 0 when absent

        a_alpha = KDE_ALPHA_DEFAULT
        if (present(alpha)) then
            ! The NaN first, as its own test: an ordered comparison raises IEEE_INVALID on one.
            if (alpha /= alpha) call kde_abort(entry, "alpha must lie in [0, 1]")
            if (alpha < 0.0_real64 .or. alpha > 1.0_real64) call kde_abort(entry, "alpha must lie in [0, 1]")
            a_alpha = alpha
        end if
        a_bmax = 0.0_real64
        if (present(bandwidth_max)) then
            if (.not. kde_positive_finite(bandwidth_max)) &
                call kde_abort(entry, "bandwidth_max must be a finite, positive number")
            a_bmax = bandwidth_max
        end if

    end subroutine check_adaptive_settings

    !> Sets a grid's geometry and settings, discarding its cells: what `%init` does once it has
    !! checked them, and what the pilot builder does with its own.
    subroutine set_geometry(self, ncells, xmin, xmax, h, kcode, bcode, has_lo, lo, has_hi, hi)
        class(pf_kde_grid), intent(inout) :: self   !! the grid
        integer, intent(in)               :: ncells !! the number of cells
        real(real64), intent(in)          :: xmin   !! the first cell's left edge
        real(real64), intent(in)          :: xmax   !! the last cell's right edge
        real(real64), intent(in)          :: h      !! the bandwidth
        integer, intent(in)               :: kcode  !! the kernel
        integer, intent(in)               :: bcode  !! the correction
        logical, intent(in)               :: has_lo !! a lower bound was given
        real(real64), intent(in)          :: lo     !! the lower bound
        logical, intent(in)               :: has_hi !! an upper bound was given
        real(real64), intent(in)          :: hi     !! the upper bound

        self%initialised = .true.
        self%nc = ncells
        self%x0 = xmin
        self%x1 = xmax
        self%dx = (xmax - xmin)/real(ncells, real64)
        self%h = h
        self%kernel_code = kcode
        self%boundary_code = bcode
        self%has_lower = has_lo
        self%has_upper = has_hi
        self%lo = lo
        self%hi = hi
        ! The exact method, which `setup_binned` overrides where `%init` was given the other one:
        ! the pilot builder reaches this routine too, and its grids are always exact.
        self%method_code = KDE_METHOD_EXACT
        self%pad = 0
        self%ntr = 0
        self%nclass = 0
        if (allocated(self%hclass)) deallocate(self%hclass)
        if (allocated(self%bins)) deallocate(self%bins)
        if (allocated(self%acc)) deallocate(self%acc)
        allocate(self%acc(ncells))

    end subroutine set_geometry


    !> `.true.` for the one binned case that needs no padding: `"reflect"` with both bounds given
    !! and the range exactly the support, on a power-of-two cell count. The transform's implicit
    !! half-sample symmetry is then the reflection itself, at both ends, exactly.
    pure function aligned_reflect(self) result(res)
        class(pf_kde_grid), intent(in) :: self !! the grid
        logical                        :: res  !! the unpadded path applies

        res = self%boundary_code == KDE_BOUNDARY_REFLECT .and. self%has_lower .and. self%has_upper
        if (.not. res) return
        ! `pf_dct` takes a power-of-two length, and `pf_bin_linear` a grid of at least two points,
        ! so the unpadded path needs both of `ncells`.
        res = self%lo == self%x0 .and. self%hi == self%x1 .and. self%nc >= 2 .and. pf_is_pow2(self%nc)

    end function aligned_reflect

    !> The refusal of a transform longer than `KDE_BINNED_L_MAX` times `ncells`, naming the three
    !! numbers that produced it: a bandwidth about twice the whole range reaches it, and a grid
    !! whose bandwidth is twice its own range is telling the user something.
    subroutine binned_too_long(self, entry)
        class(pf_kde_grid), intent(in) :: self  !! the grid
        character(len=*), intent(in)   :: entry !! the binding, for the message

        character(len=64) :: hs, rs
        character(len=32) :: cs

        write(cs, '(i0)') self%nc
        write(hs, '(es16.8e3)') self%h
        write(rs, '(es16.8e3)') self%x1 - self%x0
        call kde_abort(entry, 'method="binned" needs a transform longer than ' // &
            trim(adjustl(cs)) // ' cells can carry: the bandwidth ' // trim(adjustl(hs)) // &
            ' reaches past a range of ' // trim(adjustl(rs)) // '; use method="exact"')

    end subroutine binned_too_long

    !> Fills `%hclass`, the bandwidth classes the binned method convolves at, and answers the
    !! widest of them, which is what sets the pad.
    !!
    !! A fixed bandwidth is one class. The adaptive rule's bandwidths are bracketed at `%init` from
    !! the PILOT's density extremes, which is where they come from, so no pass over the data is
    !! needed and `%merge` cannot meet a grid with a different ladder. The classes are geometric
    !! with ratio `KDE_BINNED_H_STEP`: what sets the bucketing error is the ratio between
    !! neighbours, so fixing it makes the accuracy the same whatever spread the data produces, and
    !! the COUNT follows the spread up to `KDE_BINNED_CLASSES_MAX`.
    subroutine class_ladder(self, given_lo, given_hi, hmax)
        class(pf_kde_grid), intent(inout)  :: self     !! the grid, its rule already set
        real(real64), intent(in), optional :: given_lo !! the narrowest bandwidth, when the caller knows it
        real(real64), intent(in), optional :: given_hi !! the widest
        real(real64), intent(out)          :: hmax     !! the widest class's bandwidth

        real(real64) :: hlo, hhi, lim, e
        integer :: c, i

        if (allocated(self%hclass)) deallocate(self%hclass)
        hlo = self%h
        hhi = self%h
        if (present(given_lo) .and. present(given_hi)) then
            ! The caller's own bracket, which the binned curve takes from the fit's per-point
            ! bandwidths: the rule that produced them is not a pilot this grid holds.
            hlo = given_lo
            hhi = given_hi
        else if (self%adapt%on .and. self%adapt%alpha /= 0.0_real64 .and. .not. self%adapt%unreadable) then
            ! `h (p/g)**(-alpha)`: the pilot's largest density gives the narrowest bandwidth and
            ! its smallest the widest, so the two extremes bracket every bandwidth the rule can
            ! give a point of this grid.
            hlo = rule_h(self, self%adapt%pmax)
            hhi = rule_h(self, self%adapt%pmin)
            ! A point the pilot reads zero density at takes the cap where there is one, and `pmin`
            ! where there is not; both are already inside the bracket.
            if (self%adapt%has_bmax) then
                if (hlo > self%adapt%bmax) hlo = self%adapt%bmax
                if (hhi > self%adapt%bmax) hhi = self%adapt%bmax
            end if
        end if
        ! A bandwidth the rule could only overflow to belongs to a point that poisons the grid at
        ! `%add`, so the ladder stops at the widest one an estimate could be built from.
        lim = huge(1.0_real64)/(2.0_real64*KDE_RADIUS(self%kernel_code))
        if (.not. (hhi >= hlo)) hhi = hlo
        if (.not. (hhi <= lim)) hhi = lim
        if (.not. (hlo >= tiny(1.0_real64))) hlo = tiny(1.0_real64)
        if (hlo > hhi) hlo = hhi
        c = 1
        if (hhi > hlo) then
            e = log(hhi/hlo)/log(KDE_BINNED_H_STEP)
            if (e >= real(KDE_BINNED_CLASSES_MAX - 1, real64)) then
                c = KDE_BINNED_CLASSES_MAX
            else
                c = int(ceiling(e)) + 1
            end if
            if (kde_binned_classes_forced > 0) c = kde_binned_classes_forced
        end if
        allocate(self%hclass(c))
        self%hclass(1) = hlo
        self%hclass(c) = hhi
        ! Geometric between the two ends, both ends exact, so that the widest class is the number
        ! the pad was sized from and the narrowest is a bandwidth a point can actually take.
        do i = 2, c - 1
            self%hclass(i) = hlo*exp(real(i - 1, real64)*log(hhi/hlo)/real(c - 1, real64))
        end do
        hmax = hhi

    end subroutine class_ladder

    !> The adaptive rule's bandwidth at a pilot density `p`, without the per-point plumbing:
    !! `h (p/g)**(-alpha)`, clamped where the exponential would overflow.
    pure function rule_h(self, p) result(hj)
        class(pf_kde_grid), intent(in) :: self !! the grid, its rule set
        real(real64), intent(in)       :: p    !! the pilot's density, positive
        real(real64)                   :: hj   !! the bandwidth there

        real(real64) :: e

        hj = self%h
        if (.not. (p > 0.0_real64)) return
        e = -self%adapt%alpha*(log(p) - self%adapt%logg)
        if (e > log(huge(1.0_real64)) - log(self%h) - 1.0_real64) then
            hj = huge(1.0_real64)
        else
            hj = self%h*exp(e)
        end if

    end function rule_h

    !> Moves an adaptive rule into a grid, leaving the source empty; an absent rule is `off`.
    subroutine move_adapt(from, to)
        type(kde_adapt), intent(inout) :: from !! the rule; left empty
        type(kde_adapt), intent(inout) :: to   !! the grid's rule; replaced

        if (allocated(to%acc)) deallocate(to%acc)
        to%on = from%on
        to%alpha = from%alpha
        to%has_bmax = from%has_bmax
        to%bmax = from%bmax
        to%nc = from%nc
        to%x0 = from%x0
        to%x1 = from%x1
        to%dx = from%dx
        to%wt = from%wt
        to%unreadable = from%unreadable
        to%logg = from%logg
        to%pmin = from%pmin
        to%pmax = from%pmax
        to%digest = from%digest
        if (allocated(from%acc)) call move_alloc(from%acc, to%acc)

    end subroutine move_adapt

    !> Cell `i`'s centre on a grid starting at `x0` with cells `dx` wide, `x0 + (i - 1/2)*dx`: the
    !! one spelling of it, so that `%grid`, the deposit, the interpolation and the pilot's look-up
    !! agree to the bit.
    pure function centre_at(x0, dx, i) result(c)
        real(real64), intent(in) :: x0 !! the first cell's left edge
        real(real64), intent(in) :: dx !! the cell width
        integer, intent(in)      :: i  !! the cell
        real(real64)             :: c  !! its centre

        c = x0 + (real(i, real64) - 0.5_real64)*dx

    end function centre_at

    !> Cell `i`'s centre on this grid.
    pure function centre(self, i) result(c)
        class(pf_kde_grid), intent(in) :: self !! the grid
        integer, intent(in)            :: i    !! the cell
        real(real64)                   :: c    !! its centre

        c = centre_at(self%x0, self%dx, i)

    end function centre

    !> Every cell centre, into `x`.
    pure subroutine fill_centres(self, x)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64), intent(out)      :: x(:) !! the centres, one per cell

        integer :: i

        do i = 1, self%nc
            x(i) = centre(self, i)
        end do

    end subroutine fill_centres

    !> The weight of the point at `xj` (bandwidth `hj`) at or below `t`, summed over its images:
    !! itself, and under `"reflect"` its mirror image about each bound. `t` may be infinite.
    pure function images_cdf(self, xj, hj, t) result(s)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64), intent(in)       :: xj   !! the point
        real(real64), intent(in)       :: hj   !! its bandwidth
        real(real64), intent(in)       :: t    !! where to evaluate
        real(real64)                   :: s    !! the weight at or below `t`

        s = kde_kernel_cdf(self%kernel_code, (t - xj)/hj)
        if (self%boundary_code /= KDE_BOUNDARY_REFLECT) return
        if (self%has_lower) s = s + kde_kernel_cdf(self%kernel_code, (t - (2.0_real64*self%lo - xj))/hj)
        if (self%has_upper) s = s + kde_kernel_cdf(self%kernel_code, (t - (2.0_real64*self%hi - xj))/hj)

    end function images_cdf

    !> The team `%add` opens for `m` survivors: `resolve_thread_count`'s answer -- the caller's
    !! request or the automatic count, never a nested team where one would deadlock, clamped to
    !! the processors available -- cut so that every thread gets at least
    !! `KDE_ADD_MIN_PER_THREAD` survivors and so that the partial grids' cost does not outweigh
    !! the deposit. 1 means serial, and is the only answer without OpenMP.
    function add_team(self, threads, m) result(team)
        class(pf_kde_grid), intent(in) :: self    !! the grid
        integer, intent(in), optional  :: threads !! the caller's request; absent means automatic
        integer(int64), intent(in)     :: m       !! survivors to deposit
        integer(int64)                 :: team    !! threads to open

        integer(int64) :: nt
        real(real64) :: cells, cap

        team = 1_int64
#ifdef _OPENMP
        call resolve_thread_count(threads, m, nt)
        if (nt <= 1_int64) return
        team = min(nt, m/KDE_ADD_MIN_PER_THREAD)
        ! The cells one kernel reaches, and the team whose partial grids cost no more than the
        ! deposit: `team * ncells <= m * cells`.
        cells = min(real(self%nc, real64), 2.0_real64*KDE_RADIUS(self%kernel_code)*self%h/self%dx + 1.0_real64)
        cap = real(m, real64)*cells/real(self%nc, real64)
        if (cap < real(team, real64)) team = int(cap, int64)
        if (team < 2_int64) team = 1_int64
#else
        ! No OpenMP: there is no team to open. The locals are assigned so that a serial build does
        ! not report them unused, and none of them can change the answer.
        nt = 1_int64
        cells = real(self%nc, real64)
        cap = real(m, real64)
        if (present(threads)) team = max(1_int64, nt)
#endif

    end function add_team

    !> Deposits the survivors `x(1:m)` -- with their weights `w(1:m)` when `weighted`, `w` unread
    !! otherwise, and the bandwidth of survivor `j` at `hb(1 + (j - 1)*hstride)` -- serially or on
    !! a team with one partial grid per thread. With `parts`, the survivors are cut into that many
    !! pieces whatever the team, which the team takes in turn: the pilot's deposit, whose bits
    !! must not depend on the thread count.
    subroutine deposit(self, x, w, weighted, hb, hstride, m, threads, parts)
#ifdef _OPENMP
        use omp_lib, only : omp_get_thread_num, omp_get_num_threads
#endif
        class(pf_kde_grid), intent(inout)    :: self     !! the grid
        real(real64), intent(in)             :: x(:)     !! the survivors, inside the support
        real(real64), intent(in)             :: w(:)     !! their weights, when weighted
        logical, intent(in)                  :: weighted !! `w` holds one weight per survivor
        real(real64), intent(in)             :: hb(:)    !! the bandwidths, one per survivor or one for all
        integer(int64), intent(in)           :: hstride  !! 1 for one per survivor, 0 for one for all
        integer(int64), intent(in)           :: m        !! how many survivors
        integer, intent(in), optional        :: threads  !! the caller's request
        integer(int64), intent(in), optional :: parts    !! the pieces, fixed in advance

        real(real64), allocatable :: part(:, :), pbelow(:), pabove(:)
        integer(int64) :: team, lo_t, hi_t
        integer :: t, nt, i

        if (present(parts)) then
            call deposit_parts(self, x, w, weighted, hb, hstride, m, threads, parts)
            return
        end if
        team = add_team(self, threads, m)
        if (team <= 1_int64) then
            kde_team_used = 1
            call deposit_range(self, x, w, weighted, hb, hstride, 1_int64, m, self%acc, self%w_below, &
                self%w_above)
            return
        end if

        ! One partial grid per thread, in a shared array allocated before the region and indexed
        ! by the thread number; each thread fills its own column and nothing else.
        allocate(part(self%nc, team), pbelow(team), pabove(team))
        nt = 1
        t = 1
        lo_t = 1_int64
        hi_t = m
        !$omp parallel num_threads(int(team)) default(shared) private(t, lo_t, hi_t)
#ifdef _OPENMP
        t = omp_get_thread_num() + 1
        ! The partition follows the team that actually opened, which the runtime may make smaller
        ! than the one asked for; the thread that records it is the one every team has.
        !$omp single
        nt = omp_get_num_threads()
        kde_team_used = nt
        !$omp end single
        lo_t = (m*int(t - 1, int64))/int(nt, int64) + 1_int64
        hi_t = (m*int(t, int64))/int(nt, int64)
#endif
        part(:, t) = 0.0_real64
        pbelow(t) = 0.0_real64
        pabove(t) = 0.0_real64
        if (hi_t >= lo_t) call deposit_range(self, x, w, weighted, hb, hstride, lo_t, hi_t, part(:, t), &
            pbelow(t), pabove(t))
        !$omp end parallel

        ! Summed in thread order: the grouping is fixed by the survivor count and the team size.
        do t = 1, nt
            do i = 1, self%nc
                self%acc(i) = self%acc(i) + part(i, t)
            end do
            self%w_below = self%w_below + pbelow(t)
            self%w_above = self%w_above + pabove(t)
        end do

    end subroutine deposit


    !> `.true.` where the estimate is zero by definition: beyond a bound of the support. Only the
    !! pad can be there, the grid's own range lying inside the support by `%init`'s rule.
    pure function outside_support(self, t) result(res)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64), intent(in)       :: t    !! the position
        logical                        :: res  !! it is outside the support

        res = .false.
        if (self%has_lower) res = t < self%lo
        if (res) return
        if (self%has_upper) res = t > self%hi

    end function outside_support

    !> The transform array's value at the position `p`, interpolated linearly between the two
    !! centres around it: what a cell's reflected image contributes to it. Exact wherever the bound
    !! sits on the half-grid, which is every aligned case; second order otherwise, the same order
    !! the binning itself carries. A position beyond the array's ends is zero, which needs a kernel
    !! wider than the whole support to reach.
    pure function gather_at(self, cb, val, p) result(v)
        class(pf_kde_grid), intent(in) :: self   !! the grid
        real(real64), intent(in)       :: cb(:)  !! the array's centres
        real(real64), intent(in)       :: val(:) !! the array, before the fold
        real(real64), intent(in)       :: p      !! where the image is read
        real(real64)                   :: v      !! its value there

        real(real64) :: u, f
        integer :: k

        v = 0.0_real64
        u = (p - cb(1))/self%dx
        if (.not. (u >= 0.0_real64)) return
        if (u > real(self%ntr - 1, real64)) return
        k = int(u) + 1
        if (k >= self%ntr) then
            v = val(self%ntr)
            return
        end if
        f = u - real(k - 1, real64)
        v = val(k)*(1.0_real64 - f) + val(k + 1)*f

    end function gather_at


    !> Bins the survivors `x(1:m)` under `method="binned"`: the same shape `deposit` has, and the
    !! same promise -- a static share of the points per thread, a private bin array each, summed in
    !! thread order, so the answer is bit-identical at a given thread count.
    subroutine bin_points(self, x, w, weighted, hb, hstride, m, threads)
#ifdef _OPENMP
        use omp_lib, only : omp_get_thread_num, omp_get_num_threads
#endif
        class(pf_kde_grid), intent(inout) :: self     !! the grid
        real(real64), intent(in)          :: x(:)     !! the survivors, inside the support
        real(real64), intent(in)          :: w(:)     !! their weights, when weighted
        logical, intent(in)               :: weighted !! `w` holds one weight per survivor
        real(real64), intent(in)          :: hb(:)    !! the bandwidths, one per survivor or one for all
        integer(int64), intent(in)        :: hstride  !! 1 for one per survivor, 0 for one for all
        integer(int64), intent(in)        :: m        !! how many survivors
        integer, intent(in), optional     :: threads  !! the caller's request

        real(real64), allocatable :: cb(:), part(:, :, :), pbelow(:), pabove(:)
        integer(int64) :: team, lo_t, hi_t
        integer :: t, nt, i, c

        allocate(cb(self%ntr))
        call kde_padded_centres(self, cb)
        team = add_team(self, threads, m)
        if (team <= 1_int64) then
            kde_team_used = 1
            call kde_bin_range(self, cb, x, w, weighted, hb, hstride, 1_int64, m, self%bins, self%w_below, &
                self%w_above)
            return
        end if

        ! One partial bin array per thread, in a shared array allocated before the region and
        ! indexed by the thread number; each thread fills its own page and nothing else.
        allocate(part(self%ntr, self%nclass, team), pbelow(team), pabove(team))
        nt = 1
        t = 1
        lo_t = 1_int64
        hi_t = m
        !$omp parallel num_threads(int(team)) default(shared) private(t, lo_t, hi_t)
#ifdef _OPENMP
        t = omp_get_thread_num() + 1
        !$omp single
        nt = omp_get_num_threads()
        kde_team_used = nt
        !$omp end single
        lo_t = (m*int(t - 1, int64))/int(nt, int64) + 1_int64
        hi_t = (m*int(t, int64))/int(nt, int64)
#endif
        part(:, :, t) = 0.0_real64
        pbelow(t) = 0.0_real64
        pabove(t) = 0.0_real64
        if (hi_t >= lo_t) call kde_bin_range(self, cb, x, w, weighted, hb, hstride, lo_t, hi_t, part(:, :, t), &
            pbelow(t), pabove(t))
        !$omp end parallel

        ! Summed in thread order: the grouping is fixed by the survivor count and the team size.
        do t = 1, nt
            do c = 1, self%nclass
                do i = 1, self%ntr
                    self%bins(i, c) = self%bins(i, c) + part(i, c, t)
                end do
            end do
            self%w_below = self%w_below + pbelow(t)
            self%w_above = self%w_above + pabove(t)
        end do

    end subroutine bin_points


    !> Deposits the points `x(jlo:jhi)`, with the weights `w` when `weighted` and the bandwidth of
    !! point `j` at `hb(1 + (j - 1)*hstride)`, into `acc`, and the weight their kernels put below
    !! `xmin` and above `xmax` into `below` and `above`. One loop for one bandwidth and for many,
    !! so that an adaptive rule giving every point the global bandwidth deposits the same bits.
    subroutine deposit_range(self, x, w, weighted, hb, hstride, jlo, jhi, acc, below, above)
        class(pf_kde_grid), intent(in) :: self     !! the grid, read for its geometry
        real(real64), intent(in)       :: x(:)     !! the points, inside the support
        real(real64), intent(in)       :: w(:)     !! their weights, when weighted
        logical, intent(in)            :: weighted !! `w` holds one weight per point
        real(real64), intent(in)       :: hb(:)    !! the bandwidths, one per point or one for all
        integer(int64), intent(in)     :: hstride  !! 1 for one per point, 0 for one for all
        integer(int64), intent(in)     :: jlo      !! the first point to deposit
        integer(int64), intent(in)     :: jhi      !! the last point to deposit
        real(real64), intent(inout)    :: acc(:)   !! the cells to deposit into
        real(real64), intent(inout)    :: below    !! the weight below `xmin`
        real(real64), intent(inout)    :: above    !! the weight above `xmax`

        real(real64), allocatable :: z(:), k(:), k2(:)
        real(real64) :: wj, hmax, span
        integer(int64) :: j
        integer :: nw

        ! The work rows hold the cells ONE kernel reaches, not the whole grid. Cell `ilo` is
        ! `ceiling(tl)` and cell `ihi` is `floor(th)` with `th - tl = 2*R*h_j/dx`, so no point of
        ! this range can touch more than `floor(2*R*hmax/dx) + 1` of them; the bound is formed from
        ! the widest bandwidth in the range, and falls back to the whole grid wherever the quotient
        ! is too large to be an `integer`.
        hmax = hb(1)
        if (hstride /= 0_int64) then
            do j = jlo, jhi
                if (hb(j) > hmax) hmax = hb(j)
            end do
        end if
        span = 2.0_real64*KDE_RADIUS(self%kernel_code)*hmax/self%dx
        nw = self%nc
        if (span < real(self%nc, real64)) nw = min(self%nc, int(span) + 2)
        allocate(z(nw), k(nw), k2(nw))
        wj = 1.0_real64
        do j = jlo, jhi
            if (weighted) wj = w(j)
            call deposit_one(self, x(j), wj, hb(1_int64 + (j - 1_int64)*hstride), z, k, k2, acc, below, above)
        end do

    end subroutine deposit_range

    !> The pilot's deposit: the survivors cut into `np` pieces, each deposited into a partial grid of
    !! its own, the partials added in piece order. The cut is `np`'s alone, never the team's, so a
    !! team of any size -- one included -- answers the same bits; the team takes the pieces in
    !! turn. One piece is the serial deposit straight into the grid.
    subroutine deposit_parts(self, x, w, weighted, hb, hstride, m, threads, np)
#ifdef _OPENMP
        use omp_lib, only : omp_get_num_threads
#endif
        class(pf_kde_grid), intent(inout) :: self     !! the grid, empty
        real(real64), intent(in)          :: x(:)     !! the survivors, inside the support
        real(real64), intent(in)          :: w(:)     !! their weights, when weighted
        logical, intent(in)               :: weighted !! `w` holds one weight per survivor
        real(real64), intent(in)          :: hb(:)    !! the bandwidths, one per survivor or one for all
        integer(int64), intent(in)        :: hstride  !! 1 for one per survivor, 0 for one for all
        integer(int64), intent(in)        :: m        !! how many survivors
        integer, intent(in), optional     :: threads  !! the caller's request
        integer(int64), intent(in)        :: np       !! the pieces

        real(real64), allocatable :: part(:, :), pbelow(:), pabove(:)
        integer(int64) :: p, lo_p, hi_p, nt
        integer :: team, i

        if (np <= 1_int64) then
            kde_team_used = 1
            call deposit_range(self, x, w, weighted, hb, hstride, 1_int64, m, self%acc, self%w_below, &
                self%w_above)
            return
        end if
        team = 1
#ifdef _OPENMP
        call resolve_thread_count(threads, m, nt)
        team = int(max(1_int64, min(nt, np)))
#else
        ! No OpenMP: the pieces are deposited one after another, into the same partial grids.
        nt = 1_int64
        if (present(threads)) team = int(max(1_int64, nt))
#endif
        allocate(part(self%nc, np), pbelow(np), pabove(np))
        kde_team_used = 1
        !$omp parallel num_threads(team) if(team > 1) default(shared) private(p, lo_p, hi_p)
#ifdef _OPENMP
        !$omp single
        kde_team_used = omp_get_num_threads()
        !$omp end single
#endif
        !$omp do schedule(static)
        do p = 1_int64, np
            lo_p = (m*(p - 1_int64))/np + 1_int64
            hi_p = (m*p)/np
            part(:, p) = 0.0_real64
            pbelow(p) = 0.0_real64
            pabove(p) = 0.0_real64
            if (hi_p >= lo_p) call deposit_range(self, x, w, weighted, hb, hstride, lo_p, hi_p, part(:, p), &
                pbelow(p), pabove(p))
        end do
        !$omp end do
        !$omp end parallel

        ! Added in piece order: the grouping is fixed by `np` and the survivor count.
        do p = 1_int64, np
            do i = 1, self%nc
                self%acc(i) = self%acc(i) + part(i, p)
            end do
            self%w_below = self%w_below + pbelow(p)
            self%w_above = self%w_above + pabove(p)
        end do

    end subroutine deposit_parts

    !> How many pieces the pilot's deposit of `m` survivors is cut into: at least
    !! `KDE_ADD_MIN_PER_THREAD` survivors a piece, no more than `KDE_PILOT_PARTS`, and no more than
    !! the deposit's work over the grid's size, since each piece zeroes and then adds one whole
    !! partial grid. A function of the sample and the pilot alone.
    pure function pilot_parts(self, m) result(np)
        class(pf_kde_grid), intent(in) :: self !! the pilot, geometry set
        integer(int64), intent(in)     :: m    !! the survivors
        integer(int64)                 :: np   !! the pieces

        real(real64) :: cells, cap

        np = min(KDE_PILOT_PARTS, m/KDE_ADD_MIN_PER_THREAD)
        cells = min(real(self%nc, real64), 2.0_real64*KDE_RADIUS(self%kernel_code)*(self%h/self%dx) + 1.0_real64)
        cap = real(m, real64)*(cells/real(self%nc, real64))
        if (cap < real(np, real64)) np = int(cap, int64)
        if (np < 1_int64) np = 1_int64

    end function pilot_parts

    !> Deposits one point `xj` of weight `wj` and bandwidth `hj`. `z`, `k` and `k2` are work rows
    !! of one element per cell.
    subroutine deposit_one(self, xj, wj, hj, z, k, k2, acc, below, above)
        class(pf_kde_grid), intent(in) :: self   !! the grid, read for its geometry
        real(real64), intent(in)       :: xj     !! the point, inside the support
        real(real64), intent(in)       :: wj     !! its weight
        real(real64), intent(in)       :: hj     !! its bandwidth
        real(real64), intent(inout)    :: z(:)   !! work: the offsets of the cells it reaches
        real(real64), intent(inout)    :: k(:)   !! work: the kernel's value at each
        real(real64), intent(inout)    :: k2(:)  !! work: an image's value at each
        real(real64), intent(inout)    :: acc(:) !! the cells to deposit into
        real(real64), intent(inout)    :: below  !! the weight below `xmin`
        real(real64), intent(inout)    :: above  !! the weight above `xmax`

        real(real64) :: reach, tl, th, mass, s_below, s_above, inside, sumk, f, img
        integer :: ilo, ihi, n, i, ic
        logical :: need_mass

        reach = KDE_RADIUS(self%kernel_code)*hj
        ! Wholly beyond an end of the range: counted there, not located.
        if (xj + reach < self%x0) then
            below = below + wj
            return
        end if
        if (xj - reach > self%x1) then
            above = above + wj
            return
        end if

        ! ---- under `"linear"`, a corrected point deposits its boundary weights as they are ----
        ! A point clear of every zone takes the plain path below, so it costs what it costs today.
        if (kde_is_corrected(self%boundary_code)) then
            need_mass = .false.
            if (self%has_lower) need_mass = xj - reach < self%lo + reach
            if (self%has_upper) need_mass = need_mass .or. xj + reach > self%hi - reach
            if (need_mass) then
                call deposit_corrected(self, xj, wj, hj, reach, z, k, acc, below, above)
                return
            end if
        end if

        ! ---- the shares: inside the support, below `xmin` and above `xmax` ----
        ! `S(t)`, the images' weight at or below `t`, is formed only where a share can differ
        ! from its plain value, so a kernel wholly inside the range and clear of every bound pays
        ! for no distribution function at all.
        need_mass = .false.
        select case (self%boundary_code)
        case (KDE_BOUNDARY_REFLECT)
            ! The images keep the mass inside unless the kernel is wider than the whole support,
            ! where the doubly reflected terms they omit would carry some (`pf_kde`'s rule, F3).
            if (self%has_lower .and. self%has_upper) need_mass = reach > self%hi - self%lo
        end select
        mass = 1.0_real64
        if (need_mass) mass = images_cdf(self, xj, hj, upper_end(self)) - images_cdf(self, xj, hj, lower_end(self))
        s_below = 0.0_real64
        if (xj - reach < self%x0) then
            if (.not. self%has_lower .or. self%lo < self%x0) &
                s_below = images_cdf(self, xj, hj, self%x0) - images_cdf(self, xj, hj, lower_end(self))
        end if
        s_above = 0.0_real64
        if (xj + reach > self%x1) then
            if (.not. self%has_upper .or. self%hi > self%x1) &
                s_above = images_cdf(self, xj, hj, upper_end(self)) - images_cdf(self, xj, hj, self%x1)
        end if
        inside = mass - s_below - s_above
        if (s_below > 0.0_real64) below = below + wj*(s_below/mass)
        if (s_above > 0.0_real64) above = above + wj*(s_above/mass)
        if (.not. (inside > 0.0_real64)) return

        ! ---- the cells it reaches, and the kernel (with its images) at each centre ----
        tl = (xj - reach - self%x0)/self%dx + 0.5_real64
        th = (xj + reach - self%x0)/self%dx + 0.5_real64
        ilo = 1
        if (tl > 1.0_real64) ilo = int(ceiling(tl, int64))
        ihi = self%nc
        if (th < real(self%nc, real64)) ihi = int(floor(th, int64))
        n = ihi - ilo + 1
        sumk = 0.0_real64
        if (n > 0) then
            do i = 1, n
                z(i) = (centre(self, ilo + i - 1) - xj)/hj
            end do
            call kde_kernel_pdf_many(self%kernel_code, z(1:n), k(1:n))
            if (self%boundary_code == KDE_BOUNDARY_REFLECT) then
                ! An image reaches back into the support only from within one reach of its bound.
                if (self%has_lower) then
                    if (xj - self%lo < reach) then
                        img = 2.0_real64*self%lo - xj
                        call add_image(img)
                    end if
                end if
                if (self%has_upper) then
                    if (self%hi - xj < reach) then
                        img = 2.0_real64*self%hi - xj
                        call add_image(img)
                    end if
                end if
            end if
            do i = 1, n
                sumk = sumk + k(i)
            end do
        end if

        ! ---- the deposit: the in-range share, spread over the centres in proportion ----
        if (sumk > 0.0_real64) then
            f = wj*(inside/mass)/(self%dx*sumk)
            do i = 1, n
                acc(ilo + i - 1) = acc(ilo + i - 1) + f*k(i)
            end do
        else
            ! Narrower than a cell and between two centres: whole into the cell holding it.
            ic = 1
            if (xj > self%x0) ic = int(min(real(self%nc, real64), (xj - self%x0)/self%dx + 1.0_real64))
            ic = max(1, min(self%nc, ic))
            acc(ic) = acc(ic) + wj*(inside/mass)/self%dx
        end if

    contains

        !> Adds the kernel of the image at `pos` to the row `k`.
        subroutine add_image(pos)
            real(real64), intent(in) :: pos !! the image's position

            integer :: ii

            do ii = 1, n
                z(ii) = (centre(self, ilo + ii - 1) - pos)/hj
            end do
            call kde_kernel_pdf_many(self%kernel_code, z(1:n), k2(1:n))
            do ii = 1, n
                k(ii) = k(ii) + k2(ii)
            end do

        end subroutine add_image

    end subroutine deposit_one

    !> Deposits one point whose linear boundary correction is acting somewhere it reaches: the
    !! value the exact form would give each cell centre, `w_j (a_2 - a_1 u) K(u)/(D h_j)`, with NO
    !! per-point normalisation to the in-range share.
    !!
    !! A point deposits its linear boundary mass, which is near but not exactly its weight; the
    !! normalisation is the estimate's, once, at query time (`total_mass`). Cells can go negative,
    !! and nothing is clipped here. What the kernel puts beyond a FREE edge is counted by the plain
    !! kernel's mass there, which R1, R2 and R3 keep exact: beyond a free edge the correction is not
    !! acting.
    subroutine deposit_corrected(self, xj, wj, hj, reach, z, k, acc, below, above)
        class(pf_kde_grid), intent(in) :: self   !! the grid, read for its geometry
        real(real64), intent(in)       :: xj     !! the point, inside the support
        real(real64), intent(in)       :: wj     !! its weight
        real(real64), intent(in)       :: hj     !! its bandwidth
        real(real64), intent(in)       :: reach  !! its kernel's reach, `R h_j`
        real(real64), intent(inout)    :: z(:)   !! work: the offsets of the cells it reaches
        real(real64), intent(inout)    :: k(:)   !! work: the kernel's value at each
        real(real64), intent(inout)    :: acc(:) !! the cells to deposit into
        real(real64), intent(inout)    :: below  !! the weight below `xmin`
        real(real64), intent(inout)    :: above  !! the weight above `xmax`

        type(kde_corr_kernel) :: cf
        type(kde_point_fn) :: fn
        real(real64) :: tl, th, s_below, s_above, rj, c, v, a, b
        integer :: ilo, ihi, n, i, ic

        rj = 1.0_real64/hj
        ! What the term puts outside the range but inside the support: the CORRECTED term's own
        ! integral there, so that the cells and the two counters are the one density and still sum
        ! to its whole mass. Beyond a free edge the correction is not acting, and the integral is
        ! then the plain kernel's distribution function to the bit -- which is every case `"linear"`
        ! can reach, R1 and R2 keeping its bounded edges at the bounds.
        fn%code = self%kernel_code
        fn%bcode = self%boundary_code
        fn%has_lower = self%has_lower
        fn%lo = self%lo
        fn%has_upper = self%has_upper
        fn%hi = self%hi
        fn%xj = xj
        fn%hj = hj
        fn%r = rj
        fn%absolute = .false.
        a = xj - reach
        if (self%has_lower) a = max(a, self%lo)
        b = xj + reach
        if (self%has_upper) b = min(b, self%hi)
        s_below = 0.0_real64
        if (xj - reach < self%x0) then
            if (.not. self%has_lower .or. self%lo < self%x0) &
                s_below = kde_term_integral(fn, a, self%x0, 0.0_real64, .false.)
        end if
        s_above = 0.0_real64
        if (xj + reach > self%x1) then
            if (.not. self%has_upper .or. self%hi > self%x1) &
                s_above = kde_term_integral(fn, self%x1, b, 0.0_real64, .false.)
        end if
        if (s_below > 0.0_real64) below = below + wj*s_below
        if (s_above > 0.0_real64) above = above + wj*s_above

        ! The cells it reaches, and the corrected kernel at each centre: the moments are the
        ! centre's own, so a cell inside a zone gets the weight the exact form gives it there.
        tl = (xj - reach - self%x0)/self%dx + 0.5_real64
        th = (xj + reach - self%x0)/self%dx + 0.5_real64
        ilo = 1
        if (tl > 1.0_real64) ilo = int(ceiling(tl, int64))
        ihi = self%nc
        if (th < real(self%nc, real64)) ihi = int(floor(th, int64))
        n = ihi - ilo + 1
        if (n > 0) then
            do i = 1, n
                z(i) = (centre(self, ilo + i - 1) - xj)/hj
            end do
            call kde_kernel_pdf_many(self%kernel_code, z(1:n), k(1:n))
            do i = 1, n
                c = centre(self, ilo + i - 1)
                ! A centre a whole reach from every bound has a plain kernel there, which is what
                ! `kde_corr_factor` would answer after forming its interval: the same value,
                ! without the moments. Most cells of a wide grid are in this case.
                if (plain_at(self, c, rj)) then
                    v = k(i)
                else
                    cf = kde_corr_factor(self%boundary_code, self%kernel_code, self%has_lower, &
                        self%lo, self%has_upper, self%hi, c, rj)
                    v = kde_corr_value(cf, z(i), k(i))
                end if
                acc(ilo + i - 1) = acc(ilo + i - 1) + wj*v*rj
            end do
            return
        end if
        ! Narrower than a cell and between two centres: its in-range share goes whole into the cell
        ! holding it, as the plain deposit does.
        ic = 1
        if (xj > self%x0) ic = int(min(real(self%nc, real64), (xj - self%x0)/self%dx + 1.0_real64))
        ic = max(1, min(self%nc, ic))
        acc(ic) = acc(ic) + wj*(1.0_real64 - s_below - s_above)/self%dx

    end subroutine deposit_corrected

    !> `.true.` when a kernel of reach `reach` centred at `t` is clear of every bound, so that the
    !! correction is not acting there and the corrected kernel is the plain one. The same test
    !! `kde_corr_factor` makes on its own interval, made before the interval is formed.
    pure function plain_at(self, t, r) result(res)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64), intent(in)       :: t    !! the query
        real(real64), intent(in)       :: r    !! one over the point's bandwidth
        logical                        :: res  !! the kernel is plain there

        real(real64) :: rad

        ! `kde_corr_factor`'s own comparison, operand for operand: it clips its interval only where
        ! `(t - lo)*r < rad`, so the test has to be made on that product and not on `t - lo` against
        ! `rad*h_j`, which can land on the other side of the boundary by a last bit.
        rad = KDE_RADIUS(self%kernel_code)
        res = .true.
        if (self%has_lower) res = .not. ((t - self%lo)*r < rad)
        if (.not. res) return
        if (self%has_upper) res = .not. ((self%hi - t)*r < rad)

    end function plain_at

    !> The support's lower end, `-Infinity` when unbounded.
    pure function lower_end(self) result(t)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64)                   :: t    !! the lower end

        t = ieee_value(1.0_real64, ieee_negative_inf)
        if (self%has_lower) t = self%lo

    end function lower_end

    !> The support's upper end, `+Infinity` when unbounded.
    pure function upper_end(self) result(t)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64)                   :: t    !! the upper end

        t = ieee_value(1.0_real64, ieee_positive_inf)
        if (self%has_upper) t = self%hi

    end function upper_end

    !> A cell's density, its accumulation `a` over the total weight `wt`: the one division
    !! `%density` and `%pdf` at a centre share, so that the two agree to the bit. The divisor is
    !! `volatile`: ifx's default `-fp-model=fast` otherwise turns a division repeated over a loop
    !! into a multiply by the reciprocal, which rounds differently from a single division.
    function cell_value(a, wt) result(f)
        real(real64), intent(in) :: a  !! the cell's accumulation
        real(real64), intent(in) :: wt !! the total weight, positive
        real(real64)             :: f  !! its density

        real(real64), volatile :: d

        d = wt
        f = a/d

    end function cell_value

    !> `.true.` when every query must answer NaN: a kept NaN, a pilot with no density to read, a
    !! bandwidth the adaptive rule could not represent, or R3's kernel wider than a grid with a free
    !! edge. Every query reads both flags through this one helper.
    pure function grid_poisoned(self) result(res)
        class(pf_kde_grid), intent(in) :: self !! the grid
        logical                        :: res  !! every query answers NaN

        res = self%poisoned .or. self%reach_poisoned

    end function grid_poisoned

    !> Cell `i`'s accumulation as the queries read it: under `"linear"` the cells can go negative,
    !! and the clip is applied HERE, at query time, so that a merge of two grids and one grid over
    !! the concatenation answer the same bits.
    pure function cell_acc(self, i) result(a)
        class(pf_kde_grid), intent(in) :: self !! the grid
        integer, intent(in)            :: i    !! the cell
        real(real64)                   :: a    !! its accumulation, clipped where the correction needs it

        a = self%acc(i)
        if (kde_is_corrected(self%boundary_code)) then
            if (.not. (a > 0.0_real64)) a = 0.0_real64
        end if

    end function cell_acc

    !> The mass the queries normalise by: the total weight, and under `"linear"` the CLIPPED
    !! estimate's own mass, `step * sum(max(acc, 0))` plus what the kernels put beyond each end.
    !! Formed per call, as the running integral is; a grid whose clipped cells hold nothing answers
    !! as an empty one.
    pure function total_mass(self) result(t)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64)                   :: t    !! the mass

        ! Once the grid is closed this is what `%finish` left; `%print`, which answers at any point
        ! in the lifecycle, is the one caller that can still reach the sum itself.
        if (self%finished) then
            t = self%mass_total
        else
            t = accumulated_mass(self)
        end if

    end function total_mass

    !> The mass the queries normalise by, summed from the cells: the total weight, and under a
    !! local-polynomial correction the CLIPPED estimate's own mass, `step * sum(max(acc, 0))` plus
    !! what the kernels put beyond each end. `%finish` calls it once and every query then reads
    !! what it left.
    pure function accumulated_mass(self) result(t)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64)                   :: t    !! the mass

        integer :: i
        real(real64) :: s

        if (.not. kde_is_corrected(self%boundary_code)) then
            t = self%w_total
            return
        end if
        s = 0.0_real64
        do i = 1, self%nc
            if (self%acc(i) > 0.0_real64) s = s + self%acc(i)
        end do
        t = s*self%dx + self%w_below + self%w_above

    end function accumulated_mass

    !> Cell `i`'s density on this grid, which holds weight.
    function cell_density(self, i) result(f)
        class(pf_kde_grid), intent(in) :: self !! the grid, with weight in it
        integer, intent(in)            :: i    !! the cell
        real(real64)                   :: f    !! its density

        f = cell_value(cell_acc(self, i), total_mass(self))

    end function cell_density

    !> The interpolated density at `t`: NaN on an empty or poisoned grid, else `interp_cells`.
    function pdf_value(self, t) result(f)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64), intent(in)       :: t    !! where to evaluate
        real(real64)                   :: f    !! the density

        real(real64) :: wt

        f = ieee_value(1.0_real64, ieee_quiet_nan)
        if (grid_poisoned(self)) return
        wt = total_mass(self)
        if (.not. (wt > 0.0_real64)) return
        f = interp_cells(self%nc, self%x0, self%x1, self%dx, self%acc, wt, t, &
            kde_is_corrected(self%boundary_code))

    end function pdf_value

    !> The density cells `acc` of total weight `wt` describe at `t`: linear between neighbouring
    !! centres, constant over the outer half-cells, zero outside `[x0, x1]`, NaN at a NaN `t`. At
    !! a centre it is `cell_value` itself, the value `%density` answers there. The one
    !! interpolation, for a grid's `%pdf` and for the adaptive rule's look-up of its pilot.
    function interp_cells(nc, x0, x1, dx, acc, wt, t, clip) result(f)
        integer, intent(in)      :: nc      !! the number of cells
        real(real64), intent(in) :: x0      !! the first cell's left edge
        real(real64), intent(in) :: x1      !! the last cell's right edge
        real(real64), intent(in) :: dx      !! the cell width
        real(real64), intent(in) :: acc(:)  !! each cell's accumulation
        real(real64), intent(in) :: wt      !! the mass to divide by, positive
        real(real64), intent(in) :: t       !! where to evaluate
        logical, intent(in)      :: clip    !! the cells are clipped at zero first (`"linear"`)
        real(real64)             :: f       !! the density

        integer :: j
        real(real64) :: s, aj, aj1

        f = ieee_value(1.0_real64, ieee_quiet_nan)
        if (t /= t) return
        f = 0.0_real64
        if (t < x0 .or. t > x1) return
        if (t <= centre_at(x0, dx, 1)) then
            f = cell_value(clipped(acc(1), clip), wt)
            return
        end if
        if (t >= centre_at(x0, dx, nc)) then
            f = cell_value(clipped(acc(nc), clip), wt)
            return
        end if
        j = segment_at(nc, x0, dx, t)
        s = (t - centre_at(x0, dx, j))/dx
        if (s == 0.0_real64) then
            f = cell_value(clipped(acc(j), clip), wt)
            return
        end if
        aj = clipped(acc(j), clip)
        aj1 = clipped(acc(j + 1), clip)
        f = ((1.0_real64 - s)*aj + s*aj1)/wt

    end function interp_cells

    !> A cell's accumulation with the clip applied where the caller asks for it.
    pure function clipped(a, clip) result(v)
        real(real64), intent(in) :: a    !! the accumulation
        logical, intent(in)      :: clip !! apply the clip
        real(real64)             :: v    !! the value the queries read

        v = a
        if (clip) then
            if (.not. (v > 0.0_real64)) v = 0.0_real64
        end if

    end function clipped

    !> The `j` in `1 .. ncells - 1` with `centre(j) <= t < centre(j + 1)`, for a `t` strictly
    !! between the first and the last centre.
    pure function segment(self, t) result(j)
        class(pf_kde_grid), intent(in) :: self !! the grid, with two cells or more
        real(real64), intent(in)       :: t    !! the point
        integer                        :: j    !! the segment

        j = segment_at(self%nc, self%x0, self%dx, t)

    end function segment

    !> `segment` on a grid of `nc` cells `dx` wide from `x0`.
    pure function segment_at(nc, x0, dx, t) result(j)
        integer, intent(in)      :: nc !! the number of cells, two or more
        real(real64), intent(in) :: x0 !! the first cell's left edge
        real(real64), intent(in) :: dx !! the cell width
        real(real64), intent(in) :: t  !! the point
        integer                  :: j  !! the segment

        real(real64) :: u

        ! Centre `j` sits at `u = j - 1`, so the segment holding `t` is `floor(u) + 1`.
        u = (t - x0)/dx - 0.5_real64
        j = 1
        if (u > 0.0_real64) j = int(min(u, real(nc, real64))) + 1
        j = max(1, min(nc - 1, j))
        ! The quotient can land one either side of the true segment; the centres decide.
        if (j > 1) then
            if (t < centre_at(x0, dx, j)) j = j - 1
        end if
        if (j < nc - 1) then
            if (t >= centre_at(x0, dx, j + 1)) j = j + 1
        end if

    end function segment_at

    !> The bandwidth the adaptive rule `a` gives a point at `t` under the global bandwidth `h`.
    !!
    !! `h * (p(t)/g)**(-alpha)`, formed as `h * exp(-alpha * (log p - log g))` with the exponent
    !! tested before it is raised, so that a bandwidth too large to represent is `+Infinity`
    !! without an overflow (which would stop a program under nagfor). Exactly `h` at `alpha = 0`,
    !! then capped at `bandwidth_max`. Where the pilot reads zero, the cap is the answer; without
    !! one, the pilot's smallest positive cell density stands in for `p`. NaN from a table with
    !! nothing to read, whatever `alpha`.
    function rule_bandwidth(a, h, elim, t) result(hj)
        type(kde_adapt), intent(in) :: a    !! the rule
        real(real64), intent(in)    :: h    !! the global bandwidth
        real(real64), intent(in)    :: elim !! the largest exponent `h*exp(e)` can be formed at
        real(real64), intent(in)    :: t    !! the point
        real(real64)                :: hj   !! its bandwidth

        real(real64) :: p, e

        hj = ieee_value(1.0_real64, ieee_quiet_nan)
        if (a%unreadable) return
        if (t /= t) return
        if (a%alpha == 0.0_real64) then
            hj = h
        else
            p = interp_cells(a%nc, a%x0, a%x1, a%dx, a%acc, a%wt, t, .false.)
            if (.not. (p > 0.0_real64)) then
                if (a%has_bmax) then
                    hj = a%bmax
                    return
                end if
                p = a%pmin
            end if
            e = -a%alpha*(log(p) - a%logg)
            if (e > elim) then
                hj = ieee_value(1.0_real64, ieee_positive_inf)
            else
                hj = h*exp(e)
            end if
        end if
        if (a%has_bmax) then
            if (hj > a%bmax) hj = a%bmax
        end if

    end function rule_bandwidth

    !> Fills `%cum`: `cum(i)` is the accumulated weight per unit length integrated from `xmin` to
    !! centre `i`, under `%pdf`'s interpolant -- half a cell of `acc(1)` to the first centre, then a
    !! trapezoid per segment. Formed once by `%finish`; `%cdf`, `%quantile` and `%sample` read it.
    subroutine fill_running_mass(self)
        class(pf_kde_grid), intent(inout) :: self !! the grid, its cells final

        integer :: i

        if (allocated(self%cum)) deallocate(self%cum)
        allocate(self%cum(self%nc))
        self%cum(1) = 0.5_real64*cell_acc(self, 1)*self%dx
        do i = 2, self%nc
            self%cum(i) = self%cum(i - 1) + 0.5_real64*(cell_acc(self, i - 1) + cell_acc(self, i))*self%dx
        end do

    end subroutine fill_running_mass

    !> `P(X <= t)` from the running integral `q`: the weight below `xmin`, plus `%pdf`'s integral
    !! from `xmin` to `t`, over the total weight; exactly 0 and 1 beyond the bounds.
    pure function cdf_value(self, t) result(p)
        class(pf_kde_grid), intent(in) :: self !! the grid, finished
        real(real64), intent(in)       :: t    !! where to evaluate
        real(real64)                   :: p    !! the probability at or below `t`

        integer :: j
        real(real64) :: s, a, b, mass, wt

        p = ieee_value(1.0_real64, ieee_quiet_nan)
        if (grid_poisoned(self)) return
        wt = total_mass(self)
        if (.not. (wt > 0.0_real64)) return
        if (t /= t) return
        if (self%has_lower) then
            p = 0.0_real64
            if (t <= self%lo) return
        end if
        if (self%has_upper) then
            p = 1.0_real64
            if (t >= self%hi) return
        end if
        if (t < self%x0) then
            mass = self%w_below
        else if (t >= self%x1) then
            mass = self%w_below + self%cum(self%nc) + 0.5_real64*cell_acc(self, self%nc)*self%dx
        else if (t <= centre(self, 1)) then
            mass = self%w_below + cell_acc(self, 1)*(t - self%x0)
        else if (t >= centre(self, self%nc)) then
            mass = self%w_below + self%cum(self%nc) + cell_acc(self, self%nc)*(t - centre(self, self%nc))
        else
            j = segment(self, t)
            s = (t - centre(self, j))/self%dx
            a = cell_acc(self, j)
            b = cell_acc(self, j + 1)
            mass = self%w_below + self%cum(j) + self%dx*s*(a + 0.5_real64*(b - a)*s)
        end if
        p = mass/wt
        if (p < 0.0_real64) p = 0.0_real64
        if (p > 1.0_real64) p = 1.0_real64

    end function cdf_value

    !> The smallest point of `[xmin, xmax]` at which `%cdf` reaches `p`, from the running integral
    !! `q`. A quantile the weight below `xmin` covers is `xmin`; one only the weight above `xmax`
    !! reaches is `xmax`; otherwise the linear piece of an outer half-cell or the quadratic piece
    !! between two centres is solved for it.
    pure function quantile_value(self, p) result(x)
        class(pf_kde_grid), intent(in) :: self !! the grid, finished
        real(real64), intent(in)       :: p    !! the probability, in `[0, 1]`
        real(real64)                   :: x    !! the quantile

        real(real64) :: target, total, wt

        x = ieee_value(1.0_real64, ieee_quiet_nan)
        if (grid_poisoned(self)) return
        wt = total_mass(self)
        if (.not. (wt > 0.0_real64)) return
        total = self%cum(self%nc) + 0.5_real64*cell_acc(self, self%nc)*self%dx
        target = p*wt - self%w_below
        if (p <= 0.0_real64 .or. target <= 0.0_real64) then
            x = self%x0
            if (.not. (self%w_below > 0.0_real64)) x = left_end(self)
            return
        end if
        if (p >= 1.0_real64 .or. target >= total) then
            x = self%x1
            if (.not. (self%w_above > 0.0_real64)) x = right_end(self)
            return
        end if
        x = mass_at(self, target)

    end function quantile_value

    !> The point of `[xmin, xmax]` at which `%pdf`'s integral from `xmin`, over the cells alone,
    !! reaches `target`, strictly between zero and the cells' whole mass: the linear piece of an
    !! outer half-cell, or the quadratic piece between two centres, solved for it. What `%quantile`
    !! and `%sample` both invert.
    pure function mass_at(self, target) result(x)
        class(pf_kde_grid), intent(in) :: self   !! the grid, finished
        real(real64), intent(in)       :: target !! the mass to reach
        real(real64)                   :: x      !! where it is reached

        real(real64) :: d, a, b, s
        integer :: j, lo_j, hi_j, mid

        if (target <= self%cum(1)) then
            ! The first half-cell, where the density is the constant `acc(1)`, positive here.
            x = min(self%x0 + target/cell_acc(self, 1), centre(self, 1))
            return
        end if
        if (target > self%cum(self%nc)) then
            ! The last half-cell, where the density is the constant `acc(ncells)`, positive here.
            x = min(centre(self, self%nc) + (target - self%cum(self%nc))/cell_acc(self, self%nc), self%x1)
            return
        end if
        ! The first segment whose end reaches the target: `self%cum(j) < target <= self%cum(j + 1)`.
        lo_j = 1
        hi_j = self%nc - 1
        do while (lo_j < hi_j)
            mid = lo_j + (hi_j - lo_j)/2
            if (self%cum(mid + 1) >= target) then
                hi_j = mid
            else
                lo_j = mid + 1
            end if
        end do
        j = lo_j
        ! `step*(a*s + (b - a)*s**2/2) = d` solved in its rationalised form, which needs no case
        ! for `a == b` and cannot cancel: `s = 2*d' / (a + sqrt(a**2 + 2*(b - a)*d'))`, `d' = d/step`.
        d = (target - self%cum(j))/self%dx
        a = cell_acc(self, j)
        b = cell_acc(self, j + 1)
        s = 2.0_real64*d/(a + sqrt(max(a*a + 2.0_real64*(b - a)*d, 0.0_real64)))
        s = max(0.0_real64, min(1.0_real64, s))
        x = centre(self, j) + s*self%dx

    end function mass_at

    !> Fills `v` with draws from the grid's density inside its range, element `k` from one uniform
    !! of stream `pf_random_key(stream, k)` under the grid family's key, serially or shared among a
    !! team; NaN when the cells hold nothing to draw from or a kept NaN has poisoned the grid.
    subroutine sample_fill(self, v, seed, stream, threads)
        class(pf_kde_grid), intent(in) :: self    !! the grid
        real(real64), intent(out)      :: v(:)    !! the draws
        integer(int64), intent(in)     :: seed    !! the caller's seed
        integer(int64), intent(in)     :: stream  !! the caller's stream
        integer, intent(in), optional  :: threads !! the caller's request

        character(len=*), parameter :: EP = "pf_kde_grid%sample"
        real(real64) :: total
        integer(int64) :: k, n, key
        integer :: team

        call require_finished(self, EP)
        n = size(v, kind=int64)
        call kde_query_team(EP, threads, n, KDE_DRAW_WORK, team)
        v = ieee_value(1.0_real64, ieee_quiet_nan)
        if (grid_poisoned(self)) return
        if (.not. (total_mass(self) > 0.0_real64)) return
        total = self%cum(self%nc) + 0.5_real64*cell_acc(self, self%nc)*self%dx
        ! Weight counted only beyond the range has no place in it to be drawn at.
        if (.not. (total > 0.0_real64)) return
        key = pf_random_key(seed, KDE_GRID_FAMILY_LABEL)
        if (team <= 1) then
            kde_team_used = 1
            do k = 1_int64, n
                v(k) = cells_draw(self, total, pf_random_at(key, pf_random_key(stream, k), 1_int64))
            end do
            return
        end if
        !$omp parallel num_threads(team) default(shared) private(k)
        call kde_record_team()
        !$omp do schedule(static)
        do k = 1_int64, n
            v(k) = cells_draw(self, total, pf_random_at(key, pf_random_key(stream, k), 1_int64))
        end do
        !$omp end do
        !$omp end parallel

    end subroutine sample_fill

    !> The draw at the uniform `u`: where `%pdf`'s integral over the cells reaches the share `u` of
    !! their mass `total`; `u = 0`, and rounding at the top end, fall on where the accumulated
    !! density starts and ends.
    pure function cells_draw(self, total, u) result(x)
        class(pf_kde_grid), intent(in) :: self  !! the grid, with mass in its cells
        real(real64), intent(in)       :: total !! the cells' whole mass
        real(real64), intent(in)       :: u     !! the uniform, in `[0, 1)`
        real(real64)                   :: x     !! the draw

        real(real64) :: target

        target = u*total
        if (.not. (target > 0.0_real64)) then
            x = left_end(self)
        else if (.not. (target < total)) then
            x = right_end(self)
        else
            x = mass_at(self, target)
        end if

    end function cells_draw

    !> Where the accumulated density starts inside the range: `xmin` when the first cell holds
    !! weight, else the centre before the first cell that does, from which the interpolant rises;
    !! `xmax` when no cell holds any.
    pure function left_end(self) result(x)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64)                   :: x    !! the left end

        integer :: i

        x = self%x1
        do i = 1, self%nc
            if (cell_acc(self, i) > 0.0_real64) then
                x = self%x0
                if (i > 1) x = centre(self, i - 1)
                return
            end if
        end do

    end function left_end

    !> Where the accumulated density ends inside the range: `xmax` when the last cell holds
    !! weight, else the centre after the last cell that does; `xmin` when no cell holds any.
    pure function right_end(self) result(x)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64)                   :: x    !! the right end

        integer :: i

        x = self%x0
        do i = self%nc, 1, -1
            if (cell_acc(self, i) > 0.0_real64) then
                x = self%x1
                if (i < self%nc) x = centre(self, i + 1)
                return
            end if
        end do

    end function right_end

end submodule parquet_kde_grid ! GCOVR_EXCL_LINE
