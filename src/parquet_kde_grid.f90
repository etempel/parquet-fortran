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
!! call and writes nothing.
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
    ! The adaptive rule, for both forms; above every call to it, as nagfor requires
    ! ==========================================================================================

    module procedure kde_adapt_set

        integer :: i
        real(real64) :: p, s1, s2

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
        a%poisoned = pilot%poisoned
        allocate(a%acc(pilot%nc))
        do i = 1, pilot%nc
            a%acc(i) = pilot%acc(i)
        end do
        a%logg = 0.0_real64
        a%pmin = 0.0_real64
        if (a%poisoned) return

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
        end do
        a%logg = s2/s1

    end procedure kde_adapt_set

    module procedure kde_adapt_bandwidths

        integer(int64) :: i

        do i = 1_int64, size(x, kind=int64)
            hb(i) = rule_bandwidth(a, h, x(i))
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
        ok = .true.

    end procedure kde_build_pilot

    ! ==========================================================================================
    ! Set-up and accumulation
    ! ==========================================================================================

    module procedure grid_init

        character(len=*), parameter :: EP = "pf_kde_grid%init"
        real(real64) :: step, lo, hi, a_alpha, a_bmax
        integer :: kcode, bcode
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
        ! Every cell centre inside the support, so that no cell can hold weight the support
        ! excludes and the queries never straddle a bound.
        if (has_lo) then
            if (xmin < lo) call kde_abort(EP, "the grid's range must lie inside the support")
        end if
        if (has_hi) then
            if (xmax > hi) call kde_abort(EP, "the grid's range must lie inside the support")
        end if

        ! ---- the adaptive rule ----
        if (present(alpha) .or. present(bandwidth_max)) then
            if (.not. present(pilot)) call kde_abort(EP, "alpha= and bandwidth_max= need pilot=")
        end if
        call check_adaptive_settings(EP, alpha, bandwidth_max, a_alpha, a_bmax)
        if (present(pilot)) then
            ! A pilot must have a density to read -- a poisoned one answers NaN, and poisons this
            ! grid in turn, quietly -- and must cover the range, so that no point inside it reads
            ! a pilot that was never built there.
            if (.not. pilot%initialised) call kde_abort(EP, "pilot must be an initialised grid with points in it")
            if (.not. pilot%poisoned .and. .not. holds_weight(pilot)) &
                call kde_abort(EP, "pilot must be an initialised grid with points in it")
            if (pilot%x0 > xmin .or. pilot%x1 < xmax) call kde_abort(EP, "the pilot must cover this grid's range")
            ! The table is read out of the pilot before this grid is touched.
            call kde_adapt_set(rule, pilot, a_alpha, present(bandwidth_max), a_bmax)
        end if

        call set_geometry(self, ncells, xmin, xmax, bandwidth, kcode, bcode, has_lo, lo, has_hi, hi)
        call move_adapt(rule, self%adapt)
        call empty_grid(self)

    end procedure grid_init

    module procedure grid_add_f64_r1

        character(len=*), parameter :: EP = "pf_kde_grid%add"
        real(real64), allocatable :: keep_x(:), keep_w(:), hb(:)
        real(real64) :: one(1), v, wsum
        integer(int64) :: nv, nnull, nnan, nout, m, i, stride
        logical :: saw_nan, weighted

        call require_initialised(self, EP)
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
        if (self%poisoned .or. m == 0_int64) return

        ! ---- each point's bandwidth: its own when adaptive, the one for all otherwise ----
        if (self%adapt%on) then
            allocate(hb(m))
            call kde_adapt_bandwidths(self%adapt, self%h, keep_x(1:m), hb)
            ! A bandwidth the rule could only overflow or underflow to leaves no estimate to
            ! answer with: the grid is poisoned, as by a kept NaN.
            do i = 1_int64, m
                if (.not. kde_bandwidth_usable(self%kernel_code, hb(i))) then
                    self%poisoned = .true.
                    return
                end if
            end do
            stride = 1_int64
        else
            allocate(hb(1))
            hb(1) = self%h
            stride = 0_int64
        end if

        ! ---- the deposit ----
        if (weighted) then
            call deposit(self, keep_x, keep_w, .true., hb, stride, m, threads)
        else
            one(1) = 1.0_real64
            call deposit(self, keep_x, one, .false., hb, stride, m, threads)
        end if

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
        call grid_add_f64_r1(self, xs, vs, ws, skipnan, n_null, n_nan, n_outside)

    end procedure grid_add_f64_r0

    module procedure grid_add_f32_r1

        real(real64), allocatable :: xw(:)

        allocate(xw(size(x, kind=int64)))
        xw = real(x, real64)
        call grid_add_f64_r1(self, xw, is_valid, weights, skipnan, n_null, n_nan, n_outside, threads)

    end procedure grid_add_f32_r1

    module procedure grid_add_f32_r0

        call grid_add_f64_r0(self, real(x, real64), is_valid, weights, skipnan, n_null, n_nan, n_outside)

    end procedure grid_add_f32_r0

    module procedure grid_merge

        character(len=*), parameter :: EP = "pf_kde_grid%merge"
        integer :: i

        call require_initialised(self, EP)
        if (.not. other%initialised) call kde_abort(EP, "the other grid has not been initialised")
        if (self%nc /= other%nc) call kde_abort(EP, "the two grids differ in cells")
        if (self%x0 /= other%x0 .or. self%x1 /= other%x1) call kde_abort(EP, "the two grids differ in range")
        if (self%h /= other%h) call kde_abort(EP, "the two grids differ in bandwidth")
        if (self%kernel_code /= other%kernel_code) call kde_abort(EP, "the two grids differ in kernel")
        if (.not. same_support(self, other)) call kde_abort(EP, "the two grids differ in support")
        if (self%boundary_code /= other%boundary_code) call kde_abort(EP, "the two grids differ in boundary")
        if (.not. same_rule(self%adapt, other%adapt)) call kde_abort(EP, "the two grids differ in pilot")

        do i = 1, self%nc
            self%acc(i) = self%acc(i) + other%acc(i)
        end do
        self%w_below = self%w_below + other%w_below
        self%w_above = self%w_above + other%w_above
        self%w_total = self%w_total + other%w_total
        self%cnt_all = self%cnt_all + other%cnt_all
        self%cnt_valid = self%cnt_valid + other%cnt_valid
        self%cnt_null = self%cnt_null + other%cnt_null
        self%cnt_nan = self%cnt_nan + other%cnt_nan
        self%cnt_out = self%cnt_out + other%cnt_out
        self%poisoned = self%poisoned .or. other%poisoned

    end procedure grid_merge

    ! ==========================================================================================
    ! Queries
    ! ==========================================================================================

    module procedure grid_density

        character(len=*), parameter :: EP = "pf_kde_grid%density"
        logical :: norm
        integer :: i

        call require_initialised(self, EP)
        if (size(f, kind=int64) /= int(self%nc, int64)) call kde_abort(EP, "f must have one element per cell")
        if (present(x)) then
            if (size(x, kind=int64) /= int(self%nc, int64)) call kde_abort(EP, "x must have one element per cell")
            call fill_centres(self, x)
        end if
        norm = .true.
        if (present(normalise)) norm = normalise
        if (self%poisoned) then
            f = ieee_value(1.0_real64, ieee_quiet_nan)
        else if (.not. norm) then
            do i = 1, self%nc
                f(i) = self%acc(i)
            end do
        else if (self%w_total > 0.0_real64) then
            do i = 1, self%nc
                f(i) = cell_density(self, i)
            end do
        else
            ! Nothing accumulated: an empty estimate is zero everywhere, as an empty histogram is.
            f = 0.0_real64
        end if

    end procedure grid_density

    module procedure grid_pdf_r0

        call require_initialised(self, "pf_kde_grid%pdf")
        f = pdf_value(self, x)

    end procedure grid_pdf_r0

    module procedure grid_pdf_r1

        integer(int64) :: i

        call require_initialised(self, "pf_kde_grid%pdf")
        if (size(f, kind=int64) /= size(x, kind=int64)) &
            call kde_abort("pf_kde_grid%pdf", "f must have one element per point of x")
        do i = 1_int64, size(x, kind=int64)
            f(i) = pdf_value(self, x(i))
        end do

    end procedure grid_pdf_r1

    module procedure grid_cdf_r0

        real(real64), allocatable :: q(:)

        call require_initialised(self, "pf_kde_grid%cdf")
        call running_mass(self, q)
        p = cdf_value(self, q, x)

    end procedure grid_cdf_r0

    module procedure grid_cdf_r1

        real(real64), allocatable :: q(:)
        integer(int64) :: i

        call require_initialised(self, "pf_kde_grid%cdf")
        if (size(p, kind=int64) /= size(x, kind=int64)) &
            call kde_abort("pf_kde_grid%cdf", "p must have one element per point of x")
        call running_mass(self, q)
        do i = 1_int64, size(x, kind=int64)
            p(i) = cdf_value(self, q, x(i))
        end do

    end procedure grid_cdf_r1

    module procedure grid_quantile_r0

        real(real64), allocatable :: q(:)

        call require_initialised(self, "pf_kde_grid%quantile")
        call check_probability(p)
        call running_mass(self, q)
        x = quantile_value(self, q, p)

    end procedure grid_quantile_r0

    module procedure grid_quantile_r1

        real(real64), allocatable :: q(:)
        integer(int64) :: i

        call require_initialised(self, "pf_kde_grid%quantile")
        if (size(x, kind=int64) /= size(p, kind=int64)) &
            call kde_abort("pf_kde_grid%quantile", "x must have one element per element of p")
        do i = 1_int64, size(p, kind=int64)
            call check_probability(p(i))
        end do
        call running_mass(self, q)
        do i = 1_int64, size(p, kind=int64)
            x(i) = quantile_value(self, q, p(i))
        end do

    end procedure grid_quantile_r1

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
        write(u, '(2x,a,i0)') "cells       ", self%nc
        write(u, '(2x,a,es24.16e3)') "xmin        ", self%x0
        write(u, '(2x,a,es24.16e3)') "xmax        ", self%x1
        write(u, '(2x,a,es24.16e3)') "step        ", self%dx
        call grid_kernel_name(self, token)
        write(u, '(2x,a,a)') "kernel      ", token
        write(u, '(2x,a,es24.16e3)') "bandwidth   ", self%h
        write(u, '(2x,a,i0)') "n           ", self%cnt_all
        write(u, '(2x,a,i0)') "n_valid     ", self%cnt_valid
        write(u, '(2x,a,i0)') "n_null      ", self%cnt_null
        write(u, '(2x,a,i0)') "n_nan       ", self%cnt_nan
        write(u, '(2x,a,i0)') "n_outside   ", self%cnt_out
        write(u, '(2x,a,es24.16e3)') "sum_weights ", self%w_total
        if (self%has_lower) write(u, '(2x,a,es24.16e3)') "lower       ", self%lo
        if (self%has_upper) write(u, '(2x,a,es24.16e3)') "upper       ", self%hi
        select case (self%boundary_code)
        case (KDE_BOUNDARY_RENORMALISE)
            write(u, '(2x,a,a)') "boundary    ", "renormalise"
        case (KDE_BOUNDARY_REFLECT)
            write(u, '(2x,a,a)') "boundary    ", "reflect"
        case default
            write(u, '(2x,a,a)') "boundary    ", "none (unbounded)"
        end select
        if (self%adapt%on) then
            write(u, '(2x,a,es24.16e3)') "alpha       ", self%adapt%alpha
            if (self%adapt%has_bmax) write(u, '(2x,a,es23.16e3)') "bandwidth_max", self%adapt%bmax
            write(u, '(2x,a,i0,a,es24.16e3,a,es24.16e3)') "pilot       ", self%adapt%nc, " cells over", &
                self%adapt%x0, " to", self%adapt%x1
        end if
        if (self%poisoned) then
            ! A kept NaN, a poisoned pilot or a bandwidth the adaptive rule could not represent.
            write(u, '(2x,a)') "poisoned    every query answers NaN"
        else if (.not. (self%w_total > 0.0_real64)) then
            write(u, '(2x,a)') "empty       nothing accumulated; %density answers zeros"
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

    !> Aborts unless `p` is a probability. The NaN is screened first, as its own test.
    subroutine check_probability(p)
        real(real64), intent(in) :: p !! the caller's probability

        if (p /= p) call kde_abort("pf_kde_grid%quantile", "p must lie in [0, 1]")
        if (p < 0.0_real64 .or. p > 1.0_real64) call kde_abort("pf_kde_grid%quantile", "p must lie in [0, 1]")

    end subroutine check_probability

    !> Zeroes the accumulation and every count, keeping the geometry and the settings. A grid whose
    !! pilot is poisoned starts poisoned: its adaptive rule has nothing to read.
    subroutine empty_grid(self)
        class(pf_kde_grid), intent(inout) :: self !! the grid, initialised

        self%acc = 0.0_real64
        self%poisoned = self%adapt%on .and. self%adapt%poisoned
        self%w_total = 0.0_real64
        self%w_below = 0.0_real64
        self%w_above = 0.0_real64
        self%cnt_all = 0_int64
        self%cnt_valid = 0_int64
        self%cnt_null = 0_int64
        self%cnt_nan = 0_int64
        self%cnt_out = 0_int64

    end subroutine empty_grid

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
        if (a%poisoned .neqv. b%poisoned) return
        do i = 1, a%nc
            if (a%acc(i) /= b%acc(i)) return
        end do
        res = .true.

    end function same_rule

    !> `.true.` when some cell of a grid holds weight: a density a pilot can be read from.
    pure function holds_weight(g) result(res)
        type(pf_kde_grid), intent(in) :: g   !! the grid, initialised
        logical                       :: res !! a cell holds weight

        integer :: i

        res = .false.
        if (.not. (g%w_total > 0.0_real64)) return
        do i = 1, g%nc
            if (g%acc(i) > 0.0_real64) then
                res = .true.
                return
            end if
        end do

    end function holds_weight

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
        if (allocated(self%acc)) deallocate(self%acc)
        allocate(self%acc(ncells))

    end subroutine set_geometry

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
        to%poisoned = from%poisoned
        to%logg = from%logg
        to%pmin = from%pmin
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
        real(real64) :: wj
        integer(int64) :: j

        allocate(z(self%nc), k(self%nc), k2(self%nc))
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

        ! ---- the shares: inside the support, below `xmin` and above `xmax` ----
        ! `S(t)`, the images' weight at or below `t`, is formed only where a share can differ
        ! from its plain value, so a kernel wholly inside the range and clear of every bound pays
        ! for no distribution function at all.
        need_mass = .false.
        select case (self%boundary_code)
        case (KDE_BOUNDARY_RENORMALISE)
            if (self%has_lower) need_mass = xj - reach < self%lo
            if (self%has_upper) need_mass = need_mass .or. xj + reach > self%hi
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

    !> Cell `i`'s density on this grid, which holds weight.
    function cell_density(self, i) result(f)
        class(pf_kde_grid), intent(in) :: self !! the grid, with weight in it
        integer, intent(in)            :: i    !! the cell
        real(real64)                   :: f    !! its density

        f = cell_value(self%acc(i), self%w_total)

    end function cell_density

    !> The interpolated density at `t`: NaN on an empty or poisoned grid, else `interp_cells`.
    function pdf_value(self, t) result(f)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64), intent(in)       :: t    !! where to evaluate
        real(real64)                   :: f    !! the density

        f = ieee_value(1.0_real64, ieee_quiet_nan)
        if (self%poisoned .or. .not. (self%w_total > 0.0_real64)) return
        f = interp_cells(self%nc, self%x0, self%x1, self%dx, self%acc, self%w_total, t)

    end function pdf_value

    !> The density cells `acc` of total weight `wt` describe at `t`: linear between neighbouring
    !! centres, constant over the outer half-cells, zero outside `[x0, x1]`, NaN at a NaN `t`. At
    !! a centre it is `cell_value` itself, the value `%density` answers there. The one
    !! interpolation, for a grid's `%pdf` and for the adaptive rule's look-up of its pilot.
    function interp_cells(nc, x0, x1, dx, acc, wt, t) result(f)
        integer, intent(in)      :: nc      !! the number of cells
        real(real64), intent(in) :: x0      !! the first cell's left edge
        real(real64), intent(in) :: x1      !! the last cell's right edge
        real(real64), intent(in) :: dx      !! the cell width
        real(real64), intent(in) :: acc(:)  !! each cell's accumulation
        real(real64), intent(in) :: wt      !! the total weight, positive
        real(real64), intent(in) :: t       !! where to evaluate
        real(real64)             :: f       !! the density

        integer :: j
        real(real64) :: s

        f = ieee_value(1.0_real64, ieee_quiet_nan)
        if (t /= t) return
        f = 0.0_real64
        if (t < x0 .or. t > x1) return
        if (t <= centre_at(x0, dx, 1)) then
            f = cell_value(acc(1), wt)
            return
        end if
        if (t >= centre_at(x0, dx, nc)) then
            f = cell_value(acc(nc), wt)
            return
        end if
        j = segment_at(nc, x0, dx, t)
        s = (t - centre_at(x0, dx, j))/dx
        if (s == 0.0_real64) then
            f = cell_value(acc(j), wt)
            return
        end if
        f = ((1.0_real64 - s)*acc(j) + s*acc(j + 1))/wt

    end function interp_cells

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
    !! one, the pilot's smallest positive cell density stands in for `p`.
    function rule_bandwidth(a, h, t) result(hj)
        type(kde_adapt), intent(in) :: a  !! the rule
        real(real64), intent(in)    :: h  !! the global bandwidth
        real(real64), intent(in)    :: t  !! the point
        real(real64)                :: hj !! its bandwidth

        real(real64) :: p, e

        hj = ieee_value(1.0_real64, ieee_quiet_nan)
        if (t /= t) return
        if (a%alpha == 0.0_real64) then
            hj = h
        else
            p = interp_cells(a%nc, a%x0, a%x1, a%dx, a%acc, a%wt, t)
            if (.not. (p > 0.0_real64)) then
                if (a%has_bmax) then
                    hj = a%bmax
                    return
                end if
                p = a%pmin
            end if
            e = -a%alpha*(log(p) - a%logg)
            if (e > log(huge(h)) - log(h) - 1.0_real64) then
                hj = ieee_value(1.0_real64, ieee_positive_inf)
            else
                hj = h*exp(e)
            end if
        end if
        if (a%has_bmax) then
            if (hj > a%bmax) hj = a%bmax
        end if

    end function rule_bandwidth

    !> `q(i)`, the accumulated weight per unit length integrated from `xmin` to centre `i`, under
    !! `%pdf`'s interpolant: half a cell of `acc(1)` to the first centre, then a trapezoid per
    !! segment.
    pure subroutine running_mass(self, q)
        class(pf_kde_grid), intent(in)         :: self !! the grid
        real(real64), allocatable, intent(out) :: q(:) !! the running integral at each centre

        integer :: i

        allocate(q(self%nc))
        q(1) = 0.5_real64*self%acc(1)*self%dx
        do i = 2, self%nc
            q(i) = q(i - 1) + 0.5_real64*(self%acc(i - 1) + self%acc(i))*self%dx
        end do

    end subroutine running_mass

    !> `P(X <= t)` from the running integral `q`: the weight below `xmin`, plus `%pdf`'s integral
    !! from `xmin` to `t`, over the total weight; exactly 0 and 1 beyond the bounds.
    pure function cdf_value(self, q, t) result(p)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64), intent(in)       :: q(:) !! the running integral at each centre
        real(real64), intent(in)       :: t    !! where to evaluate
        real(real64)                   :: p    !! the probability at or below `t`

        integer :: j
        real(real64) :: s, a, b, mass

        p = ieee_value(1.0_real64, ieee_quiet_nan)
        if (self%poisoned .or. .not. (self%w_total > 0.0_real64)) return
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
            mass = self%w_below + q(self%nc) + 0.5_real64*self%acc(self%nc)*self%dx
        else if (t <= centre(self, 1)) then
            mass = self%w_below + self%acc(1)*(t - self%x0)
        else if (t >= centre(self, self%nc)) then
            mass = self%w_below + q(self%nc) + self%acc(self%nc)*(t - centre(self, self%nc))
        else
            j = segment(self, t)
            s = (t - centre(self, j))/self%dx
            a = self%acc(j)
            b = self%acc(j + 1)
            mass = self%w_below + q(j) + self%dx*s*(a + 0.5_real64*(b - a)*s)
        end if
        p = mass/self%w_total
        if (p < 0.0_real64) p = 0.0_real64
        if (p > 1.0_real64) p = 1.0_real64

    end function cdf_value

    !> The smallest point of `[xmin, xmax]` at which `%cdf` reaches `p`, from the running integral
    !! `q`. A quantile the weight below `xmin` covers is `xmin`; one only the weight above `xmax`
    !! reaches is `xmax`; otherwise the linear piece of an outer half-cell or the quadratic piece
    !! between two centres is solved for it.
    pure function quantile_value(self, q, p) result(x)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64), intent(in)       :: q(:) !! the running integral at each centre
        real(real64), intent(in)       :: p    !! the probability, in `[0, 1]`
        real(real64)                   :: x    !! the quantile

        real(real64) :: target, total, d, a, b, s
        integer :: j, lo_j, hi_j, mid

        x = ieee_value(1.0_real64, ieee_quiet_nan)
        if (self%poisoned .or. .not. (self%w_total > 0.0_real64)) return
        total = q(self%nc) + 0.5_real64*self%acc(self%nc)*self%dx
        target = p*self%w_total - self%w_below
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
        if (target <= q(1)) then
            ! The first half-cell, where the density is the constant `acc(1)`, positive here.
            x = min(self%x0 + target/self%acc(1), centre(self, 1))
            return
        end if
        if (target > q(self%nc)) then
            ! The last half-cell, where the density is the constant `acc(ncells)`, positive here.
            x = min(centre(self, self%nc) + (target - q(self%nc))/self%acc(self%nc), self%x1)
            return
        end if
        ! The first segment whose end reaches the target: `q(j) < target <= q(j + 1)`.
        lo_j = 1
        hi_j = self%nc - 1
        do while (lo_j < hi_j)
            mid = lo_j + (hi_j - lo_j)/2
            if (q(mid + 1) >= target) then
                hi_j = mid
            else
                lo_j = mid + 1
            end if
        end do
        j = lo_j
        ! `step*(a*s + (b - a)*s**2/2) = d` solved in its rationalised form, which needs no case
        ! for `a == b` and cannot cancel: `s = 2*d' / (a + sqrt(a**2 + 2*(b - a)*d'))`, `d' = d/step`.
        d = (target - q(j))/self%dx
        a = self%acc(j)
        b = self%acc(j + 1)
        s = 2.0_real64*d/(a + sqrt(max(a*a + 2.0_real64*(b - a)*d, 0.0_real64)))
        s = max(0.0_real64, min(1.0_real64, s))
        x = centre(self, j) + s*self%dx

    end function quantile_value

    !> Where the accumulated density starts inside the range: `xmin` when the first cell holds
    !! weight, else the centre before the first cell that does, from which the interpolant rises;
    !! `xmax` when no cell holds any.
    pure function left_end(self) result(x)
        class(pf_kde_grid), intent(in) :: self !! the grid
        real(real64)                   :: x    !! the left end

        integer :: i

        x = self%x1
        do i = 1, self%nc
            if (self%acc(i) > 0.0_real64) then
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
            if (self%acc(i) > 0.0_real64) then
                x = self%x1
                if (i < self%nc) x = centre(self, i + 1)
                return
            end if
        end do

    end function right_end

end submodule parquet_kde_grid ! GCOVR_EXCL_LINE
