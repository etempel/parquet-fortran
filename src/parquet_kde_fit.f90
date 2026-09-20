!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `pf_kde`: the fit, the exact queries, the curve, the sampler, the accessors and the printer.
!!
!! **Every query sums a window, never the whole sample.** The retained points are ascending and
!! every kernel has compact support, so the points that can reach a query point `x` are exactly
!! those in `[x - R*h, x + R*h]`, found by two binary searches; a point to the left of that window
!! contributes all of its mass to `%cdf` and nothing to `%pdf`, which is what the running weight
!! sum `cw` is for. The searches are this file's own rather than `pf_lower_bound`, whose one call
!! extracts the whole array's sort keys: a query loop, and `%quantile`'s iteration above all, would
!! pay that O(n) on every step.
!!
!! **The boundary corrections share one formula.** A point's mass to the left of `t` is `S(t)`,
!! the sum of its kernel's distribution function over its IMAGES -- itself, plus under `"reflect"`
!! its mirror image about each bound -- and its share of `%cdf` at an interior `x` is
!! `(S(x) - S(lower)) / mass`, where `mass = S(upper) - S(lower)` is its mass inside the support.
!! Under `"renormalise"` that division is the correction; under `"reflect"` the mass is one except
!! where the kernel is wider than the whole range, where the doubly reflected terms the images omit
!! would carry mass, and the division puts it back. Where `mass` is one it is not stored and not
!! divided by, so an unbounded or reflected-with-room estimate carries no rounding from it.
!!
!! **Within the support, the window holds every image that reaches `x`.** An image about the lower
!! bound sits at `2*lower - x_j`, and it reaches an interior `x` only when `x_j < 2*lower - x + R*h`,
!! which is inside the direct window whenever `x >= lower`; the same holds at the upper bound. So a
!! point outside the window contributes exactly 0 or exactly 1 to `%cdf`, under every correction.
!! Under the adaptive kernel the window is sized by the largest point bandwidth, `hmax`, and the
!! same argument holds point by point, each bandwidth being at most that.
!!
!! **One loop serves the fixed and the adaptive estimate.** Point `j`'s reciprocal bandwidth is read
!! from `hr(1 + (j - 1)*hstride)`: one element, one over the global bandwidth, read with stride 0,
!! for a fixed fit, and one per point for an adaptive one, every one formed at `%fit` by the same
!! scalar division. Each kernel is evaluated at `(t - x_j)*r` and scaled by `h/h_j` only where `r`
!! differs from one over `h`, so an adaptive fit whose every bandwidth is the global one --
!! `alpha = 0` -- runs the same instructions on the same values and answers the fixed estimate bit
!! for bit. Two loops written to mirror each other would not (a compiler may hoist the reciprocal
!! of a divisor it can see is constant in one of them and not in the other), and there is no branch
!! on the kind of fit for a compiler to split the loop at. A query divides by nothing per kernel.
!!
!! **A bulk query shares its elements among a team, and each element is computed alone.** `%pdf`,
!! `%cdf`, `%quantile`, `%curve` and `%sample` over an array give each thread a contiguous share of
!! the elements, and an element's answer is the serial one: nothing is summed across threads, so the
!! answer is the same bits at every thread count. The team opens only where the work, estimated from
!! the points one kernel reaches, pays for it (`kde_query_team`).
!!
!! **`%sample` is the smoothed bootstrap, addressed by `(seed, stream, k)`.** Element `k` reads the
!! stream `pf_random_key(stream, k)` under `pf_random_key(seed, KDE_FAMILY_LABEL)` and takes its
!! draws in order: the first chooses the point -- `pf_random_int_at` unweighted, a uniform located in
!! the running weight otherwise -- and the next ones its kernel's variate (a normal, one uniform for
!! the box, four summed for the cubic B-spline, which is the box convolved four times, and three
!! under Devroye's rule for the Epanechnikov kernel). A variate beyond the Gaussian's cut, or a draw
!! beyond a bound under `"renormalise"`, or one that a mirror under `"reflect"` leaves outside the
!! support (the doubly reflected mass the images omit), is drawn again from the same point's
!! kernel with the stream's next draws -- which samples the truncated, corrected kernel exactly --
!! and after `KDE_SAMPLE_TRIES` such draws the point's corrected distribution function is inverted
!! by bisection instead, from the next uniform. A redraw never reads another element's stream.
submodule (parquet_kde) parquet_kde_fit

    implicit none

    !> What `%quantile` costs at one probability, in queries: its bracketed Newton iteration takes
    !! about this many evaluations of `%cdf` and `%pdf`.
    real(real64), parameter :: KDE_QUANTILE_STEPS = 40.0_real64

contains

    ! ==========================================================================================
    ! Fitting
    ! ==========================================================================================

    module procedure kde_fit_f64

        character(len=*), parameter :: EP = "pf_kde%fit"
        real(real64), allocatable :: keep_x(:), keep_w(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: nv, nnull, nnan, nout, m, i, c0, c1, rate
        real(real64) :: v, hh, adj, a_alpha, a_bmax, one(1)
        logical :: saw_nan, freq, want_adaptive, built, found

        call kde_clear(self)
        kde_fit_ns = 0_int64

        ! ---- the caller's contract, checked before anything is read ----
        if (present(threads)) then
            if (threads < 1) call kde_abort(EP, "threads must be positive")
        end if
        if (present(bandwidth) .and. present(rule)) &
            call kde_abort(EP, "bandwidth= and rule= cannot both be given; use adjust= to scale a rule")
        if (present(bandwidth)) then
            if (.not. kde_positive_finite(bandwidth)) &
                call kde_abort(EP, "bandwidth must be a finite, positive number")
            self%rule_code = KDE_RULE_EXPLICIT
        else if (present(rule)) then
            call kde_resolve_rule(EP, rule, self%rule_code)
        else
            self%rule_code = KDE_RULE_ISJ
        end if
        adj = 1.0_real64
        if (present(adjust)) then
            if (.not. kde_positive_finite(adjust)) call kde_abort(EP, "adjust must be a finite, positive number")
            adj = adjust
        end if
        call kde_resolve_setup(EP, kernel, lower, upper, boundary, self%kernel_code, self%has_lower, &
            self%lo, self%has_upper, self%hi, self%boundary_code)
        want_adaptive = .false.
        if (present(adaptive)) want_adaptive = adaptive
        if (present(alpha) .or. present(bandwidth_max)) then
            if (.not. want_adaptive) call kde_abort(EP, "alpha= and bandwidth_max= need adaptive=.true.")
        end if
        a_alpha = KDE_ALPHA_DEFAULT
        if (present(alpha)) then
            ! The NaN first, as its own test: an ordered comparison raises IEEE_INVALID on one.
            if (alpha /= alpha) call kde_abort(EP, "alpha must lie in [0, 1]")
            if (alpha < 0.0_real64 .or. alpha > 1.0_real64) call kde_abort(EP, "alpha must lie in [0, 1]")
            a_alpha = alpha
        end if
        a_bmax = 0.0_real64
        if (present(bandwidth_max)) then
            if (.not. kde_positive_finite(bandwidth_max)) &
                call kde_abort(EP, "bandwidth_max must be a finite, positive number")
            a_bmax = bandwidth_max
        end if
        call stats_weight_kind(EP, weight_type, freq)
        ! The rule is on from here, whatever the population turns out to be, so that `%is_adaptive`
        ! reports what was asked for; its table is filled once there is a pilot to read.
        self%adapt%on = want_adaptive
        self%adapt%alpha = a_alpha
        self%adapt%has_bmax = present(bandwidth_max)
        self%adapt%bmax = a_bmax

        ! ---- the population: the family's exclusions, then the support ----
        ! `stats_compact` checks the array sizes and every weight it examines, in the family's
        ! order and with the family's texts composed from `EP`.
        call stats_compact(x, EP, is_valid, weights, skipnan, keep_x, keep_w, nv, nnull, nnan, &
            saw_nan)
        self%weighted = present(weights)
        nout = 0_int64
        m = 0_int64
        do i = 1_int64, nv
            v = keep_x(i)
            ! A NaN kept by `skipnan = .false.` stays, to poison the estimate below; it is neither
            ! inside nor outside the support, and it must not reach an ordered comparison.
            if (v == v) then
                if (kde_outside(self%has_lower, self%lo, self%has_upper, self%hi, v)) then
                    nout = nout + 1_int64
                    cycle
                end if
            end if
            m = m + 1_int64
            keep_x(m) = v
            if (self%weighted) keep_w(m) = keep_w(i)
        end do
        self%cnt_all = size(x, kind=int64)
        self%cnt_valid = m
        self%cnt_null = nnull
        self%cnt_nan = nnan
        self%cnt_out = nout
        self%fitted = .true.
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        if (present(n_outside)) n_outside = nout
        if (present(ok)) ok = .false.
        self%h = ieee_value(1.0_real64, ieee_quiet_nan)
        ! An empty population, or one a kept NaN has poisoned, is an undefined estimate: fitted,
        ! with every answer a quiet NaN, and never an abort.
        if (saw_nan .or. m == 0_int64) return

        ! ---- the retained sample, ascending, and its running weight ----
        call system_clock(count=c0, count_rate=rate)
        call pf_argsort(keep_x(1:m), perm, threads=threads)
        allocate(self%x(m))
        do i = 1_int64, m
            self%x(i) = keep_x(perm(i))
        end do
        if (self%weighted) then
            allocate(self%w(m), self%cw(m))
            do i = 1_int64, m
                self%w(i) = keep_w(perm(i))
            end do
            self%cw(1) = self%w(1)
            do i = 2_int64, m
                self%cw(i) = self%cw(i - 1_int64) + self%w(i)
            end do
            self%w_total = self%cw(m)
        else
            self%w_total = real(m, real64)
        end if
        call system_clock(count=c1)
        call add_phase(1, c0, c1, rate)

        ! ---- the bandwidth ----
        if (present(bandwidth)) then
            hh = bandwidth
        else if (self%rule_code == KDE_RULE_ISJ) then
            call kde_isj_bandwidth(self%x, freq, self%w, self%has_lower, self%lo, self%has_upper, self%hi, &
                hh, found)
            ! The default never fails where a rule of thumb would not: when the ISJ rule finds no
            ! bandwidth and was not asked for by name, Silverman's rule gives one, and `%rule` says
            ! which rule did. Named, it is left undefined, as a rule that finds no scale is.
            if (.not. found .and. .not. present(rule)) then
                self%rule_code = KDE_RULE_SILVERMAN
                call kde_rule_bandwidth(self%rule_code, self%x, freq, self%w, weight_type, threads, hh)
            end if
        else
            call kde_rule_bandwidth(self%rule_code, self%x, freq, self%w, weight_type, threads, hh)
        end if
        ! A rule that found no scale answers NaN; an explicit bandwidth times `adjust` can fail by
        ! overflowing, or by reaching so far that the kernel's reach does. Either way there is no
        ! estimate to answer with, and the product is tested before it is formed: the overflow
        ! itself would stop a program under nagfor.
        if (ieee_is_nan(hh)) return
        if (adj > 1.0_real64) then
            if (hh > huge(hh)/adj) return
        end if
        hh = hh*adj
        if (.not. kde_bandwidth_usable(self%kernel_code, hh)) return
        self%h = hh

        ! ---- each point's bandwidth: from the pilot when adaptive, the global one otherwise ----
        if (self%adapt%on) then
            call system_clock(count=c0)
            one(1) = 1.0_real64
            if (self%weighted) then
                call kde_build_pilot(self%pilot_grid, self%x, self%w, .true., hh, self%kernel_code, self%has_lower, &
                    self%lo, self%has_upper, self%hi, self%boundary_code, self%w_total, threads, built)
            else
                call kde_build_pilot(self%pilot_grid, self%x, one, .false., hh, self%kernel_code, self%has_lower, &
                    self%lo, self%has_upper, self%hi, self%boundary_code, self%w_total, threads, built)
            end if
            ! Only points beyond half the largest number leave no range to build it over.
            if (.not. built) then
                self%h = ieee_value(1.0_real64, ieee_quiet_nan)
                return
            end if
            call kde_adapt_set(self%adapt, self%pilot_grid, a_alpha, present(bandwidth_max), a_bmax)
            call system_clock(count=c1)
            call add_phase(2, c0, c1, rate)
            call system_clock(count=c0)
            allocate(self%hb(m))
            call kde_adapt_bandwidths(self%adapt, hh, self%x, self%hb)
            ! A bandwidth the rule could only overflow or underflow to leaves no estimate either.
            do i = 1_int64, m
                if (.not. kde_bandwidth_usable(self%kernel_code, self%hb(i))) then
                    self%h = ieee_value(1.0_real64, ieee_quiet_nan)
                    return
                end if
            end do
            self%hmax = self%hb(1)
            do i = 2_int64, m
                if (self%hb(i) > self%hmax) self%hmax = self%hb(i)
            end do
            allocate(self%hr(m))
            do i = 1_int64, m
                self%hr(i) = reciprocal(self%hb(i))
            end do
            self%hstride = 1_int64
        else
            call system_clock(count=c0)
            allocate(self%hb(1), self%hr(1))
            self%hb(1) = hh
            self%hr(1) = reciprocal(hh)
            self%hmax = hh
            self%hstride = 0_int64
        end if
        self%hinv = reciprocal(hh)
        call set_extent(self)
        call set_density_ends(self)

        ! ---- each point's mass inside the support, where it can differ from one ----
        call build_mass(self)
        ! ---- under `"linear"`, the zones, their tables and the mass that normalises the estimate ----
        if (self%boundary_code == KDE_BOUNDARY_LINEAR) then
            call build_linear(self)
            ! The clipped estimate holding no mass leaves nothing to normalise by: a data condition,
            ! answered as every other undefined estimate is.
            if (.not. (self%znorm > 0.0_real64 .and. self%znorm <= huge(1.0_real64))) then
                self%h = ieee_value(1.0_real64, ieee_quiet_nan)
                return
            end if
        end if
        call system_clock(count=c1)
        call add_phase(3, c0, c1, rate)
        self%defined = .true.
        if (present(ok)) ok = .true.

    end procedure kde_fit_f64

    module procedure kde_fit_f32

        real(real64), allocatable :: xw(:)

        ! Widened first, so that the sample is ordered, searched and summed in `real64` exactly as
        ! the `real64` form does it.
        allocate(xw(size(x, kind=int64)))
        xw = real(x, real64)
        call kde_fit_f64(self, xw, bandwidth, rule, adjust, kernel, adaptive, alpha, bandwidth_max, lower, &
            upper, boundary, is_valid, weights, weight_type, skipnan, n_null, n_nan, n_outside, ok, threads)

    end procedure kde_fit_f32

    module procedure kde_fit_col

        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)

        ! The column's validity becomes `is_valid`: unallocated when it has no null, and so absent.
        call kde_widen_column("pf_kde%fit", x, is_valid, wide, mask)
        call kde_fit_f64(self, wide, bandwidth, rule, adjust, kernel, adaptive, alpha, bandwidth_max, lower, &
            upper, boundary, mask, weights, weight_type, skipnan, n_null, n_nan, n_outside, ok, threads)

    end procedure kde_fit_col

    ! ==========================================================================================
    ! Queries
    ! ==========================================================================================

    module procedure kde_pdf_r0

        call require_fitted(self, "pf_kde%pdf")
        f = density_at(self, x)

    end procedure kde_pdf_r0

    module procedure kde_pdf_r1

        character(len=*), parameter :: EP = "pf_kde%pdf"
        integer(int64) :: i, n
        integer :: team

        call require_fitted(self, EP)
        n = size(x, kind=int64)
        if (size(f, kind=int64) /= n) call kde_abort(EP, "f must have one element per point of x")
        call kde_query_team(EP, threads, n, query_work(self), team)
        if (team <= 1) then
            kde_team_used = 1
            do i = 1_int64, n
                f(i) = density_at(self, x(i))
            end do
            return
        end if
        !$omp parallel num_threads(team) default(shared) private(i)
        call kde_record_team()
        !$omp do schedule(static)
        do i = 1_int64, n
            f(i) = density_at(self, x(i))
        end do
        !$omp end do
        !$omp end parallel

    end procedure kde_pdf_r1

    module procedure kde_cdf_r0

        call require_fitted(self, "pf_kde%cdf")
        p = cdf_at(self, x)

    end procedure kde_cdf_r0

    module procedure kde_cdf_r1

        character(len=*), parameter :: EP = "pf_kde%cdf"
        integer(int64) :: i, n
        integer :: team

        call require_fitted(self, EP)
        n = size(x, kind=int64)
        if (size(p, kind=int64) /= n) call kde_abort(EP, "p must have one element per point of x")
        call kde_query_team(EP, threads, n, cdf_work(self), team)
        if (team <= 1) then
            kde_team_used = 1
            do i = 1_int64, n
                p(i) = cdf_at(self, x(i))
            end do
            return
        end if
        !$omp parallel num_threads(team) default(shared) private(i)
        call kde_record_team()
        !$omp do schedule(static)
        do i = 1_int64, n
            p(i) = cdf_at(self, x(i))
        end do
        !$omp end do
        !$omp end parallel

    end procedure kde_cdf_r1

    module procedure kde_quantile_r0

        call require_fitted(self, "pf_kde%quantile")
        call check_probability(p)
        x = quantile_at(self, p)

    end procedure kde_quantile_r0

    module procedure kde_quantile_r1

        character(len=*), parameter :: EP = "pf_kde%quantile"
        integer(int64) :: i, n
        integer :: team

        call require_fitted(self, EP)
        n = size(p, kind=int64)
        if (size(x, kind=int64) /= n) call kde_abort(EP, "x must have one element per element of p")
        call kde_query_team(EP, threads, n, KDE_QUANTILE_STEPS*cdf_work(self), team)
        ! Every probability is checked before any is answered, and serially, so that an abort is
        ! taken outside the team.
        do i = 1_int64, n
            call check_probability(p(i))
        end do
        if (team <= 1) then
            kde_team_used = 1
            do i = 1_int64, n
                x(i) = quantile_at(self, p(i))
            end do
            return
        end if
        !$omp parallel num_threads(team) default(shared) private(i)
        call kde_record_team()
        !$omp do schedule(static)
        do i = 1_int64, n
            x(i) = quantile_at(self, p(i))
        end do
        !$omp end do
        !$omp end parallel

    end procedure kde_quantile_r1

    module procedure kde_curve

        character(len=*), parameter :: EP = "pf_kde%curve"
        real(real64) :: a, b, c, nan
        integer(int64) :: n, i
        integer :: team

        call require_fitted(self, EP)
        n = size(x, kind=int64)
        if (size(f, kind=int64) /= n) call kde_abort(EP, "x and f must have the same size")
        call kde_query_team(EP, threads, n, query_work(self), team)
        c = 3.0_real64
        if (present(cut)) then
            if (ieee_is_nan(cut)) call kde_abort(EP, "cut must not be negative")
            if (cut < 0.0_real64) call kde_abort(EP, "cut must not be negative")
            c = cut
        end if
        if (present(xmin)) then
            if (.not. ieee_is_finite(xmin)) call kde_abort(EP, "xmin and xmax must be finite")
        end if
        if (present(xmax)) then
            if (.not. ieee_is_finite(xmax)) call kde_abort(EP, "xmin and xmax must be finite")
        end if
        if (present(xmin) .and. present(xmax)) then
            if (.not. (xmin < xmax)) call kde_abort(EP, "xmin must be below xmax")
        end if
        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        if (.not. self%defined) then
            f = nan
            x = nan
            if (present(xmin) .and. present(xmax)) call spaced(xmin, xmax, x)
            return
        end if

        ! The default range: `cut` bandwidths beyond the data, never beyond the support.
        a = self%x(1) - c*self%h
        b = self%x(size(self%x, kind=int64)) + c*self%h
        if (self%has_lower) a = max(a, self%lo)
        if (self%has_upper) b = min(b, self%hi)
        if (present(xmin)) a = xmin
        if (present(xmax)) b = xmax
        if (.not. (a < b)) call kde_abort(EP, "xmin must be below xmax")
        call spaced(a, b, x)
        if (team <= 1) then
            kde_team_used = 1
            do i = 1_int64, n
                f(i) = density_at(self, x(i))
            end do
            return
        end if
        !$omp parallel num_threads(team) default(shared) private(i)
        call kde_record_team()
        !$omp do schedule(static)
        do i = 1_int64, n
            f(i) = density_at(self, x(i))
        end do
        !$omp end do
        !$omp end parallel

    end procedure kde_curve

    module procedure kde_sample_s32

        integer(int64) :: s

        s = 0_int64
        if (present(stream)) s = int(stream, int64)
        call sample_fill(self, v, seed, s, threads)

    end procedure kde_sample_s32

    module procedure kde_sample_s64

        call sample_fill(self, v, seed, stream, threads)

    end procedure kde_sample_s64

    module procedure kde_bandwidths

        character(len=*), parameter :: EP = "pf_kde%bandwidths"
        integer(int64) :: i

        call require_fitted(self, EP)
        if (size(h, kind=int64) /= self%cnt_valid) call kde_abort(EP, "h must have one element per retained point")
        if (present(x)) then
            if (size(x, kind=int64) /= self%cnt_valid) &
                call kde_abort(EP, "x must have one element per retained point")
            ! The points are kept whenever the population is, which a kept NaN prevents.
            if (allocated(self%x)) then
                do i = 1_int64, self%cnt_valid
                    x(i) = self%x(i)
                end do
            else
                x = ieee_value(1.0_real64, ieee_quiet_nan)
            end if
        end if
        if (.not. self%defined) then
            h = ieee_value(1.0_real64, ieee_quiet_nan)
            return
        end if
        do i = 1_int64, self%cnt_valid
            h(i) = self%hb(1_int64 + (i - 1_int64)*self%hstride)
        end do

    end procedure kde_bandwidths

    module procedure kde_bandwidth_at_r0

        real(real64) :: hs(1)

        call require_fitted(self, "pf_kde%bandwidth_at")
        call bandwidths_at(self, [x], hs)
        h = hs(1)

    end procedure kde_bandwidth_at_r0

    module procedure kde_bandwidth_at_r1

        call require_fitted(self, "pf_kde%bandwidth_at")
        if (size(h, kind=int64) /= size(x, kind=int64)) &
            call kde_abort("pf_kde%bandwidth_at", "h must have one element per point of x")
        call bandwidths_at(self, x, h)

    end procedure kde_bandwidth_at_r1

    module procedure kde_pilot_copy

        call require_fitted(self, "pf_kde%pilot")
        if (.not. self%adapt%on) call kde_abort("pf_kde%pilot", "the fit is not adaptive")
        ! An adaptive fit with nothing to build a pilot from hands back a grid never initialised,
        ! which is what `g` is on entry.
        if (self%pilot_grid%initialised) g = self%pilot_grid

    end procedure kde_pilot_copy

    ! ==========================================================================================
    ! Accessors
    ! ==========================================================================================

    module procedure kde_bandwidth
        call require_fitted(self, "pf_kde%bandwidth")
        h = self%h
    end procedure kde_bandwidth

    module procedure kde_kernel_name
        call require_fitted(self, "pf_kde%kernel")
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
    end procedure kde_kernel_name

    module procedure kde_rule_name
        call require_fitted(self, "pf_kde%rule")
        select case (self%rule_code)
        case (KDE_RULE_EXPLICIT)
            name = "explicit"
        case (KDE_RULE_SCOTT)
            name = "scott"
        case (KDE_RULE_ISJ)
            name = "isj"
        case default
            name = "silverman"
        end select
    end procedure kde_rule_name

    module procedure kde_bounds
        call require_fitted(self, "pf_kde%bounds")
        lo = ieee_value(1.0_real64, ieee_negative_inf)
        hi = ieee_value(1.0_real64, ieee_positive_inf)
        if (self%has_lower) lo = self%lo
        if (self%has_upper) hi = self%hi
    end procedure kde_bounds

    module procedure kde_n
        call require_fitted(self, "pf_kde%n")
        n = self%cnt_all
    end procedure kde_n

    module procedure kde_n_valid
        call require_fitted(self, "pf_kde%n_valid")
        n = self%cnt_valid
    end procedure kde_n_valid

    module procedure kde_n_null
        call require_fitted(self, "pf_kde%n_null")
        n = self%cnt_null
    end procedure kde_n_null

    module procedure kde_n_nan
        call require_fitted(self, "pf_kde%n_nan")
        n = self%cnt_nan
    end procedure kde_n_nan

    module procedure kde_n_outside
        call require_fitted(self, "pf_kde%n_outside")
        n = self%cnt_out
    end procedure kde_n_outside

    module procedure kde_sum_weights
        call require_fitted(self, "pf_kde%sum_weights")
        s = self%w_total
    end procedure kde_sum_weights

    module procedure kde_is_fitted
        res = self%fitted
    end procedure kde_is_fitted

    module procedure kde_is_adaptive
        call require_fitted(self, "pf_kde%is_adaptive")
        res = self%adapt%on
    end procedure kde_is_adaptive

    module procedure kde_print

        integer :: u
        character(len=:), allocatable :: token

        ! Solicited output, so `verbosity = "silent"` governs it as it governs every printer here.
        if (parquet_output_is_suppressed()) return
        u = parquet_message_unit()
        if (present(unit)) u = unit
        if (.not. self%fitted) then
            write(u, '(a)') "pf_kde: not fitted (call %fit first)"
            return
        end if
        write(u, '(a)') "pf_kde"
        ! Every row's label in the one thirteen-character column, `kde_label`'s.
        call kde_kernel_name(self, token)
        write(u, '(2x,a,a)') kde_label("kernel"), token
        call kde_rule_name(self, token)
        write(u, '(2x,a,a)') kde_label("rule"), token
        write(u, '(2x,a,es24.16e3)') kde_label("bandwidth"), self%h
        write(u, '(2x,a,i0)') kde_label("n"), self%cnt_all
        write(u, '(2x,a,i0)') kde_label("n_valid"), self%cnt_valid
        write(u, '(2x,a,i0)') kde_label("n_null"), self%cnt_null
        write(u, '(2x,a,i0)') kde_label("n_nan"), self%cnt_nan
        write(u, '(2x,a,i0)') kde_label("n_outside"), self%cnt_out
        if (self%has_lower) write(u, '(2x,a,es24.16e3)') kde_label("lower"), self%lo
        if (self%has_upper) write(u, '(2x,a,es24.16e3)') kde_label("upper"), self%hi
        select case (self%boundary_code)
        case (KDE_BOUNDARY_RENORMALISE)
            write(u, '(2x,a,a)') kde_label("boundary"), "renormalise"
        case (KDE_BOUNDARY_REFLECT)
            write(u, '(2x,a,a)') kde_label("boundary"), "reflect"
        case (KDE_BOUNDARY_LINEAR)
            write(u, '(2x,a,a)') kde_label("boundary"), "linear"
            ! What the fit's own integrals came to. Nothing is emitted about a status other than
            ! converged: a quadrature reaching round-off at the last digit is not a finding about
            ! the caller's data, and a caller who asks can read it here.
            write(u, '(2x,a,a)') kde_label("quadrature"), trim(quadrature_text(self%zone_status))
        case default
            write(u, '(2x,a,a)') kde_label("boundary"), "none (unbounded)"
        end select
        if (self%adapt%on) then
            write(u, '(2x,a,es24.16e3)') kde_label("alpha"), self%adapt%alpha
            if (self%adapt%has_bmax) write(u, '(2x,a,es24.16e3)') kde_label("bandwidth_max"), self%adapt%bmax
            if (self%pilot_grid%initialised) write(u, '(2x,a,i0,a,es24.16e3,a,es24.16e3)') kde_label("pilot"), &
                self%pilot_grid%nc, " cells over", self%pilot_grid%x0, " to", self%pilot_grid%x1
            if (self%defined) then
                write(u, '(2x,a,es24.16e3)') kde_label("h_j min"), minval(self%hb)
                write(u, '(2x,a,es24.16e3)') kde_label("h_j max"), self%hmax
            end if
        end if
        if (.not. self%defined) write(u, '(2x,a,a)') kde_label("undefined"), "every query answers NaN (ok = .false.)"

    end procedure kde_print

    module procedure kde_clear

        if (allocated(self%x)) deallocate(self%x)
        if (allocated(self%w)) deallocate(self%w)
        if (allocated(self%cw)) deallocate(self%cw)
        if (allocated(self%mass)) deallocate(self%mass)
        if (allocated(self%hb)) deallocate(self%hb)
        if (allocated(self%hr)) deallocate(self%hr)
        if (allocated(self%cwz)) deallocate(self%cwz)
        self%zones(KDE_ZONE_LO) = kde_zone()
        self%zones(KDE_ZONE_HI) = kde_zone()
        self%dens_lo = 0.0_real64
        self%dens_hi = 0.0_real64
        self%zmid = 0.0_real64
        self%znorm = 1.0_real64
        self%zone_status = PF_INT_OK
        self%adapt = kde_adapt()
        self%pilot_grid = pf_kde_grid()
        self%hmax = 0.0_real64
        self%ext_lo = 0.0_real64
        self%ext_hi = 0.0_real64
        self%hinv = 0.0_real64
        self%hstride = 0_int64
        self%fitted = .false.
        self%defined = .false.
        self%kernel_code = KDE_GAUSSIAN
        self%rule_code = KDE_RULE_ISJ
        self%boundary_code = KDE_BOUNDARY_NONE
        self%has_lower = .false.
        self%has_upper = .false.
        self%lo = 0.0_real64
        self%hi = 0.0_real64
        self%h = 0.0_real64
        self%w_total = 0.0_real64
        self%weighted = .false.
        self%cnt_all = 0_int64
        self%cnt_valid = 0_int64
        self%cnt_null = 0_int64
        self%cnt_nan = 0_int64
        self%cnt_out = 0_int64

    end procedure kde_clear

    ! ==========================================================================================
    ! Private helpers
    ! ==========================================================================================

    !> Aborts unless the object has been fitted. Impure deliberately, like every guard here.
    subroutine require_fitted(self, entry)
        class(pf_kde), intent(in)    :: self  !! the estimate
        character(len=*), intent(in) :: entry !! the binding, for the message

        if (.not. self%fitted) call kde_abort(entry, "the estimate has not been fitted")

    end subroutine require_fitted

    !> Aborts unless `p` is a probability. The NaN is screened first, as its own test.
    subroutine check_probability(p)
        real(real64), intent(in) :: p !! the caller's probability

        if (p /= p) call kde_abort("pf_kde%quantile", "p must lie in [0, 1]")
        if (p < 0.0_real64 .or. p > 1.0_real64) call kde_abort("pf_kde%quantile", "p must lie in [0, 1]")

    end subroutine check_probability

    !> Fills `x` with `size(x)` equally spaced points from `a` to `b`, both ends exact.
    pure subroutine spaced(a, b, x)
        real(real64), intent(in)  :: a    !! the first point
        real(real64), intent(in)  :: b    !! the last point
        real(real64), intent(out) :: x(:) !! the points

        integer(int64) :: n, i
        real(real64) :: step

        n = size(x, kind=int64)
        if (n == 0_int64) return
        x(1) = a
        if (n == 1_int64) return
        step = (b - a)/real(n - 1_int64, real64)
        do i = 2_int64, n - 1_int64
            x(i) = a + real(i - 1_int64, real64)*step
        end do
        x(n) = b

    end subroutine spaced

    !> The first index `i` in `1 .. size(v)+1` with `v(i) >= t`, over ascending `v`; `t` is not NaN.
    pure function first_at_or_above(v, t) result(i)
        real(real64), intent(in) :: v(:) !! ascending values
        real(real64), intent(in) :: t    !! the threshold
        integer(int64)           :: i    !! the index

        integer(int64) :: hi, mid

        i = 1_int64
        hi = size(v, kind=int64) + 1_int64
        do while (i < hi)
            mid = i + (hi - i)/2_int64
            if (v(mid) >= t) then
                hi = mid
            else
                i = mid + 1_int64
            end if
        end do

    end function first_at_or_above

    !> The last index `i` in `0 .. size(v)` with `v(i) <= t`, over ascending `v`; `t` is not NaN.
    pure function last_at_or_below(v, t) result(i)
        real(real64), intent(in) :: v(:) !! ascending values
        real(real64), intent(in) :: t    !! the threshold
        integer(int64)           :: i    !! the index

        integer(int64) :: lo, hi, mid

        lo = 1_int64
        hi = size(v, kind=int64) + 1_int64
        do while (lo < hi)
            mid = lo + (hi - lo)/2_int64
            if (v(mid) > t) then
                hi = mid
            else
                lo = mid + 1_int64
            end if
        end do
        i = lo - 1_int64

    end function last_at_or_below

    !> Point `j`'s mass at or below `t`, summed over its images: itself, and under `"reflect"` its
    !! mirror image about each bound, for the point's own bandwidth, whose reciprocal is `r`. Not
    !! divided by the point's mass inside the support.
    pure function images_cdf(self, j, t, r) result(s)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: t    !! where to evaluate
        real(real64), intent(in)   :: r    !! one over the point's bandwidth
        real(real64)               :: s    !! the mass at or below `t`

        real(real64) :: xj

        xj = self%x(j)
        s = kde_kernel_cdf(self%kernel_code, (t - xj)*r)
        if (self%boundary_code /= KDE_BOUNDARY_REFLECT) return
        if (self%has_lower) s = s + kde_kernel_cdf(self%kernel_code, (t - (2.0_real64*self%lo - xj))*r)
        if (self%has_upper) s = s + kde_kernel_cdf(self%kernel_code, (t - (2.0_real64*self%hi - xj))*r)

    end function images_cdf

    !> Point `j`'s kernel density at `t`, summed over its images, in standard-deviation units of its
    !! own bandwidth, whose reciprocal is `r` (not yet divided by a bandwidth or by the point's mass
    !! inside the support).
    pure function images_pdf(self, j, t, r) result(k)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: t    !! where to evaluate
        real(real64), intent(in)   :: r    !! one over the point's bandwidth
        real(real64)               :: k    !! the density there

        real(real64) :: xj

        xj = self%x(j)
        k = kde_kernel_pdf(self%kernel_code, (t - xj)*r)
        if (self%boundary_code /= KDE_BOUNDARY_REFLECT) return
        if (self%has_lower) k = k + kde_kernel_pdf(self%kernel_code, (t - (2.0_real64*self%lo - xj))*r)
        if (self%has_upper) k = k + kde_kernel_pdf(self%kernel_code, (t - (2.0_real64*self%hi - xj))*r)

    end function images_pdf

    !> One over point `j`'s bandwidth: its own under the adaptive kernel, the global one otherwise,
    !! read with the stride that makes the two one loop.
    pure function point_reciprocal(self, j) result(r)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64)               :: r    !! one over its bandwidth

        r = self%hr(1_int64 + (j - 1_int64)*self%hstride)

    end function point_reciprocal

    !> `1/v` by one scalar division through a `volatile` operand, so that every reciprocal the fit
    !! stores -- the global one and each point's -- is the correctly rounded quotient formed the
    !! same way: ifx's default `-fp-model=fast` would otherwise vectorise a loop of them, or hoist
    !! one, into a different instruction for some elements than for others, and `alpha = 0` would
    !! no longer be the fixed estimate bit for bit.
    function reciprocal(v) result(r)
        real(real64), intent(in) :: v !! the bandwidth, positive
        real(real64)             :: r !! its reciprocal

        real(real64), volatile :: d

        d = v
        r = 1.0_real64/d

    end function reciprocal

    !> The estimate's extent, before it is clipped to the support: the smallest `x_j - R h_j` and the
    !! largest `x_j + R h_j` over the retained points, `R` the kernel's reach in bandwidths. For a
    !! fixed fit they are the extreme points' reaches, `x(1) - R h` and `x(m) + R h`; under the
    !! adaptive kernel an extreme point need not have the widest kernel, and the density then starts
    !! where the first kernel starts rather than a whole `R hmax` below the first point.
    subroutine set_extent(self)
        class(pf_kde), intent(inout) :: self !! the estimate, sample and bandwidths set

        integer(int64) :: j, m
        real(real64) :: v, rad

        m = size(self%x, kind=int64)
        rad = KDE_RADIUS(self%kernel_code)
        self%ext_lo = self%x(1) - rad*self%hb(1)
        self%ext_hi = self%x(m) + rad*self%hb(1_int64 + (m - 1_int64)*self%hstride)
        if (self%hstride == 0_int64) return
        do j = 2_int64, m
            v = self%x(j) - rad*self%hb(j)
            if (v < self%ext_lo) self%ext_lo = v
        end do
        do j = 1_int64, m - 1_int64
            v = self%x(j) + rad*self%hb(j)
            if (v > self%ext_hi) self%ext_hi = v
        end do

    end subroutine set_extent

    !> Where the density starts and stops: the extent clipped to the bounds, which is what
    !! `%quantile(0)` and `%quantile(1)` answer under every correction. `build_linear` moves either
    !! one to a crossing where the clip removes a stretch that starts at it.
    subroutine set_density_ends(self)
        class(pf_kde), intent(inout) :: self !! the estimate, extent set

        self%dens_lo = self%ext_lo
        self%dens_hi = self%ext_hi
        if (self%has_lower) self%dens_lo = max(self%dens_lo, self%lo)
        if (self%has_upper) self%dens_hi = min(self%dens_hi, self%hi)

    end subroutine set_density_ends

    !> Each point's mass inside the support, stored only where a correction makes it differ from
    !! one: under `"renormalise"` whenever a bound is given, and under `"reflect"` only when both
    !! bounds are given and a kernel is wider than the range between them, which is the one case
    !! where the images omit mass (the doubly reflected terms).
    subroutine build_mass(self)
        class(pf_kde), intent(inout) :: self !! the estimate, sample and bandwidths set

        integer(int64) :: j, m
        real(real64) :: s_lo, s_hi, r

        select case (self%boundary_code)
        case (KDE_BOUNDARY_RENORMALISE)
            continue
        case (KDE_BOUNDARY_REFLECT)
            if (.not. (self%has_lower .and. self%has_upper)) return
            if (KDE_RADIUS(self%kernel_code)*self%hmax <= self%hi - self%lo) return
        case default
            return
        end select
        m = size(self%x, kind=int64)
        allocate(self%mass(m))
        do j = 1_int64, m
            r = point_reciprocal(self, j)
            s_lo = 0.0_real64
            if (self%has_lower) s_lo = images_cdf(self, j, self%lo, r)
            if (self%has_upper) then
                s_hi = images_cdf(self, j, self%hi, r)
            else
                s_hi = 1.0_real64
            end if
            self%mass(j) = s_hi - s_lo
        end do

    end subroutine build_mass

    !> The density at `t`: the weighted sum of the kernels within reach, each over its own
    !! bandwidth, over the total weight. Zero outside the support; NaN at a NaN `t` or on an
    !! undefined estimate.
    pure function density_at(self, t) result(f)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64), intent(in)  :: t    !! where to evaluate
        real(real64)              :: f    !! the density

        integer(int64) :: j, jb, first, last
        real(real64) :: reach, k, acc, r

        f = ieee_value(1.0_real64, ieee_quiet_nan)
        if (.not. self%defined) return
        if (t /= t) return
        f = 0.0_real64
        if (self%has_lower) then
            if (t < self%lo) return
        end if
        if (self%has_upper) then
            if (t > self%hi) return
        end if
        if (self%boundary_code == KDE_BOUNDARY_LINEAR) then
            ! The linear correction's own sum, clipped where it is negative and divided by the mass
            ! the clipped estimate holds over the support. The clip is a test on a value the sum has
            ! made finite, never `max` on one that could be NaN.
            call window_at(self, t, first, last)
            call linear_sum(self, t, first, last, acc)
            k = acc/(self%w_total*self%h)
            if (k > 0.0_real64) f = k/self%znorm
            return
        end if
        reach = KDE_RADIUS(self%kernel_code)*self%hmax
        first = first_at_or_above(self%x, t - reach)
        last = last_at_or_below(self%x, t + reach)
        acc = 0.0_real64
        jb = 1_int64 + (first - 1_int64)*self%hstride
        do j = first, last
            r = self%hr(jb)
            jb = jb + self%hstride
            k = images_pdf(self, j, t, r)
            ! The sum is divided by the global bandwidth below, so a kernel of any other is scaled
            ! here by `h/h_j`, and one of the global bandwidth is left exactly as it is.
            if (r /= self%hinv) k = k*(self%h*r)
            if (allocated(self%mass)) k = k/self%mass(j)
            if (self%weighted) k = self%w(j)*k
            acc = acc + k
        end do
        f = acc/(self%w_total*self%h)

    end function density_at

    !> `P(X <= t)`: the weight wholly to the left of the window, plus each windowed point's share,
    !! over the total weight. Exactly 0 at and below the lower bound and exactly 1 at and above the
    !! upper one; clamped to `[0, 1]` against rounding.
    !!
    !! Not `pure` under `"linear"`, where a query inside a zone integrates the nearby points' terms
    !! through `pf_integrate`, which takes its integrand `intent(inout)` and can abort.
    function cdf_at(self, t) result(p)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64), intent(in)  :: t    !! where to evaluate
        real(real64)              :: p    !! the probability at or below `t`

        integer(int64) :: j, jb, first, last
        real(real64) :: reach, c, acc, s_lo, r

        p = ieee_value(1.0_real64, ieee_quiet_nan)
        if (.not. self%defined) return
        if (t /= t) return
        if (self%has_lower) then
            p = 0.0_real64
            if (t <= self%lo) return
        end if
        if (self%has_upper) then
            p = 1.0_real64
            if (t >= self%hi) return
        end if
        if (self%boundary_code == KDE_BOUNDARY_LINEAR) then
            p = cdf_linear(self, t)
            if (p < 0.0_real64) p = 0.0_real64
            if (p > 1.0_real64) p = 1.0_real64
            return
        end if
        reach = KDE_RADIUS(self%kernel_code)*self%hmax
        first = first_at_or_above(self%x, t - reach)
        last = last_at_or_below(self%x, t + reach)
        if (first <= 1_int64) then
            acc = 0.0_real64
        else if (self%weighted) then
            acc = self%cw(first - 1_int64)
        else
            acc = real(first - 1_int64, real64)
        end if
        jb = 1_int64 + (first - 1_int64)*self%hstride
        do j = first, last
            r = self%hr(jb)
            jb = jb + self%hstride
            s_lo = 0.0_real64
            if (self%has_lower) s_lo = images_cdf(self, j, self%lo, r)
            c = images_cdf(self, j, t, r) - s_lo
            if (allocated(self%mass)) c = c/self%mass(j)
            if (self%weighted) c = self%w(j)*c
            acc = acc + c
        end do
        p = acc/self%w_total
        if (p < 0.0_real64) p = 0.0_real64
        if (p > 1.0_real64) p = 1.0_real64

    end function cdf_at

    !> The smallest `x` at which `%cdf` reaches `p`, by Newton's method on `%cdf` inside a bracket
    !! that bisection takes over whenever a step would leave it or the density is zero. `p = 0` and
    !! `p = 1` are the two ends of the estimate's support, where its density starts and stops:
    !! `dens_lo` and `dens_hi`, the extent clipped to the bounds and, under `"linear"`, moved to the
    !! crossing where a stretch the clip removes ends at such an end. Not `pure`, because `cdf_at`
    !! is not.
    function quantile_at(self, p) result(x)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64), intent(in)  :: p    !! the probability, in `[0, 1]`
        real(real64)              :: x    !! the quantile

        real(real64) :: a, b

        x = ieee_value(1.0_real64, ieee_quiet_nan)
        if (.not. self%defined) return
        a = self%dens_lo
        b = self%dens_hi
        if (p <= 0.0_real64) then
            x = a
            return
        end if
        if (p >= 1.0_real64) then
            x = b
            return
        end if
        x = solve_cdf(self, p, a, b, a + p*(b - a))

    end function quantile_at

    !> The point of `[a0, b0]` at which `%cdf` reaches `p`, from the first guess `guess`: Newton on
    !! `%cdf`, with bisection taking over whenever a step would leave the bracket or the density is
    !! zero -- which a stretch the clip removes is. `%quantile` solves over the whole support, and a
    !! zone draw's fallback over one zone.
    function solve_cdf(self, p, a0, b0, guess) result(x)
        class(pf_kde), intent(in) :: self  !! the fitted estimate
        real(real64), intent(in)  :: p     !! the probability, in `(0, 1)`
        real(real64), intent(in)  :: a0    !! the bracket's lower end
        real(real64), intent(in)  :: b0    !! its upper end
        real(real64), intent(in)  :: guess !! the first point to try
        real(real64)              :: x     !! the answer

        integer, parameter :: MAX_STEPS = 200
        real(real64) :: a, b, fx, dx, xn
        integer :: it

        a = a0
        b = b0
        ! `a` always has `%cdf < p` and `b` always `%cdf >= p`; the answer is the point between.
        x = guess
        do it = 1, MAX_STEPS
            fx = cdf_at(self, x)
            if (fx >= p) then
                b = x
            else
                a = x
            end if
            if (b - a <= 4.0_real64*spacing(max(abs(a), abs(b)))) exit
            dx = density_at(self, x)
            xn = a + 0.5_real64*(b - a)
            if (dx > 0.0_real64) then
                ! A Newton step is taken only when it lands strictly inside the bracket.
                if (abs(fx - p) <= dx*(b - a)) then
                    xn = x - (fx - p)/dx
                    if (.not. (xn > a .and. xn < b)) xn = a + 0.5_real64*(b - a)
                end if
            end if
            if (abs(xn - x) <= 2.0_real64*spacing(abs(x))) then
                x = xn
                return
            end if
            x = xn
        end do
        x = b

    end function solve_cdf

    !> The bandwidth the rule gives each point of `x`, into `h`: the adaptive rule's look-up of the
    !! pilot, the one `%fit` used, or the global bandwidth; NaN at a NaN point and everywhere on an
    !! undefined estimate.
    subroutine bandwidths_at(self, x, h)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64), intent(in)  :: x(:) !! the points
        real(real64), intent(out) :: h(:) !! the bandwidth at each

        integer(int64) :: i

        if (.not. self%defined) then
            h = ieee_value(1.0_real64, ieee_quiet_nan)
        else if (self%adapt%on) then
            call kde_adapt_bandwidths(self%adapt, self%h, x, h)
        else
            do i = 1_int64, size(x, kind=int64)
                h(i) = self%h
                if (x(i) /= x(i)) h(i) = ieee_value(1.0_real64, ieee_quiet_nan)
            end do
        end if

    end subroutine bandwidths_at

    !> About how many kernel evaluations a query at one point costs: the points within the widest
    !! kernel's reach of it, were the sample spread evenly over its range, and the two searches that
    !! find them. What the query team is sized by; 1 on an undefined estimate, which answers at once.
    pure function query_work(self) result(w)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64)              :: w    !! the cost of one query point

        integer(int64) :: m
        real(real64) :: half, reach

        w = 1.0_real64
        if (.not. self%defined) return
        m = size(self%x, kind=int64)
        ! Halved before the difference, which cannot then overflow; the reach is finite, the fit
        ! having refused any bandwidth whose kernel would reach beyond the largest number.
        half = 0.5_real64*self%x(m) - 0.5_real64*self%x(1)
        reach = KDE_RADIUS(self%kernel_code)*self%hmax
        if (reach < half) then
            w = real(m, real64)*(reach/half)
        else
            w = real(m, real64)
        end if
        w = w + 64.0_real64

    end function query_work

    !> What one `%cdf` or one step of one `%quantile` costs, in kernel evaluations: the window,
    !! multiplied under `"linear"` by what a zone query's per-point integrals cost over a plain sum,
    !! so that a bulk query there opens a team where the plain estimate's work would not pay for one.
    !> What one draw costs, in kernel evaluations: a point chosen and a variate drawn, multiplied
    !! under `"linear"` by what a zone draw's inversion and its pass over the window cost.
    pure function draw_work(self) result(w)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64)              :: w    !! the cost of one draw

        w = KDE_DRAW_WORK
        if (self%boundary_code == KDE_BOUNDARY_LINEAR) w = w*KDE_LINEAR_CDF_WORK

    end function draw_work

    pure function cdf_work(self) result(w)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64)              :: w    !! the cost of one query point

        w = query_work(self)
        if (self%boundary_code == KDE_BOUNDARY_LINEAR) w = w*KDE_LINEAR_CDF_WORK

    end function cdf_work

    !> What `%print`'s `quadrature` row says of the worst status the fit's own integrals reached:
    !! `converged`, or `not converged: ` and a word for it. `parquet_integrate` names no status, so
    !! the words are mapped here.
    pure function quadrature_text(status) result(text)
        integer, intent(in) :: status !! the worst `PF_INT_*` status
        character(len=32)   :: text   !! what to print

        select case (status)
        case (PF_INT_OK)
            text = "converged"
        case (PF_INT_LIMIT)
            text = "not converged: limit"
        case (PF_INT_ROUNDOFF)
            text = "not converged: roundoff"
        case (PF_INT_BAD_INTEGRAND)
            text = "not converged: bad integrand"
        case (PF_INT_NO_CONVERGENCE)
            text = "not converged: no convergence"
        case (PF_INT_DIVERGENT)
            text = "not converged: divergent"
        case default
            text = "not converged: bad value"
        end select

    end function quadrature_text

    !> Fills `v` with draws from the estimate, element `k` from stream `pf_random_key(stream, k)`
    !! under the family key, serially or shared among a team; NaN on an undefined estimate.
    subroutine sample_fill(self, v, seed, stream, threads)
        class(pf_kde), intent(in)     :: self    !! the fitted estimate
        real(real64), intent(out)     :: v(:)    !! the draws
        integer(int64), intent(in)    :: seed    !! the caller's seed
        integer(int64), intent(in)    :: stream  !! the caller's stream
        integer, intent(in), optional :: threads !! the caller's request

        character(len=*), parameter :: EP = "pf_kde%sample"
        integer(int64) :: k, n, key
        integer :: team

        call require_fitted(self, EP)
        n = size(v, kind=int64)
        call kde_query_team(EP, threads, n, draw_work(self), team)
        if (.not. self%defined) then
            v = ieee_value(1.0_real64, ieee_quiet_nan)
            return
        end if
        key = pf_random_key(seed, KDE_FAMILY_LABEL)
        if (team <= 1) then
            kde_team_used = 1
            do k = 1_int64, n
                v(k) = draw_at(self, key, pf_random_key(stream, k))
            end do
            return
        end if
        !$omp parallel num_threads(team) default(shared) private(k)
        call kde_record_team()
        !$omp do schedule(static)
        do k = 1_int64, n
            v(k) = draw_at(self, key, pf_random_key(stream, k))
        end do
        !$omp end do
        !$omp end parallel

    end subroutine sample_fill

    !> One draw from the estimate, from the stream `sk` under the family key `key`: its first draw
    !! chooses a point in proportion to its weight and the next ones its kernel's variate, redrawn
    !! where the kernel's cut or the support rejects it and, after `sample_tries()` redraws, found
    !! by inverting the point's corrected distribution function instead.
    !!
    !! Under `"linear"` the draw is `draw_linear`'s instead, whose first draw chooses among the
    !! zones and the interior. Not `pure`: a zone draw reaches `pf_integrate`, and the redraw cap
    !! is a test hook's to move.
    function draw_at(self, key, sk) result(y)
        class(pf_kde), intent(in)  :: self !! the fitted estimate, defined
        integer(int64), intent(in) :: key  !! the family key
        integer(int64), intent(in) :: sk   !! this element's stream
        real(real64)               :: y    !! the draw

        integer(int64) :: j, m, d
        integer :: attempt
        real(real64) :: xj, hj, e
        logical :: kept

        if (self%boundary_code == KDE_BOUNDARY_LINEAR) then
            y = draw_linear(self, key, sk)
            return
        end if
        m = size(self%x, kind=int64)
        d = 1_int64
        if (self%weighted) then
            ! The first point whose running weight passes a uniform share of the total; rounding
            ! at the top end is held to the last point.
            j = last_at_or_below(self%cw, pf_random_at(key, sk, d)*self%w_total) + 1_int64
            if (j > m) j = m
        else
            j = pf_random_int_at(key, sk, 1_int64, m, d)
        end if
        d = d + 1_int64
        xj = self%x(j)
        hj = self%hb(1_int64 + (j - 1_int64)*self%hstride)
        do attempt = 1, sample_tries()
            call kernel_variate(self%kernel_code, key, sk, d, e)
            ! The Gaussian is cut at five standard deviations, inclusive, and a variate beyond is
            ! drawn again: the fitted kernel is the cut one.
            if (self%kernel_code == KDE_GAUSSIAN) then
                if (abs(e) > KDE_GAUSS_CUT) cycle
            end if
            y = xj + hj*e
            call place(self, y, kept)
            if (kept) return
        end do
        y = invert_point(self, j, xj, hj, pf_random_at(key, sk, d))

    end function draw_at

    !> Whether `y` is a draw the corrected estimate keeps, moving it where `"reflect"` puts it:
    !! kept inside the support under `"renormalise"`; under `"reflect"`, mirrored about the bound it
    !! crossed and kept if then inside the support, the doubly reflected draw being one the images
    !! omit.
    pure subroutine place(self, y, ok)
        class(pf_kde), intent(in)   :: self !! the fitted estimate
        real(real64), intent(inout) :: y    !! the draw; mirrored under `"reflect"`
        logical, intent(out)        :: ok   !! it is kept

        ok = .true.
        select case (self%boundary_code)
        case (KDE_BOUNDARY_RENORMALISE)
            if (self%has_lower) then
                if (y < self%lo) ok = .false.
            end if
            if (self%has_upper) then
                if (y > self%hi) ok = .false.
            end if
        case (KDE_BOUNDARY_REFLECT)
            if (self%has_lower) then
                if (y < self%lo) then
                    y = 2.0_real64*self%lo - y
                    if (self%has_upper) then
                        if (y > self%hi) ok = .false.
                    end if
                    return
                end if
            end if
            if (self%has_upper) then
                if (y > self%hi) then
                    y = 2.0_real64*self%hi - y
                    if (self%has_lower) then
                        if (y < self%lo) ok = .false.
                    end if
                end if
            end if
        end select

    end subroutine place

    !> One variate of the kernel `code` in standard-deviation units, from the stream `sk` under
    !! `key` starting at draw `d`, which is advanced past the draws it read: a normal; one uniform
    !! on `[-1, 1)` for the box; four uniforms on `[-1/2, 1/2)` summed for the cubic B-spline, which
    !! is the box convolved four times; and for the Epanechnikov kernel Devroye's rule over three
    !! uniforms on `[-1, 1)`, the second unless the third is the largest in size.
    pure subroutine kernel_variate(code, key, sk, d, e)
        integer, intent(in)           :: code !! the kernel
        integer(int64), intent(in)    :: key  !! the family key
        integer(int64), intent(in)    :: sk   !! this element's stream
        integer(int64), intent(inout) :: d    !! the next draw to read; advanced past those read
        real(real64), intent(out)     :: e    !! the variate

        real(real64) :: u1, u2, u3, u4

        select case (code)
        case (KDE_GAUSSIAN)
            e = pf_random_normal_at(key, sk, d)
            d = d + 1_int64
        case (KDE_EPANECHNIKOV)
            u1 = 2.0_real64*pf_random_at(key, sk, d) - 1.0_real64
            u2 = 2.0_real64*pf_random_at(key, sk, d + 1_int64) - 1.0_real64
            u3 = 2.0_real64*pf_random_at(key, sk, d + 2_int64) - 1.0_real64
            d = d + 3_int64
            e = u3
            if (abs(u3) >= abs(u2)) then
                if (abs(u3) >= abs(u1)) e = u2
            end if
            e = KDE_SCALE(code)*e
        case (KDE_BSPLINE)
            u1 = pf_random_at(key, sk, d)
            u2 = pf_random_at(key, sk, d + 1_int64)
            u3 = pf_random_at(key, sk, d + 2_int64)
            u4 = pf_random_at(key, sk, d + 3_int64)
            d = d + 4_int64
            e = KDE_SCALE(code)*(((u1 + u2) + (u3 + u4)) - 2.0_real64)
        case default
            e = KDE_SCALE(code)*(2.0_real64*pf_random_at(key, sk, d) - 1.0_real64)
            d = d + 1_int64
        end select

    end subroutine kernel_variate

    !> The draw from point `j` (at `xj`, bandwidth `hj`) at which its corrected distribution
    !! function -- its images' under `"reflect"`, over the support and within its kernel's reach --
    !! reaches the share `u` of its mass, by bisection to the last bit: what `draw_at` falls back to
    !! where redrawing keeps being rejected.
    pure function invert_point(self, j, xj, hj, u) result(y)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: xj   !! where it is
        real(real64), intent(in)   :: hj   !! its bandwidth
        real(real64), intent(in)   :: u    !! the share of its mass, in `[0, 1)`
        real(real64)               :: y    !! the draw

        real(real64) :: a, b, reach

        reach = KDE_RADIUS(self%kernel_code)*hj
        a = xj - reach
        b = xj + reach
        if (self%has_lower) then
            if (a < self%lo) a = self%lo
        end if
        if (self%has_upper) then
            if (b > self%hi) b = self%hi
        end if
        y = invert_between(self, j, xj, hj, u, a, b)

    end function invert_point

    !> The draw from point `j` at which its corrected distribution function reaches the share `u` of
    !! its mass over `[a, b]`, by bisection to the last bit: `invert_point` over the support, and the
    !! interior draw's fallback over the interval between the zones.
    pure function invert_between(self, j, xj, hj, u, a0, b0) result(y)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: xj   !! where it is
        real(real64), intent(in)   :: hj   !! its bandwidth
        real(real64), intent(in)   :: u    !! the share of its mass, in `[0, 1)`
        real(real64), intent(in)   :: a0   !! the interval's lower end
        real(real64), intent(in)   :: b0   !! its upper end
        real(real64)               :: y    !! the draw

        integer :: it
        real(real64) :: a, b, mid, r, s_a, target

        r = point_reciprocal(self, j)
        a = a0
        b = b0
        s_a = images_cdf(self, j, a, r)
        target = s_a + u*(images_cdf(self, j, b, r) - s_a)
        do it = 1, 256
            mid = a + 0.5_real64*(b - a)
            if (.not. (mid > a .and. mid < b)) exit
            if (images_cdf(self, j, mid, r) >= target) then
                b = mid
            else
                a = mid
            end if
        end do
        y = b

    end function invert_between

    ! ==========================================================================================
    ! The linear boundary correction
    ! ==========================================================================================

    !> Point `j`'s weight, or one where the fit is unweighted.
    pure function point_weight(self, j) result(w)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64)               :: w    !! its weight

        w = 1.0_real64
        if (self%weighted) w = self%w(j)

    end function point_weight

    !> The window of points that can reach `t`: those within the widest kernel's reach of it.
    pure subroutine window_at(self, t, first, last)
        class(pf_kde), intent(in)   :: self  !! the fitted estimate
        real(real64), intent(in)    :: t     !! the query
        integer(int64), intent(out) :: first !! the first point within reach
        integer(int64), intent(out) :: last  !! the last

        real(real64) :: reach

        reach = KDE_RADIUS(self%kernel_code)*self%hmax
        first = first_at_or_above(self%x, t - reach)
        last = last_at_or_below(self%x, t + reach)

    end subroutine window_at

    !> The raw linear estimate's window sum at `t`: `acc`, which `density_at` divides by `W h` as
    !! it divides the plain sum, and optionally `env`, the same sum over the terms' MAGNITUDES --
    !! the envelope the zone sampler draws from -- and `hnear`, the narrowest bandwidth among the
    !! kernels that reach `t`, which the fit's scan steps by (0 when none reaches it).
    !!
    !! One loop for the fixed and the adaptive estimate, as the plain sum's is: the moments are
    !! formed afresh only where a point's reciprocal bandwidth differs from the previous point's,
    !! which is once per query on a fixed fit, and each kernel is scaled by `h/h_j` only where that
    !! reciprocal differs from the global one -- so an adaptive fit whose every bandwidth is the
    !! global one answers the fixed estimate's bits.
    pure subroutine linear_sum(self, t, first, last, acc, env, hnear)
        class(pf_kde), intent(in)           :: self  !! the fitted estimate
        real(real64), intent(in)            :: t     !! the query, inside the support
        integer(int64), intent(in)          :: first !! the window's first point
        integer(int64), intent(in)          :: last  !! its last
        real(real64), intent(out)           :: acc   !! the weighted sum of the corrected kernels
        real(real64), intent(out), optional :: env   !! the same over their magnitudes
        real(real64), intent(out), optional :: hnear !! the narrowest bandwidth reaching `t`

        type(kde_lin_factor) :: lf
        integer(int64) :: j, jb
        real(real64) :: r, rprev, u, k, hj, near, rad

        acc = 0.0_real64
        if (present(env)) env = 0.0_real64
        near = 0.0_real64
        rprev = 0.0_real64
        rad = KDE_RADIUS(self%kernel_code)
        jb = 1_int64 + (first - 1_int64)*self%hstride
        do j = first, last
            r = self%hr(jb)
            hj = self%hb(jb)
            jb = jb + self%hstride
            u = (t - self%x(j))*r
            ! A kernel sitting exactly on its edge is worth nothing here and everything to the
            ! scan's spacing, which must resolve the kernel that STARTS at the point it is standing
            ! on: the reach decides `hnear`, the value decides the sum.
            if (abs(u) <= rad) then
                if (near == 0.0_real64 .or. hj < near) near = hj
            end if
            k = kde_kernel_pdf(self%kernel_code, u)
            if (k == 0.0_real64) cycle
            if (r /= rprev) then
                lf = kde_linear_factor(self%kernel_code, self%has_lower, self%lo, self%has_upper, &
                    self%hi, t, r)
                rprev = r
            end if
            k = kde_linear_value(lf, u, k)
            ! The sum is divided by the global bandwidth by the caller, so a kernel of any other is
            ! scaled here by `h/h_j` and one of the global bandwidth is left exactly as it is.
            if (r /= self%hinv) k = k*(self%h*r)
            if (self%weighted) k = self%w(j)*k
            acc = acc + k
            if (present(env)) env = env + abs(k)
        end do
        if (present(hnear)) hnear = near

    end subroutine linear_sum

    !> The raw linear estimate at `t`, before the clip and before `Z`: the density the correction
    !! gives, which the fit's scan looks for the negative stretches of.
    pure function raw_density(self, t) result(f)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64), intent(in)  :: t    !! the query, inside the support
        real(real64)              :: f    !! the raw density there

        integer(int64) :: first, last
        real(real64) :: acc

        call window_at(self, t, first, last)
        call linear_sum(self, t, first, last, acc)
        f = acc/(self%w_total*self%h)

    end function raw_density

    !> Point `j`'s corrected term at `x`, `g_j(x)`: its own kernel at its own bandwidth, with no
    !! weight and nothing divided out. What every per-point integral integrates.
    pure function term_density(self, j, x) result(v)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: x    !! where to evaluate
        real(real64)               :: v    !! the term there

        type(kde_lin_factor) :: lf
        real(real64) :: r, u

        r = point_reciprocal(self, j)
        u = (x - self%x(j))*r
        v = kde_kernel_pdf(self%kernel_code, u)
        if (v /= 0.0_real64) then
            lf = kde_linear_factor(self%kernel_code, self%has_lower, self%lo, self%has_upper, self%hi, x, r)
            v = kde_linear_value(lf, u, v)*r
        end if

    end function term_density

    !> The linear kernel's factor for point `j` at `x`: what multiplies the plain kernel there, and
    !! exactly one where the point is plain. It increases with the distance from a lower bound and
    !! decreases with the distance from an upper one, so its sign changes at most once in a
    !! one-sided zone.
    pure function term_factor(self, j, x) result(v)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: x    !! where to evaluate
        real(real64)               :: v    !! the factor there

        type(kde_lin_factor) :: lf
        real(real64) :: r

        r = point_reciprocal(self, j)
        lf = kde_linear_factor(self%kernel_code, self%has_lower, self%lo, self%has_upper, self%hi, x, r)
        v = 1.0_real64
        if (.not. lf%plain) v = (lf%a - lf%b*((x - self%x(j))*r - lf%c))*lf%dinv

    end function term_factor

    !> Point `j`'s own term of the raw estimate at `x`, `w_j g_j(x)/W`: what one edge's jump is
    !! measured on.
    pure function term_value(self, j, x) result(v)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: x    !! where to evaluate
        real(real64)               :: v    !! the term there

        v = point_weight(self, j)*term_density(self, j, x)/self%w_total

    end function term_value

    !> One point's corrected term as `pf_integrate` evaluates it.
    module procedure kde_point_eval

        type(kde_lin_factor) :: lf
        real(real64) :: u

        u = (x - this%xj)*this%r
        f = kde_kernel_pdf(this%code, u)
        if (f /= 0.0_real64) then
            lf = kde_linear_factor(this%code, this%has_lower, this%lo, this%has_upper, this%hi, x, this%r)
            f = kde_linear_value(lf, u, f)*this%r
        end if
        if (this%absolute) f = abs(f)

    end procedure kde_point_eval

    !> Point `j`'s own breakpoints strictly inside `(alpha, beta)`, ascending and distinct: the
    !! knots of its kernel and of its moments for the cubic B-spline, its two correction edges, and
    !! the sign change of its term where the sampler integrates the magnitude.
    !!
    !! Nothing here is taken from the data, so no call can see a repeated breakpoint whatever the
    !! sample -- `pf_integrate` aborts on one -- and the list is at most `KDE_LINEAR_MAX_BREAKS`
    !! long, far below the budget's pieces.
    pure subroutine piece_breaks(self, j, alpha, beta, sigma, has_sigma, bp, n)
        class(pf_kde), intent(in)  :: self       !! the fitted estimate
        integer(int64), intent(in) :: j          !! the point
        real(real64), intent(in)   :: alpha      !! the piece's lower end
        real(real64), intent(in)   :: beta       !! its upper end
        real(real64), intent(in)   :: sigma      !! the term's sign change
        logical, intent(in)        :: has_sigma  !! the sign change is one of the breakpoints
        real(real64), intent(out)  :: bp(:)      !! the breakpoints, ascending and distinct
        integer, intent(out)       :: n          !! how many

        real(real64) :: cand(KDE_LINEAR_MAX_BREAKS), hj, rad, sc, v
        integer :: nc, i, p
        logical :: dup

        hj = self%hb(1_int64 + (j - 1_int64)*self%hstride)
        rad = KDE_RADIUS(self%kernel_code)
        sc = KDE_SCALE(self%kernel_code)
        nc = 0
        if (self%kernel_code == KDE_BSPLINE) then
            ! The kernel's own knots, where its cubic pieces meet, and its moments', where the
            ! truncated moments' pieces do.
            cand(nc + 1) = self%x(j) - sc*hj
            cand(nc + 2) = self%x(j)
            cand(nc + 3) = self%x(j) + sc*hj
            nc = nc + 3
            if (self%has_lower) then
                nc = nc + 1
                cand(nc) = self%lo + sc*hj
            end if
            if (self%has_upper) then
                nc = nc + 1
                cand(nc) = self%hi - sc*hj
            end if
        end if
        if (self%has_lower) then
            nc = nc + 1
            cand(nc) = self%lo + rad*hj
        end if
        if (self%has_upper) then
            nc = nc + 1
            cand(nc) = self%hi - rad*hj
        end if
        if (has_sigma) then
            nc = nc + 1
            cand(nc) = sigma
        end if
        ! Strictly inside the piece, made distinct, then insertion-sorted: the distinctness test
        ! comes first, because a repeat found after the shift would leave the list holding it twice.
        n = 0
        do i = 1, nc
            v = cand(i)
            if (.not. (v > alpha .and. v < beta)) cycle
            dup = .false.
            do p = 1, n
                if (bp(p) == v) dup = .true.
            end do
            if (dup) cycle
            p = n
            do while (p >= 1)
                if (bp(p) <= v) exit
                bp(p + 1) = bp(p)
                p = p - 1
            end do
            bp(p + 1) = v
            n = n + 1
        end do

    end subroutine piece_breaks

    !> The integral of point `j`'s term over `[alpha, beta]`, a piece on which it is corrected, by
    !! `pf_integrate` to `KDE_LINEAR_RTOL` and `KDE_LINEAR_ATOL` with the point's own breakpoints.
    !! `status` keeps the worst status seen.
    function term_piece(self, j, alpha, beta, absolute, sigma, has_sigma, status) result(v)
        class(pf_kde), intent(in)  :: self      !! the fitted estimate
        integer(int64), intent(in) :: j         !! the point
        real(real64), intent(in)   :: alpha     !! the piece's lower end
        real(real64), intent(in)   :: beta      !! its upper end
        logical, intent(in)        :: absolute  !! integrate the term's magnitude
        real(real64), intent(in)   :: sigma     !! its sign change, a breakpoint of the magnitude
        logical, intent(in)        :: has_sigma !! the sign change is known
        integer, intent(inout)     :: status    !! the worst `pf_integrate` status so far
        real(real64)               :: v         !! the integral

        type(kde_point_fn) :: fn
        type(pf_integration_info) :: info
        type(pf_tolerance) :: tol
        real(real64) :: bp(KDE_LINEAR_MAX_BREAKS)
        integer :: n

        v = 0.0_real64
        if (.not. (beta > alpha)) return
        fn%code = self%kernel_code
        fn%has_lower = self%has_lower
        fn%lo = self%lo
        fn%has_upper = self%has_upper
        fn%hi = self%hi
        fn%xj = self%x(j)
        fn%r = point_reciprocal(self, j)
        fn%absolute = absolute
        tol%rtol = KDE_LINEAR_RTOL
        tol%atol = KDE_LINEAR_ATOL
        call piece_breaks(self, j, alpha, beta, sigma, has_sigma, bp, n)
        if (n > 0) then
            v = pf_integrate(fn, alpha, beta, tol, info=info, breakpoints=bp(1:n), &
                context='pf_kde, boundary="linear"')
        else
            v = pf_integrate(fn, alpha, beta, tol, info=info, context='pf_kde, boundary="linear"')
        end if
        ! A status other than `PF_INT_OK` is used as answered and never aborted on; `%fit` keeps the
        ! worst of its own, which `%print` reports, and a query records nothing.
        if (info%status > status) status = info%status

    end function term_piece

    !> The integral of point `j`'s term between `s` and `t`, both inside the support: closed form
    !! by the kernel's own distribution function where the term is plain, `pf_integrate` where it is
    !! corrected. `absolute` integrates the magnitude instead, whose sign change the caller supplies.
    function term_integral(self, j, s, t, absolute, sigma, has_sigma, status) result(v)
        class(pf_kde), intent(in)  :: self      !! the fitted estimate
        integer(int64), intent(in) :: j         !! the point
        real(real64), intent(in)   :: s         !! the lower end
        real(real64), intent(in)   :: t         !! the upper end
        logical, intent(in)        :: absolute  !! integrate the term's magnitude
        real(real64), intent(in)   :: sigma     !! its sign change
        logical, intent(in)        :: has_sigma !! the sign change is known
        integer, intent(inout)     :: status    !! the worst `pf_integrate` status so far
        real(real64)               :: v         !! the integral

        real(real64) :: r, hj, xj, reach, s2, t2, cj, dj, ps, pe, plain

        v = 0.0_real64
        if (.not. (t > s)) return
        xj = self%x(j)
        hj = self%hb(1_int64 + (j - 1_int64)*self%hstride)
        r = point_reciprocal(self, j)
        reach = KDE_RADIUS(self%kernel_code)*hj
        ! Only the part its kernel reaches.
        s2 = max(s, xj - reach)
        t2 = min(t, xj + reach)
        if (.not. (t2 > s2)) return
        ! The correction edges: below `c_j` the lower bound corrects the term, above `d_j` the upper
        ! one. Where they cross -- a kernel wider than half the support -- the whole piece is
        ! corrected on both sides, and both edges enter as breakpoints.
        cj = s2
        dj = t2
        if (self%has_lower) cj = self%lo + reach
        if (self%has_upper) dj = self%hi - reach
        if (cj > dj) then
            v = term_piece(self, j, s2, t2, absolute, sigma, has_sigma, status)
            return
        end if
        if (s2 < cj) v = v + term_piece(self, j, s2, min(t2, cj), absolute, sigma, has_sigma, status)
        ps = max(s2, cj)
        pe = min(t2, dj)
        if (pe > ps) then
            plain = kde_kernel_cdf(self%kernel_code, (pe - xj)*r) - kde_kernel_cdf(self%kernel_code, (ps - xj)*r)
            v = v + plain
        end if
        if (t2 > dj) v = v + term_piece(self, j, max(s2, dj), t2, absolute, sigma, has_sigma, status)

    end function term_integral

    !> A bound on what the discontinuity at point `j`'s edge `e` can do to the raw estimate over an
    !! interval `dt` wide: the jump in the point's term there, plus the jump in its slope times the
    !! width. Both by one-sided differences a ten-thousandth of the point's bandwidth from the edge,
    !! which is far inside the piece and far above rounding -- so a smooth edge measures as nothing
    !! whatever the kernel, and no per-kernel jump has to be written out here.
    pure function edge_jump(self, j, e, dt) result(b)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point whose edge it is
        real(real64), intent(in)   :: e    !! the edge
        real(real64), intent(in)   :: dt   !! the interval's width
        real(real64)               :: b    !! the bound

        real(real64) :: d, l1, l2, r1, r2, vl, vr, sl, sr

        d = 1.0e-4_real64*self%hb(1_int64 + (j - 1_int64)*self%hstride)
        l1 = term_value(self, j, e - d)
        l2 = term_value(self, j, e - (d + d))
        r1 = term_value(self, j, e + d)
        r2 = term_value(self, j, e + (d + d))
        vl = l1 + (l1 - l2)
        vr = r1 + (r1 - r2)
        sl = (l1 - l2)/d
        sr = (r2 - r1)/d
        b = abs(vr - vl) + dt*abs(sr - sl)

    end function edge_jump

    !> Appends `v` to `edges(1:n)`, growing it when it is full.
    pure subroutine push_edge(edges, n, v)
        real(real64), allocatable, intent(inout) :: edges(:) !! the list
        integer, intent(inout)                   :: n        !! how many it holds
        real(real64), intent(in)                 :: v        !! the edge to append

        real(real64), allocatable :: bigger(:)

        if (n >= size(edges)) then
            allocate(bigger(2*size(edges, kind=int64)))
            bigger(1:n) = edges(1:n)
            call move_alloc(bigger, edges)
        end if
        n = n + 1
        edges(n) = v

    end subroutine push_edge

    !> The kernel and correction edges strictly inside `(t0, t1)`, ascending and distinct, and a
    !! bound on what they can do to the raw estimate there.
    !!
    !! A kernel's own edges matter where its value or its slope jumps there -- the box's edges, the
    !! Gaussian's cut, the Epanechnikov kernel's kinks -- and the cubic B-spline's do not, being
    !! twice differentiable at every knot. A correction edge matters where the kernel is not zero at
    !! its radius, which is the box's and the Gaussian's: there the truncated moments' slope jumps.
    !! Between two of these the raw estimate is smooth.
    subroutine step_edges(self, t0, t1, edges, nedge, bound)
        class(pf_kde), intent(in)                :: self   !! the fitted estimate
        real(real64), intent(in)                 :: t0     !! the interval's lower end
        real(real64), intent(in)                 :: t1     !! its upper end
        real(real64), allocatable, intent(inout) :: edges(:) !! receives the edges, ascending
        integer, intent(out)                     :: nedge  !! how many
        real(real64), intent(out)                :: bound  !! the bound on their effect

        integer(int64) :: j, jb, first, last
        real(real64) :: reach, rad, hj, cand(4), v
        integer :: nc, i, p
        logical :: kernel_edges, corr_edges

        nedge = 0
        bound = 0.0_real64
        kernel_edges = self%kernel_code /= KDE_BSPLINE
        corr_edges = self%kernel_code == KDE_GAUSSIAN .or. self%kernel_code == KDE_BOX
        if (.not. (kernel_edges .or. corr_edges)) return
        rad = KDE_RADIUS(self%kernel_code)
        reach = rad*self%hmax
        first = first_at_or_above(self%x, t0 - reach)
        last = last_at_or_below(self%x, t1 + reach)
        jb = 1_int64 + (first - 1_int64)*self%hstride
        do j = first, last
            hj = self%hb(jb)
            jb = jb + self%hstride
            nc = 0
            if (kernel_edges) then
                cand(1) = self%x(j) - rad*hj
                cand(2) = self%x(j) + rad*hj
                nc = 2
            end if
            if (corr_edges) then
                if (self%has_lower) then
                    nc = nc + 1
                    cand(nc) = self%lo + rad*hj
                end if
                if (self%has_upper) then
                    nc = nc + 1
                    cand(nc) = self%hi - rad*hj
                end if
            end if
            do i = 1, nc
                v = cand(i)
                if (.not. (v > t0 .and. v < t1)) cycle
                call push_edge(edges, nedge, v)
                bound = bound + edge_jump(self, j, v, t1 - t0)
            end do
        end do
        ! Insertion-sorted and made distinct, so that the refinement walks them in order and no
        ! interval it makes is of zero width.
        p = 1
        do i = 2, nedge
            v = edges(i)
            p = i - 1
            do while (p >= 1)
                if (edges(p) <= v) exit
                edges(p + 1) = edges(p)
                p = p - 1
            end do
            edges(p + 1) = v
        end do
        p = 0
        do i = 1, nedge
            if (p >= 1) then
                if (edges(p) == edges(i)) cycle
            end if
            p = p + 1
            edges(p) = edges(i)
        end do
        nedge = p

    end subroutine step_edges

    !> The raw estimate at `t` with the narrowest bandwidth among the kernels that reach it.
    pure subroutine raw_at(self, t, f, hnear)
        class(pf_kde), intent(in) :: self  !! the fitted estimate
        real(real64), intent(in)  :: t     !! the query
        real(real64), intent(out) :: f     !! the raw density there
        real(real64), intent(out) :: hnear !! the narrowest bandwidth reaching it, or 0

        integer(int64) :: first, last
        real(real64) :: acc

        call window_at(self, t, first, last)
        call linear_sum(self, t, first, last, acc, hnear=hnear)
        f = acc/(self%w_total*self%h)

    end subroutine raw_at

    !> The scan's next point after `t`: a spacing of `hnear/KDE_LINEAR_SCAN_PER_H`, never stepping
    !! past the start of a kernel narrower than that -- which would otherwise be resolved at another
    !! kernel's spacing -- and never past the zone's end `s1`.
    !!
    !! Where nothing reaches `t` the raw estimate is exactly zero there, and the step jumps to where
    !! the next kernel starts: no kernel can reach between the two.
    pure subroutine step_to(self, t, hnear, s1, t1)
        class(pf_kde), intent(in) :: self  !! the fitted estimate
        real(real64), intent(in)  :: t     !! the current point
        real(real64), intent(in)  :: hnear !! the narrowest bandwidth reaching it, or 0
        real(real64), intent(in)  :: s1    !! the zone's end
        real(real64), intent(out) :: t1    !! the next point

        integer(int64) :: j, jb, first, last
        real(real64) :: reach, rad, hj, st

        rad = KDE_RADIUS(self%kernel_code)
        reach = rad*self%hmax
        if (hnear > 0.0_real64) then
            t1 = t + hnear/KDE_LINEAR_SCAN_PER_H
            if (t1 > s1) t1 = s1
            ! A kernel narrower than the step, starting inside it: the scan lands on its start and
            ! steps by its own bandwidth from there.
            first = first_at_or_above(self%x, t)
            last = last_at_or_below(self%x, t1 + reach)
            jb = 1_int64 + (first - 1_int64)*self%hstride
            do j = first, last
                hj = self%hb(jb)
                jb = jb + self%hstride
                if (hj < hnear) then
                    st = self%x(j) - rad*hj
                    if (st > t .and. st < t1) t1 = st
                end if
            end do
        else
            ! Nothing reaches `t`: no kernel of a point beyond `t + 2 reach` can start before
            ! `t + reach`, so that is the longest safe jump.
            t1 = min(s1, t + reach)
            first = first_at_or_above(self%x, t)
            last = last_at_or_below(self%x, t + (reach + reach))
            jb = 1_int64 + (first - 1_int64)*self%hstride
            do j = first, last
                hj = self%hb(jb)
                jb = jb + self%hstride
                st = self%x(j) - rad*hj
                if (st > t .and. st < t1) t1 = st
            end do
        end if
        if (.not. (t1 > t)) t1 = min(nearest(t, 1.0_real64), s1)

    end subroutine step_to

    !> The crossing between a point where the raw estimate is not negative and one where it is,
    !! bisected on its sign until the two are neighbouring numbers; the answer is the one where it
    !! is not negative, so a stretch runs from the last such point to the first one after it.
    pure function crossing(self, x_nonneg, x_neg) result(x)
        class(pf_kde), intent(in) :: self      !! the fitted estimate
        real(real64), intent(in)  :: x_nonneg  !! where the raw estimate is not negative
        real(real64), intent(in)  :: x_neg     !! where it is
        real(real64)              :: x         !! the crossing

        integer, parameter :: MAX_STEPS = 300
        real(real64) :: p, q, mid
        integer :: it

        p = x_nonneg
        q = x_neg
        do it = 1, MAX_STEPS
            mid = p + 0.5_real64*(q - p)
            if (.not. (mid > min(p, q) .and. mid < max(p, q))) exit
            if (raw_density(self, mid) < 0.0_real64) then
                q = mid
            else
                p = mid
            end if
        end do
        x = p

    end function crossing

    !> The stretches of `[s0, s1]` where the raw estimate is negative and the clip removes it, into
    !! `z`; `pos_before` and `pos_after` say whether a positive value was seen before the first and
    !! after the last, which is how `%quantile`'s ends know where the density starts and stops.
    !!
    !! The scan evaluates at `KDE_LINEAR_SCAN_PER_H` points to the local bandwidth and, in an
    !! interval whose smaller end value is within twice what its edges can do (`step_edges`), at
    !! every edge inside it as well; every sign change between two neighbouring evaluations is
    !! bisected. What it can still miss is a dip of a smooth piece that starts and ends between two
    !! neighbouring points, whose mass is at most the cube of the spacing times the raw estimate's
    !! curvature over twelve.
    subroutine scan_zone(self, s0, s1, z, pos_before, pos_after)
        class(pf_kde), intent(in)     :: self       !! the fitted estimate
        real(real64), intent(in)      :: s0         !! the zone's lower end
        real(real64), intent(in)      :: s1         !! its upper end
        type(kde_zone), intent(inout) :: z          !! receives the stretches
        logical, intent(out)          :: pos_before !! a positive value before the first stretch
        logical, intent(out)          :: pos_after  !! a positive value after the last

        real(real64), allocatable :: ss(:), ee(:), edges(:)
        real(real64) :: t, t1, f0, f1, hn0, hn1, eb, px, cx, cf, start
        integer :: ns, nedge, i
        logical :: open_stretch

        allocate(ss(8), ee(8), edges(16))
        ns = 0
        open_stretch = .false.
        pos_before = .false.
        pos_after = .false.
        start = s0
        call raw_at(self, s0, f0, hn0)
        if (f0 < 0.0_real64) then
            open_stretch = .true.
        else if (f0 > 0.0_real64) then
            pos_before = .true.
            pos_after = .true.
        end if
        t = s0
        do while (t < s1)
            call step_to(self, t, hn0, s1, t1)
            if (.not. (t1 > t)) exit
            call raw_at(self, t1, f1, hn1)
            call step_edges(self, t, t1, edges, nedge, eb)
            ! Away from zero by more than twice what the edges inside can do, no jump or kink can
            ! hide a stretch, and the interval is taken as it is.
            if (min(f0, f1) > 2.0_real64*eb) nedge = 0
            px = t
            do i = 1, 2*nedge + 1
                if (i <= 2*nedge) then
                    ! Each edge is looked at from BOTH sides: the number before it and the edge
                    ! itself. A kernel that jumps is already off AT its own edge -- the box's
                    ! support is open there -- so a dip that ends at the jump is invisible to an
                    ! evaluation at the edge alone.
                    if (mod(i, 2) == 1) then
                        cx = nearest(edges((i + 1)/2), -1.0_real64)
                    else
                        cx = edges(i/2)
                    end if
                    if (.not. (cx > px)) cycle
                    cf = raw_density(self, cx)
                else
                    cx = t1
                    cf = f1
                end if
                if (open_stretch) then
                    if (.not. (cf < 0.0_real64)) then
                        call push_stretch(ss, ee, ns, start, crossing(self, cx, px))
                        open_stretch = .false.
                        pos_after = cf > 0.0_real64
                    end if
                else if (cf < 0.0_real64) then
                    start = crossing(self, px, cx)
                    open_stretch = .true.
                else if (cf > 0.0_real64) then
                    if (ns == 0) pos_before = .true.
                    pos_after = .true.
                end if
                px = cx
            end do
            t = t1
            f0 = f1
            hn0 = hn1
        end do
        if (open_stretch) call push_stretch(ss, ee, ns, start, s1)
        z%ns = ns
        if (ns > 0) then
            allocate(z%s(ns), z%e(ns), z%nu(ns), z%base(ns))
            z%s = ss(1:ns)
            z%e = ee(1:ns)
            z%nu = 0.0_real64
            z%base = 0.0_real64
        end if

    end subroutine scan_zone

    !> Appends one stretch to the lists, growing them when they are full.
    pure subroutine push_stretch(ss, ee, ns, s, e)
        real(real64), allocatable, intent(inout) :: ss(:) !! the stretches' lower ends
        real(real64), allocatable, intent(inout) :: ee(:) !! their upper ends
        integer, intent(inout)                   :: ns    !! how many there are
        real(real64), intent(in)                 :: s     !! the new stretch's lower end
        real(real64), intent(in)                 :: e     !! its upper end

        real(real64), allocatable :: bigger(:)

        if (ns >= size(ss)) then
            allocate(bigger(2*size(ss, kind=int64)))
            bigger(1:ns) = ss(1:ns)
            call move_alloc(bigger, ss)
            allocate(bigger(2*size(ee, kind=int64)))
            bigger(1:ns) = ee(1:ns)
            call move_alloc(bigger, ee)
        end if
        ns = ns + 1
        ss(ns) = s
        ee(ns) = e

    end subroutine push_stretch

    !> Point `j`'s kernel mass below the lower zone's inner edge; zero without a lower bound.
    pure function mass_below_zlo(self, j, r) result(c)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: r    !! one over its bandwidth
        real(real64)               :: c    !! the mass below the edge

        c = 0.0_real64
        if (self%has_lower) c = kde_kernel_cdf(self%kernel_code, (self%zones(KDE_ZONE_LO)%edge - self%x(j))*r)

    end function mass_below_zlo

    !> Point `j`'s kernel mass below the upper zone's inner edge; one without an upper bound.
    pure function mass_below_zhi(self, j, r) result(c)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: r    !! one over its bandwidth
        real(real64)               :: c    !! the mass below the edge

        c = 1.0_real64
        if (self%has_upper) c = kde_kernel_cdf(self%kernel_code, (self%zones(KDE_ZONE_HI)%edge - self%x(j))*r)

    end function mass_below_zhi

    !> The raw estimate's integral between zone `iz`'s bound and `t`, a point inside that zone.
    !!
    !! A window point whose kernel ends before `t` contributes its whole integral over the zone, from
    !! the table; one whose kernel starts after `t` nothing; one that straddles `t` its own integral,
    !! which is where a query's `pf_integrate` calls are. The points outside the window on the
    !! bound's side lie wholly inside and are read from the running sum, never summed again.
    function zone_raw_mass(self, iz, t) result(l)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        integer, intent(in)       :: iz   !! the zone
        real(real64), intent(in)  :: t    !! the query, inside the zone
        real(real64)              :: l    !! the integral, over the total weight

        integer(int64) :: j, jb, k, n, first, last, j1, j2
        real(real64) :: acc, reach, hj, o
        integer :: status
        logical :: up

        up = iz == KDE_ZONE_HI
        j1 = self%zones(iz)%j1
        j2 = self%zones(iz)%j2
        n = j2 - j1 + 1_int64
        status = PF_INT_OK
        o = self%lo
        if (up) o = self%hi
        call window_at(self, t, first, last)
        ! The points the window leaves out on the bound's side: their kernels lie wholly between the
        ! bound and `t`, so each contributes its whole zone integral.
        acc = 0.0_real64
        if (.not. up) then
            k = min(first - 1_int64, n)
            if (k >= 1_int64) acc = self%zones(iz)%cum(k)
        else
            k = max(last + 1_int64, j1) - j1 + 1_int64
            if (k <= n) acc = self%zones(iz)%cum(k)
        end if
        jb = 1_int64 + (max(first, j1) - 1_int64)*self%hstride
        do j = max(first, j1), min(last, j2)
            hj = self%hb(jb)
            jb = jb + self%hstride
            reach = KDE_RADIUS(self%kernel_code)*hj
            if (.not. up) then
                if (self%x(j) + reach <= t) then
                    acc = acc + point_weight(self, j)*self%zones(iz)%val(j - j1 + 1_int64)
                    cycle
                end if
                if (self%x(j) - reach >= t) cycle
                acc = acc + point_weight(self, j)*term_integral(self, j, o, t, .false., 0.0_real64, &
                    .false., status)
            else
                if (self%x(j) - reach >= t) then
                    acc = acc + point_weight(self, j)*self%zones(iz)%val(j - j1 + 1_int64)
                    cycle
                end if
                if (self%x(j) + reach <= t) cycle
                acc = acc + point_weight(self, j)*term_integral(self, j, t, o, .false., 0.0_real64, &
                    .false., status)
            end if
        end do
        l = acc/self%w_total

    end function zone_raw_mass

    !> Where `t` falls among zone `iz`'s stretches: `inside` when the clip removes the estimate
    !! there, with `base` the mass the distribution function keeps through it, and otherwise `cut`,
    !! the stretches' whole negative mass between the zone's bound and `t`.
    pure subroutine stretch_lookup(self, iz, t, inside, base, cut)
        class(pf_kde), intent(in) :: self   !! the fitted estimate
        integer, intent(in)       :: iz     !! the zone
        real(real64), intent(in)  :: t      !! the query, inside the zone
        logical, intent(out)      :: inside !! `t` lies in a stretch
        real(real64), intent(out) :: base   !! the clipped mass up to that stretch's near end
        real(real64), intent(out) :: cut    !! the negative mass to subtract otherwise

        integer(int64) :: k
        integer :: ns

        inside = .false.
        base = 0.0_real64
        cut = 0.0_real64
        ns = self%zones(iz)%ns
        if (ns == 0) return
        if (iz == KDE_ZONE_LO) then
            k = last_at_or_below(self%zones(iz)%e(1:ns), t)
            if (k >= 1_int64) cut = self%zones(iz)%nu(k)
            if (k < int(ns, int64)) then
                if (self%zones(iz)%s(k + 1_int64) <= t) then
                    inside = .true.
                    base = self%zones(iz)%base(k + 1_int64)
                end if
            end if
        else
            k = first_at_or_above(self%zones(iz)%s(1:ns), t)
            if (k <= int(ns, int64)) cut = self%zones(iz)%nu(k)
            if (k > 1_int64) then
                if (self%zones(iz)%e(k - 1_int64) >= t) then
                    inside = .true.
                    base = self%zones(iz)%base(k - 1_int64)
                end if
            end if
        end if

    end subroutine stretch_lookup

    !> The clipped estimate's mass between zone `iz`'s bound and `t`: the raw integral less the
    !! stretches the clip removes, and inside a stretch the value it keeps at that stretch's near
    !! end.
    function zone_cdf_mass(self, iz, t) result(q)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        integer, intent(in)       :: iz   !! the zone
        real(real64), intent(in)  :: t    !! the query, inside the zone
        real(real64)              :: q    !! the mass, over the total weight

        real(real64) :: base, cut
        logical :: inside

        call stretch_lookup(self, iz, t, inside, base, cut)
        if (inside) then
            q = base
            return
        end if
        q = zone_raw_mass(self, iz, t) - cut

    end function zone_cdf_mass

    !> The estimate's mass between the lower zone's inner edge and `t`, both in the interior, where
    !! every term is its plain kernel: a closed form, with the points left of the window read from
    !! the running sum `cwz`.
    pure function interior_mass(self, t) result(q)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64), intent(in)  :: t    !! the query, in the interior
        real(real64)              :: q    !! the mass, over the total weight

        integer(int64) :: j, jb, first, last
        real(real64) :: acc, r, c

        call window_at(self, t, first, last)
        acc = 0.0_real64
        if (first > 1_int64) acc = self%cwz(first - 1_int64)
        jb = 1_int64 + (first - 1_int64)*self%hstride
        do j = first, last
            r = self%hr(jb)
            jb = jb + self%hstride
            c = kde_kernel_cdf(self%kernel_code, (t - self%x(j))*r) - mass_below_zlo(self, j, r)
            if (self%weighted) c = self%w(j)*c
            acc = acc + c
        end do
        q = acc/self%w_total

    end function interior_mass

    !> `P(X <= t)` under `"linear"`, for a `t` strictly inside the support: the clipped estimate's
    !! mass up to `t` over its mass over the whole support. Inside a zone that mass is integrated
    !! point by point; between the zones it is the zone's mass plus a closed form.
    function cdf_linear(self, t) result(p)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64), intent(in)  :: t    !! the query
        real(real64)              :: p    !! the probability at or below `t`

        if (self%zones(KDE_ZONE_LO)%on .and. t <= self%zones(KDE_ZONE_LO)%edge) then
            p = zone_cdf_mass(self, KDE_ZONE_LO, t)/self%znorm
        else if (self%zones(KDE_ZONE_HI)%on .and. t >= self%zones(KDE_ZONE_HI)%edge) then
            p = 1.0_real64 - zone_cdf_mass(self, KDE_ZONE_HI, t)/self%znorm
        else
            p = (self%zones(KDE_ZONE_LO)%mass + interior_mass(self, t))/self%znorm
        end if

    end function cdf_linear

    !> The raw estimate's integral over `[s, t]`, summed over the window: what each stretch's
    !! negative mass is.
    function stretch_raw(self, s, t, status) result(v)
        class(pf_kde), intent(in) :: self   !! the fitted estimate
        real(real64), intent(in)  :: s      !! the lower end
        real(real64), intent(in)  :: t      !! the upper end
        integer, intent(inout)    :: status !! the worst `pf_integrate` status so far
        real(real64)              :: v      !! the integral, over the total weight

        integer(int64) :: j, first, last
        real(real64) :: acc, reach

        reach = KDE_RADIUS(self%kernel_code)*self%hmax
        first = first_at_or_above(self%x, s - reach)
        last = last_at_or_below(self%x, t + reach)
        acc = 0.0_real64
        do j = first, last
            acc = acc + point_weight(self, j)*term_integral(self, j, s, t, .false., 0.0_real64, .false., status)
        end do
        v = acc/self%w_total

    end function stretch_raw

    !> One zone's tables: the points that can reach it, each one's integral of its term over it and
    !! their running weighted sum, and the stretches the clip removes, from the scan.
    subroutine build_zone_tables(self, iz, edge, z, status, pos_before, pos_after)
        class(pf_kde), intent(in)     :: self       !! the fitted estimate, sample and bandwidths set
        integer, intent(in)           :: iz         !! the zone
        real(real64), intent(in)      :: edge       !! its inner end
        type(kde_zone), intent(out)   :: z          !! the tables
        integer, intent(inout)        :: status     !! the worst `pf_integrate` status so far
        logical, intent(out)          :: pos_before !! a positive value before the first stretch
        logical, intent(out)          :: pos_after  !! a positive value after the last

        integer(int64) :: j, k, n, m
        real(real64) :: reach, s0, s1, acc

        m = size(self%x, kind=int64)
        reach = KDE_RADIUS(self%kernel_code)*self%hmax
        z%on = .true.
        z%edge = edge
        if (iz == KDE_ZONE_HI) then
            s0 = edge
            s1 = self%hi
            z%j1 = first_at_or_above(self%x, edge - reach)
            z%j2 = m
        else
            s0 = self%lo
            s1 = edge
            z%j1 = 1_int64
            z%j2 = last_at_or_below(self%x, edge + reach)
        end if
        call scan_zone(self, s0, s1, z, pos_before, pos_after)
        n = z%j2 - z%j1 + 1_int64
        if (n < 1_int64) then
            z%j1 = 1_int64
            z%j2 = 0_int64
            allocate(z%val(0), z%cum(0))
            return
        end if
        allocate(z%val(n), z%cum(n))
        do k = 1_int64, n
            j = z%j1 + k - 1_int64
            z%val(k) = term_integral(self, j, s0, s1, .false., 0.0_real64, .false., status)
        end do
        ! The running weighted sum, from the zone's bound inwards, so that a query reads one entry
        ! for every point its window leaves out on that side.
        acc = 0.0_real64
        if (iz == KDE_ZONE_HI) then
            do k = n, 1_int64, -1_int64
                acc = acc + point_weight(self, z%j1 + k - 1_int64)*z%val(k)
                z%cum(k) = acc
            end do
        else
            do k = 1_int64, n
                acc = acc + point_weight(self, z%j1 + k - 1_int64)*z%val(k)
                z%cum(k) = acc
            end do
        end if

    end subroutine build_zone_tables

    !> One zone's stretch integrals and the mass it holds, once its tables are in place: each
    !! stretch's integral of the raw estimate (negative), accumulated from the zone's bound, and the
    !! clipped mass up to each stretch's near end, which the distribution function answers inside it.
    subroutine build_zone_stretches(self, iz, status)
        class(pf_kde), intent(inout) :: self   !! the fitted estimate, tables in place
        integer, intent(in)          :: iz     !! the zone
        integer, intent(inout)       :: status !! the worst `pf_integrate` status so far

        integer :: ns, k
        integer(int64) :: n
        real(real64) :: acc, total

        n = self%zones(iz)%j2 - self%zones(iz)%j1 + 1_int64
        total = 0.0_real64
        if (n >= 1_int64) then
            if (iz == KDE_ZONE_HI) then
                total = self%zones(iz)%cum(1)
            else
                total = self%zones(iz)%cum(n)
            end if
        end if
        total = total/self%w_total
        ns = self%zones(iz)%ns
        if (ns == 0) then
            self%zones(iz)%mass = total
            return
        end if
        acc = 0.0_real64
        if (iz == KDE_ZONE_HI) then
            do k = ns, 1, -1
                acc = acc + stretch_raw(self, self%zones(iz)%s(k), self%zones(iz)%e(k), status)
                self%zones(iz)%nu(k) = acc
            end do
            do k = ns, 1, -1
                self%zones(iz)%base(k) = zone_raw_mass(self, iz, self%zones(iz)%e(k))
                if (k < ns) self%zones(iz)%base(k) = self%zones(iz)%base(k) - self%zones(iz)%nu(k + 1)
            end do
        else
            do k = 1, ns
                acc = acc + stretch_raw(self, self%zones(iz)%s(k), self%zones(iz)%e(k), status)
                self%zones(iz)%nu(k) = acc
            end do
            do k = 1, ns
                self%zones(iz)%base(k) = zone_raw_mass(self, iz, self%zones(iz)%s(k))
                if (k > 1) self%zones(iz)%base(k) = self%zones(iz)%base(k) - self%zones(iz)%nu(k - 1)
            end do
        end if
        self%zones(iz)%mass = total - acc

    end subroutine build_zone_stretches

    !> Point `j`'s region inside zone `iz`: where its kernel and the zone overlap.
    pure subroutine zone_region(self, iz, j, a1, b1)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer, intent(in)        :: iz   !! the zone
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(out)  :: a1   !! the region's lower end
        real(real64), intent(out)  :: b1   !! its upper end

        real(real64) :: reach

        reach = KDE_RADIUS(self%kernel_code)*self%hb(1_int64 + (j - 1_int64)*self%hstride)
        if (iz == KDE_ZONE_HI) then
            a1 = max(self%zones(iz)%edge, self%x(j) - reach)
            b1 = min(self%hi, self%x(j) + reach)
        else
            a1 = max(self%lo, self%x(j) - reach)
            b1 = min(self%zones(iz)%edge, self%x(j) + reach)
        end if

    end subroutine zone_region

    !> Where point `j`'s term changes sign inside a one-sided zone, bisected on its factor to the
    !! last bit; the end of its region the term is negative from, and `found` false, where it does
    !! not change sign there at all.
    pure subroutine term_sign_change(self, iz, j, sigma, found)
        class(pf_kde), intent(in)  :: self  !! the fitted estimate
        integer, intent(in)        :: iz    !! the zone
        integer(int64), intent(in) :: j     !! the point
        real(real64), intent(out)  :: sigma !! the sign change
        logical, intent(out)       :: found !! the term does change sign

        integer, parameter :: MAX_STEPS = 300
        real(real64) :: a1, b1, neg_end, pos_end, mid
        integer :: it

        call zone_region(self, iz, j, a1, b1)
        ! The term is negative at the end nearer the bound, if anywhere: the factor grows away from
        ! it and is exactly one where the correction stops.
        if (iz == KDE_ZONE_HI) then
            neg_end = b1
            pos_end = a1
        else
            neg_end = a1
            pos_end = b1
        end if
        sigma = neg_end
        found = .false.
        if (.not. (b1 > a1)) return
        if (.not. (term_factor(self, j, neg_end) < 0.0_real64)) return
        found = .true.
        do it = 1, MAX_STEPS
            mid = neg_end + 0.5_real64*(pos_end - neg_end)
            if (.not. (mid > min(a1, b1) .and. mid < max(a1, b1))) exit
            if (term_factor(self, j, mid) < 0.0_real64) then
                neg_end = mid
            else
                pos_end = mid
            end if
        end do
        sigma = pos_end

    end subroutine term_sign_change

    !> The zone sampler's tables: each point's sign change, the magnitude of its term's negative
    !! part, and the running weighted sum of the integral of `|g_j|` over the zone, which the
    !! sampler chooses a point from. Only for a one-sided zone; where the zones meet a term can
    !! change sign twice and every draw there inverts the zone's own integral instead.
    subroutine build_zone_envelope(self, iz, status)
        class(pf_kde), intent(inout) :: self   !! the fitted estimate, tables in place
        integer, intent(in)          :: iz     !! the zone
        integer, intent(inout)       :: status !! the worst `pf_integrate` status so far

        integer(int64) :: j, k, n
        real(real64) :: a1, b1, sigma, acc, negv
        logical :: found

        n = self%zones(iz)%j2 - self%zones(iz)%j1 + 1_int64
        if (self%zones(iz)%two_sided .or. n < 1_int64) return
        allocate(self%zones(iz)%sig(n), self%zones(iz)%neg(n), self%zones(iz)%cabs(n))
        acc = 0.0_real64
        do k = 1_int64, n
            j = self%zones(iz)%j1 + k - 1_int64
            call term_sign_change(self, iz, j, sigma, found)
            call zone_region(self, iz, j, a1, b1)
            negv = 0.0_real64
            if (found) then
                if (iz == KDE_ZONE_HI) then
                    negv = -term_integral(self, j, sigma, b1, .false., 0.0_real64, .false., status)
                else
                    negv = -term_integral(self, j, a1, sigma, .false., 0.0_real64, .false., status)
                end if
            end if
            if (.not. (negv > 0.0_real64)) negv = 0.0_real64
            self%zones(iz)%sig(k) = sigma
            self%zones(iz)%neg(k) = negv
            ! The magnitude's integral is the term's own plus twice what the negative part removed.
            acc = acc + point_weight(self, j)*(self%zones(iz)%val(k) + 2.0_real64*negv)
            self%zones(iz)%cabs(k) = acc
        end do

    end subroutine build_zone_envelope

    !> Everything `"linear"` fixes at `%fit`: the two zones and their tables, the stretches the clip
    !! removes, the interior masses, `Z`, and where the density starts and stops.
    subroutine build_linear(self)
        class(pf_kde), intent(inout) :: self !! the estimate, sample and bandwidths set

        type(kde_zone) :: z
        integer(int64) :: j, jb, m
        integer :: status
        real(real64) :: rad, hj, c, d, elo, ehi, acc, r, mj
        logical :: pos_before_lo, pos_after_lo, pos_before_hi, pos_after_hi, two_sided

        m = size(self%x, kind=int64)
        rad = KDE_RADIUS(self%kernel_code)
        status = PF_INT_OK
        pos_before_lo = .false.
        pos_after_lo = .false.
        pos_before_hi = .false.
        pos_after_hi = .false.

        ! ---- the zones' inner edges ----
        ! Point `j`'s term is corrected below `a + R h_j` and above `b - R h_j`, and only where its
        ! kernel reaches: a point at or beyond `a + 2 R h_j` starts at or above its own edge and
        ! corrects nothing there. So each zone reaches as far as the last edge that is reached.
        elo = self%lo
        ehi = self%hi
        jb = 1_int64
        do j = 1_int64, m
            hj = self%hb(jb)
            jb = jb + self%hstride
            if (self%has_lower) then
                if (self%x(j) < self%lo + 2.0_real64*rad*hj) then
                    c = self%lo + rad*hj
                    if (c > elo) elo = c
                end if
            end if
            if (self%has_upper) then
                if (self%x(j) > self%hi - 2.0_real64*rad*hj) then
                    d = self%hi - rad*hj
                    if (d < ehi) ehi = d
                end if
            end if
        end do
        ! Clipped to the other bound, and where the two would cross the zones meet: the whole
        ! support is corrected, the moments are two-sided everywhere and nothing lies between them.
        ! Only where there IS another bound -- with one bound the other zone does not exist, and
        ! the absent bound's placeholder must not clip anything.
        two_sided = .false.
        if (self%has_lower .and. self%has_upper) then
            if (elo > self%hi) elo = self%hi
            if (ehi < self%lo) ehi = self%lo
            ! A term is two-sided somewhere exactly when the two edges would cross: a kernel wider
            ! than half the support is corrected at both bounds wherever it reaches.
            two_sided = ehi < elo
            if (two_sided) ehi = elo
        end if
        self%zones(KDE_ZONE_LO)%two_sided = two_sided
        self%zones(KDE_ZONE_HI)%two_sided = two_sided
        self%zones(KDE_ZONE_LO)%edge = elo
        self%zones(KDE_ZONE_HI)%edge = ehi

        ! ---- each zone's tables ----
        if (self%has_lower .and. elo > self%lo) then
            call build_zone_tables(self, KDE_ZONE_LO, elo, z, status, pos_before_lo, pos_after_lo)
            z%two_sided = two_sided
            self%zones(KDE_ZONE_LO) = z
            call build_zone_stretches(self, KDE_ZONE_LO, status)
            call build_zone_envelope(self, KDE_ZONE_LO, status)
        end if
        if (self%has_upper .and. ehi < self%hi) then
            call build_zone_tables(self, KDE_ZONE_HI, ehi, z, status, pos_before_hi, pos_after_hi)
            z%two_sided = two_sided
            self%zones(KDE_ZONE_HI) = z
            call build_zone_stretches(self, KDE_ZONE_HI, status)
            call build_zone_envelope(self, KDE_ZONE_HI, status)
        end if

        ! ---- each point's mass between the zones, and its running weighted sum ----
        allocate(self%cwz(m))
        acc = 0.0_real64
        jb = 1_int64
        do j = 1_int64, m
            r = self%hr(jb)
            jb = jb + self%hstride
            mj = mass_below_zhi(self, j, r) - mass_below_zlo(self, j, r)
            ! Where the zones meet the two edges are one point and the difference is zero; rounding
            ! can make it a negative of the last bit, which nothing may accumulate.
            if (.not. (mj > 0.0_real64)) mj = 0.0_real64
            acc = acc + point_weight(self, j)*mj
            self%cwz(j) = acc
        end do
        self%zmid = acc/self%w_total
        self%znorm = self%zones(KDE_ZONE_LO)%mass + self%zmid + self%zones(KDE_ZONE_HI)%mass

        ! ---- where the density starts and stops ----
        ! The extent, clipped to the bounds; but where the clip removes a stretch that starts at
        ! such an end, with nothing positive below it, the density starts where that stretch ends.
        self%dens_lo = self%ext_lo
        self%dens_hi = self%ext_hi
        if (self%has_lower) self%dens_lo = max(self%dens_lo, self%lo)
        if (self%has_upper) self%dens_hi = min(self%dens_hi, self%hi)
        if (self%zones(KDE_ZONE_LO)%on .and. self%zones(KDE_ZONE_LO)%ns > 0) then
            if (.not. pos_before_lo) self%dens_lo = self%zones(KDE_ZONE_LO)%e(1)
        end if
        if (self%zones(KDE_ZONE_HI)%on .and. self%zones(KDE_ZONE_HI)%ns > 0) then
            if (.not. pos_after_hi) self%dens_hi = self%zones(KDE_ZONE_HI)%s(self%zones(KDE_ZONE_HI)%ns)
        end if
        self%zone_status = status

    end subroutine build_linear

    ! ==========================================================================================
    ! `%sample` under the linear correction
    ! ==========================================================================================

    !> How many attempts a draw makes before it falls back to an inversion: `KDE_SAMPLE_TRIES`,
    !! or what `parquet_debug_set_kde_sample_tries` forces, which a test uses to reach the fallback.
    function sample_tries() result(n)
        integer :: n !! the cap

        n = KDE_SAMPLE_TRIES
        if (kde_sample_tries_forced >= 0) n = kde_sample_tries_forced

    end function sample_tries

    !> Where the `|g_j|` mass of point `j` from `a1` reaches `target`, inside `[a1, b1]`: Newton on
    !! the term's own integral, with the bracket taken over whenever a step would leave it. The
    !! integral is increasing and its derivative is the term's magnitude, which the same evaluation
    !! gives, so a handful of steps place the draw to the last bits.
    function solve_term_mass(self, j, a1, b1, target, status) result(y)
        class(pf_kde), intent(in)  :: self   !! the fitted estimate
        integer(int64), intent(in) :: j      !! the point
        real(real64), intent(in)   :: a1     !! the piece's lower end, where the mass is zero
        real(real64), intent(in)   :: b1     !! its upper end
        real(real64), intent(in)   :: target !! the mass to reach
        integer, intent(inout)     :: status !! the worst `pf_integrate` status so far
        real(real64)               :: y      !! where it is reached

        integer, parameter :: MAX_STEPS = 100
        real(real64) :: lo_b, hi_b, v, g, yn, cand
        integer :: it

        lo_b = a1
        hi_b = b1
        y = a1 + 0.5_real64*(b1 - a1)
        if (.not. (b1 > a1)) then
            y = a1
            return
        end if
        do it = 1, MAX_STEPS
            v = term_integral(self, j, a1, y, .true., 0.0_real64, .false., status)
            if (v >= target) then
                hi_b = y
            else
                lo_b = y
            end if
            if (hi_b - lo_b <= 4.0_real64*spacing(max(abs(lo_b), abs(hi_b)))) exit
            yn = lo_b + 0.5_real64*(hi_b - lo_b)
            g = abs(term_density(self, j, y))
            if (g > 0.0_real64) then
                ! A Newton step is taken only when it lands strictly inside the bracket.
                if (abs(v - target) <= g*(hi_b - lo_b)) then
                    cand = y - (v - target)/g
                    if (cand > lo_b .and. cand < hi_b) yn = cand
                end if
            end if
            ! The step, never the bisection's own midpoint against itself: comparing the two would
            ! read every bisection step as no step at all and stop after the second one.
            if (abs(yn - y) <= 2.0_real64*spacing(abs(y))) then
                y = yn
                exit
            end if
            y = yn
        end do

    end function solve_term_mass

    !> A draw from `|g_j|` over point `j`'s part of zone `iz`, by inverting that one term: the
    !! uniform `u` locates the mass, which the sign change splits into the part the term is negative
    !! on and the part it is positive on.
    function invert_abs_term(self, iz, j, k, u, status) result(y)
        class(pf_kde), intent(in)  :: self   !! the fitted estimate
        integer, intent(in)        :: iz     !! the zone
        integer(int64), intent(in) :: j      !! the point
        integer(int64), intent(in) :: k      !! its place in the zone's tables
        real(real64), intent(in)   :: u      !! the uniform, in `[0, 1)`
        integer, intent(inout)     :: status !! the worst `pf_integrate` status so far
        real(real64)               :: y      !! the draw

        real(real64) :: a1, b1, sigma, amass, target, upto_sigma

        call zone_region(self, iz, j, a1, b1)
        sigma = min(max(self%zones(iz)%sig(k), a1), b1)
        amass = self%zones(iz)%val(k) + 2.0_real64*self%zones(iz)%neg(k)
        target = u*amass
        ! The mass from `a1` up to the sign change: the negative part at a lower bound, everything
        ! but it at an upper one, where the term turns negative on its way to the bound.
        if (iz == KDE_ZONE_HI) then
            upto_sigma = amass - self%zones(iz)%neg(k)
        else
            upto_sigma = self%zones(iz)%neg(k)
        end if
        if (target <= upto_sigma) then
            y = solve_term_mass(self, j, a1, sigma, target, status)
        else
            y = solve_term_mass(self, j, sigma, b1, target - upto_sigma, status)
        end if

    end function invert_abs_term

    !> The fallback for a draw in zone `iz`: the point at which the estimate's own distribution
    !! function reaches the share `u` of the zone's mass, by the quantile solve restricted to the
    !! zone. What every draw takes where the zones meet, and what a draw takes after
    !! `sample_tries()` rejected attempts.
    function invert_zone(self, iz, u) result(y)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        integer, intent(in)       :: iz   !! the zone
        real(real64), intent(in)  :: u    !! the uniform, in `[0, 1)`
        real(real64)              :: y    !! the draw

        real(real64) :: p, a1, b1

        if (iz == KDE_ZONE_HI) then
            p = (self%zones(KDE_ZONE_LO)%mass + self%zmid + (1.0_real64 - u)*self%zones(iz)%mass)/self%znorm
            a1 = self%zones(iz)%edge
            b1 = max(self%dens_hi, a1)
        else
            p = u*self%zones(iz)%mass/self%znorm
            b1 = self%zones(iz)%edge
            a1 = min(self%dens_lo, b1)
        end if
        y = solve_cdf(self, p, a1, b1, a1 + 0.5_real64*(b1 - a1))

    end function invert_zone

    !> One draw from a zone: a point chosen in proportion to the integral of its term's magnitude
    !! over the zone, a position drawn from that magnitude by inverting the one term, and the draw
    !! kept with probability `f_+/e`, `e` the weighted sum of every term's magnitude there.
    !!
    !! The envelope lies above the clipped estimate everywhere, so the kept draws follow it exactly;
    !! about one attempt in `M/A` is rejected, `A` the envelope's own mass. After `sample_tries()`
    !! attempts, and always where the zones meet, the zone's integral is inverted instead.
    function draw_zone(self, iz, key, sk, d) result(y)
        class(pf_kde), intent(in)     :: self !! the fitted estimate
        integer, intent(in)           :: iz   !! the zone
        integer(int64), intent(in)    :: key  !! the family key
        integer(int64), intent(in)    :: sk   !! this element's stream
        integer(int64), intent(inout) :: d    !! the next draw to read; advanced past those read
        real(real64)                  :: y    !! the draw

        integer(int64) :: j, k, n
        integer :: attempt, status
        real(real64) :: total, acc, env, fplus
        integer(int64) :: first, last

        status = PF_INT_OK
        n = self%zones(iz)%j2 - self%zones(iz)%j1 + 1_int64
        if (.not. self%zones(iz)%two_sided .and. n >= 1_int64) then
            if (allocated(self%zones(iz)%cabs)) then
                total = self%zones(iz)%cabs(n)
                if (total > 0.0_real64) then
                    do attempt = 1, sample_tries()
                        k = last_at_or_below(self%zones(iz)%cabs(1:n), pf_random_at(key, sk, d)*total) + 1_int64
                        if (k > n) k = n
                        d = d + 1_int64
                        j = self%zones(iz)%j1 + k - 1_int64
                        y = invert_abs_term(self, iz, j, k, pf_random_at(key, sk, d), status)
                        d = d + 1_int64
                        ! The estimate and the envelope at the draw, from one pass over its window.
                        call window_at(self, y, first, last)
                        call linear_sum(self, y, first, last, acc, env=env)
                        fplus = 0.0_real64
                        if (acc > 0.0_real64) fplus = acc
                        if (env > 0.0_real64) then
                            if (pf_random_at(key, sk, d)*env < fplus) then
                                d = d + 1_int64
                                return
                            end if
                        end if
                        d = d + 1_int64
                    end do
                end if
            end if
        end if
        y = invert_zone(self, iz, pf_random_at(key, sk, d))
        d = d + 1_int64

    end function draw_zone

    !> One draw from between the zones, where every term is its plain kernel: a point chosen in
    !! proportion to the mass it holds there, then its kernel's variate, redrawn while it lands
    !! outside and inverted over the same interval after `sample_tries()` of them.
    function draw_interior(self, key, sk, d) result(y)
        class(pf_kde), intent(in)     :: self !! the fitted estimate
        integer(int64), intent(in)    :: key  !! the family key
        integer(int64), intent(in)    :: sk   !! this element's stream
        integer(int64), intent(inout) :: d    !! the next draw to read; advanced past those read
        real(real64)                  :: y    !! the draw

        integer(int64) :: j, m
        integer :: attempt
        real(real64) :: xj, hj, e, reach, alo, ahi, total

        m = size(self%x, kind=int64)
        total = self%cwz(m)
        j = last_at_or_below(self%cwz, pf_random_at(key, sk, d)*total) + 1_int64
        if (j > m) j = m
        d = d + 1_int64
        xj = self%x(j)
        hj = self%hb(1_int64 + (j - 1_int64)*self%hstride)
        reach = KDE_RADIUS(self%kernel_code)*hj
        alo = -huge(1.0_real64)
        ahi = huge(1.0_real64)
        if (self%has_lower) alo = self%zones(KDE_ZONE_LO)%edge
        if (self%has_upper) ahi = self%zones(KDE_ZONE_HI)%edge
        do attempt = 1, sample_tries()
            call kernel_variate(self%kernel_code, key, sk, d, e)
            if (self%kernel_code == KDE_GAUSSIAN) then
                if (abs(e) > KDE_GAUSS_CUT) cycle
            end if
            y = xj + hj*e
            if (y >= alo .and. y <= ahi) return
        end do
        y = invert_between(self, j, xj, hj, pf_random_at(key, sk, d), max(xj - reach, alo), min(xj + reach, ahi))

    end function draw_interior

    !> One draw under `"linear"`: the first draw chooses among the two zones and the interior by
    !! their masses, and the next ones place it there.
    function draw_linear(self, key, sk) result(y)
        class(pf_kde), intent(in)  :: self !! the fitted estimate, defined
        integer(int64), intent(in) :: key  !! the family key
        integer(int64), intent(in) :: sk   !! this element's stream
        real(real64)               :: y    !! the draw

        integer(int64) :: d
        real(real64) :: u, mlo, mmid, mhi
        integer :: iz

        d = 1_int64
        u = pf_random_at(key, sk, d)*self%znorm
        d = d + 1_int64
        mlo = self%zones(KDE_ZONE_LO)%mass
        mmid = self%zmid
        mhi = self%zones(KDE_ZONE_HI)%mass
        iz = 0
        if (self%zones(KDE_ZONE_LO)%on .and. u < mlo) then
            iz = KDE_ZONE_LO
        else if (self%zones(KDE_ZONE_HI)%on .and. u >= mlo + mmid) then
            iz = KDE_ZONE_HI
        else if (.not. (mmid > 0.0_real64)) then
            ! Nothing lies between the zones -- they meet, or one of them holds everything -- so the
            ! rounding that put the draw here decides nothing.
            if (self%zones(KDE_ZONE_LO)%on) then
                iz = KDE_ZONE_LO
            else if (self%zones(KDE_ZONE_HI)%on) then
                iz = KDE_ZONE_HI
            end if
        end if
        if (iz == 0) then
            y = draw_interior(self, key, sk, d)
        else
            y = draw_zone(self, iz, key, sk, d)
        end if

    end function draw_linear

    !> Records phase `k` of the fit, from clock reading `c0` to `c1`, in nanoseconds; nothing when
    !! the processor has no clock.
    subroutine add_phase(k, c0, c1, rate)
        integer, intent(in)        :: k    !! the phase: 1 the sort, 2 the pilot, 3 the bandwidths
        integer(int64), intent(in) :: c0   !! the clock at the phase's start, in its counts
        integer(int64), intent(in) :: c1   !! the clock at its end
        integer(int64), intent(in) :: rate !! the clock's counts per second; 0 without a clock

        if (rate <= 0_int64) return
        kde_fit_ns(k) = nint(real(c1 - c0, real64)*(1.0e9_real64/real(rate, real64)), kind=int64)

    end subroutine add_phase

end submodule parquet_kde_fit ! GCOVR_EXCL_LINE
