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

        ! ---- each point's mass inside the support, where it can differ from one ----
        call build_mass(self)
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
        call kde_query_team(EP, threads, n, query_work(self), team)
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
        call kde_query_team(EP, threads, n, KDE_QUANTILE_STEPS*query_work(self), team)
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
        call kde_kernel_name(self, token)
        write(u, '(2x,a,a)') "kernel      ", token
        call kde_rule_name(self, token)
        write(u, '(2x,a,a)') "rule        ", token
        write(u, '(2x,a,es24.16e3)') "bandwidth   ", self%h
        write(u, '(2x,a,i0)') "n           ", self%cnt_all
        write(u, '(2x,a,i0)') "n_valid     ", self%cnt_valid
        write(u, '(2x,a,i0)') "n_null      ", self%cnt_null
        write(u, '(2x,a,i0)') "n_nan       ", self%cnt_nan
        write(u, '(2x,a,i0)') "n_outside   ", self%cnt_out
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
            if (self%pilot_grid%initialised) write(u, '(2x,a,i0,a,es24.16e3,a,es24.16e3)') "pilot       ", &
                self%pilot_grid%nc, " cells over", self%pilot_grid%x0, " to", self%pilot_grid%x1
            if (self%defined) then
                write(u, '(2x,a,es24.16e3)') "h_j min     ", minval(self%hb)
                write(u, '(2x,a,es24.16e3)') "h_j max     ", self%hmax
            end if
        end if
        if (.not. self%defined) write(u, '(2x,a)') "undefined   every query answers NaN (ok = .false.)"

    end procedure kde_print

    module procedure kde_clear

        if (allocated(self%x)) deallocate(self%x)
        if (allocated(self%w)) deallocate(self%w)
        if (allocated(self%cw)) deallocate(self%cw)
        if (allocated(self%mass)) deallocate(self%mass)
        if (allocated(self%hb)) deallocate(self%hb)
        if (allocated(self%hr)) deallocate(self%hr)
        self%adapt = kde_adapt()
        self%pilot_grid = pf_kde_grid(adapt=kde_adapt())
        self%hmax = 0.0_real64
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
    pure function cdf_at(self, t) result(p)
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
    !! `p = 1` are the two ends of the estimate's support.
    pure function quantile_at(self, p) result(x)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64), intent(in)  :: p    !! the probability, in `[0, 1]`
        real(real64)              :: x    !! the quantile

        integer, parameter :: MAX_STEPS = 200
        real(real64) :: a, b, reach, fx, dx, xn
        integer :: it

        x = ieee_value(1.0_real64, ieee_quiet_nan)
        if (.not. self%defined) return
        reach = KDE_RADIUS(self%kernel_code)*self%hmax
        a = self%x(1) - reach
        b = self%x(size(self%x, kind=int64)) + reach
        if (self%has_lower) a = max(a, self%lo)
        if (self%has_upper) b = min(b, self%hi)
        if (p <= 0.0_real64) then
            x = a
            return
        end if
        if (p >= 1.0_real64) then
            x = b
            return
        end if
        ! `a` always has `%cdf < p` and `b` always `%cdf >= p`; the answer is the point between.
        x = a + p*(b - a)
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

    end function quantile_at

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
        call kde_query_team(EP, threads, n, KDE_DRAW_WORK, team)
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
    !! where the kernel's cut or the support rejects it and, after `KDE_SAMPLE_TRIES` redraws,
    !! found by inverting the point's corrected distribution function instead.
    pure function draw_at(self, key, sk) result(y)
        class(pf_kde), intent(in)  :: self !! the fitted estimate, defined
        integer(int64), intent(in) :: key  !! the family key
        integer(int64), intent(in) :: sk   !! this element's stream
        real(real64)               :: y    !! the draw

        integer(int64) :: j, m, d
        integer :: attempt
        real(real64) :: xj, hj, e
        logical :: kept

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
        do attempt = 1, KDE_SAMPLE_TRIES
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

        integer :: it
        real(real64) :: a, b, mid, r, reach, s_a, target

        r = point_reciprocal(self, j)
        reach = KDE_RADIUS(self%kernel_code)*hj
        a = xj - reach
        b = xj + reach
        if (self%has_lower) then
            if (a < self%lo) a = self%lo
        end if
        if (self%has_upper) then
            if (b > self%hi) b = self%hi
        end if
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

    end function invert_point

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
