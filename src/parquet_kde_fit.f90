!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `pf_kde`: the fit, the exact queries, the curve, the accessors and the printer.
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
submodule (parquet_kde) parquet_kde_fit

    implicit none

contains

    ! ==========================================================================================
    ! Fitting
    ! ==========================================================================================

    module procedure kde_fit_f64

        character(len=*), parameter :: EP = "pf_kde%fit"
        real(real64), allocatable :: keep_x(:), keep_w(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: nv, nnull, nnan, nout, m, i
        real(real64) :: v, hh, adj
        logical :: saw_nan, freq

        call kde_clear(self)

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
            self%rule_code = KDE_RULE_SILVERMAN
        end if
        adj = 1.0_real64
        if (present(adjust)) then
            if (.not. kde_positive_finite(adjust)) call kde_abort(EP, "adjust must be a finite, positive number")
            adj = adjust
        end if
        call kde_resolve_setup(EP, kernel, lower, upper, boundary, self%kernel_code, self%has_lower, &
            self%lo, self%has_upper, self%hi, self%boundary_code)
        call stats_weight_kind(EP, weight_type, freq)

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

        ! ---- the bandwidth ----
        if (present(bandwidth)) then
            hh = bandwidth
        else
            call kde_rule_bandwidth(self%rule_code, self%x, freq, self%w, weight_type, threads, hh)
        end if
        ! A rule that found no scale answers NaN, and an explicit bandwidth times `adjust` can only
        ! fail by overflowing; either way there is no estimate to answer with.
        if (ieee_is_nan(hh)) return
        hh = hh*adj
        if (.not. kde_positive_finite(hh)) return
        self%h = hh

        ! ---- each point's mass inside the support, where it can differ from one ----
        call build_mass(self)
        self%defined = .true.
        if (present(ok)) ok = .true.

    end procedure kde_fit_f64

    module procedure kde_fit_f32

        real(real64), allocatable :: xw(:)

        ! Widened first, so that the sample is ordered, searched and summed in `real64` exactly as
        ! the `real64` form does it.
        allocate(xw(size(x, kind=int64)))
        xw = real(x, real64)
        call kde_fit_f64(self, xw, bandwidth, rule, adjust, kernel, lower, upper, boundary, &
            is_valid, weights, weight_type, skipnan, n_null, n_nan, n_outside, ok, threads)

    end procedure kde_fit_f32

    ! ==========================================================================================
    ! Queries
    ! ==========================================================================================

    module procedure kde_pdf_r0

        call require_fitted(self, "pf_kde%pdf")
        f = density_at(self, x)

    end procedure kde_pdf_r0

    module procedure kde_pdf_r1

        integer(int64) :: i

        call require_fitted(self, "pf_kde%pdf")
        if (size(f, kind=int64) /= size(x, kind=int64)) &
            call kde_abort("pf_kde%pdf", "f must have one element per point of x")
        do i = 1_int64, size(x, kind=int64)
            f(i) = density_at(self, x(i))
        end do

    end procedure kde_pdf_r1

    module procedure kde_cdf_r0

        call require_fitted(self, "pf_kde%cdf")
        p = cdf_at(self, x)

    end procedure kde_cdf_r0

    module procedure kde_cdf_r1

        integer(int64) :: i

        call require_fitted(self, "pf_kde%cdf")
        if (size(p, kind=int64) /= size(x, kind=int64)) &
            call kde_abort("pf_kde%cdf", "p must have one element per point of x")
        do i = 1_int64, size(x, kind=int64)
            p(i) = cdf_at(self, x(i))
        end do

    end procedure kde_cdf_r1

    module procedure kde_quantile_r0

        call require_fitted(self, "pf_kde%quantile")
        call check_probability(p)
        x = quantile_at(self, p)

    end procedure kde_quantile_r0

    module procedure kde_quantile_r1

        integer(int64) :: i

        call require_fitted(self, "pf_kde%quantile")
        if (size(x, kind=int64) /= size(p, kind=int64)) &
            call kde_abort("pf_kde%quantile", "x must have one element per element of p")
        do i = 1_int64, size(p, kind=int64)
            call check_probability(p(i))
        end do
        do i = 1_int64, size(p, kind=int64)
            x(i) = quantile_at(self, p(i))
        end do

    end procedure kde_quantile_r1

    module procedure kde_curve

        character(len=*), parameter :: EP = "pf_kde%curve"
        real(real64) :: a, b, c, nan
        integer(int64) :: n, i

        call require_fitted(self, EP)
        n = size(x, kind=int64)
        if (size(f, kind=int64) /= n) call kde_abort(EP, "x and f must have the same size")
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
        do i = 1_int64, n
            f(i) = density_at(self, x(i))
        end do

    end procedure kde_curve

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
        res = .false.
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
        if (.not. self%defined) write(u, '(2x,a)') "undefined   every query answers NaN (ok = .false.)"

    end procedure kde_print

    module procedure kde_clear

        if (allocated(self%x)) deallocate(self%x)
        if (allocated(self%w)) deallocate(self%w)
        if (allocated(self%cw)) deallocate(self%cw)
        if (allocated(self%mass)) deallocate(self%mass)
        self%fitted = .false.
        self%defined = .false.
        self%kernel_code = KDE_GAUSSIAN
        self%rule_code = KDE_RULE_SILVERMAN
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
    !! mirror image about each bound. Not divided by the point's mass inside the support.
    pure function images_cdf(self, j, t) result(s)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: t    !! where to evaluate
        real(real64)               :: s    !! the mass at or below `t`

        real(real64) :: xj

        xj = self%x(j)
        s = kde_kernel_cdf(self%kernel_code, (t - xj)/self%h)
        if (self%boundary_code /= KDE_BOUNDARY_REFLECT) return
        if (self%has_lower) s = s + kde_kernel_cdf(self%kernel_code, (t - (2.0_real64*self%lo - xj))/self%h)
        if (self%has_upper) s = s + kde_kernel_cdf(self%kernel_code, (t - (2.0_real64*self%hi - xj))/self%h)

    end function images_cdf

    !> Point `j`'s kernel density at `t`, summed over its images, in standard-deviation units (not
    !! yet divided by the bandwidth or by the point's mass inside the support).
    pure function images_pdf(self, j, t) result(k)
        class(pf_kde), intent(in)  :: self !! the fitted estimate
        integer(int64), intent(in) :: j    !! the point
        real(real64), intent(in)   :: t    !! where to evaluate
        real(real64)               :: k    !! the density there

        real(real64) :: xj

        xj = self%x(j)
        k = kde_kernel_pdf(self%kernel_code, (t - xj)/self%h)
        if (self%boundary_code /= KDE_BOUNDARY_REFLECT) return
        if (self%has_lower) k = k + kde_kernel_pdf(self%kernel_code, (t - (2.0_real64*self%lo - xj))/self%h)
        if (self%has_upper) k = k + kde_kernel_pdf(self%kernel_code, (t - (2.0_real64*self%hi - xj))/self%h)

    end function images_pdf

    !> Each point's mass inside the support, stored only where a correction makes it differ from
    !! one: under `"renormalise"` whenever a bound is given, and under `"reflect"` only when both
    !! bounds are given and the kernel is wider than the range between them, which is the one case
    !! where the images omit mass (the doubly reflected terms).
    subroutine build_mass(self)
        class(pf_kde), intent(inout) :: self !! the estimate, sample and bandwidth set

        integer(int64) :: j, m
        real(real64) :: s_lo, s_hi

        select case (self%boundary_code)
        case (KDE_BOUNDARY_RENORMALISE)
            continue
        case (KDE_BOUNDARY_REFLECT)
            if (.not. (self%has_lower .and. self%has_upper)) return
            if (KDE_RADIUS(self%kernel_code)*self%h <= self%hi - self%lo) return
        case default
            return
        end select
        m = size(self%x, kind=int64)
        allocate(self%mass(m))
        do j = 1_int64, m
            s_lo = 0.0_real64
            if (self%has_lower) s_lo = images_cdf(self, j, self%lo)
            if (self%has_upper) then
                s_hi = images_cdf(self, j, self%hi)
            else
                s_hi = 1.0_real64
            end if
            self%mass(j) = s_hi - s_lo
        end do

    end subroutine build_mass

    !> The density at `t`: the weighted sum of the kernels within reach, over the total weight
    !! and the bandwidth. Zero outside the support; NaN at a NaN `t` or on an undefined estimate.
    pure function density_at(self, t) result(f)
        class(pf_kde), intent(in) :: self !! the fitted estimate
        real(real64), intent(in)  :: t    !! where to evaluate
        real(real64)              :: f    !! the density

        integer(int64) :: j, first, last
        real(real64) :: reach, k, acc

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
        reach = KDE_RADIUS(self%kernel_code)*self%h
        first = first_at_or_above(self%x, t - reach)
        last = last_at_or_below(self%x, t + reach)
        acc = 0.0_real64
        do j = first, last
            k = images_pdf(self, j, t)
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

        integer(int64) :: j, first, last
        real(real64) :: reach, c, acc, s_lo

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
        reach = KDE_RADIUS(self%kernel_code)*self%h
        first = first_at_or_above(self%x, t - reach)
        last = last_at_or_below(self%x, t + reach)
        if (first <= 1_int64) then
            acc = 0.0_real64
        else if (self%weighted) then
            acc = self%cw(first - 1_int64)
        else
            acc = real(first - 1_int64, real64)
        end if
        do j = first, last
            s_lo = 0.0_real64
            if (self%has_lower) s_lo = images_cdf(self, j, self%lo)
            c = images_cdf(self, j, t) - s_lo
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
        reach = KDE_RADIUS(self%kernel_code)*self%h
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

end submodule parquet_kde_fit ! GCOVR_EXCL_LINE
