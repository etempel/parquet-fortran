!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The workers both forms are built on: the abort, the token resolution, the four kernels'
!> densities and distribution functions, the three bandwidth rules, the column forms' widening
!> and the test hooks.
!!
!! Every kernel is written in STANDARD-DEVIATION units: `z` is the offset from the kernel's centre
!! divided by the bandwidth, and the kernel's own scale `KDE_SCALE` is applied here, once, so that
!! no caller ever sees the unit-scale form. Each distribution function is evaluated on the lower
!! half and mirrored, `C(z) = 1 - C(-z)` for `z > 0`, which keeps the kernel exactly symmetric and
!! forms every polynomial in a cancellation-free factored shape near its lower end.
submodule (parquet_kde) parquet_kde_core

    implicit none

contains

    module procedure kde_abort

        ! One thread aborts, not several: two threads reaching ERROR STOP at once leave the exit
        ! status nondeterministic under ifx (`api-conventions.md`).
        !$omp critical (parquet_kde_abort)
        error stop entry // ": " // text
        !$omp end critical (parquet_kde_abort)

    end procedure kde_abort

    module procedure kde_query_team

        integer(int64) :: nt
        real(real64) :: cap

        team = 1
        if (present(threads)) then
            if (threads < 1) call kde_abort(entry, "threads must be positive")
        end if
#ifdef _OPENMP
        if (n < 2_int64) return
        call resolve_thread_count(threads, n, nt)
        if (nt <= 1_int64) return
        ! The team whose every thread gets at least the floor's worth of work; the division first,
        ! so that the product cannot overflow.
        cap = real(n, real64)*(work/KDE_QUERY_MIN_WORK)
        if (cap < real(nt, real64)) nt = int(cap, int64)
        if (nt >= 2_int64) team = int(nt)
#else
        ! No OpenMP: there is no team to open. The locals are assigned so that a serial build does
        ! not report them unused, and none of them can change the answer.
        nt = n
        cap = work
#endif

    end procedure kde_query_team

    module procedure kde_record_team
#ifdef _OPENMP
        use omp_lib, only : omp_get_num_threads
#endif

        ! The thread that records it is the one every team has; the others wait at the end of the
        ! construct, so the value is in place before the region's work begins.
        !$omp single
#ifdef _OPENMP
        kde_team_used = omp_get_num_threads()
#else
        kde_team_used = 1
#endif
        !$omp end single

    end procedure kde_record_team

    module procedure kde_fold

        character(len=len(token)) :: t
        integer :: i, c

        folded = ""
        t = adjustl(token)
        ! Refused whole rather than cut short: a cut token could fold to a valid name.
        if (len_trim(t) > len(folded)) return
        do i = 1, len_trim(t)
            c = iachar(t(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) c = c + (iachar("a") - iachar("A"))
            folded(i:i) = achar(c)
        end do

    end procedure kde_fold

    module procedure kde_resolve_kernel

        select case (kde_fold(token))
        case ("gaussian")
            code = KDE_GAUSSIAN
        case ("epanechnikov")
            code = KDE_EPANECHNIKOV
        case ("bspline")
            code = KDE_BSPLINE
        case ("box")
            code = KDE_BOX
        case default
            code = 0
            call kde_abort(entry, 'kernel must be "gaussian", "epanechnikov", "bspline" or "box"')
        end select

    end procedure kde_resolve_kernel

    module procedure kde_resolve_rule

        select case (kde_fold(token))
        case ("silverman")
            code = KDE_RULE_SILVERMAN
        case ("scott")
            code = KDE_RULE_SCOTT
        case ("isj")
            code = KDE_RULE_ISJ
        case default
            code = 0
            call kde_abort(entry, 'rule must be "isj", "silverman" or "scott"')
        end select

    end procedure kde_resolve_rule

    module procedure kde_resolve_boundary

        select case (kde_fold(token))
        case ("renormalise")
            code = KDE_BOUNDARY_RENORMALISE
        case ("reflect")
            code = KDE_BOUNDARY_REFLECT
        case default
            code = 0
            call kde_abort(entry, 'boundary must be "renormalise" or "reflect"')
        end select

    end procedure kde_resolve_boundary

    module procedure kde_resolve_setup

        kernel_code = KDE_GAUSSIAN
        if (present(kernel)) call kde_resolve_kernel(entry, kernel, kernel_code)
        has_lower = present(lower)
        has_upper = present(upper)
        lo = 0.0_real64
        hi = 0.0_real64
        if (has_lower) then
            if (.not. ieee_is_finite(lower)) call kde_abort(entry, "lower and upper must be finite")
            lo = lower
        end if
        if (has_upper) then
            if (.not. ieee_is_finite(upper)) call kde_abort(entry, "lower and upper must be finite")
            hi = upper
        end if
        if (has_lower .and. has_upper) then
            if (.not. (lo < hi)) call kde_abort(entry, "lower must be below upper")
        end if
        boundary_code = KDE_BOUNDARY_NONE
        if (present(boundary)) then
            if (.not. (has_lower .or. has_upper)) call kde_abort(entry, "boundary= needs lower= or upper=")
            call kde_resolve_boundary(entry, boundary, boundary_code)
        else if (has_lower .or. has_upper) then
            boundary_code = KDE_BOUNDARY_RENORMALISE
        end if

    end procedure kde_resolve_setup

    module procedure kde_positive_finite

        res = .false.
        if (ieee_is_nan(v)) return
        if (.not. ieee_is_finite(v)) return
        res = v > 0.0_real64

    end procedure kde_positive_finite

    module procedure kde_bandwidth_usable

        ! The NaN is screened first, as its own test: an ordered comparison raises IEEE_INVALID on
        ! one.
        res = .false.
        if (h /= h) return
        if (.not. (h > 0.0_real64)) return
        res = h <= huge(h)/KDE_RADIUS(code)

    end procedure kde_bandwidth_usable

    module procedure kde_outside

        res = .not. (abs(v) <= huge(v))
        if (res) return
        if (has_lower) res = v < lo
        if (res) return
        if (has_upper) res = v > hi

    end procedure kde_outside

    module procedure kde_kernel_pdf

        real(real64) :: u, a

        k = 0.0_real64
        a = abs(z)
        ! Every caller screens a NaN before forming `z`: an ordered comparison raises IEEE_INVALID
        ! on one, which nagfor's default `-ieee=stop` turns into a dead process.
        if (.not. (a < KDE_RADIUS(code))) then
            ! The Gaussian is the one kernel with mass AT its radius: its cut is inclusive, so the
            ! kernel is exactly the renormalised `phi` on `[-5, 5]`.
            if (code == KDE_GAUSSIAN .and. a == KDE_GAUSS_CUT) k = pf_norm_pdf(a)/KDE_GAUSS_MASS
            return
        end if
        select case (code)
        case (KDE_GAUSSIAN)
            k = pf_norm_pdf(a)/KDE_GAUSS_MASS
        case (KDE_EPANECHNIKOV)
            u = a/KDE_SCALE(code)
            k = 0.75_real64*(1.0_real64 - u)*(1.0_real64 + u)/KDE_SCALE(code)
        case (KDE_BSPLINE)
            u = a/KDE_SCALE(code)
            if (u < 1.0_real64) then
                k = (2.0_real64/3.0_real64 - u*u + 0.5_real64*u*u*u)/KDE_SCALE(code)
            else
                k = (2.0_real64 - u)**3/(6.0_real64*KDE_SCALE(code))
            end if
        case (KDE_BOX)
            k = 0.5_real64/KDE_SCALE(code)
        end select

    end procedure kde_kernel_pdf

    module procedure kde_kernel_pdf_many

        real(real64) :: u, a, c, r
        integer(int64) :: i

        ! `kde_kernel_pdf`'s formulas and support tests, written out once per kernel so that the
        ! choice is made once per row rather than once per cell; KEEP THE TWO IN STEP. The
        ! Gaussian still calls the library's `phi`, which is the one spelling of it. The grid
        ! converging to the exact estimate as the cell width shrinks is the test that sees the two
        ! disagree. Every caller screens a NaN before forming `z`, as for the scalar form.
        c = KDE_SCALE(code)
        r = KDE_RADIUS(code)
        select case (code)
        case (KDE_GAUSSIAN)
            ! The one kernel with mass AT its radius: the cut is inclusive.
            do i = 1_int64, size(z, kind=int64)
                a = abs(z(i))
                k(i) = 0.0_real64
                if (a <= KDE_GAUSS_CUT) k(i) = pf_norm_pdf(a)/KDE_GAUSS_MASS
            end do
        case (KDE_EPANECHNIKOV)
            do i = 1_int64, size(z, kind=int64)
                a = abs(z(i))
                k(i) = 0.0_real64
                if (a < r) then
                    u = a/c
                    k(i) = 0.75_real64*(1.0_real64 - u)*(1.0_real64 + u)/c
                end if
            end do
        case (KDE_BSPLINE)
            do i = 1_int64, size(z, kind=int64)
                a = abs(z(i))
                k(i) = 0.0_real64
                if (a < r) then
                    u = a/c
                    if (u < 1.0_real64) then
                        k(i) = (2.0_real64/3.0_real64 - u*u + 0.5_real64*u*u*u)/c
                    else
                        k(i) = (2.0_real64 - u)**3/(6.0_real64*c)
                    end if
                end if
            end do
        case default
            do i = 1_int64, size(z, kind=int64)
                k(i) = 0.0_real64
                if (abs(z(i)) < r) k(i) = 0.5_real64/c
            end do
        end select

    end procedure kde_kernel_pdf_many

    module procedure kde_kernel_cdf

        real(real64) :: t, u, lower

        ! `t` is the offset folded onto the lower half; `lower` is the mass at or below `t`.
        t = -abs(z)
        if (.not. (t > -KDE_RADIUS(code))) then
            lower = 0.0_real64
        else
            select case (code)
            case (KDE_GAUSSIAN)
                ! The mass between the cut and `t`, over the mass the cut keeps.
                lower = (pf_norm_cdf(t) - KDE_GAUSS_TAIL)/KDE_GAUSS_MASS
            case (KDE_EPANECHNIKOV)
                ! `(1 + u)**2 (2 - u)/4` is `1/2 + 3u/4 - u**3/4`, factored so that nothing
                ! cancels as `u` approaches -1.
                u = t/KDE_SCALE(code)
                lower = 0.25_real64*(1.0_real64 + u)**2*(2.0_real64 - u)
            case (KDE_BSPLINE)
                u = t/KDE_SCALE(code)
                if (u <= -1.0_real64) then
                    lower = (2.0_real64 + u)**4/24.0_real64
                else
                    ! The integral of `2/3 - u**2 - u**3/2` from -1, plus the outer piece's 1/24.
                    lower = 0.5_real64 + u*(2.0_real64/3.0_real64 - u*u*(1.0_real64/3.0_real64 &
                        + 0.125_real64*u))
                end if
            case (KDE_BOX)
                u = t/KDE_SCALE(code)
                lower = 0.5_real64*(1.0_real64 + u)
            end select
        end if
        if (z > 0.0_real64) then
            c = 1.0_real64 - lower
        else
            c = lower
        end if

    end procedure kde_kernel_cdf

    module procedure kde_n_eff

        if (present(weights)) then
            if (freq) then
                n_eff = sum(weights)
            else
                n_eff = sum(weights)**2/sum(weights*weights)
            end if
        else
            n_eff = real(m, real64)
        end if

    end procedure kde_n_eff

    module procedure kde_rule_bandwidth

        real(real64) :: s, r, a, n_eff
        logical :: ok_s, ok_r

        h = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_stddev(x, s, weights=weights, weight_type=weight_type, ok=ok_s, threads=threads)
        call pf_iqr(x, r, weights=weights, weight_type=weight_type, ok=ok_r, threads=threads)
        ! One point, or a constant sample, has no scale: the estimate is left undefined rather
        ! than given a bandwidth of zero.
        if (.not. ok_s) return
        if (.not. (s > 0.0_real64)) return
        a = s
        ! The robust scale, unless the interquartile range is zero -- a sample more than half of
        ! whose values tie -- where it would say "no spread" about a population that has one.
        if (ok_r) then
            if (r > 0.0_real64) a = min(s, r/KDE_IQR_NORMAL)
        end if
        n_eff = kde_n_eff(size(x, kind=int64), freq, weights)
        select case (rule_code)
        case (KDE_RULE_SCOTT)
            h = KDE_SCOTT_C*a*n_eff**(-0.2_real64)
        case default
            h = KDE_SILVERMAN_C*a*n_eff**(-0.2_real64)
        end select

    end procedure kde_rule_bandwidth

    module procedure kde_isj_eval

        real(real64) :: f, log_n
        integer :: s

        ! The first stage reads the norm of the highest derivative at `t` itself; each later one at
        ! the time the norm above it makes optimal, formed through logarithms so that no quotient
        ! or power can overflow whatever the sample size and the norm.
        f = isj_norm(this, KDE_ISJ_STAGES, x)
        log_n = log(this%n_eff)
        do s = KDE_ISJ_STAGES - 1, 2, -1
            ! A norm this small puts the next time beyond every term, and every later norm at zero:
            ! the function's limit there is minus infinity, and `-huge` has its sign.
            if (.not. (f >= tiny(f))) then
                y = -huge(1.0_real64)
                return
            end if
            f = isj_norm(this, s, exp((2.0_real64/real(3 + 2*s, real64))*(KDE_ISJ_LOG_C(s) - log_n - log(f))))
        end do
        if (.not. (f >= tiny(f))) then
            y = -huge(1.0_real64)
            return
        end if
        y = x - exp(-0.4_real64*(KDE_ISJ_LOG_2SQRTPI + log_n + log(f)))

    end procedure kde_isj_eval

    module procedure kde_isj_bandwidth

        type(kde_isj_point) :: fp
        type(pf_bracket_expansion) :: grow
        type(pf_root_info) :: info
        real(real64), allocatable :: c(:), mass(:), y(:)
        real(real64) :: span, a, b, r, dx, below, above, total, n_eff, tlo, t, kk, pw, bk
        integer(int64) :: m, i1, i2
        integer :: nc, j, k, s

        h = ieee_value(1.0_real64, ieee_quiet_nan)
        found = .false.
        m = size(x, kind=int64)
        if (m < 2_int64) return
        ! The grid below is formed from these two points; a quarter of the largest number keeps every
        ! end, width and centre of it finite. Screened as two statements: the survivors are finite,
        ! so neither comparison meets a NaN.
        if (.not. (abs(x(1)) <= KDE_ISJ_LIMIT)) return
        if (.not. (abs(x(m)) <= KDE_ISJ_LIMIT)) return
        span = x(m) - x(1)
        if (.not. (span > 0.0_real64)) return

        ! ---- the grid: the range widened on each side, clipped to the support ----
        a = x(1) - KDE_ISJ_WIDEN*span
        b = x(m) + KDE_ISJ_WIDEN*span
        if (has_lower) then
            if (lo > a) a = lo
        end if
        if (has_upper) then
            if (hi < b) b = hi
        end if
        r = b - a
        nc = KDE_ISJ_CELLS
        if (kde_isj_cells_forced > 0) nc = kde_isj_cells_forced
        dx = r/real(nc, real64)
        allocate(c(nc))
        do j = 1, nc
            c(j) = a + (real(j, real64) - 0.5_real64)*dx
        end do
        ! A spread too narrow for the grid's centres to be told apart has no grid to be binned on.
        do j = 2, nc
            if (.not. (c(j) > c(j - 1))) return
        end do

        ! ---- the binned sample, normalised to one ----
        ! A point in the half cell beyond the first or the last centre lies between that centre and
        ! the grid's edge, where the cosine basis reflects: its whole weight is the centre's. Such
        ! points exist only where a bound clipped the grid, and they are the sorted sample's ends.
        below = 0.0_real64
        i1 = 1_int64
        do while (i1 <= m)
            if (.not. (x(i1) < c(1))) exit
            if (present(weights)) then
                below = below + weights(i1)
            else
                below = below + 1.0_real64
            end if
            i1 = i1 + 1_int64
        end do
        above = 0.0_real64
        i2 = m
        do while (i2 >= i1)
            if (.not. (x(i2) > c(nc))) exit
            if (present(weights)) then
                above = above + weights(i2)
            else
                above = above + 1.0_real64
            end if
            i2 = i2 - 1_int64
        end do
        allocate(mass(nc))
        if (present(weights)) then
            call pf_bin_linear(x(i1:i2), c, mass, weights=weights(i1:i2))
        else
            call pf_bin_linear(x(i1:i2), c, mass)
        end if
        mass(1) = mass(1) + below
        mass(nc) = mass(nc) + above
        total = sum(mass)
        if (.not. (total > 0.0_real64)) return
        if (.not. (total <= huge(total))) return
        do j = 1, nc
            mass(j) = mass(j)/total
        end do

        ! ---- the transform, and what the fixed point reads of it ----
        allocate(y(nc))
        call pf_dct(mass, y, context="pf_kde%fit, the ISJ rule")
        n_eff = kde_n_eff(m, freq, weights)
        if (.not. (n_eff > 0.0_real64)) return
        if (.not. (n_eff <= huge(n_eff))) return
        fp%n_eff = n_eff
        fp%kmax = nc - 1
        allocate(fp%kk(fp%kmax), fp%p(fp%kmax, 2:KDE_ISJ_STAGES))
        do k = 1, fp%kmax
            kk = real(k, real64)**2
            fp%kk(k) = kk
            bk = (0.5_real64*y(k + 1))**2
            pw = kk
            do s = 2, KDE_ISJ_STAGES
                pw = pw*kk
                fp%p(k, s) = pw*bk
            end do
        end do

        ! ---- the smallest root at or above one cell ----
        ! Below one cell the binned points themselves are what the fixed point sees: a sample of
        ! values rounded to a step the grid resolves has a root there, at a small fraction of a cell,
        ! far below the bandwidth its density calls for. So the search starts at one cell, and a
        ! function already non-negative there has no root the grid can stand behind.
        tlo = 1.0_real64/real(nc, real64)**2
        if (.not. (fp%eval(tlo) < 0.0_real64)) return
        ! Doubled from there, as the rule's reference search does, the expansion stops at the first
        ! sign change: the smallest root a probe steps over.
        grow%mode = PF_EXPAND_UP
        grow%factor = 2.0_real64
        grow%upper_limit = KDE_ISJ_T_MAX
        call pf_find_root(fp, tlo, 2.0_real64*tlo, t, expand=grow, info=info, context="pf_kde%fit, the ISJ rule")
        if (info%status /= PF_ROOT_OK) return
        h = sqrt(t)*r
        found = .true.

    end procedure kde_isj_bandwidth

    module procedure kde_widen_column

        character(len=:), allocatable :: kname
        integer :: k

        ! The family's widener takes a logical column as zeros and ones, a count it can average; a
        ! density has no use for it, so this refuses every kind but the four numeric ones, by name.
        k = x%kindof()
        select case (k)
        case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64)
            continue
        case default
            call parquet_kind_name(k, kname)
            call kde_abort(entry, "a column of kind " // kname // " holds no numbers to place kernels at; " // &
                "the column must be int32, int64, float32 or float64")
        end select
        call col_to_real64(x, entry, is_valid, wide, mask)

    end procedure kde_widen_column

    module procedure parquet_debug_kde_threads_used
        n = kde_team_used
    end procedure parquet_debug_kde_threads_used

    module procedure parquet_debug_set_kde_pilot_cells
        kde_pilot_cells_forced = max(0, n)
    end procedure parquet_debug_set_kde_pilot_cells

    module procedure parquet_debug_set_kde_isj_cells
        if (n <= 0) then
            kde_isj_cells_forced = 0
            return
        end if
        if (.not. pf_is_pow2(n)) call kde_abort("parquet_debug_set_kde_isj_cells", &
            "n must be a power of two from 16 to 1048576")
        if (n < 16 .or. n > 1048576) call kde_abort("parquet_debug_set_kde_isj_cells", &
            "n must be a power of two from 16 to 1048576")
        kde_isj_cells_forced = n
    end procedure parquet_debug_set_kde_isj_cells

    module procedure parquet_debug_kde_fit_nanos
        sort = kde_fit_ns(1)
        pilot = kde_fit_ns(2)
        lookup = kde_fit_ns(3)
    end procedure parquet_debug_kde_fit_nanos

    ! ==========================================================================================
    ! Private helpers
    ! ==========================================================================================

    !> The norm of the density's `s`-th derivative at the time `t`, as the ISJ rule estimates it from
    !! the binned transform: `2 pi**(2s) sum_k k**(2s) (y_k/2)**2 exp(-k**2 pi**2 t)`.
    !!
    !! The sum stops at the last `k` whose exponent is within `KDE_ISJ_EXP_LIMIT`: the terms after
    !! it are below the normal range, and below rounding against the terms before them, so what
    !! it leaves out is nothing the answer can hold. Each term's `exp(-k**2 c)` is the previous one's
    !! times `exp(-(2k - 1) c)`, and that factor the previous one's times `exp(-2c)`, over runs of
    !! `KDE_ISJ_RUN` terms, each run starting from `exp` itself.
    pure function isj_norm(this, s, t) result(f)
        class(kde_isj_point), intent(in) :: this !! the sample's binned transform
        integer, intent(in)              :: s    !! the derivative
        real(real64), intent(in)         :: t    !! the time, at least zero
        real(real64)                     :: f    !! the norm
        real(real64) :: ct, e, g, q
        integer :: k, j, kmax, kend

        ct = KDE_ISJ_PI2*t
        kmax = this%kmax
        ! The quotient is formed only where the last term is past the limit, so `ct` is not small.
        if (ct*this%kk(kmax) > KDE_ISJ_EXP_LIMIT) kmax = int(sqrt(KDE_ISJ_EXP_LIMIT/ct))
        q = exp(-2.0_real64*ct)
        f = 0.0_real64
        k = 1
        do while (k <= kmax)
            ! `e` is term `k`'s exponential and `g` the factor to the next: `(k + 1)**2 - k**2` is
            ! `2k + 1`. Every factor and every term stays in the normal range, since the last
            ! term's exponent is within the limit.
            e = exp(-this%kk(k)*ct)
            g = exp(-real(2*k + 1, real64)*ct)
            f = f + this%p(k, s)*e
            kend = min(kmax, k + KDE_ISJ_RUN - 1)
            do j = k + 1, kend
                e = e*g
                g = g*q
                f = f + this%p(j, s)*e
            end do
            k = kend + 1
        end do
        f = KDE_ISJ_NORM_C(s)*f

    end function isj_norm

end submodule parquet_kde_core ! GCOVR_EXCL_LINE
