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

    module procedure kde_label

        lab = text

    end procedure kde_label

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
        case ("lscv")
            code = KDE_RULE_LSCV
        case default
            code = 0
            call kde_abort(entry, 'rule must be "isj", "lscv", "silverman" or "scott"')
        end select

    end procedure kde_resolve_rule

    module procedure kde_resolve_boundary

        select case (kde_fold(token))
        case ("renormalise")
            code = KDE_BOUNDARY_RENORMALISE
        case ("reflect")
            code = KDE_BOUNDARY_REFLECT
        case ("linear")
            code = KDE_BOUNDARY_LINEAR
        case default
            code = 0
            call kde_abort(entry, 'boundary must be "renormalise", "reflect" or "linear"')
        end select

    end procedure kde_resolve_boundary

    module procedure kde_resolve_method

        select case (kde_fold(token))
        case ("exact")
            code = KDE_METHOD_EXACT
        case ("binned")
            code = KDE_METHOD_BINNED
        case default
            code = KDE_METHOD_EXACT
            call kde_abort(entry, 'method must be "exact" or "binned"')
        end select

    end procedure kde_resolve_method

    module procedure kde_resolve_setup

        kernel_code = KDE_BSPLINE
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
            ! The default correction. `"reflect"` is unbiased at the bound -- exactly as unbiased as
            ! the corrected `"renormalise"`, both doubling the one-sided sum there -- and it keeps a
            ! closed-form `%cdf`, which the corrected `"renormalise"` does not.
            boundary_code = KDE_BOUNDARY_REFLECT
        end if

    end procedure kde_resolve_setup

    module procedure kde_is_corrected

        res = bcode == KDE_BOUNDARY_RENORMALISE .or. bcode == KDE_BOUNDARY_LINEAR

    end procedure kde_is_corrected

    module procedure kde_positive_finite

        res = .false.
        if (ieee_is_nan(v)) return
        if (.not. ieee_is_finite(v)) return
        res = v > 0.0_real64

    end procedure kde_positive_finite

    module procedure kde_bandwidth_usable

        ! The NaN is screened first, as its own test: an ordered comparison raises IEEE_INVALID on
        ! one. Then NORMAL, not merely positive: a subnormal bandwidth puts the kernel's whole
        ! support inside the gap between two neighbouring numbers, so the density is zero wherever
        ! it is asked for while the distribution function still steps from 0 to 1. Then the reach,
        ! which is how far a kernel of it looks.
        res = .false.
        if (h /= h) return
        if (.not. (h >= tiny(h))) return
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

    module procedure kde_kernel_m1

        real(real64) :: t, v, w

        ! `u K(u)` is odd, so the moment is even in `z`: it is formed on the lower half, where every
        ! closed form below is a product or a short Horner sum that nothing cancels in, and read
        ! there for either sign. Each was checked against quadrature at 50 digits
        ! (`tools/generate_kde_vectors.py --self-test`).
        m = 0.0_real64
        t = -abs(z)
        if (.not. (t > -KDE_RADIUS(code))) return
        select case (code)
        case (KDE_GAUSSIAN)
            ! `(phi(R) - phi(t))/M`: the cut Gaussian's `-phi` between the cut and `t`.
            m = (KDE_GAUSS_PHI_CUT - pf_norm_pdf(t))/KDE_GAUSS_MASS
        case (KDE_EPANECHNIKOV)
            ! `-(3C/16) (1 - v**2)**2` on the unit-scale variable `v = t/C`.
            v = t/KDE_SCALE(code)
            m = -(0.1875_real64*KDE_SCALE(code))*((1.0_real64 - v)*(1.0_real64 + v))**2
        case (KDE_BSPLINE)
            v = t/KDE_SCALE(code)
            if (v <= -1.0_real64) then
                ! The outer piece, `C W**4 (2W - 5)/60` with `W = 2 + v`.
                w = 2.0_real64 + v
                m = KDE_SCALE(code)*w**4*(2.0_real64*w - 5.0_real64)/60.0_real64
            else
                ! The inner piece, `C (v**2/3 - v**4/4 - v**5/10 - 7/30)`, from the outer piece's
                ! `-C/20` at `v = -1`.
                m = KDE_SCALE(code)*(v*v*(1.0_real64/3.0_real64 - v*v*(0.25_real64 + 0.1_real64*v)) &
                    - 7.0_real64/30.0_real64)
            end if
        case (KDE_BOX)
            ! `C (v**2 - 1)/4`.
            v = t/KDE_SCALE(code)
            m = -(0.25_real64*KDE_SCALE(code))*(1.0_real64 - v)*(1.0_real64 + v)
        end select

    end procedure kde_kernel_m1

    module procedure kde_kernel_m2

        real(real64) :: t, v, w, lower

        ! `u**2 K(u)` is even, so the moment above zero is the variance less the moment below
        ! `-z`: formed on the lower half and mirrored, as `kde_kernel_cdf` is, which keeps it exactly
        ! symmetric and cancellation-free near either end.
        t = -abs(z)
        lower = 0.0_real64
        if (t > -KDE_RADIUS(code)) then
            select case (code)
            case (KDE_GAUSSIAN)
                ! `(Phi(t) - Phi(-R) - t phi(t) - R phi(R))/M`.
                lower = ((pf_norm_cdf(t) - KDE_GAUSS_TAIL) - (t*pf_norm_pdf(t) &
                    + KDE_GAUSS_CUT*KDE_GAUSS_PHI_CUT))/KDE_GAUSS_MASS
            case (KDE_EPANECHNIKOV)
                ! `(3C**2/4)(v**3/3 - v**5/5 + 2/15)` is `(1 + v)**2 (2 - 4v + 6v**2 - 3v**3)/4`.
                v = t/KDE_SCALE(code)
                lower = 0.25_real64*(1.0_real64 + v)**2*(2.0_real64 - v*(4.0_real64 - v*(6.0_real64 &
                    - 3.0_real64*v)))
            case (KDE_BSPLINE)
                v = t/KDE_SCALE(code)
                if (v <= -1.0_real64) then
                    ! The outer piece, `C**2 W**4 (5W**2 - 24W + 30)/180` with `W = 2 + v`, `C**2 = 3`.
                    w = 2.0_real64 + v
                    lower = w**4*(30.0_real64 - w*(24.0_real64 - 5.0_real64*w))/60.0_real64
                else
                    ! The inner piece, `C**2 (1/6 + 2v**3/9 - v**5/5 - v**6/12)`, from `11/60` at
                    ! `v = -1` to `1/2` at `v = 0`.
                    lower = 0.5_real64 + v**3*(2.0_real64/3.0_real64 - v*v*(0.6_real64 + 0.25_real64*v))
                end if
            case (KDE_BOX)
                ! `C**2 (v**3 + 1)/6` is `(1 + v)(1 - v + v**2)/2`, `C**2 = 3`.
                v = t/KDE_SCALE(code)
                lower = 0.5_real64*(1.0_real64 + v)*(1.0_real64 - v*(1.0_real64 - v))
            end select
        end if
        if (z > 0.0_real64) then
            m = KDE_VARIANCE(code) - lower
        else
            m = lower
        end if

    end procedure kde_kernel_m2

    module procedure kde_dct_filter

        real(real64), allocatable :: z(:), k(:), r(:)
        real(real64) :: s, d0
        integer :: nl, j

        nl = size(lam)
        allocate(z(nl + 1), k(nl + 1), r(nl))
        ! The filter's taps: `h(d) = dx K(d dx/h)/h`, the kernel's value at an offset of `d` whole
        ! cells, times the cell width. Every one beyond the kernel's reach is zero.
        s = dx/hj
        do j = 0, nl
            z(j + 1) = real(j, real64)*s
        end do
        call kde_kernel_pdf_many(code, z, k)
        ! The symmetric convolution of a unit at index 0 with the filter, the basis being
        ! half-sample even about index -1/2: `r(m) = h(m) + h(m+1)`.
        do j = 1, nl
            r(j) = (k(j) + k(j + 1))*s
        end do
        call pf_dct(r, lam, context="the binned method's kernel filter")
        do j = 1, nl
            lam(j) = lam(j)/(2.0_real64*cos(KDE_PI*real(j - 1, real64)/(2.0_real64*real(nl, real64))))
        end do
        ! One at zero frequency, whatever the sampled kernel's own mass is. A bandwidth far narrower
        ! than a cell has a mass far above one, and dividing by it leaves a filter that is a unit at
        ! one tap -- which is where the exact deposit puts such a point too.
        d0 = lam(1)
        do j = 1, nl
            lam(j) = lam(j)/d0
        end do

    end procedure kde_dct_filter

    module procedure kde_dst_filter

        real(real64), allocatable :: z(:), k(:), y(:), sc(:)
        real(real64) :: s, d0
        integer :: nl, j

        nl = size(lam)
        allocate(z(nl + 1), k(nl + 1), y(nl), sc(nl))
        s = dx/hj
        do j = 0, nl
            z(j + 1) = real(j, real64)*s
        end do
        call kde_kernel_pdf_many(code, z, k)
        ! The ODD filter's taps, `g(d) = dx (d dx/h) K(d dx/h)/h`, odd in `d` and zero at `d = 0`;
        ! and the symmetric convolution of a unit at index 0 with it, `g(m) + g(m+1)`.
        do j = 1, nl
            y(j) = s*(z(j)*k(j) + z(j + 1)*k(j + 1))
        end do
        ! The EVEN filter's mass, which both convolutions are divided by: a scale they must share,
        ! or their ratio is not the correction the exact deposit applies.
        d0 = k(1)
        do j = 2, nl + 1
            d0 = d0 + 2.0_real64*k(j)
        end do
        d0 = d0*s
        call pf_dst(y, sc, context="the binned method's odd kernel filter")
        ! Coefficient `j` of `pf_dct` carries frequency `j - 1`, and `sc(j)` of `pf_dst` carries
        ! frequency `j`: the multiplier at cosine position `j` is the sine coefficient one below.
        lam(1) = 0.0_real64
        do j = 2, nl
            lam(j) = sc(j - 1)/(2.0_real64*cos(KDE_PI*real(j - 1, real64)/(2.0_real64*real(nl, real64))))
            lam(j) = lam(j)/d0
        end do

    end procedure kde_dst_filter

    module procedure kde_corr_factor

        real(real64) :: rad, lo_z, hi_z, w, c, g, a0, a1, a2, m0, m1, m2

        ! The part of the kernel's support inside the bounds, in bandwidths: `[max(-R, -q),
        ! min(R, p)]`. A bound a reach or more away leaves that end the kernel's own.
        rad = KDE_RADIUS(code)
        hi_z = rad
        lo_z = -rad
        if (has_lower) then
            if ((t - lo)*r < rad) hi_z = (t - lo)*r
        end if
        if (has_upper) then
            if ((hi - t)*r < rad) lo_z = -((hi - t)*r)
        end if
        cf%plain = .true.
        cf%a = 0.0_real64
        cf%b = 0.0_real64
        cf%c = 0.0_real64
        cf%dinv = 0.0_real64
        cf%a0 = 1.0_real64
        if (hi_z >= rad .and. lo_z <= -rad) return
        cf%plain = .false.
        w = hi_z - lo_z
        if (w >= KDE_CORR_NARROW) then
            ! The moments as differences of the closed forms; a one-sided interval's lower end is
            ! minus the radius, where every closed form is exactly zero.
            a0 = kde_kernel_cdf(code, hi_z) - kde_kernel_cdf(code, lo_z)
            cf%a0 = a0
            if (bcode /= KDE_BOUNDARY_LINEAR) then
                ! The constant member: the kernel over the mass it keeps inside the bounds. It
                ! needs no moment beyond the zeroth, and it is never negative.
                cf%a = 1.0_real64
                cf%dinv = 1.0_real64/a0
                return
            end if
            a1 = kde_kernel_m1(code, hi_z) - kde_kernel_m1(code, lo_z)
            a2 = kde_kernel_m2(code, hi_z) - kde_kernel_m2(code, lo_z)
            cf%a = a2
            cf%b = a1
            cf%dinv = 1.0_real64/(a0*a2 - a1*a1)
            return
        end if
        ! A narrow two-sided interval: the moments about its midpoint `c`, in the scaled offset
        ! `s = (u - c)/w`, `m_l` the integral of `s**l K(c + w s)` over `[-1/2, 1/2]`, so that each is
        ! of the kernel's own size whatever the width. With `g = c/w` the kernel is
        ! `((m_2 + g m_1) - (m_1 + g m_0)(u - c)/w) K(u) / (w (m_0 m_2 - m_1**2))`, the linear boundary
        ! kernel written about `c`: its zeroth moment is one and its first moment about zero, not
        ! about `c`, is zero. `m_1**2` is two powers of the width below `m_0 m_2`, so the determinant
        ! loses nothing.
        c = 0.5_real64*(lo_z + hi_z)
        call centred_moments(code, lo_z, hi_z, c, w, m0, m1, m2)
        ! `a_0` is the width times the zeroth centred moment, never the difference of two
        ! distribution functions: here they are of order one and differ by the width.
        cf%a0 = w*m0
        if (bcode /= KDE_BOUNDARY_LINEAR) then
            cf%a = 1.0_real64
            cf%dinv = 1.0_real64/cf%a0
            return
        end if
        g = c/w
        cf%a = m2 + g*m1
        cf%b = (m1 + g*m0)/w
        cf%c = c
        cf%dinv = 1.0_real64/(w*(m0*m2 - m1*m1))

    end procedure kde_corr_factor

    module procedure kde_corr_value

        v = k
        if (.not. cf%plain) v = k*((cf%a - cf%b*(u - cf%c))*cf%dinv)

    end procedure kde_corr_value

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

        real(real64) :: norm, log_n
        integer :: s

        ! The first stage reads the norm of the highest derivative at `t` itself; each later one at
        ! the time the norm above it makes optimal, formed through logarithms so that no quotient
        ! or power can overflow whatever the sample size and the norm.
        norm = isj_norm(self, KDE_ISJ_STAGES, x)
        log_n = log(self%n_eff)
        do s = KDE_ISJ_STAGES - 1, 2, -1
            ! A norm this small puts the next time beyond every term, and every later norm at zero:
            ! the function's limit there is minus infinity, and `-huge` has its sign.
            if (.not. (norm >= tiny(norm))) then
                f = -huge(1.0_real64)
                return
            end if
            norm = isj_norm(self, s, &
                            exp((2.0_real64/real(3 + 2*s, real64))*(KDE_ISJ_LOG_C(s) - log_n - log(norm))))
        end do
        if (.not. (norm >= tiny(norm))) then
            f = -huge(1.0_real64)
            return
        end if
        f = x - exp(-0.4_real64*(KDE_ISJ_LOG_2SQRTPI + log_n + log(norm)))

    end procedure kde_isj_eval

    module procedure kde_isj_bandwidth

        type(kde_isj_point) :: fp
        type(pf_bracket_expansion) :: grow
        type(pf_root_info) :: info
        real(real64), allocatable :: c(:), mass(:), y(:)
        real(real64) :: span, a, b, r, dx, below, above, total, n_eff, tlo, t, kk, pw, bk, gap, g
        integer(int64) :: m, i, i1, i2
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

        ! ---- no finer than the sample's own resolution ----
        ! A kernel narrower than the closest pair of distinct observations is resolving structure
        ! the sample cannot express. Values rounded to a step this grid resolves put a root far
        ! below that step, and the estimate taken there is one spike per distinct value rather than
        ! a density. Refused, which is the same answer as no root at all, so the caller's fallback
        ! carries it: Silverman's rule where the rule was not named, an undefined estimate where it
        ! was. The two populations are two orders of magnitude apart -- a flagged bandwidth is a few
        ! hundredths of the gap, a sound one tens of thousands of times it -- so the threshold of
        ! one gap sits in an empty middle and needs no constant to tune.
        !
        ! The MINIMUM gap, deliberately, not a robust statistic: one near-duplicate pair in
        ! otherwise continuous data makes it tiny and disarms the guard, which fails safe, where a
        ! median or low-quantile gap could fire on a legitimately spiky density. Do not replace it.
        gap = huge(1.0_real64)
        do i = 2_int64, m
            g = x(i) - x(i - 1_int64)
            if (g > 0.0_real64 .and. g < gap) gap = g
        end do
        if (.not. (h >= gap)) then
            h = ieee_value(1.0_real64, ieee_quiet_nan)
            return
        end if
        found = .true.

    end procedure kde_isj_bandwidth

    module procedure kde_lscv_bandwidth

        real(real64), allocatable :: y(:), v(:), hj(:), bw(:), bhat(:)
        real(real64) :: a, b, c, d, fc, fd, dx, sv2
        integer(int64) :: m, i
        integer :: it, nl
        logical :: ok

        h = ieee_value(1.0_real64, ieee_quiet_nan)
        found = .false.
        ! Leaving one point out of a sample of one leaves nothing to estimate from.
        if (size(x, kind=int64) < 3_int64) return
        if (.not. kde_positive_finite(h0)) return

        call lscv_subsample(x, weights, y, v, m)
        if (m < 3_int64) return
        allocate(hj(m))
        ! The fixed arm's two terms are self-convolutions of the weighted sample, so one binning
        ! and one transform of it serve every candidate: each then costs a filter and an inverse
        ! rather than a pass over every pair. The adaptive arm gets a zero-length `bhat` and the
        ! exact double sum, its pair widths depending on the pair.
        nl = 0
        if (.not. adaptive .and. kde_lscv_grid_on) call lscv_binned_setup(y, v, m, h0, bw, bhat, dx, nl)
        if (nl == 0) then
            allocate(bw(0), bhat(0))
            dx = 0.0_real64
        end if
        sv2 = 0.0_real64
        do i = 1_int64, m
            sv2 = sv2 + v(i)*v(i)
        end do

        ! ---- golden section over the bracket ----
        ! The criterion has no derivative to follow and can have a shallow second minimum on a
        ! comb, so the bracket is fixed rather than grown: widening it on such a sample moves the
        ! answer between two minima from run to run, which is worse than a slightly clipped one.
        a = h0/KDE_LSCV_BRACKET
        b = h0*KDE_LSCV_BRACKET
        if (.not. (b > a)) return
        c = b - KDE_GOLDEN*(b - a)
        d = a + KDE_GOLDEN*(b - a)
        fc = lscv_at(y, v, hj, m, c, adaptive, alpha, kernel_code, has_lower, lo, has_upper, hi, &
            boundary_code, w_total, bw, bhat, dx, sv2, ok)
        if (.not. ok) return
        fd = lscv_at(y, v, hj, m, d, adaptive, alpha, kernel_code, has_lower, lo, has_upper, hi, &
            boundary_code, w_total, bw, bhat, dx, sv2, ok)
        if (.not. ok) return
        do it = 1, KDE_LSCV_STEPS
            if (fc < fd) then
                b = d
                d = c
                fd = fc
                c = b - KDE_GOLDEN*(b - a)
                fc = lscv_at(y, v, hj, m, c, adaptive, alpha, kernel_code, has_lower, lo, has_upper, &
                    hi, boundary_code, w_total, bw, bhat, dx, sv2, ok)
            else
                a = c
                c = d
                fc = fd
                d = a + KDE_GOLDEN*(b - a)
                fd = lscv_at(y, v, hj, m, d, adaptive, alpha, kernel_code, has_lower, lo, has_upper, &
                    hi, boundary_code, w_total, bw, bhat, dx, sv2, ok)
            end if
            if (.not. ok) return
            if (b - a <= KDE_LSCV_TOL*(a + b)) exit
        end do
        h = 0.5_real64*(a + b)
        if (.not. kde_positive_finite(h)) then
            ! Not reachable from a bandwidth the criterion can score: `a` and `b` are positive and
            ! finite on entry (`b > a` was tested), the section only shrinks them, and `lscv_at`
            ! stops answering far below where their sum could overflow -- the per-pair width
            ! `sqrt(h_i**2 + h_j**2)` overflows first, as the window's own note says. Measured on
            ! a sample scaled towards the largest number: the rule refuses at a Silverman
            ! bandwidth near `1.9e307`, while `0.5*(a + b)` needs one above `3.2e307`.
            ! GCOVR_EXCL_START
            h = ieee_value(1.0_real64, ieee_quiet_nan)
            return
            ! GCOVR_EXCL_STOP
        end if
        found = .true.

    end procedure kde_lscv_bandwidth

    !> The points the criterion is evaluated over: the population itself where it is small enough,
    !! and otherwise `KDE_LSCV_MAX` of them drawn without replacement at `KDE_LSCV_SEED`.
    !!
    !! A partial Fisher-Yates walk over an index array, which draws without replacement in one pass
    !! and touches only the prefix it keeps. The draw reads `parquet_random`'s coordinate-addressed
    !! generator, so one sample gives one subsample however often the rule is asked for, and a
    !! caller's own draws at any `(seed, stream)` are untouched.
    subroutine lscv_subsample(x, weights, y, v, m)
        real(real64), intent(in)               :: x(:)       !! the population, ascending
        real(real64), intent(in), optional     :: weights(:) !! its weights, when weighted
        real(real64), allocatable, intent(out) :: y(:)       !! the points kept
        real(real64), allocatable, intent(out) :: v(:)       !! their weights; all one when unweighted
        integer(int64), intent(out)            :: m          !! how many were kept

        integer(int64), allocatable :: idx(:)
        integer(int64) :: n, i, j, t, key
        real(real64) :: u

        n = size(x, kind=int64)
        m = min(n, KDE_LSCV_MAX)
        allocate(y(m), v(m))
        if (m == n) then
            y = x
            if (present(weights)) then
                v = weights
            else
                v = 1.0_real64
            end if
            return
        end if
        allocate(idx(n))
        do i = 1_int64, n
            idx(i) = i
        end do
        key = pf_random_key(KDE_LSCV_SEED, 1_int64)
        do i = 1_int64, m
            u = pf_random_at(KDE_LSCV_SEED, key, i)
            j = i + int(u*real(n - i + 1_int64, real64), int64)
            if (j > n) j = n
            if (j < i) j = i
            t = idx(i)
            idx(i) = idx(j)
            idx(j) = t
            y(i) = x(idx(i))
            if (present(weights)) then
                v(i) = weights(idx(i))
            else
                v(i) = 1.0_real64
            end if
        end do
        ! The criterion reads the points pairwise, so their order does not matter; the pilot below
        ! does want them ascending, and sorting `m` of them is cheap beside the double sum.
        call lscv_sort_pairs(y, v, m)

    end subroutine lscv_subsample

    !> Orders the drawn points ascending, carrying their weights, so that the pilot the adaptive arm
    !! builds over them sees the ascending sample it is documented to take.
    subroutine lscv_sort_pairs(y, v, m)
        real(real64), intent(inout) :: y(:)  !! the points, ordered in place
        real(real64), intent(inout) :: v(:)  !! their weights, carried along
        integer(int64), intent(in)  :: m     !! how many

        integer(int64), allocatable :: perm(:)
        real(real64), allocatable :: ty(:), tv(:)
        integer(int64) :: i

        call pf_argsort(y(1:m), perm)
        allocate(ty(m), tv(m))
        do i = 1_int64, m
            ty(i) = y(perm(i))
            tv(i) = v(perm(i))
        end do
        y(1:m) = ty
        v(1:m) = tv

    end subroutine lscv_sort_pairs

    !> The LSCV criterion at one bandwidth, over the drawn points.
    !!
    !! `integral of fhat**2 - (2/n) sum_i fhat_{-i}(x_i)`, both terms exact for the Gaussian:
    !! the integral of a product of two normal densities is a normal density of the combined
    !! width at their separation, and the leave-one-out sum simply omits `i == j`.
    !!
    !! Under `adaptive` every point carries its own bandwidth, read from a pilot built over these
    !! points AT THIS `h` -- which is what makes the criterion score the adaptive estimator rather
    !! than the fixed one, and what no plug-in rule can do.
    function lscv_at(y, v, hj, m, h, adaptive, alpha, kernel_code, has_lower, lo, has_upper, hi, &
            boundary_code, w_total, bw, bhat, dx, sv2, ok) result(crit)
        real(real64), intent(in)    :: y(:)     !! the drawn points, ascending
        real(real64), intent(in)    :: v(:)     !! their weights
        real(real64), intent(inout) :: hj(:)    !! scratch: each point's bandwidth at this `h`
        integer(int64), intent(in)  :: m        !! how many points
        real(real64), intent(in)    :: h        !! the candidate bandwidth
        logical, intent(in)         :: adaptive !! score the adaptive estimate
        real(real64), intent(in)    :: alpha    !! the adaptive rule's sensitivity
        integer, intent(in)         :: kernel_code   !! the kernel the pilot is built with
        logical, intent(in)         :: has_lower     !! a lower bound was given
        real(real64), intent(in)    :: lo            !! the lower bound
        logical, intent(in)         :: has_upper     !! an upper bound was given
        real(real64), intent(in)    :: hi            !! the upper bound
        integer, intent(in)         :: boundary_code !! the boundary correction
        real(real64), intent(in)    :: w_total       !! the population's total weight
        real(real64), intent(in)    :: bw(:)         !! the binned weights, or zero-length for the
                                                     !! exact double sum
        real(real64), intent(in)    :: bhat(:)       !! their cosine transform, the same length
        real(real64), intent(in)    :: dx            !! the binning's cell width
        real(real64), intent(in)    :: sv2           !! `sum(v**2)`, the exact diagonal's weight
        logical, intent(out)        :: ok            !! the criterion could be formed
        real(real64)                :: crit          !! the criterion's value

        !> The widest separation a pair can be seen across, in units of the widest bandwidth: the
        !! integral term's own width is `sqrt(h_i**2 + h_j**2) <= sqrt(2) h_max` and the
        !! leave-one-out term's is `h_j <= h_max`, so the wider of the two bounds both.
        real(real64), parameter :: WIN_FACTOR = KDE_NORM_CUT*sqrt(2.0_real64)

        type(pf_kde_grid) :: pilot
        type(kde_adapt) :: rule
        real(real64) :: sw, s_int, s_loo, dz, si, wij, one(1), hmax, win
        integer(int64) :: i, j, jlo, jhi
        logical :: built

        ok = .false.
        crit = 0.0_real64
        if (adaptive) then
            one(1) = 1.0_real64
            call kde_build_pilot(pilot, y(1:m), v(1:m), .true., h, kernel_code, has_lower, lo, &
                has_upper, hi, boundary_code, w_total, threads_serial(), built)
            if (.not. built) return
            ! The criterion scores the estimator the fit will build, so it takes the same default
            ! spread cap; a caller's own `spread_max` is not threaded in, the rule being asked for
            ! the bandwidth of an estimate whose other arguments it does not see either.
            call kde_adapt_set(rule, pilot, alpha, .false., 0.0_real64, KDE_SPREAD_MAX)
            if (rule%unreadable) return
            call kde_adapt_bandwidths(rule, h, y(1:m), hj(1:m))
            do i = 1_int64, m
                if (.not. kde_positive_finite(hj(i))) return
            end do
        else
            do i = 1_int64, m
                hj(i) = h
            end do
        end if

        sw = 0.0_real64
        do i = 1_int64, m
            sw = sw + v(i)
        end do
        if (.not. (sw > 0.0_real64)) return

        ! ---- the fixed arm: two convolutions instead of a pass over every pair ----
        if (size(bhat) > 0) then
            crit = lscv_binned_at(bw, bhat, dx, h, sw, sv2)
            ok = crit == crit .and. abs(crit) <= huge(1.0_real64)
            return
        end if

        ! ---- the window each point's partners lie in ----
        ! `kde_norm_at` is EXACTLY zero beyond `KDE_NORM_CUT` standard deviations, and adding
        ! `w*0.0` to a finite sum changes no bit of it, so a pair outside the window contributes
        ! exactly nothing: skipping it is bit-for-bit, not merely equivalent, and no summation
        ! order moves -- each accumulator still sees its terms `i`-major and `j` ascending. `y` is
        ! ascending, so each `i`'s contributing partners form one contiguous run that two monotone
        ! pointers find with no search, and `jlo <= i <= jhi` always, the point being its own
        ! partner. The per-pair cut stays inside the window, which is what keeps the adaptive arm
        ! exact where the bandwidths differ; the window only has to be conservative.
        !
        ! `WIN_FACTOR*hmax` overflowing would need a bandwidth above 2e307, an order of magnitude
        ! past where `hj(i)*hj(i)` in the integral term below overflows first: the window adds no
        ! failure mode of its own.
        hmax = hj(1)
        do i = 2_int64, m
            if (hj(i) > hmax) hmax = hj(i)
        end do
        win = WIN_FACTOR*hmax

        s_int = 0.0_real64
        s_loo = 0.0_real64
        jlo = 1_int64
        jhi = 0_int64
        do i = 1_int64, m
            do while (jlo < m)
                if (.not. (y(i) - y(jlo) > win)) exit
                jlo = jlo + 1_int64
            end do
            do while (jhi < m)
                if (y(jhi + 1_int64) - y(i) > win) exit
                jhi = jhi + 1_int64
            end do
            do j = jlo, jhi
                wij = v(i)*v(j)
                dz = y(i) - y(j)
                ! The integral term: the two kernels' product integrates to one normal density of
                ! the combined width, evaluated at their separation.
                si = sqrt(hj(i)*hj(i) + hj(j)*hj(j))
                s_int = s_int + wij*kde_norm_at(dz, si)
                ! The leave-one-out term omits the point's own kernel, which is the whole point:
                ! kept, every bandwidth below the data's resolution would win.
                if (i /= j) s_loo = s_loo + wij*kde_norm_at(dz, hj(j))
            end do
        end do
        crit = s_int/(sw*sw) - 2.0_real64*s_loo/(sw*(sw - 1.0_real64))
        ok = crit == crit .and. abs(crit) <= huge(1.0_real64)

    end function lscv_at

    !> The binning the fixed arm's criterion reads, and its cosine transform: formed ONCE for the
    !! whole golden section, since neither depends on the candidate bandwidth.
    !!
    !! `nl` comes back zero where no usable geometry exists -- a range too many narrow bandwidths
    !! wide for `KDE_LSCV_GRID_MAX`, or a degenerate `h0` -- and every candidate then takes the
    !! exact double sum instead.
    !!
    !! The geometry answers to the two ends of the bracket at once. The cells resolve the NARROWEST
    !! candidate, `h0/KDE_LSCV_BRACKET`, at `KDE_CURVE_BINNED_PER_H` of them to a bandwidth, which
    !! is where the binned estimate stops being distinguishable from the exact sum; the range
    !! reaches the WIDEST candidate's own cut beyond both extreme points, `KDE_NORM_CUT*sqrt(2)`
    !! times `h0*KDE_LSCV_BRACKET`, so that no pair's kernel is truncated by the array's end and
    !! nothing wraps onto the far side of the cosine basis.
    subroutine lscv_binned_setup(y, v, m, h0, bw, bhat, dx, nl)
        real(real64), intent(in)               :: y(:)    !! the drawn points, ascending
        real(real64), intent(in)               :: v(:)    !! their weights
        integer(int64), intent(in)             :: m       !! how many points
        real(real64), intent(in)               :: h0      !! the bracket's centre
        real(real64), allocatable, intent(out) :: bw(:)   !! the binned weights
        real(real64), allocatable, intent(out) :: bhat(:) !! their cosine transform
        real(real64), intent(out)              :: dx      !! the cell width
        integer, intent(out)                   :: nl      !! the transform's length; 0 where none

        real(real64) :: reach, span, x0, t, frac, cells
        integer(int64) :: i
        integer :: k

        nl = 0
        dx = 0.0_real64
        if (.not. kde_positive_finite(h0)) return
        dx = h0/(KDE_LSCV_BRACKET*KDE_CURVE_BINNED_PER_H)
        if (.not. (dx > 0.0_real64)) return
        reach = KDE_NORM_CUT*sqrt(2.0_real64)*KDE_LSCV_BRACKET*h0
        span = (y(m) - y(1)) + 2.0_real64*reach
        if (.not. (span > 0.0_real64 .and. span <= huge(1.0_real64))) return
        ! Formed in real64 and compared before any conversion: the quotient can be far past the
        ! largest integer, and `int()` of that is undefined rather than large.
        cells = span/dx + 2.0_real64
        if (.not. (cells <= real(KDE_LSCV_GRID_MAX, real64))) return
        nl = pf_next_pow2(max(4, int(ceiling(cells))))
        if (nl > KDE_LSCV_GRID_MAX) then
            ! Not reachable: `cells` was held at or below `KDE_LSCV_GRID_MAX` on the line above,
            ! that bound is itself a power of two, and `pf_next_pow2` answers the smallest power of
            ! two AT OR ABOVE its argument -- so `nl` cannot pass it. Kept as the guard on the
            ! length the transform is allocated at, beside the one on the quotient that produced it.
            ! GCOVR_EXCL_START
            nl = 0
            return
            ! GCOVR_EXCL_STOP
        end if
        allocate(bw(nl), bhat(nl))
        bw = 0.0_real64
        x0 = y(1) - reach
        ! Linear binning: each point's weight is split between the two centres around it, in
        ! proportion to how near it lies to each. `reach` keeps every point clear of both ends, so
        ! no weight is lost off the array.
        do i = 1_int64, m
            t = (y(i) - x0)/dx - 0.5_real64
            k = int(floor(t)) + 1
            frac = t - real(k - 1, real64)
            if (k >= 1 .and. k <= nl) bw(k) = bw(k) + v(i)*(1.0_real64 - frac)
            if (k + 1 >= 1 .and. k + 1 <= nl) bw(k + 1) = bw(k + 1) + v(i)*frac
        end do
        call pf_dct(bw, bhat, context="the LSCV criterion's binning")

    end subroutine lscv_binned_setup

    !> The LSCV criterion at one bandwidth, read off the binning rather than off every pair.
    !!
    !! Both terms are self-convolutions of the weighted sample: the integral term at the combined
    !! width `h*sqrt(2)`, since the product of two normal densities integrates to one normal
    !! density of that width at their separation, and the leave-one-out term at `h` with the
    !! diagonal removed. A self-convolution read at the points themselves is, once the sample is
    !! binned, the inner product of the binning with its own smoothing -- one filter and one
    !! inverse transform each, whatever the sample's size.
    !!
    !! **The diagonal is subtracted EXACTLY, not from the binning.** `sum_i w_i**2 phi_h(0)` is a
    !! closed form over the true weights, and it is the one part of the criterion that decides
    !! whether every bandwidth below the data's resolution wins; leaving it to the binning would
    !! put the binning's own error into the term the minimisation is most sensitive to.
    !!
    !! `KDE_GAUSS_MASS` puts the answer on the exact form's scale. The filter is normalised to one
    !! at zero frequency, so the convolution carries the kernel RENORMALISED over its cut, while
    !! `kde_norm_at` carries it unrenormalised; the factor between them is that mass. It is the
    !! same at both widths, so it could not move the minimiser -- it is applied so that this
    !! function and `lscv_at`'s pair loop can be compared as numbers.
    function lscv_binned_at(bw, bhat, dx, h, sw, sv2) result(crit)
        real(real64), intent(in) :: bw(:)   !! the binned weights
        real(real64), intent(in) :: bhat(:) !! their cosine transform
        real(real64), intent(in) :: dx      !! the cell width
        real(real64), intent(in) :: h       !! the candidate bandwidth
        real(real64), intent(in) :: sw      !! the drawn points' total weight
        real(real64), intent(in) :: sv2     !! `sum(v**2)`
        real(real64)             :: crit    !! the criterion's value

        real(real64) :: s_int, s_all, s_loo

        s_int = KDE_GAUSS_MASS*lscv_self_convolve(bw, bhat, dx, h*sqrt(2.0_real64))
        s_all = KDE_GAUSS_MASS*lscv_self_convolve(bw, bhat, dx, h)
        s_loo = s_all - kde_norm_at(0.0_real64, h)*sv2
        crit = s_int/(sw*sw) - 2.0_real64*s_loo/(sw*(sw - 1.0_real64))

    end function lscv_binned_at

    !> `sum_i sum_j w_i w_j K_s(y_i - y_j)` over the binned sample: the inner product of the
    !! binning with the binning convolved against the kernel at width `s`, divided by the cell
    !! width because the filter conserves WEIGHT rather than answering a density.
    function lscv_self_convolve(bw, bhat, dx, s) result(total)
        real(real64), intent(in) :: bw(:)   !! the binned weights
        real(real64), intent(in) :: bhat(:) !! their cosine transform
        real(real64), intent(in) :: dx      !! the cell width
        real(real64), intent(in) :: s       !! the kernel's standard deviation
        real(real64)             :: total   !! the double sum

        real(real64), allocatable :: lam(:), wk(:), sm(:)
        integer :: nl, k

        nl = size(bw)
        allocate(lam(nl), wk(nl), sm(nl))
        call kde_dct_filter(KDE_GAUSSIAN, s, dx, lam)
        do k = 1, nl
            wk(k) = bhat(k)*lam(k)
        end do
        call pf_idct(wk, sm, context="the LSCV criterion's convolution")
        total = 0.0_real64
        do k = 1, nl
            total = total + bw(k)*sm(k)
        end do
        total = total/dx

    end function lscv_self_convolve

    !> The normal density of standard deviation `s` at `z`, which both of the criterion's terms are
    !! built from. Zero where `s` is not usable, so a degenerate bandwidth contributes nothing
    !! rather than a NaN.
    pure function kde_norm_at(z, s) result(f)
        real(real64), intent(in) :: z !! the separation
        real(real64), intent(in) :: s !! the standard deviation
        real(real64)             :: f !! the density

        real(real64) :: t

        f = 0.0_real64
        if (.not. (s > 0.0_real64)) return
        t = z/s
        if (abs(t) > KDE_NORM_CUT) return
        f = exp(-0.5_real64*t*t)/(s*KDE_ROOT_TWO_PI)

    end function kde_norm_at

    !> One thread: the criterion is minimised inside a rule that may itself be running under a
    !! caller's team, and a pilot built per candidate bandwidth is far too small to pay for one.
    pure function threads_serial() result(n)
        integer :: n !! always one

        n = 1

    end function threads_serial

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

    module procedure parquet_debug_set_kde_binned_classes
        kde_binned_classes_forced = max(0, min(n, KDE_BINNED_CLASSES_MAX))
    end procedure parquet_debug_set_kde_binned_classes

    module procedure parquet_debug_set_kde_sample_tries
        ! A negative `n` restores the default; every other value, zero included, is the cap.
        kde_sample_tries_forced = n
        if (n < 0) kde_sample_tries_forced = -1
    end procedure parquet_debug_set_kde_sample_tries

    module procedure parquet_debug_kde_fit_nanos
        sort = kde_fit_ns(1)
        pilot = kde_fit_ns(2)
        lookup = kde_fit_ns(3)
    end procedure parquet_debug_kde_fit_nanos

    module procedure parquet_debug_kde_scan_counts
        steps = kde_scan_ns(1)
        exact = kde_scan_ns(2)
    end procedure parquet_debug_kde_scan_counts

    module procedure parquet_debug_set_kde_scan_grid
        kde_scan_grid_on = on
    end procedure parquet_debug_set_kde_scan_grid

    module procedure parquet_debug_set_kde_lscv_grid
        kde_lscv_grid_on = on
    end procedure parquet_debug_set_kde_lscv_grid

    module procedure parquet_debug_kde_lscv_at

        real(real64), allocatable :: y(:), v(:), hj(:), bw(:), bhat(:)
        real(real64) :: dx, sv2, w_total
        integer(int64) :: m, i
        integer :: nl

        crit = 0.0_real64
        ok = .false.
        m = size(x, kind=int64)
        ! Leaving one point out of a sample of one leaves nothing to estimate from, and the
        ! criterion's `sw - 1` denominator wants a second point besides. The same floor the rule
        ! itself takes.
        if (m < 3_int64) return
        if (.not. kde_positive_finite(h)) return
        if (present(weights)) then
            if (size(weights, kind=int64) /= m) return
        end if
        allocate(y(m), v(m), hj(m))
        y = x
        if (present(weights)) then
            v = weights
        else
            v = 1.0_real64
        end if
        ! `lscv_at` reads its points ascending: the window it sums inside walks two monotone
        ! pointers over them. The rule reaches it through `lscv_subsample`, which sorts; this hook
        ! draws nothing, so it sorts here instead.
        call lscv_sort_pairs(y, v, m)
        w_total = 0.0_real64
        sv2 = 0.0_real64
        do i = 1_int64, m
            w_total = w_total + v(i)
            sv2 = sv2 + v(i)*v(i)
        end do
        ! Sized for a bracket centred on the one bandwidth asked about, which is what the rule
        ! would do at `h0 = h`; `nl == 0` where no usable geometry exists, and the pair sum carries
        ! it, exactly as it does inside the rule.
        nl = 0
        if (kde_lscv_grid_on) call lscv_binned_setup(y, v, m, h, bw, bhat, dx, nl)
        if (nl == 0) then
            allocate(bw(0), bhat(0))
            dx = 0.0_real64
        end if
        ! The FIXED arm only: the adaptive one's pair widths come from a pilot built at the
        ! candidate bandwidth, which is a function of the rule's own machinery rather than a closed
        ! form over the arguments, and it is the closed form that an oracle can certify. The
        ! kernel, the bounds and the boundary reach `lscv_at` only through that pilot, so their
        ! values here are inert.
        crit = lscv_at(y, v, hj, m, h, .false., 0.0_real64, KDE_GAUSSIAN, .false., 0.0_real64, &
            .false., 0.0_real64, KDE_BOUNDARY_NONE, w_total, bw, bhat, dx, sv2, ok)
        if (.not. ok) crit = 0.0_real64

    end procedure parquet_debug_kde_lscv_at

    module procedure kde_term_integral

        real(real64) :: reach, s2, t2, cj, dj, ps, pe

        v = 0.0_real64
        if (.not. (t > s)) return
        reach = KDE_RADIUS(fn%code)*fn%hj
        ! Only the part its kernel reaches.
        s2 = max(s, fn%xj - reach)
        t2 = min(t, fn%xj + reach)
        if (.not. (t2 > s2)) return
        ! The correction edges: below `c_j` the lower bound corrects the term, above `d_j` the upper
        ! one. Where they cross -- a kernel wider than half the support -- the whole piece is
        ! corrected on both sides, and both edges enter as breakpoints.
        cj = s2
        dj = t2
        if (fn%has_lower) cj = fn%lo + reach
        if (fn%has_upper) dj = fn%hi - reach
        if (cj > dj) then
            v = term_piece(fn, s2, t2, sigma, has_sigma)
            return
        end if
        if (s2 < cj) v = v + term_piece(fn, s2, min(t2, cj), sigma, has_sigma)
        ps = max(s2, cj)
        pe = min(t2, dj)
        if (pe > ps) v = v + (kde_kernel_cdf(fn%code, (pe - fn%xj)*fn%r) &
            - kde_kernel_cdf(fn%code, (ps - fn%xj)*fn%r))
        if (t2 > dj) v = v + term_piece(fn, max(s2, dj), t2, sigma, has_sigma)

    end procedure kde_term_integral

    ! ==========================================================================================
    ! Private helpers
    ! ==========================================================================================

    !> The integral of the term `fn` over `[alpha, beta]`, a piece on which it is corrected: the
    !! term's own breakpoints cut it into analytic pieces, each piece is cut again into parts no
    !! wider than `KDE_GL_SPAN` bandwidths, and `KDE_GL16_X`'s rule is applied to each part.
    !!
    !! **A fixed rule, not an adaptive one.** The breakpoints are the kernel's knots, its moments'
    !! knots and the two correction edges, every one of them a closed form of the point's own
    !! bandwidth and the bounds -- nothing about the integrand is discovered at run time, so there
    !! is nothing for a convergence test to find. The cost is `16 * (parts)` evaluations, known
    !! before the integral starts, where an adaptive rule spent 135 to 190 on the same integrand.
    function term_piece(fn, alpha, beta, sigma, has_sigma) result(v)
        type(kde_point_fn), intent(inout) :: fn        !! the point's scalars
        real(real64), intent(in)          :: alpha     !! the piece's lower end
        real(real64), intent(in)          :: beta      !! its upper end
        real(real64), intent(in)          :: sigma     !! its sign change, a breakpoint of the magnitude
        logical, intent(in)               :: has_sigma !! the sign change is known
        real(real64)                      :: v         !! the integral

        real(real64) :: bp(KDE_CORR_MAX_BREAKS), lo, hi
        integer :: n, i

        v = 0.0_real64
        if (.not. (beta > alpha)) return
        call term_breaks(fn, alpha, beta, sigma, has_sigma, bp, n)
        lo = alpha
        do i = 1, n + 1
            if (i <= n) then
                hi = bp(i)
            else
                hi = beta
            end if
            v = v + gauss_piece(fn, lo, hi)
            lo = hi
        end do

    end function term_piece

    !> `KDE_GL16_X`'s rule over `[a, b]`, an interval the term is analytic on, split into equal
    !! parts no wider than `KDE_GL_SPAN` bandwidths so that the rule's geometric convergence is not
    !! asked to cover more width than it can.
    function gauss_piece(fn, a, b) result(v)
        type(kde_point_fn), intent(inout) :: fn !! the point's scalars
        real(real64), intent(in)          :: a  !! the interval's lower end
        real(real64), intent(in)          :: b  !! its upper end
        real(real64)                      :: v  !! the integral

        real(real64) :: w, span, step, mid, half
        integer :: ns, p, i

        v = 0.0_real64
        w = b - a
        if (.not. (w > 0.0_real64)) return
        ! The width in bandwidths: `fn%r` is one over the point's, so `w*r` is the count.
        span = w*fn%r
        ns = 1
        if (span > KDE_GL_SPAN) ns = int(ceiling(span/KDE_GL_SPAN))
        step = w/real(ns, real64)
        do p = 1, ns
            mid = a + (real(p, real64) - 0.5_real64)*step
            half = 0.5_real64*step
            do i = 1, 16
                v = v + half*KDE_GL16_W(i)*fn%eval(mid + half*KDE_GL16_X(i))
            end do
        end do

    end function gauss_piece

    !> The term's own breakpoints strictly inside `(alpha, beta)`, ascending and distinct: the knots
    !! of its kernel and of its moments for the cubic B-spline, its two correction edges, and the
    !! sign change of the term where the sampler integrates the magnitude.
    !!
    !! Nothing here is taken from the data, so no call can see a repeated breakpoint whatever the
    !! sample -- `pf_integrate` aborts on one -- and the list is at most `KDE_CORR_MAX_BREAKS`
    !! long, far below the budget's pieces.
    pure subroutine term_breaks(fn, alpha, beta, sigma, has_sigma, bp, n)
        type(kde_point_fn), intent(in) :: fn        !! the point's scalars
        real(real64), intent(in)       :: alpha     !! the piece's lower end
        real(real64), intent(in)       :: beta      !! its upper end
        real(real64), intent(in)       :: sigma     !! the term's sign change
        logical, intent(in)            :: has_sigma !! the sign change is one of the breakpoints
        real(real64), intent(out)      :: bp(:)     !! the breakpoints, ascending and distinct
        integer, intent(out)           :: n         !! how many

        real(real64) :: cand(KDE_CORR_MAX_BREAKS), rad, sc, v
        integer :: nc, i, p
        logical :: dup

        rad = KDE_RADIUS(fn%code)
        sc = KDE_SCALE(fn%code)
        nc = 0
        if (fn%code == KDE_BSPLINE) then
            ! The kernel's own knots, where its cubic pieces meet, and its moments', where the
            ! truncated moments' pieces do.
            cand(nc + 1) = fn%xj - sc*fn%hj
            cand(nc + 2) = fn%xj
            cand(nc + 3) = fn%xj + sc*fn%hj
            nc = nc + 3
            if (fn%has_lower) then
                nc = nc + 1
                cand(nc) = fn%lo + sc*fn%hj
            end if
            if (fn%has_upper) then
                nc = nc + 1
                cand(nc) = fn%hi - sc*fn%hj
            end if
        end if
        if (fn%has_lower) then
            nc = nc + 1
            cand(nc) = fn%lo + rad*fn%hj
        end if
        if (fn%has_upper) then
            nc = nc + 1
            cand(nc) = fn%hi - rad*fn%hj
        end if
        if (has_sigma) then
            ! Not reachable from inside the library: every call of `kde_term_integral` passes
            ! `has_sigma = .false.`, `solve_term_mass`'s `absolute` call included, so the
            ! magnitude's own sign change is never offered as a breakpoint. Kept because the sign
            ! change is part of that integral's published contract.
            ! GCOVR_EXCL_START
            nc = nc + 1
            cand(nc) = sigma
            ! GCOVR_EXCL_STOP
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

    end subroutine term_breaks

    !> The linear kernel's moments over the narrow interval `[lo_z, hi_z]`, in standard deviations,
    !! about its midpoint `c` and scaled by its width `w`: `m_l`, the integral of `s**l K(c + w s)`
    !! over `s` in `[-1/2, 1/2]`, by the eight-point Gauss-Legendre rule on each piece between the
    !! kernel's knots inside the interval (the cubic B-spline's at `-C`, `0` and `C`; the other
    !! kernels have none inside their support). The rule is exact for a polynomial piece of degree
    !! up to fifteen, so for the three polynomial kernels these are the moments to rounding, and for
    !! the Gaussian, over an interval under one standard deviation wide, the rule's own error is
    !! below rounding too.
    pure subroutine centred_moments(code, lo_z, hi_z, c, w, m0, m1, m2)
        integer, intent(in)       :: code !! the kernel
        real(real64), intent(in)  :: lo_z !! the interval's lower end
        real(real64), intent(in)  :: hi_z !! its upper end
        real(real64), intent(in)  :: c    !! its midpoint
        real(real64), intent(in)  :: w    !! its width, positive
        real(real64), intent(out) :: m0   !! the zeroth moment
        real(real64), intent(out) :: m1   !! the first, about `c`, over `w`
        real(real64), intent(out) :: m2   !! the second, about `c`, over `w**2`
        real(real64) :: cuts(5), knots(3), mid, half, s, k, ws
        integer :: n, i, p

        ! The pieces, in `s`: the interval's ends and every knot strictly inside it, ascending.
        n = 1
        cuts(1) = -0.5_real64
        if (code == KDE_BSPLINE) then
            knots = [-KDE_SCALE(code), 0.0_real64, KDE_SCALE(code)]
            do i = 1, 3
                if (knots(i) > lo_z .and. knots(i) < hi_z) then
                    n = n + 1
                    cuts(n) = (knots(i) - c)/w
                end if
            end do
        end if
        n = n + 1
        cuts(n) = 0.5_real64
        m0 = 0.0_real64
        m1 = 0.0_real64
        m2 = 0.0_real64
        do p = 1, n - 1
            mid = 0.5_real64*(cuts(p) + cuts(p + 1))
            half = 0.5_real64*(cuts(p + 1) - cuts(p))
            do i = 1, 8
                s = mid + half*KDE_GL8_X(i)
                k = kde_kernel_pdf(code, c + w*s)
                ws = half*KDE_GL8_W(i)*k
                m0 = m0 + ws
                m1 = m1 + ws*s
                m2 = m2 + (ws*s)*s
            end do
        end do

    end subroutine centred_moments

    !> The norm of the density's `s`-th derivative at the time `t`, as the ISJ rule estimates it from
    !! the binned transform: `2 pi**(2s) sum_k k**(2s) (y_k/2)**2 exp(-k**2 pi**2 t)`.
    !!
    !! The sum stops at the last `k` whose exponent is within `KDE_ISJ_EXP_LIMIT`: the terms after
    !! it are below the normal range, and below rounding against the terms before them, so what
    !! it leaves out is nothing the answer can hold. Each term's `exp(-k**2 c)` is the previous one's
    !! times `exp(-(2k - 1) c)`, and that factor the previous one's times `exp(-2c)`, over runs of
    !! `KDE_ISJ_RUN` terms, each run starting from `exp` itself.
    !!
    !! **No factor is formed that no term reads.** Every exponential and every factor a term reads
    !! is within the limit, so in the normal range. The ones no term reads are not, once `c` is
    !! large -- the step `exp(-2c)` where no run has three terms, a run's first factor where the
    !! run has one, and the factor past a run's last term -- and the search for a fixed point
    !! reaches such a `c` on a sample the rule finds no root for. Forming one raises
    !! IEEE_UNDERFLOW for a value nothing reads (`test_isj_sum_forms_no_unused_factor`), so none is
    !! formed; the terms and their order are unchanged, so the sum is too. A term's PRODUCT with
    !! its coefficient can still fall below the normal range, since a coefficient can be
    !! arbitrarily small; that underflow is accepted, the term being far below rounding against
    !! the sum.
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
        ! The step is read only by a run of three terms or more, where `9c` is within the limit.
        q = 0.0_real64
        if (kmax >= 3) q = exp(-2.0_real64*ct)
        f = 0.0_real64
        k = 1
        do while (k <= kmax)
            ! `e` is term `k`'s exponential and `g` the factor to the next: `(k + 1)**2 - k**2` is
            ! `2k + 1`. A factor is formed only for a term that follows, so it is within the
            ! limit as that term is; the run's last term is taken outside the loop for that reason.
            e = exp(-this%kk(k)*ct)
            f = f + this%p(k, s)*e
            kend = min(kmax, k + KDE_ISJ_RUN - 1)
            if (kend > k) then
                g = exp(-real(2*k + 1, real64)*ct)
                do j = k + 1, kend - 1
                    e = e*g
                    g = g*q
                    f = f + this%p(j, s)*e
                end do
                e = e*g
                f = f + this%p(kend, s)*e
            end if
            k = kend + 1
        end do
        f = KDE_ISJ_NORM_C(s)*f

    end function isj_norm

end submodule parquet_kde_core ! GCOVR_EXCL_LINE
