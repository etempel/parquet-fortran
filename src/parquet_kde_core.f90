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
