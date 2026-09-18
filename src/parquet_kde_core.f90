!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The workers `pf_kde` is built on: the abort, the token resolution, the four kernels' densities
!> and distribution functions, and the two bandwidth rules.
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
        case default
            code = 0
            call kde_abort(entry, 'rule must be "silverman" or "scott"')
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
        if (present(weights)) then
            if (freq) then
                n_eff = sum(weights)
            else
                n_eff = sum(weights)**2/sum(weights*weights)
            end if
        else
            n_eff = real(size(x, kind=int64), real64)
        end if
        select case (rule_code)
        case (KDE_RULE_SCOTT)
            h = KDE_SCOTT_C*a*n_eff**(-0.2_real64)
        case default
            h = KDE_SILVERMAN_C*a*n_eff**(-0.2_real64)
        end select

    end procedure kde_rule_bandwidth

end submodule parquet_kde_core ! GCOVR_EXCL_LINE
