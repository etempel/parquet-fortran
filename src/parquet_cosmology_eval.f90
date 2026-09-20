!> Every `pure` binding of `pf_cosmology`: the kernel, the table readers, the fallback rule, the
!! closed forms over them, the two inverses and the three free redshift conversions.
!!
!! Nothing here writes an object, so every procedure may be called from any number of threads at
!! once. The screens of `feature_cosmology.md` section 4.8 live here: a NaN is tested with
!! `x /= x` as its own statement BEFORE any ordered comparison, `min`/`max` or transcendental,
!! because an ordered comparison against a quiet NaN raises `IEEE_INVALID` under gfortran and
!! ends the process under nagfor's default `-ieee=stop`.
submodule (parquet_cosmology) parquet_cosmology_eval

    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_negative_inf

    implicit none

    integer, parameter :: PFC_GL_N = 20
        !! Points in the fixed Gauss-Legendre rule the fallback uses.

    !> The 20-point Gauss-Legendre abscissae on `[-1, 1]`.
    !! Derived at 30 digits by `tools/generate_cosmology_reference.py`, whose `--self-test` holds
    !! each literal to the double nearest its own value and checks that the rule is exact for every
    !! polynomial through degree 39.
    real(real64), parameter :: pfc_gl_x(PFC_GL_N) = [ &
        -0.9931285991850949_real64, -0.9639719272779138_real64, -0.912234428251326_real64, -0.8391169718222188_real64, &
        -0.7463319064601508_real64, -0.636053680726515_real64, -0.5108670019508271_real64, -0.37370608871541955_real64, &
        -0.22778585114164507_real64, -0.07652652113349734_real64, 0.07652652113349734_real64, 0.22778585114164507_real64, &
        0.37370608871541955_real64, 0.5108670019508271_real64, 0.636053680726515_real64, 0.7463319064601508_real64, &
        0.8391169718222188_real64, 0.912234428251326_real64, 0.9639719272779138_real64, 0.9931285991850949_real64]
    !> The 20-point Gauss-Legendre weights on `[-1, 1]`, in the order of the abscissae above.
    real(real64), parameter :: pfc_gl_w(PFC_GL_N) = [ &
        0.017614007139152118_real64, 0.04060142980038694_real64, 0.06267204833410907_real64, 0.08327674157670475_real64, &
        0.10193011981724044_real64, 0.11819453196151841_real64, 0.13168863844917664_real64, 0.14209610931838204_real64, &
        0.14917298647260374_real64, 0.15275338713072584_real64, 0.15275338713072584_real64, 0.14917298647260374_real64, &
        0.14209610931838204_real64, 0.13168863844917664_real64, 0.11819453196151841_real64, 0.10193011981724044_real64, &
        0.08327674157670475_real64, 0.06267204833410907_real64, 0.04060142980038694_real64, 0.017614007139152118_real64]

    integer, parameter :: PFC_AGE_PANELS = 8
        !! Panels in `b` the age tail takes. ONE is not enough: the radiation-to-matter transition
        !! sits at `b` of order 0.1 to 1, where a single panel is `1.4e-8` out for any model with
        !! radiation, and invisible both at `z >= 1100` (`b <= 0.03`) and without radiation. Eight
        !! hold `6.6e-15` at every reachable `b_top` (`feature_cosmology.md` section 4.4).

    real(real64), parameter :: PFC_PI = 3.141592653589793_real64
        !! pi, for the volume and the angular scales.
    real(real64), parameter :: PFC_RAD_PER_ARCMIN = PFC_PI / 10800.0_real64
        !! One arcminute in radians.
    real(real64), parameter :: PFC_RAD_PER_ARCSEC = PFC_PI / 648000.0_real64
        !! One arcsecond in radians.

    integer, parameter :: PFC_SOLVE_STEPS = 80
        !! The bracketed Newton's iteration cap. It converges in a handful; the cap only bounds a
        !! `pure` loop, and bisection alone would reach one ulp of a unit bracket in about 52.

    !> The series for a curved comoving volume, `V_C = (4 pi/3) D_M^3 [1 - 3u/10 + 9u^2/56 - 5u^3/48]`.
    real(real64), parameter :: PFC_VOL_C1 = -0.3_real64                 !! the `u` coefficient
    real(real64), parameter :: PFC_VOL_C2 = 9.0_real64 / 56.0_real64    !! the `u^2` coefficient
    real(real64), parameter :: PFC_VOL_C3 = -5.0_real64 / 48.0_real64   !! the `u^3` coefficient

contains

    ! =========================================================================================
    ! The kernel
    ! =========================================================================================

    module procedure cosmology_nu_rel

        real(real64) :: total, y
        integer      :: i

        ! The massless branch, which `N_nu = 0` also takes, so `neff / n_nu` is never formed.
        if (d%n_nu == 0 .or. d%n_massless == d%n_nu) then
            v = pfc_komatsu_a * p%neff
            return
        end if
        total = real(d%n_massless, real64)
        do i = 1, d%n_nu
            if (p%m_nu(i) > 0.0_real64) then
                y = p%m_nu(i) / (pfc_k_b_ev * d%tnu0)
                total = total + (1.0_real64 + (pfc_komatsu_c * y / x) ** pfc_komatsu_p) ** pfc_komatsu_invp
            end if
        end do
        v = pfc_komatsu_a * (p%neff / real(d%n_nu, real64)) * total

    end procedure cosmology_nu_rel

    module procedure cosmology_e2

        real(real64) :: x, ell, f_de

        ! The NaN screen comes first and stands alone: every test below it is ordered.
        if (zeta /= zeta) then
            v = zeta
            return
        end if
        x = exp(zeta)

        ! The CPL factor is formed through its LOGARITHM and screened, because it is the one term
        ! whose exponent the caller sets and the one that blows up towards z = -1: `wa = 3` at
        ! z = -0.99 already asks for exp(891). An infinite dark-energy density is the right answer
        ! there -- it does diverge -- and it carries through as E = +Infinity and 1/E = 0.
        if (p%w0 == -1.0_real64 .and. p%wa == 0.0_real64) then
            f_de = 1.0_real64
        else
            ! `zeta` IS `ln x`, so no second logarithm is taken.
            ell = 3.0_real64 * (1.0_real64 + p%w0 + p%wa) * zeta - 3.0_real64 * p%wa * (x - 1.0_real64) / x
            if (ell > PFC_EXP_CEILING) then
                v = ieee_value(v, ieee_positive_inf)
                return
            end if
            f_de = exp(ell)
        end if

        v = p%om0 * x ** 3 + d%ok0 * x ** 2 + p%ode0 * f_de
        if (d%ogamma0 /= 0.0_real64) v = v + d%ogamma0 * x ** 4 * (1.0_real64 + cosmology_nu_rel(p, d, x))

    end procedure cosmology_e2

    module procedure cosmology_integrand_at

        real(real64) :: v2, zeta

        select case (which)
        case (PFC_INT_AGE_TAIL)
            ! `x` is `b = sqrt(a)`. The integrand vanishes like b^3 (with radiation) or b^2
            ! (without), so the origin is answered directly rather than by a division.
            if (x /= x) then
                v = x
                return
            end if
            if (x <= 0.0_real64) then
                v = 0.0_real64
                return
            end if
            zeta = -2.0_real64 * log(x)
            v2 = cosmology_e2(p, d, zeta)
            if (v2 /= v2) then
                v = v2
            else if (v2 <= 0.0_real64) then
                v = ieee_value(v, ieee_quiet_nan)
            else
                v = 2.0_real64 / (x * sqrt(v2))
            end if
        case default
            v2 = cosmology_e2(p, d, x)
            if (v2 /= v2) then
                v = v2
            else if (v2 <= 0.0_real64) then
                ! `E^2 <= 0` is "no big bang": a quiet NaN, never a square root of a negative.
                v = ieee_value(v, ieee_quiet_nan)
            else if (which == PFC_INT_DISTANCE) then
                v = exp(x) / sqrt(v2)
            else
                v = 1.0_real64 / sqrt(v2)
            end if
        end select

    end procedure cosmology_integrand_at

    module procedure cosmology_panel

        real(real64) :: half, mid, total
        integer      :: i

        half = 0.5_real64 * (b - a)
        mid = 0.5_real64 * (a + b)
        total = 0.0_real64
        do i = 1, PFC_GL_N
            total = total + pfc_gl_w(i) * cosmology_integrand_at(p, d, which, mid + half * pfc_gl_x(i))
        end do
        v = half * total

    end procedure cosmology_panel

    module procedure cosmology_walk

        real(real64) :: a, b

        v = 0.0_real64
        if (z0 /= z0 .or. z1 /= z1) then
            v = ieee_value(v, ieee_quiet_nan)
            return
        end if
        ! A panel taken with `b < a` returns the negative integral, so the downward walk needs no
        ! sign of its own. Every caller has clamped `z1` to `+/-PFC_ZETA_CEILING`, which bounds
        ! this loop at about 24 turns.
        a = z0
        if (z1 >= z0) then
            do while (a < z1)
                b = min(a + PFC_PANEL, z1)
                v = v + cosmology_panel(p, d, which, a, b)
                a = b
            end do
        else
            do while (a > z1)
                b = max(a - PFC_PANEL, z1)
                v = v + cosmology_panel(p, d, which, a, b)
                a = b
            end do
        end if

    end procedure cosmology_walk

    module procedure cosmology_age_tail

        real(real64) :: b_top, lo, hi
        integer      :: i

        if (zeta /= zeta) then
            v = zeta
            return
        end if
        b_top = exp(-0.5_real64 * zeta)
        v = 0.0_real64
        do i = 1, PFC_AGE_PANELS
            lo = b_top * real(i - 1, real64) / real(PFC_AGE_PANELS, real64)
            hi = b_top * real(i, real64) / real(PFC_AGE_PANELS, real64)
            v = v + cosmology_panel(p, d, PFC_INT_AGE_TAIL, lo, hi)
        end do

    end procedure cosmology_age_tail

    ! =========================================================================================
    ! Domain screens and the table readers
    ! =========================================================================================

    !> `zeta` for a redshift inside the domain, or a quiet NaN for one outside it.
    !!
    !! The domain is `|zeta| <= PFC_ZETA_CEILING`, which in redshift is
    !! `[exp(-PFC_ZETA_CEILING) - 1, PFC_Z_CEILING]` -- about `[-0.9999999999, 1e10]`. It is stated
    !! in `zeta` because that is where the fallback's panel walk is bounded, and because it makes
    !! `%comoving_distance` and `%comoving_distance_zeta` share one domain exactly.
    pure function zeta_of_z(z) result(zeta)
        real(real64), intent(in) :: z    !! redshift
        real(real64)             :: zeta !! `ln(1 + z)`, or NaN outside the domain

        if (z /= z) then
            zeta = z
            return
        end if
        if (z <= -1.0_real64 .or. z > PFC_Z_CEILING) then
            zeta = ieee_value(zeta, ieee_quiet_nan)
            return
        end if
        zeta = pf_z2zeta(z)
        if (zeta < -PFC_ZETA_CEILING) zeta = ieee_value(zeta, ieee_quiet_nan)

    end function zeta_of_z

    !> A `zeta` given directly, screened against the same domain.
    pure function zeta_in_domain(zeta) result(out)
        real(real64), intent(in) :: zeta !! `ln(1 + z)`
        real(real64)             :: out  !! `zeta`, or NaN outside the domain

        if (zeta /= zeta) then
            out = zeta
            return
        end if
        if (zeta > PFC_ZETA_CEILING .or. zeta < -PFC_ZETA_CEILING) then
            out = ieee_value(out, ieee_quiet_nan)
        else
            out = zeta
        end if

    end function zeta_in_domain

    !> `D_C(zeta)` in Mpc: the table inside its range, the panel rule outside it.
    !!
    !! The fallback starts from the TABULATED `D_N`, so the seam is continuous by construction.
    pure function dc_at(this, zeta) result(d)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64)                    :: d    !! `D_C` in Mpc

        if (zeta > this%d%zeta_n) then
            d = this%d%d_n + this%d%dh * cosmology_walk(this%p, this%d, PFC_INT_DISTANCE, this%d%zeta_n, zeta)
        else if (zeta >= 0.0_real64) then
            d = zeta * this%f%eval(zeta)
        else
            d = this%d%dh * cosmology_walk(this%p, this%d, PFC_INT_DISTANCE, 0.0_real64, zeta)
        end if

    end function dc_at

    !> `t_L(zeta)` in Gyr, the same way.
    pure function tl_at(this, zeta) result(t)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64)                    :: t    !! `t_L` in Gyr

        if (zeta > this%d%zeta_n) then
            t = this%d%t_n + this%d%th * cosmology_walk(this%p, this%d, PFC_INT_TIME, this%d%zeta_n, zeta)
        else if (zeta >= 0.0_real64) then
            t = zeta * this%g%eval(zeta)
        else
            t = this%d%th * cosmology_walk(this%p, this%d, PFC_INT_TIME, 0.0_real64, zeta)
        end if

    end function tl_at

    !> The age at `zeta`, in Gyr.
    !!
    !! Three regimes, and the middle one is the whole point of the design: the age has its OWN
    !! table, so it is never `age(0) - t_L(z)` where that would cancel. Below zero the difference
    !! IS used, because `t_L` is negative at a blueshift and the two terms add.
    pure function age_at(this, zeta) result(t)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64)                    :: t    !! the age in Gyr

        if (this%age_diverges) then
            t = ieee_value(t, ieee_positive_inf)
        else if (zeta > this%d%zeta_n) then
            t = this%d%th * cosmology_age_tail(this%p, this%d, zeta)
        else if (zeta >= 0.0_real64) then
            t = exp(this%a%eval(zeta))
        else
            t = this%d%age0 - tl_at(this, zeta)
        end if

    end function age_at

    !> `D_M` from a comoving distance, in Mpc. Hogg (1999) equation 16.
    pure function dm_of_dc(this, dc) result(dm)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: dc   !! `D_C` in Mpc
        real(real64)                    :: dm   !! `D_M` in Mpc

        real(real64) :: s, arg

        if (dc /= dc) then
            dm = dc
            return
        end if
        if (this%flat) then
            dm = dc
            return
        end if
        if (this%d%ok0 > 0.0_real64) then
            s = sqrt(this%d%ok0)
            arg = s * dc / this%d%dh
            ! A NEGATIVE `ode0` breaks the bound that would otherwise keep this below 23, so the
            ! argument is screened rather than left to overflow inside `sinh`.
            if (arg > PFC_SINH_CEILING) then
                dm = ieee_value(dm, ieee_positive_inf)
            else if (arg < -PFC_SINH_CEILING) then
                dm = ieee_value(dm, ieee_negative_inf)
            else
                dm = this%d%dh / s * sinh(arg)
            end if
        else
            s = sqrt(-this%d%ok0)
            dm = this%d%dh / s * sin(s * dc / this%d%dh)
        end if

    end function dm_of_dc

    !> `V_C` from a transverse distance, in Mpc^3. Hogg (1999) equation 29.
    !!
    !! Below `|u| = PFC_VOLUME_SERIES` the closed form is replaced by its series: the bracket
    !! differences two quantities that both tend to `q`, so as `u -> 0` it keeps nothing. The
    !! closed form has NO correct digit at `z = 1e-8` for a curved model, and the series holds
    !! `1e-14` there; they meet with three decades of margin at the crossover.
    pure function vc_of_dm(this, dm) result(v)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: dm   !! `D_M` in Mpc
        real(real64)                    :: v    !! `V_C` in Mpc^3

        real(real64) :: q, u, s, root

        if (dm /= dm) then
            v = dm
            return
        end if
        if (this%flat) then
            v = 4.0_real64 * PFC_PI / 3.0_real64 * dm ** 3
            return
        end if
        q = dm / this%d%dh
        u = this%d%ok0 * q * q
        if (abs(u) < PFC_VOLUME_SERIES) then
            v = 4.0_real64 * PFC_PI / 3.0_real64 * dm ** 3 &
                * (1.0_real64 + u * (PFC_VOL_C1 + u * (PFC_VOL_C2 + u * PFC_VOL_C3)))
            return
        end if
        root = sqrt(max(1.0_real64 + u, 0.0_real64))
        if (this%d%ok0 > 0.0_real64) then
            s = sqrt(this%d%ok0)
            v = 4.0_real64 * PFC_PI * this%d%dh ** 3 / (2.0_real64 * this%d%ok0) * (q * root - asinh(s * q) / s)
        else
            s = sqrt(-this%d%ok0)
            ! `s q` is a sine by construction, so the clamp only absorbs rounding at the antipode.
            v = 4.0_real64 * PFC_PI * this%d%dh ** 3 / (2.0_real64 * this%d%ok0) &
                * (q * root - asin(max(-1.0_real64, min(1.0_real64, s * q))) / s)
        end if

    end function vc_of_dm

    ! =========================================================================================
    ! The distances
    ! =========================================================================================

    module procedure cosmology_comoving_distance

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%comoving_distance: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            d = zeta
        else
            d = dc_at(this, zeta)
        end if

    end procedure cosmology_comoving_distance

    module procedure cosmology_comoving_distance_zeta

        real(real64) :: t

        if (.not. this%ready) error stop "pf_cosmology%comoving_distance_zeta: the cosmology is not initialised"
        t = zeta_in_domain(zeta)
        if (t /= t) then
            d = t
        else
            d = dc_at(this, t)
        end if

    end procedure cosmology_comoving_distance_zeta

    module procedure cosmology_comoving_transverse

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%comoving_transverse_distance: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            d = zeta
        else
            d = dm_of_dc(this, dc_at(this, zeta))
        end if

    end procedure cosmology_comoving_transverse

    module procedure cosmology_luminosity_distance

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%luminosity_distance: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            d = zeta
        else
            d = exp(zeta) * dm_of_dc(this, dc_at(this, zeta))
        end if

    end procedure cosmology_luminosity_distance

    module procedure cosmology_angular_diameter

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%angular_diameter_distance: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            d = zeta
        else
            d = dm_of_dc(this, dc_at(this, zeta)) / exp(zeta)
        end if

    end procedure cosmology_angular_diameter

    module procedure cosmology_angular_diameter_z1z2

        real(real64) :: zeta1, zeta2

        if (.not. this%ready) error stop "pf_cosmology%angular_diameter_distance_z1z2: the cosmology is not initialised"
        zeta1 = zeta_of_z(z1)
        zeta2 = zeta_of_z(z2)
        if (zeta1 /= zeta1) then
            d = zeta1
        else if (zeta2 /= zeta2) then
            d = zeta2
        else
            ! The transverse distance OF THE DIFFERENCE, never the difference of two transverse
            ! distances: the latter is right only for a flat model.
            d = dm_of_dc(this, dc_at(this, zeta2) - dc_at(this, zeta1)) / exp(zeta2)
        end if

    end procedure cosmology_angular_diameter_z1z2

    module procedure cosmology_comoving_volume

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%comoving_volume: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
        else
            v = vc_of_dm(this, dm_of_dc(this, dc_at(this, zeta)))
        end if

    end procedure cosmology_comoving_volume

    module procedure cosmology_differential_volume

        real(real64) :: zeta, dm, e2

        if (.not. this%ready) error stop "pf_cosmology%differential_comoving_volume: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
            return
        end if
        dm = dm_of_dc(this, dc_at(this, zeta))
        e2 = cosmology_e2(this%p, this%d, zeta)
        if (e2 /= e2) then
            v = e2
        else if (e2 <= 0.0_real64) then
            v = ieee_value(v, ieee_quiet_nan)
        else
            v = this%d%dh * dm * dm / sqrt(e2)
        end if

    end procedure cosmology_differential_volume

    module procedure cosmology_distmod

        real(real64) :: zeta, dl

        if (.not. this%ready) error stop "pf_cosmology%distmod: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            mu = zeta
            return
        end if
        dl = exp(zeta) * dm_of_dc(this, dc_at(this, zeta))
        if (dl /= dl) then
            mu = dl
        else if (dl == 0.0_real64) then
            ! `log10(0)` raises IEEE_DIVIDE_BY_ZERO in libm, which is fatal under nagfor.
            mu = ieee_value(mu, ieee_negative_inf)
        else
            ! The absolute value, so that a blueshift has a distance modulus at all.
            mu = 5.0_real64 * log10(abs(dl)) + 25.0_real64
        end if

    end procedure cosmology_distmod

    ! =========================================================================================
    ! The times and the expansion
    ! =========================================================================================

    module procedure cosmology_lookback_time

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%lookback_time: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            t = zeta
        else
            t = tl_at(this, zeta)
        end if

    end procedure cosmology_lookback_time

    module procedure cosmology_age

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%age: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            t = zeta
        else
            t = age_at(this, zeta)
        end if

    end procedure cosmology_age

    module procedure cosmology_efunc

        real(real64) :: zeta, v

        if (.not. this%ready) error stop "pf_cosmology%efunc: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            e = zeta
            return
        end if
        v = cosmology_e2(this%p, this%d, zeta)
        if (v /= v) then
            e = v
        else if (v <= 0.0_real64) then
            e = ieee_value(e, ieee_quiet_nan)
        else
            e = sqrt(v)
        end if

    end procedure cosmology_efunc

    module procedure cosmology_inv_efunc

        real(real64) :: zeta, v

        if (.not. this%ready) error stop "pf_cosmology%inv_efunc: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            e = zeta
            return
        end if
        v = cosmology_e2(this%p, this%d, zeta)
        if (v /= v) then
            e = v
        else if (v <= 0.0_real64) then
            e = ieee_value(e, ieee_quiet_nan)
        else
            e = 1.0_real64 / sqrt(v)
        end if

    end procedure cosmology_inv_efunc

    module procedure cosmology_hubble

        real(real64) :: zeta, v

        if (.not. this%ready) error stop "pf_cosmology%hubble: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            h = zeta
            return
        end if
        v = cosmology_e2(this%p, this%d, zeta)
        if (v /= v) then
            h = v
        else if (v <= 0.0_real64) then
            h = ieee_value(h, ieee_quiet_nan)
        else
            h = this%p%h0 * sqrt(v)
        end if

    end procedure cosmology_hubble

    module procedure cosmology_kpc_proper

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%kpc_proper_per_arcmin: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            s = zeta
        else
            s = 1000.0_real64 * (dm_of_dc(this, dc_at(this, zeta)) / exp(zeta)) * PFC_RAD_PER_ARCMIN
        end if

    end procedure cosmology_kpc_proper

    module procedure cosmology_kpc_comoving

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%kpc_comoving_per_arcmin: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            s = zeta
        else
            s = 1000.0_real64 * dm_of_dc(this, dc_at(this, zeta)) * PFC_RAD_PER_ARCMIN
        end if

    end procedure cosmology_kpc_comoving

    module procedure cosmology_arcsec_proper

        real(real64) :: zeta, da

        if (.not. this%ready) error stop "pf_cosmology%arcsec_per_kpc_proper: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            s = zeta
            return
        end if
        da = dm_of_dc(this, dc_at(this, zeta)) / exp(zeta)
        if (da /= da) then
            s = da
        else if (da == 0.0_real64) then
            s = ieee_value(s, ieee_positive_inf)
        else
            s = 1.0_real64 / (1000.0_real64 * da * PFC_RAD_PER_ARCSEC)
        end if

    end procedure cosmology_arcsec_proper

    module procedure cosmology_arcsec_comoving

        real(real64) :: zeta, dm

        if (.not. this%ready) error stop "pf_cosmology%arcsec_per_kpc_comoving: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            s = zeta
            return
        end if
        dm = dm_of_dc(this, dc_at(this, zeta))
        if (dm /= dm) then
            s = dm
        else if (dm == 0.0_real64) then
            s = ieee_value(s, ieee_positive_inf)
        else
            s = 1.0_real64 / (1000.0_real64 * dm * PFC_RAD_PER_ARCSEC)
        end if

    end procedure cosmology_arcsec_comoving

    ! =========================================================================================
    ! The inverses
    ! =========================================================================================

    !> `dD_C/dzeta = D_H e^zeta / E(zeta)`, the analytic derivative, for the Newton steps outside
    !! the table. Zero where `E` is infinite, which stops the iteration rather than dividing.
    pure function dc_slope(this, zeta) result(s)
        class(pf_cosmology), intent(in) :: this !! the cosmology
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`
        real(real64)                    :: s    !! `dD_C/dzeta` in Mpc

        real(real64) :: v

        v = cosmology_e2(this%p, this%d, zeta)
        if (v /= v) then
            s = v
        else if (v <= 0.0_real64) then
            s = ieee_value(s, ieee_quiet_nan)
        else
            s = this%d%dh * exp(zeta) / sqrt(v)
        end if

    end function dc_slope

    !> `dt_L/dzeta = t_H / E(zeta)`.
    pure function tl_slope(this, zeta) result(s)
        class(pf_cosmology), intent(in) :: this !! the cosmology
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`
        real(real64)                    :: s    !! `dt_L/dzeta` in Gyr

        real(real64) :: v

        v = cosmology_e2(this%p, this%d, zeta)
        if (v /= v) then
            s = v
        else if (v <= 0.0_real64) then
            s = ieee_value(s, ieee_quiet_nan)
        else
            s = this%d%th / sqrt(v)
        end if

    end function tl_slope

    !> A bracketed Newton solve for `zeta` where a monotone quantity equals `target`.
    !!
    !! Newton where the step stays inside the bracket, bisection where it does not, so it cannot
    !! run away on the flat tail: `dD_C/dz` falls like `z^(-3/2)`, and near the ceiling a distance
    !! carrying one ulp determines `z` only to about one part in ten. That is the mathematics, not
    !! the iteration -- the round trip is asserted in the DISTANCE, where it is stable both ways.
    pure function solve_zeta(this, which, target_value, lo_in, hi_in) result(zeta)
        class(pf_cosmology), intent(in) :: this         !! the cosmology
        integer, intent(in)             :: which        !! `PFC_INT_DISTANCE` or `PFC_INT_TIME`
        real(real64), intent(in)        :: target_value !! the value to invert
        real(real64), intent(in)        :: lo_in        !! a `zeta` whose value is at or below it
        real(real64), intent(in)        :: hi_in        !! a `zeta` whose value is at or above it
        real(real64)                    :: zeta         !! the `zeta` there

        real(real64) :: lo, hi, f_lo, f_mid, slope, step, previous
        integer      :: i

        lo = lo_in
        hi = hi_in
        f_lo = value_at(this, which, lo) - target_value
        zeta = 0.5_real64 * (lo + hi)
        do i = 1, PFC_SOLVE_STEPS
            f_mid = value_at(this, which, zeta) - target_value
            if (f_mid /= f_mid) return
            if (f_mid == 0.0_real64) return
            if ((f_lo < 0.0_real64) .eqv. (f_mid < 0.0_real64)) then
                lo = zeta
                f_lo = f_mid
            else
                hi = zeta
            end if
            if (which == PFC_INT_DISTANCE) then
                slope = dc_slope(this, zeta)
            else
                slope = tl_slope(this, zeta)
            end if
            previous = zeta
            if (slope /= slope) then
                zeta = 0.5_real64 * (lo + hi)
            else if (slope <= 0.0_real64) then
                zeta = 0.5_real64 * (lo + hi)
            else
                step = f_mid / slope
                zeta = zeta - step
                if (zeta <= lo .or. zeta >= hi) zeta = 0.5_real64 * (lo + hi)
            end if
            if (abs(zeta - previous) <= 1.0e-16_real64 * (1.0_real64 + abs(zeta))) return
        end do

    end function solve_zeta

    !> `D_C` or `t_L` at `zeta`, for the solver.
    pure function value_at(this, which, zeta) result(v)
        class(pf_cosmology), intent(in) :: this  !! the cosmology
        integer, intent(in)             :: which !! `PFC_INT_DISTANCE` or `PFC_INT_TIME`
        real(real64), intent(in)        :: zeta  !! `ln(1 + z)`
        real(real64)                    :: v     !! the quantity there

        if (which == PFC_INT_DISTANCE) then
            v = dc_at(this, zeta)
        else
            v = tl_at(this, zeta)
        end if

    end function value_at

    module procedure cosmology_z_at_distance

        real(real64) :: zeta, guess, slope, lo, hi, v_lo, v_hi

        if (.not. this%ready) error stop "pf_cosmology%z_at_comoving_distance: the cosmology is not initialised"
        if (d /= d) then
            z = d
            return
        end if
        if (d > this%d%d_ceiling .or. d < this%d%d_floor) then
            z = ieee_value(z, ieee_quiet_nan)
            return
        end if
        if (d >= 0.0_real64 .and. d <= this%d%d_inv_top) then
            ! The inverse table, then ONE Newton step on the FORWARD interpolant, which takes it
            ! from about 1e-9 to that interpolant's own inverse.
            guess = d * this%zd%eval(d)
            if (guess < 0.0_real64) guess = 0.0_real64
            if (guess > this%d%zeta_d_inv) guess = this%d%zeta_d_inv
            slope = this%f%eval(guess) + guess * this%f%derivative(guess)
            if (slope > 0.0_real64) then
                zeta = guess - (guess * this%f%eval(guess) - d) / slope
            else
                zeta = guess
            end if
        else
            call bracket_outside(this, PFC_INT_DISTANCE, d, lo, hi, v_lo, v_hi)
            zeta = solve_zeta(this, PFC_INT_DISTANCE, d, lo, hi)
        end if
        z = pf_zeta2z(zeta)

    end procedure cosmology_z_at_distance

    module procedure cosmology_z_at_lookback

        real(real64) :: zeta, guess, slope, lo, hi, v_lo, v_hi

        if (.not. this%ready) error stop "pf_cosmology%z_at_lookback_time: the cosmology is not initialised"
        if (t /= t) then
            z = t
            return
        end if
        if (t > this%d%t_ceiling .or. t < this%d%t_floor) then
            z = ieee_value(z, ieee_quiet_nan)
            return
        end if
        if (t >= 0.0_real64 .and. t <= this%d%t_inv_top) then
            guess = t * this%zt%eval(t)
            if (guess < 0.0_real64) guess = 0.0_real64
            if (guess > this%d%zeta_t_inv) guess = this%d%zeta_t_inv
            slope = this%g%eval(guess) + guess * this%g%derivative(guess)
            if (slope > 0.0_real64) then
                zeta = guess - (guess * this%g%eval(guess) - t) / slope
            else
                zeta = guess
            end if
        else
            call bracket_outside(this, PFC_INT_TIME, t, lo, hi, v_lo, v_hi)
            zeta = solve_zeta(this, PFC_INT_TIME, t, lo, hi)
        end if
        z = pf_zeta2z(zeta)

    end procedure cosmology_z_at_lookback

    !> Widens a bracket by unit panels from the table's edge until it contains `target_value`,
    !! and no further than the domain's ceiling.
    pure subroutine bracket_outside(this, which, target_value, lo, hi, v_lo, v_hi)
        class(pf_cosmology), intent(in) :: this         !! the cosmology
        integer, intent(in)             :: which        !! `PFC_INT_DISTANCE` or `PFC_INT_TIME`
        real(real64), intent(in)        :: target_value !! the value to bracket
        real(real64), intent(out)       :: lo           !! the lower `zeta`
        real(real64), intent(out)       :: hi           !! the upper `zeta`
        real(real64), intent(out)       :: v_lo         !! the quantity at `lo`
        real(real64), intent(out)       :: v_hi         !! the quantity at `hi`

        real(real64) :: edge, v_edge, scale
        integer      :: i, panels

        ! The walk starts where the INVERSE table ends, which for a large `zmax` is short of the
        ! forward table's edge (see `strictly_increasing_prefix`).
        if (which == PFC_INT_DISTANCE) then
            edge = this%d%zeta_d_inv
            v_edge = this%d%d_inv_top
            scale = this%d%dh
        else
            edge = this%d%zeta_t_inv
            v_edge = this%d%t_inv_top
            scale = this%d%th
        end if
        panels = int(2.0_real64 * PFC_ZETA_CEILING / PFC_PANEL) + 2
        if (target_value > v_edge) then
            lo = edge
            v_lo = v_edge
            do i = 1, panels
                hi = min(lo + PFC_PANEL, PFC_ZETA_CEILING)
                v_hi = v_lo + scale * cosmology_panel(this%p, this%d, which, lo, hi)
                if (v_hi >= target_value .or. hi >= PFC_ZETA_CEILING) return
                lo = hi
                v_lo = v_hi
            end do
        else
            hi = 0.0_real64
            v_hi = 0.0_real64
            do i = 1, panels
                lo = max(hi - PFC_PANEL, -PFC_ZETA_CEILING)
                v_lo = v_hi + scale * cosmology_panel(this%p, this%d, which, hi, lo)
                if (v_lo <= target_value .or. lo <= -PFC_ZETA_CEILING) return
                hi = lo
                v_hi = v_lo
            end do
        end if

    end subroutine bracket_outside

    ! =========================================================================================
    ! The parameters and the flags
    ! =========================================================================================

    module procedure cosmology_hubble_distance
        if (.not. this%ready) error stop "pf_cosmology%hubble_distance: the cosmology is not initialised"
        v = this%d%dh
    end procedure cosmology_hubble_distance

    module procedure cosmology_hubble_time
        if (.not. this%ready) error stop "pf_cosmology%hubble_time: the cosmology is not initialised"
        v = this%d%th
    end procedure cosmology_hubble_time

    module procedure cosmology_h0
        if (.not. this%ready) error stop "pf_cosmology%h0: the cosmology is not initialised"
        v = this%p%h0
    end procedure cosmology_h0

    module procedure cosmology_little_h
        if (.not. this%ready) error stop "pf_cosmology%little_h: the cosmology is not initialised"
        v = this%p%h0 / 100.0_real64
    end procedure cosmology_little_h

    module procedure cosmology_om0
        if (.not. this%ready) error stop "pf_cosmology%om0: the cosmology is not initialised"
        v = this%p%om0
    end procedure cosmology_om0

    module procedure cosmology_ode0
        if (.not. this%ready) error stop "pf_cosmology%ode0: the cosmology is not initialised"
        v = this%p%ode0
    end procedure cosmology_ode0

    module procedure cosmology_ok0
        if (.not. this%ready) error stop "pf_cosmology%ok0: the cosmology is not initialised"
        v = this%d%ok0
    end procedure cosmology_ok0

    module procedure cosmology_ogamma0
        if (.not. this%ready) error stop "pf_cosmology%ogamma0: the cosmology is not initialised"
        v = this%d%ogamma0
    end procedure cosmology_ogamma0

    module procedure cosmology_onu0
        if (.not. this%ready) error stop "pf_cosmology%onu0: the cosmology is not initialised"
        v = this%d%onu0
    end procedure cosmology_onu0

    module procedure cosmology_ob0
        if (.not. this%ready) error stop "pf_cosmology%ob0: the cosmology is not initialised"
        if (this%has_ob0) then
            v = this%p%ob0
        else
            v = ieee_value(v, ieee_quiet_nan)
        end if
    end procedure cosmology_ob0

    module procedure cosmology_tcmb0
        if (.not. this%ready) error stop "pf_cosmology%tcmb0: the cosmology is not initialised"
        v = this%p%tcmb0
    end procedure cosmology_tcmb0

    module procedure cosmology_tnu0
        if (.not. this%ready) error stop "pf_cosmology%tnu0: the cosmology is not initialised"
        v = this%d%tnu0
    end procedure cosmology_tnu0

    module procedure cosmology_neff
        if (.not. this%ready) error stop "pf_cosmology%neff: the cosmology is not initialised"
        v = this%p%neff
    end procedure cosmology_neff

    module procedure cosmology_w0
        if (.not. this%ready) error stop "pf_cosmology%w0: the cosmology is not initialised"
        v = this%p%w0
    end procedure cosmology_w0

    module procedure cosmology_wa
        if (.not. this%ready) error stop "pf_cosmology%wa: the cosmology is not initialised"
        v = this%p%wa
    end procedure cosmology_wa

    module procedure cosmology_zmax
        if (.not. this%ready) error stop "pf_cosmology%zmax: the cosmology is not initialised"
        v = this%p%zmax
    end procedure cosmology_zmax

    module procedure cosmology_is_flat
        if (.not. this%ready) error stop "pf_cosmology%is_flat: the cosmology is not initialised"
        ok = this%flat
    end procedure cosmology_is_flat

    module procedure cosmology_has_massive_nu
        if (.not. this%ready) error stop "pf_cosmology%has_massive_nu: the cosmology is not initialised"
        ok = this%massive_nu
    end procedure cosmology_has_massive_nu

    module procedure cosmology_is_initialised
        ! The one binding that never aborts: it is how a caller asks.
        ok = this%ready
    end procedure cosmology_is_initialised

    ! =========================================================================================
    ! The free functions
    ! =========================================================================================

    module procedure pf_z2zeta

        if (z /= z) then
            zeta = z
            return
        end if
        if (z <= -1.0_real64) then
            zeta = ieee_value(zeta, ieee_quiet_nan)
        else if (abs(z) > PFC_Z1P_EXACT) then
            ! `1 + z` is formed with full relative accuracy once `|z|` is this large -- and by
            ! Sterbenz's lemma it is EXACT for every `z` in `[-0.5, -1)`, which is why the
            ! blueshift end takes this branch too -- so `log(1 + z)` is correct to rounding.
            ! An infinite `z` takes this branch and answers `+Infinity`.
            zeta = log(1.0_real64 + z)
        else
            ! Below that, `1 + z` rounds away the digits `log` would need (`log(1 + 1e-8)` is
            ! `6e-9` out), and the `log1p` identity `parquet_random` already carries is exact.
            ! Its argument is at most `0.2` in magnitude here, so `atanh` is nowhere near the
            ! pole at 1 that makes this form useless at large `z`.
            zeta = 2.0_real64 * atanh(z / (2.0_real64 + z))
        end if

    end procedure pf_z2zeta

    module procedure pf_zeta2z

        real(real64) :: half

        if (zeta /= zeta) then
            z = zeta
            return
        end if
        if (zeta > PFC_EXP_CEILING) then
            z = ieee_value(z, ieee_positive_inf)
        else if (zeta < -PFC_EXP_CEILING) then
            ! `exp(zeta) - 1` is exactly -1 in double long before here; taking the branch keeps
            ! `exp(-inf) * sinh(-inf)` from forming a `0 * Infinity` NaN.
            z = -1.0_real64
        else
            half = 0.5_real64 * zeta
            z = 2.0_real64 * exp(half) * sinh(half)
        end if

    end procedure pf_zeta2z

    module procedure pf_z_combine

        ! `z1 + z2 + z1*z2`, not `(1 + z1)*(1 + z2) - 1`, which cancels at small redshifts: the
        ! product form is about 2e-13 out at z = 1e-3. A NaN argument propagates on its own.
        z = z1 + z2 + z1 * z2

    end procedure pf_z_combine

end submodule parquet_cosmology_eval
