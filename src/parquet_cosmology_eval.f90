!> Every `pure` binding of `pf_cosmology`: the kernel, the table readers, the fallback rule, the
!! closed forms over them, the five inverses and the three free redshift conversions.
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

    ! ---- Which QUANTITY an inverse is solving for ----------------------------------------------
    !
    ! Deliberately NOT the `PFC_INT_*` integrand codes, which say which integrand a panel is taken
    ! over. The two vocabularies overlap by two entries and differ by three, and one number meaning
    ! both would be a trap: `bracket_outside` lays panels, so it maps a quantity to its integrand
    ! itself rather than passing the caller's code through.

    integer, parameter :: PFC_Q_DC = 11
        !! Invert `D_C(zeta)`.
    integer, parameter :: PFC_Q_TL = 12
        !! Invert `t_L(zeta)`.
    integer, parameter :: PFC_Q_LOG_AGE = 13
        !! Invert `-ln(age(zeta))`, which INCREASES with `zeta` as the age falls.
    integer, parameter :: PFC_Q_DL = 14
        !! Invert `D_L(zeta)`.

    real(real64), parameter :: PFC_LOG10_LIMIT = 300.0_real64
        !! `10^p` is formed only for `|p|` below this: `10^309` overflows and `10^-324` is zero,
        !! and both would raise a flag on the way. Neither end is reachable by a distance modulus
        !! anyone means -- `mu = 1525` is `10^300 Mpc`, and `mu = -1475` is `10^-300 Mpc`.

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

        ! `Tcmb0 = 0` switches radiation AND NEUTRINOS off entirely, as astropy does, and this arm
        ! is what makes that true for a MASSIVE species as well: `y = m/(k_B T_nu0)` divides by a
        ! zero neutrino temperature, which raises `IEEE_DIVIDE_BY_ZERO` -- fatal under nagfor's
        ! default `-ieee=stop` -- and then `Onu0 = Ogamma0 * Infinity` is `0 * Infinity`, a NaN
        ! that `%init` reports as an out-of-range density. `init(h0, om0, m_nu=[0, 0, 0.06])` with
        ! `tcmb0` left at its default is the reachable call, and astropy accepts it.
        if (d%tnu0 == 0.0_real64) then
            v = 0.0_real64
            return
        end if
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

    !> The CPL dark-energy factor `f_DE = x^(3(1 + w0 + wa)) exp(-3 wa z/x)` at `x = e^zeta`.
    !!
    !! Formed through its LOGARITHM and screened, because it is the one term whose exponent the
    !! caller sets and the one that blows up towards `z = -1`: `wa = 3` at `z = -0.99` already asks
    !! for `exp(891)`. An infinite dark-energy density is the right answer there -- it does
    !! diverge -- and it carries through as `E = +Infinity` and `1/E = 0`. Exactly 1 for a
    !! cosmological constant, by assignment rather than by `exp(0)`.
    !!
    !! `zeta` IS `ln x`, so no second logarithm is taken.
    pure function f_de_at(p, zeta, x) result(f)
        type(cosmology_params), intent(in) :: p    !! the parameters
        real(real64), intent(in)           :: zeta !! `ln(1 + z)`, known not to be a NaN
        real(real64), intent(in)           :: x    !! `e^zeta`, which the caller already has
        real(real64)                       :: f    !! `f_DE`, or `+Infinity` past the screen

        real(real64) :: ell

        if (p%w0 == -1.0_real64 .and. p%wa == 0.0_real64) then
            f = 1.0_real64
            return
        end if
        ell = 3.0_real64 * (1.0_real64 + p%w0 + p%wa) * zeta - 3.0_real64 * p%wa * (x - 1.0_real64) / x
        if (ell > PFC_DE_EXP_CEILING) then
            f = ieee_value(f, ieee_positive_inf)
        else if (ell < -PFC_DE_EXP_CEILING) then
            ! The screen is SYMMETRIC, and the floor is not cosmetic: `exp` of a large negative
            ! argument underflows through the subnormals to zero, raising `IEEE_UNDERFLOW`, which
            ! nagfor reports as one line at program exit attached to nothing and which would then
            ! hide a later finding. `w0 = 3, wa = -3` -- both inside the admitted range -- reaches
            ! `ell = -9e10` at the bottom of the domain, where `%init` walks to compute the stored
            ! `d_floor` and `t_floor`. Zero is the value the dark-energy density really has there,
            ! and it is `2.6e-302` at the floor itself, far below every other term of `E^2`.
            f = 0.0_real64
        else
            f = exp(ell)
        end if

    end function f_de_at

    !> `Ode0 f_DE(z)`, the dark-energy TERM of `E^2`.
    !!
    !! The product, never the two factors handed out separately: past the CPL screen `f_DE` is
    !! `+Infinity`, and a model with no dark energy at all would then form `0 * Infinity` -- a NaN
    !! and an `IEEE_INVALID` -- for a term that is simply zero. A NEGATIVE `ode0` gives
    !! `-Infinity`, which carries through to a non-positive `E^2` and the quiet NaN every binding
    !! answers for "no big bang".
    pure function de_term_at(p, zeta, x) result(v)
        type(cosmology_params), intent(in) :: p    !! the parameters
        real(real64), intent(in)           :: zeta !! `ln(1 + z)`, known not to be a NaN
        real(real64), intent(in)           :: x    !! `e^zeta`
        real(real64)                       :: v    !! `Ode0 f_DE`

        if (p%ode0 == 0.0_real64) then
            v = 0.0_real64
        else
            v = p%ode0 * f_de_at(p, zeta, x)
        end if

    end function de_term_at

    module procedure cosmology_e2

        real(real64) :: x, de

        ! The NaN screen comes first and stands alone: every test below it is ordered.
        if (zeta /= zeta) then
            v = zeta
            return
        end if
        x = exp(zeta)
        de = de_term_at(p, zeta, x)
        if (de > huge(de)) then
            ! The screen inside `f_de_at` fired, so the dark-energy density has overflowed and
            ! `E^2` is infinite whatever the other terms are. An ordered comparison is safe here:
            ! `de` is zero, a finite product, or a signed infinity, and never a NaN.
            v = ieee_value(v, ieee_positive_inf)
            return
        end if

        v = p%om0 * x ** 3 + d%ok0 * x ** 2 + de
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
    ! What the universe is made of at z
    ! =========================================================================================

    !> One density parameter: a term of `E^2` over `E^2`.
    !!
    !! `term` is finite at every admitted redshift for matter, curvature, photons and neutrinos,
    !! so where `E^2` has overflowed -- a big-rip model towards `z = -1` -- this answers exactly
    !! zero without raising a flag. The DARK-ENERGY term does not come through here: there
    !! `Infinity / Infinity` would be a NaN and an `IEEE_INVALID` rather than the limit, which is
    !! one, so `%ode` tests for that regime itself.
    pure function density_fraction(term, e2) result(v)
        real(real64), intent(in) :: term !! the term of `E^2` this parameter counts
        real(real64), intent(in) :: e2   !! `E^2` there
        real(real64)             :: v    !! the density parameter

        if (e2 /= e2) then
            v = e2
        else if (e2 <= 0.0_real64) then
            ! `E^2 <= 0` is "no big bang", which `%efunc` answers NaN for; so does every fraction
            ! of it.
            v = ieee_value(v, ieee_quiet_nan)
        else
            v = term / e2
        end if

    end function density_fraction

    module procedure cosmology_om

        real(real64) :: zeta, x

        if (.not. this%ready) error stop "pf_cosmology%om: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
            return
        end if
        x = exp(zeta)
        v = density_fraction(this%p%om0 * x ** 3, cosmology_e2(this%p, this%d, zeta))

    end procedure cosmology_om

    module procedure cosmology_ode

        real(real64) :: zeta, x, e2

        if (.not. this%ready) error stop "pf_cosmology%ode: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
            return
        end if
        x = exp(zeta)
        e2 = cosmology_e2(this%p, this%d, zeta)
        if (e2 /= e2) then
            v = e2
        else if (e2 <= 0.0_real64) then
            v = ieee_value(v, ieee_quiet_nan)
        else if (e2 > huge(e2)) then
            ! The CPL factor overflowed, so dark energy is ALL of `E^2` and the limit is one.
            ! `Ode0 f_DE / E^2` would be `Infinity / Infinity` here: a NaN, and an `IEEE_INVALID`.
            v = 1.0_real64
        else
            v = de_term_at(this%p, zeta, x) / e2
        end if

    end procedure cosmology_ode

    module procedure cosmology_ok

        real(real64) :: zeta, x

        if (.not. this%ready) error stop "pf_cosmology%ok: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
            return
        end if
        x = exp(zeta)
        ! A flat model has `ok0` exactly zero by assignment, so this is exactly zero too.
        v = density_fraction(this%d%ok0 * x ** 2, cosmology_e2(this%p, this%d, zeta))

    end procedure cosmology_ok

    module procedure cosmology_ogamma

        real(real64) :: zeta, x

        if (.not. this%ready) error stop "pf_cosmology%ogamma: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
            return
        end if
        x = exp(zeta)
        v = density_fraction(this%d%ogamma0 * x ** 4, cosmology_e2(this%p, this%d, zeta))

    end procedure cosmology_ogamma

    module procedure cosmology_onu

        real(real64) :: zeta, x

        if (.not. this%ready) error stop "pf_cosmology%onu: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
            return
        end if
        x = exp(zeta)
        ! `Ogamma(z)` TIMES the fit, in that order, so that the five density parameters sum to one
        ! to a few ulp: the radiation term of `E^2` is `Ogamma0 x^4 (1 + nu_rel)`.
        v = density_fraction(this%d%ogamma0 * x ** 4, cosmology_e2(this%p, this%d, zeta)) &
            * cosmology_nu_rel(this%p, this%d, x)

    end procedure cosmology_onu

    module procedure cosmology_tcmb

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%tcmb: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
        else
            v = this%p%tcmb0 * exp(zeta)
        end if

    end procedure cosmology_tcmb

    module procedure cosmology_w

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%w: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
        else if (this%p%wa == 0.0_real64) then
            ! Exactly `w0`, with no arithmetic to round it -- and it keeps `wa * z/(1 + z)` from
            ! being formed at all for the cosmological constant, where `z/(1 + z)` is `-1e5` at
            ! the blueshift end and `0 * (-1e5)` is only accidentally zero.
            v = this%p%w0
        else
            ! `z/(1 + z)` as astropy forms it, on the caller's own `z` rather than on `e^zeta`:
            ! it keeps its digits at a small `z`, where `1 - e^-zeta` would cancel.
            v = this%p%w0 + this%p%wa * z / (1.0_real64 + z)
        end if

    end procedure cosmology_w

    module procedure cosmology_de_density_scale

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%de_density_scale: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
        else
            v = f_de_at(this%p, zeta, exp(zeta))
        end if

    end procedure cosmology_de_density_scale

    module procedure cosmology_critical_density

        real(real64) :: zeta, e2

        if (.not. this%ready) error stop "pf_cosmology%critical_density: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
            return
        end if
        e2 = cosmology_e2(this%p, this%d, zeta)
        if (e2 /= e2) then
            v = e2
        else if (e2 <= 0.0_real64) then
            v = ieee_value(v, ieee_quiet_nan)
        else
            ! `rho_crit0` is already in M_sun/Mpc^3; `%init` did the conversion once.
            v = this%d%rho_crit0 * e2
        end if

    end procedure cosmology_critical_density

    module procedure cosmology_lookback_distance

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%lookback_distance: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
        else
            v = this%d%dh * tl_at(this, zeta) / this%d%th
        end if

    end procedure cosmology_lookback_distance

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

    !> `dD_M/dD_C`: one for a flat model, `cosh` for an open one, `cos` for a closed one.
    pure function dm_slope_of_dc(this, dc) result(s)
        class(pf_cosmology), intent(in) :: this !! the cosmology
        real(real64), intent(in)        :: dc   !! `D_C` in Mpc
        real(real64)                    :: s    !! `dD_M/dD_C`, dimensionless

        real(real64) :: arg

        if (dc /= dc) then
            s = dc
        else if (this%flat) then
            s = 1.0_real64
        else if (this%d%ok0 > 0.0_real64) then
            arg = sqrt(this%d%ok0) * dc / this%d%dh
            if (abs(arg) > PFC_SINH_CEILING) then
                ! A NaN rather than an infinity, so that the solver BISECTS here instead of taking
                ! a zero-length Newton step and declaring itself converged where it is not.
                s = ieee_value(s, ieee_quiet_nan)
            else
                s = cosh(arg)
            end if
        else
            s = cos(sqrt(-this%d%ok0) * dc / this%d%dh)
        end if

    end function dm_slope_of_dc

    !> `D_L(zeta) = e^zeta D_M(D_C(zeta))`, in Mpc.
    pure function dl_at(this, zeta) result(v)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64)                    :: v    !! `D_L` in Mpc

        v = exp(zeta) * dm_of_dc(this, dc_at(this, zeta))

    end function dl_at

    !> `dD_L/dzeta = e^zeta [D_M + (dD_M/dD_C)(dD_C/dzeta)]`.
    pure function dl_slope(this, zeta) result(s)
        class(pf_cosmology), intent(in) :: this !! the cosmology
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`
        real(real64)                    :: s    !! `dD_L/dzeta` in Mpc

        real(real64) :: dc

        dc = dc_at(this, zeta)
        s = exp(zeta) * (dm_of_dc(this, dc) + dm_slope_of_dc(this, dc) * dc_slope(this, zeta))

    end function dl_slope

    !> A bracketed Newton solve for `zeta` where a monotone quantity equals `target`.
    !!
    !! Newton where the step stays inside the bracket, bisection where it does not, so it cannot
    !! run away on the flat tail: `dD_C/dz` falls like `z^(-3/2)`, and near the ceiling a distance
    !! carrying one ulp determines `z` only to about one part in ten. That is the mathematics, not
    !! the iteration -- the round trip is asserted in the DISTANCE, where it is stable both ways.
    pure function solve_zeta(this, which, target_value, lo_in, hi_in) result(zeta)
        class(pf_cosmology), intent(in) :: this         !! the cosmology
        integer, intent(in)             :: which        !! a `PFC_Q_*` quantity code
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
            slope = slope_at(this, which, zeta)
            previous = zeta
            if (slope /= slope) then
                zeta = 0.5_real64 * (lo + hi)
            else if (slope <= 0.0_real64 .or. slope > huge(slope)) then
                ! An INFINITE slope bisects too. A Newton step of `f/Infinity` is exactly zero,
                ! which the convergence test below would read as "arrived" at a point the
                ! iteration has not examined.
                zeta = 0.5_real64 * (lo + hi)
            else
                step = f_mid / slope
                zeta = zeta - step
                if (zeta <= lo .or. zeta >= hi) zeta = 0.5_real64 * (lo + hi)
            end if
            if (abs(zeta - previous) <= 1.0e-16_real64 * (1.0_real64 + abs(zeta))) return
        end do

    end function solve_zeta

    !> The quantity the solver is inverting, at `zeta`. Every one of the four INCREASES with
    !! `zeta`, which is what lets one bracketed iteration serve them all.
    pure function value_at(this, which, zeta) result(v)
        class(pf_cosmology), intent(in) :: this  !! the cosmology
        integer, intent(in)             :: which !! a `PFC_Q_*` quantity code
        real(real64), intent(in)        :: zeta  !! `ln(1 + z)`
        real(real64)                    :: v     !! the quantity there

        real(real64) :: t

        select case (which)
        case (PFC_Q_TL)
            v = tl_at(this, zeta)
        case (PFC_Q_DL)
            v = dl_at(this, zeta)
        case (PFC_Q_LOG_AGE)
            ! The age FALLS as `zeta` rises, so its negative logarithm is the increasing quantity.
            ! The logarithm is the point: at the top of the domain the age is `7.6e-18 Gyr` beside
            ! an `age(0)` of `13.8`, and only a relative measure of it has any digits left.
            t = age_at(this, zeta)
            if (t /= t) then
                v = t
            else if (t <= 0.0_real64) then
                v = ieee_value(v, ieee_quiet_nan)
            else
                v = -log(t)
            end if
        case default
            v = dc_at(this, zeta)
        end select

    end function value_at

    !> The derivative of `value_at` with respect to `zeta`, for the Newton step.
    pure function slope_at(this, which, zeta) result(s)
        class(pf_cosmology), intent(in) :: this  !! the cosmology
        integer, intent(in)             :: which !! a `PFC_Q_*` quantity code
        real(real64), intent(in)        :: zeta  !! `ln(1 + z)`
        real(real64)                    :: s     !! the derivative there

        real(real64) :: t

        select case (which)
        case (PFC_Q_TL)
            s = tl_slope(this, zeta)
        case (PFC_Q_DL)
            s = dl_slope(this, zeta)
        case (PFC_Q_LOG_AGE)
            ! `d(age)/dzeta` is exactly `-t_H/E`, because `age + t_L` is constant; so the slope of
            ! `-ln(age)` is the lookback time's own slope over the age.
            t = age_at(this, zeta)
            s = tl_slope(this, zeta)
            if (t /= t .or. s /= s) then
                s = ieee_value(s, ieee_quiet_nan)
            else if (t <= 0.0_real64) then
                s = ieee_value(s, ieee_quiet_nan)
            else
                s = s / t
            end if
        case default
            s = dc_slope(this, zeta)
        end select

    end function slope_at

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
            call bracket_outside(this, PFC_Q_DC, d, lo, hi, v_lo, v_hi)
            zeta = solve_zeta(this, PFC_Q_DC, d, lo, hi)
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
            call bracket_outside(this, PFC_Q_TL, t, lo, hi, v_lo, v_hi)
            zeta = solve_zeta(this, PFC_Q_TL, t, lo, hi)
        end if
        z = pf_zeta2z(zeta)

    end procedure cosmology_z_at_lookback

    module procedure cosmology_z_at_age

        real(real64) :: lo, hi, zeta

        if (.not. this%ready) error stop "pf_cosmology%z_at_age: the cosmology is not initialised"
        if (t /= t) then
            z = t
            return
        end if
        ! A model whose age integral diverges is infinitely old at EVERY redshift, so no age names
        ! one: the screen is the model's own flag rather than a comparison against an infinity.
        if (this%age_diverges .or. t <= 0.0_real64 .or. t < this%d%a_ceiling &
            .or. t > this%d%a_floor) then
            z = ieee_value(z, ieee_quiet_nan)
            return
        end if
        ! The bracket comes from the three stored bounds, so no walk is needed to find one. The
        ! age DECREASES with `zeta`, which is why the tests read the way round they do.
        if (t >= this%d%age0) then
            lo = -PFC_ZETA_CEILING
            hi = 0.0_real64
        else if (t >= this%d%a_n) then
            lo = 0.0_real64
            hi = this%d%zeta_n
        else
            lo = this%d%zeta_n
            hi = PFC_ZETA_CEILING
        end if
        ! On `-ln(age)`, never on `age(0) - t`: at the top of the domain that difference has no
        ! digit of the age left in it, and would answer an arbitrary redshift.
        zeta = solve_zeta(this, PFC_Q_LOG_AGE, -log(t), lo, hi)
        z = pf_zeta2z(zeta)

    end procedure cosmology_z_at_age

    !> The first unit panel of `zeta` above zero in which `D_L` reaches `target_value`.
    !!
    !! Only a CLOSED model needs this. There `D_M` turns over at the antipode, so `D_L` is not
    !! one-to-one and a bracket spanning the whole domain would return whichever crossing the
    !! bisection happened to land on; walking outward from `z = 0` returns the SMALLEST redshift,
    !! which is what the binding promises.
    pure subroutine bracket_upwards(this, target_value, lo, hi, found)
        class(pf_cosmology), intent(in) :: this         !! the cosmology
        real(real64), intent(in)        :: target_value !! the `D_L` to bracket
        real(real64), intent(out)       :: lo           !! the lower `zeta`
        real(real64), intent(out)       :: hi           !! the upper `zeta`
        logical, intent(out)            :: found        !! a panel containing it was reached

        real(real64) :: v_lo, v_hi
        integer      :: i, panels

        panels = int(PFC_ZETA_CEILING / PFC_PANEL) + 2
        lo = 0.0_real64
        v_lo = 0.0_real64
        hi = 0.0_real64
        found = .false.
        do i = 1, panels
            hi = min(lo + PFC_PANEL, PFC_ZETA_CEILING)
            v_hi = dl_at(this, hi)
            if (v_hi /= v_hi) return
            if (v_lo <= target_value .and. target_value <= v_hi) then
                found = .true.
                return
            end if
            if (hi >= PFC_ZETA_CEILING) return
            lo = hi
            v_lo = v_hi
        end do

    end subroutine bracket_upwards

    module procedure cosmology_z_at_luminosity

        real(real64) :: lo, hi, zeta
        logical      :: found

        if (.not. this%ready) error stop "pf_cosmology%z_at_luminosity_distance: the cosmology is not initialised"
        if (d /= d) then
            z = d
            return
        end if
        if (d < 0.0_real64) then
            ! `D_L` is negative at a blueshift and returns to zero as `z -> -1`, so a negative
            ! argument names two redshifts or none. This binding answers `z >= 0` only.
            z = ieee_value(z, ieee_quiet_nan)
            return
        end if
        if (d == 0.0_real64) then
            z = 0.0_real64
            return
        end if
        if (this%d%ok0 >= 0.0_real64) then
            ! Flat or open: `D_L` is strictly increasing in `z >= 0`, since `e^zeta`, `D_C` and
            ! `sinh` all are. The domain itself is then the bracket, and the ceiling is read off
            ! the stored `D_C(zeta_ceiling)` rather than walked to.
            if (d > exp(PFC_ZETA_CEILING) * dm_of_dc(this, this%d%d_ceiling)) then
                z = ieee_value(z, ieee_quiet_nan)
                return
            end if
            lo = 0.0_real64
            hi = PFC_ZETA_CEILING
        else
            call bracket_upwards(this, d, lo, hi, found)
            if (.not. found) then
                z = ieee_value(z, ieee_quiet_nan)
                return
            end if
        end if
        zeta = solve_zeta(this, PFC_Q_DL, d, lo, hi)
        z = pf_zeta2z(zeta)

    end procedure cosmology_z_at_luminosity

    module procedure cosmology_z_at_distmod

        real(real64) :: power

        if (.not. this%ready) error stop "pf_cosmology%z_at_distmod: the cosmology is not initialised"
        if (mu /= mu) then
            z = mu
            return
        end if
        power = (mu - 25.0_real64) / 5.0_real64
        if (power > PFC_LOG10_LIMIT) then
            ! `10^power` would overflow. Such a distance is past every admitted model's horizon,
            ! so the answer is the NaN `%z_at_luminosity_distance` gives beyond its own ceiling.
            z = ieee_value(z, ieee_quiet_nan)
        else if (power < -PFC_LOG10_LIMIT) then
            ! `10^power` would underflow to zero, and the redshift at zero distance is zero.
            z = 0.0_real64
        else
            z = cosmology_z_at_luminosity(this, 10.0_real64 ** power)
        end if

    end procedure cosmology_z_at_distmod

    !> Widens a bracket by unit panels from the table's edge until it contains `target_value`,
    !! and no further than the domain's ceiling.
    pure subroutine bracket_outside(this, which, target_value, lo, hi, v_lo, v_hi)
        class(pf_cosmology), intent(in) :: this         !! the cosmology
        integer, intent(in)             :: which        !! `PFC_Q_DC` or `PFC_Q_TL`, the two whose
                                                        !! quantity IS a tabulated integral
        real(real64), intent(in)        :: target_value !! the value to bracket
        real(real64), intent(out)       :: lo           !! the lower `zeta`
        real(real64), intent(out)       :: hi           !! the upper `zeta`
        real(real64), intent(out)       :: v_lo         !! the quantity at `lo`
        real(real64), intent(out)       :: v_hi         !! the quantity at `hi`

        real(real64) :: edge, v_edge, scale
        integer      :: i, panels, integrand

        ! The walk starts where the INVERSE table ends, which for a large `zmax` is short of the
        ! forward table's edge (see `strictly_increasing_prefix`). The quantity code is mapped to
        ! the INTEGRAND code here rather than being passed through, because the two vocabularies
        ! are not the same numbers.
        if (which == PFC_Q_DC) then
            edge = this%d%zeta_d_inv
            v_edge = this%d%d_inv_top
            scale = this%d%dh
            integrand = PFC_INT_DISTANCE
        else
            edge = this%d%zeta_t_inv
            v_edge = this%d%t_inv_top
            scale = this%d%th
            integrand = PFC_INT_TIME
        end if
        panels = int(2.0_real64 * PFC_ZETA_CEILING / PFC_PANEL) + 2
        if (target_value > v_edge) then
            lo = edge
            v_lo = v_edge
            do i = 1, panels
                hi = min(lo + PFC_PANEL, PFC_ZETA_CEILING)
                v_hi = v_lo + scale * cosmology_panel(this%p, this%d, integrand, lo, hi)
                if (v_hi >= target_value .or. hi >= PFC_ZETA_CEILING) return
                lo = hi
                v_lo = v_hi
            end do
        else
            hi = 0.0_real64
            v_hi = 0.0_real64
            do i = 1, panels
                lo = max(hi - PFC_PANEL, -PFC_ZETA_CEILING)
                v_lo = v_hi + scale * cosmology_panel(this%p, this%d, integrand, hi, lo)
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

    module procedure cosmology_odm0
        if (.not. this%ready) error stop "pf_cosmology%odm0: the cosmology is not initialised"
        if (this%has_ob0) then
            v = this%p%om0 - this%p%ob0
        else
            v = ieee_value(v, ieee_quiet_nan)
        end if
    end procedure cosmology_odm0

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
            !
            ! **An infinite `z` answers `+Infinity`, and `log` is kept off it.** nagfor's `log`
            ! raises `IEEE_INVALID` on `+Infinity` although it answers `+Infinity` correctly
            ! (`fortran-gotchas.md`, the nagfor group); `log(huge)` is quiet, so only the infinity
            ! bites. The clamp is on the ARGUMENT rather than a guard around the call because a
            ! guard does not keep an optimiser from forming what it guards: `min` makes the operand
            ! harmless whichever arm is evaluated, and the assignment afterwards selects. `z` is
            ! not a NaN here -- that is screened above, as its own statement -- so `min` cannot
            ! raise on one.
            zeta = log(1.0_real64 + min(z, huge(z)))
            if (z > huge(z)) zeta = z
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
