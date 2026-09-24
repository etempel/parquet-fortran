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

    real(real64), parameter :: PFC_SOUND_PANEL = 1.0_real64
        !! The width in `v` of one panel of the sound horizon, `b = sound_c sinh(v)`. A WIDTH and
        !! not a count, as `PFC_PANEL` is a width in `zeta`, because the interval's own length
        !! runs from `0.19` at `z = 1e5` to `37` at the domain's floor for an extreme model.
        !!
        !! Measured against a 40-digit `mpmath` evaluation of the same integral, worst over four
        !! redshifts (`0`, `1100`, `1e5`, `-0.9999999999`) and six models from `Tcmb0 = 2.7255` to
        !! `Tcmb0 = 1e-4`:
        !!
        !! | rule | worst relative error | panels |
        !! |---|---|---|
        !! | 8 panels, uniform in `b` | 9.7e-02 | 8 |
        !! | 64 panels, uniform in `b` | 2.7e-02 | 64 |
        !! | 8 panels, uniform in `v` | 1.8e-06 | 8 |
        !! | width 1.0 in `v` | **4.9e-19** | up to 37 |
        !! | width 0.75 in `v` | 8.6e-23 | up to 49 |
        !!
        !! The first two rows are why the variable is `v` and not `b`: a uniform mesh in `b` does
        !! not converge at all once `1/sqrt(R0)` falls below a panel's width, and `R0` rises like
        !! `Tcmb0^-4`. The third is why the count is not fixed: the interval grows like the
        !! logarithm of the model's own scale. See `sound_scales`.
    integer, parameter :: PFC_SOUND_MAX_PANELS = 512
        !! The cap on that count, which bounds a `pure` loop as `PFC_SOLVE_STEPS` bounds one. The
        !! panels still span the whole interval when it binds -- they are laid as fractions of it
        !! -- so a capped rule is less accurate and never wrong. It first binds at about
        !! `Ogamma0 = 1e-200`, which no `Tcmb0` anyone writes reaches.

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
    integer, parameter :: PFC_Q_SOUND = 15
        !! Invert `-r_s(zeta)`, which INCREASES with `zeta` as the sound horizon falls. The
        !! NEGATIVE is the quantity, so `%z_drag` passes `-%r_drag()` as its target.

    real(real64), parameter :: PFC_DRAG_Z_LO = 100.0_real64
        !! The bottom of `%z_drag`'s bracket in `z`. Input-sanity bounds, not a setting: the drag
        !! epoch of a model this module admits is around 1060, and a bracket wider than this buys
        !! nothing a solve over a monotone function needs.
    real(real64), parameter :: PFC_DRAG_Z_HI = 1.0e5_real64
        !! The top of it. `%z_drag` answers a quiet NaN for a model whose `r_d` is not reached
        !! between the two, rather than the endpoint a sign-blind iteration would walk to.


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

    module procedure cosmology_de2_dzeta

        real(real64) :: x, de, w_z, rad, dnu

        if (zeta /= zeta) then
            ! every public entry screens its redshift through `x_of_z` or `zeta_of_z` before reaching
            ! here, and the quadrature evaluates only at nodes interior to a finite panel, so this
            ! argument is never a NaN. The screen stays because an ordered comparison against one
            ! raises `IEEE_INVALID`, which is fatal under nagfor's default `-ieee=stop`.
            ! GCOVR_EXCL_START
            v = zeta
            return
            ! GCOVR_EXCL_STOP
        end if
        x = exp(zeta)
        de = de_term_at(p, zeta, x)
        if (de > huge(de) .or. de < -huge(de)) then
            ! `f_de_at`'s own overflow screen cannot fire inside the admitted box: `|w0|` and `|wa|` are
            ! at most 3, so the CPL exponent is at least -15 and `x` at least `e^-23`, which bounds the
            ! factor at about `1e150`.
            ! GCOVR_EXCL_START
            v = ieee_value(v, ieee_positive_inf)
            return
            ! GCOVR_EXCL_STOP
        end if
        ! `dln f_DE/dzeta` is exactly `3(1 + w(z))`: the CPL exponent differentiates to
        ! `3(1 + w0 + wa) - 3 wa/x`, which is that same expression written out.
        w_z = p%w0 + p%wa * (x - 1.0_real64) / x
        v = 3.0_real64 * p%om0 * x ** 3 + 2.0_real64 * d%ok0 * x ** 2 &
            + 3.0_real64 * (1.0_real64 + w_z) * de
        if (d%ogamma0 == 0.0_real64) return

        dnu = cosmology_dnu_rel(p, d, x)
        rad = cosmology_nu_rel(p, d, x)
        v = v + d%ogamma0 * x ** 4 * (4.0_real64 * (1.0_real64 + rad) + dnu)

    end procedure cosmology_de2_dzeta

    module procedure cosmology_dnu_rel

        real(real64) :: total, u, y
        integer      :: i

        v = 0.0_real64
        if (d%tnu0 == 0.0_real64 .or. d%n_nu == 0 .or. d%n_massless == d%n_nu) return
        total = 0.0_real64
        do i = 1, d%n_nu
            if (p%m_nu(i) > 0.0_real64) then
                y = p%m_nu(i) / (pfc_k_b_ev * d%tnu0)
                u = (pfc_komatsu_c * y / x) ** pfc_komatsu_p
                total = total - u * (1.0_real64 + u) ** (pfc_komatsu_invp - 1.0_real64)
            end if
        end do
        v = pfc_komatsu_a * (p%neff / real(d%n_nu, real64)) * total

    end procedure cosmology_dnu_rel

    module procedure cosmology_e2

        real(real64) :: x, de, nu, nu_slope

        ! The NaN screen comes first and stands alone: every test below it is ordered.
        if (zeta /= zeta) then
            ! every public entry screens its redshift through `x_of_z` or `zeta_of_z` before reaching
            ! here, and the quadrature evaluates only at nodes interior to a finite panel, so this
            ! argument is never a NaN. The screen stays because an ordered comparison against one
            ! raises `IEEE_INVALID`, which is fatal under nagfor's default `-ieee=stop`.
            ! GCOVR_EXCL_START
            v = zeta
            return
            ! GCOVR_EXCL_STOP
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
        if (d%ogamma0 /= 0.0_real64) then
            ! **The Komatsu fit is two `pow` calls per massive species, and it is 42 ns of the
            ! 45 this whole kernel costs.** `%init` therefore tabulates it on the build grid --
            ! value and analytic slope at every node, read back by the same quintic the distance
            ! table uses -- and sets the flag on the INTEGRAND's copy of the derived record only.
            ! Every per-query path holds a record whose flag is false and evaluates the fit
            ! exactly, so `E(z)` remains astropy's formula evaluated rather than interpolated,
            ! which the guide page claims in so many words. Outside the tabulated range -- the age
            ! tail's scale factors, and the panels below the table that find the floor bounds --
            ! the fit is evaluated exactly whatever the flag says.
            if (d%use_nu_tab .and. zeta >= d%nu_zeta0 .and. zeta <= d%nu_zeta1) then
                call quintic_at(d%nu_tab, d%nu_slope, d%nu_n, d%nu_zeta0, zeta, nu, nu_slope)
            else
                nu = cosmology_nu_rel(p, d, x)
            end if
            v = v + d%ogamma0 * x ** 4 * (1.0_real64 + nu)
        end if

    end procedure cosmology_e2

    module procedure cosmology_growth_coef

        real(real64) :: e2, x

        ! The NaN screen comes first and stands alone, exactly as it does in the kernel.
        if (zeta /= zeta) then
            ! every public entry screens its redshift through `x_of_z` or `zeta_of_z` before reaching
            ! here, and the quadrature evaluates only at nodes interior to a finite panel, so this
            ! argument is never a NaN. The screen stays because an ordered comparison against one
            ! raises `IEEE_INVALID`, which is fatal under nagfor's default `-ieee=stop`.
            ! GCOVR_EXCL_START
            q = zeta
            src = zeta
            return
            ! GCOVR_EXCL_STOP
        end if
        e2 = cosmology_e2(p, d, zeta)
        if (e2 /= e2) then
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            q = e2
            src = e2
            return
            ! GCOVR_EXCL_STOP
        end if
        ! `E^2 <= 0` is a redshift the model does not reach, and an INFINITE `E^2` -- a big rip's
        ! dark-energy term at a deep blueshift -- would make `dlnE^2/dzeta` an `Infinity/Infinity`
        ! NaN a step later. Both answer the NaN that redshift deserves, here, once.
        if (e2 <= 0.0_real64 .or. e2 > huge(e2)) then
            q = ieee_value(q, ieee_quiet_nan)
            src = q
            return
        end if
        x = exp(zeta)
        q = 2.0_real64 - 0.5_real64 * cosmology_de2_dzeta(p, d, zeta) / e2
        src = 1.5_real64 * p%om0 * x ** 3 / e2

    end procedure cosmology_growth_coef

    module procedure cosmology_growth_step

        real(real64) :: qm, sm, q1, s1, k1, k2, k3, k4, f0, f1, f2, f3, hh

        hh = 0.5_real64 * h
        call cosmology_growth_coef(p, d, zeta + hh, qm, sm)
        call cosmology_growth_coef(p, d, zeta + h, q1, s1)
        f0 = f
        k1 = f0 * f0 + q0 * f0 - s0
        f1 = f0 + hh * k1
        k2 = f1 * f1 + qm * f1 - sm
        f2 = f0 + hh * k2
        k3 = f2 * f2 + qm * f2 - sm
        f3 = f0 + h * k3
        k4 = f3 * f3 + q1 * f3 - s1
        ! **The weighted MEAN slope, then one multiplication by `h`.** Written as
        ! `(h/6) * (k1 + ...)` instead, the Einstein-de Sitter case -- where every `k` is exactly
        ! zero and the four `f` are exactly one -- loses its exactness in `h/6` alone.
        f = f0 + h * ((k1 + 2.0_real64 * (k2 + k3) + k4) / 6.0_real64)
        ! `du/dzeta` is `1 - f` at every stage, so the second component needs no right-hand side
        ! of its own: its four slopes are the four `f` values the first component already formed.
        u = u + h * (1.0_real64 - (f0 + 2.0_real64 * (f1 + f2) + f3) / 6.0_real64)
        q0 = q1
        s0 = s1

    end procedure cosmology_growth_step

    module procedure cosmology_integrand_at

        real(real64) :: v2, zeta, b

        select case (which)
        case (PFC_INT_SOUND)
            ! `x` is `v`, and `b = sound_c sinh(v)`. The integrand is `b g(b^2)` in `b` times
            ! `db/dv`, so in `v` it is odd and analytic and vanishes like `v`; the origin is
            ! answered directly rather than by a division, exactly as the age tail's is.
            if (x /= x) then
                ! every public entry screens its redshift through `x_of_z` or `zeta_of_z` before reaching
                ! here, and the quadrature evaluates only at nodes interior to a finite panel, so this
                ! argument is never a NaN. The screen stays because an ordered comparison against one
                ! raises `IEEE_INVALID`, which is fatal under nagfor's default `-ieee=stop`.
                ! GCOVR_EXCL_START
                v = x
                return
                ! GCOVR_EXCL_STOP
            end if
            if (x <= 0.0_real64) then
                ! the Gauss-Kronrod nodes are strictly interior to the panel, so the substitution variable
                ! is never zero or negative here; the origin is a limit the panel never evaluates at.
                ! GCOVR_EXCL_START
                v = 0.0_real64
                return
                ! GCOVR_EXCL_STOP
            end if
            b = d%sound_c * sinh(x)
            v2 = cosmology_e2(p, d, -2.0_real64 * log(b))
            if (v2 /= v2) then
                ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
                ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
                ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
                ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
                ! because it is what keeps the ORDERED test beside it off a NaN.
                ! GCOVR_EXCL_START
                v = v2
                ! GCOVR_EXCL_STOP
            else if (v2 <= 0.0_real64) then
                ! reached only from the solver, whose bracket is built from bounds `%init` stored at the
                ! model's own floor, so no iterate reaches a `zeta` at which `E^2` is not positive.
                ! GCOVR_EXCL_START
                v = ieee_value(v, ieee_quiet_nan)
                ! GCOVR_EXCL_STOP
            else
                ! `sqrt(3 (1 + R0 b^2))` as `sqrt(3) hypot(1, sqrt(R0) b)`: the product `R0 b^2`
                ! is `R(z)`, the baryon-photon ratio at that redshift, and it overflows for a
                ! `Tcmb0` the module admits. `hypot` is exactly this case's intrinsic.
                v = 2.0_real64 * d%sound_c * cosh(x) &
                    / (b ** 3 * sqrt(v2) * sqrt(3.0_real64) &
                       * hypot(1.0_real64, d%sound_r0_root * b))
            end if
        case (PFC_INT_AGE_TAIL)
            ! `x` is `b = sqrt(a)`. The integrand vanishes like b^3 (with radiation) or b^2
            ! (without), so the origin is answered directly rather than by a division.
            if (x /= x) then
                ! every public entry screens its redshift through `x_of_z` or `zeta_of_z` before reaching
                ! here, and the quadrature evaluates only at nodes interior to a finite panel, so this
                ! argument is never a NaN. The screen stays because an ordered comparison against one
                ! raises `IEEE_INVALID`, which is fatal under nagfor's default `-ieee=stop`.
                ! GCOVR_EXCL_START
                v = x
                return
                ! GCOVR_EXCL_STOP
            end if
            if (x <= 0.0_real64) then
                ! the Gauss-Kronrod nodes are strictly interior to the panel, so the substitution variable
                ! is never zero or negative here; the origin is a limit the panel never evaluates at.
                ! GCOVR_EXCL_START
                v = 0.0_real64
                return
                ! GCOVR_EXCL_STOP
            end if
            zeta = -2.0_real64 * log(x)
            v2 = cosmology_e2(p, d, zeta)
            if (v2 /= v2) then
                ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
                ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
                ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
                ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
                ! because it is what keeps the ORDERED test beside it off a NaN.
                ! GCOVR_EXCL_START
                v = v2
                ! GCOVR_EXCL_STOP
            else if (v2 <= 0.0_real64) then
                ! reached only from the solver, whose bracket is built from bounds `%init` stored at the
                ! model's own floor, so no iterate reaches a `zeta` at which `E^2` is not positive.
                ! GCOVR_EXCL_START
                v = ieee_value(v, ieee_quiet_nan)
                ! GCOVR_EXCL_STOP
            else
                v = 2.0_real64 / (x * sqrt(v2))
            end if
        case default
            v2 = cosmology_e2(p, d, x)
            if (v2 /= v2) then
                ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
                ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
                ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
                ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
                ! because it is what keeps the ORDERED test beside it off a NaN.
                ! GCOVR_EXCL_START
                v = v2
                ! GCOVR_EXCL_STOP
            else if (v2 <= 0.0_real64) then
                ! `E^2 <= 0` is "no big bang": a quiet NaN, never a square root of a negative.
                v = ieee_value(v, ieee_quiet_nan)
            else if (which == PFC_INT_DISTANCE) then
                v = exp(x) / sqrt(v2)
            else if (which == PFC_INT_ABSORPTION) then
                ! `e^(3 zeta)/E`. Formed as the cube of `e^zeta` rather than as `exp(3*zeta)`,
                ! which would overflow one decade sooner and costs a second transcendental.
                v = exp(x) ** 3 / sqrt(v2)
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
            ! every public entry screens its redshift through `x_of_z` or `zeta_of_z` before reaching
            ! here, and the quadrature evaluates only at nodes interior to a finite panel, so this
            ! argument is never a NaN. The screen stays because an ordered comparison against one
            ! raises `IEEE_INVALID`, which is fatal under nagfor's default `-ieee=stop`.
            ! GCOVR_EXCL_START
            v = ieee_value(v, ieee_quiet_nan)
            return
            ! GCOVR_EXCL_STOP
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
            ! every public entry screens its redshift through `x_of_z` or `zeta_of_z` before reaching
            ! here, and the quadrature evaluates only at nodes interior to a finite panel, so this
            ! argument is never a NaN. The screen stays because an ordered comparison against one
            ! raises `IEEE_INVALID`, which is fatal under nagfor's default `-ieee=stop`.
            ! GCOVR_EXCL_START
            v = zeta
            return
            ! GCOVR_EXCL_STOP
        end if
        b_top = exp(-0.5_real64 * zeta)
        v = 0.0_real64
        do i = 1, PFC_AGE_PANELS
            lo = b_top * real(i - 1, real64) / real(PFC_AGE_PANELS, real64)
            hi = b_top * real(i, real64) / real(PFC_AGE_PANELS, real64)
            v = v + cosmology_panel(p, d, PFC_INT_AGE_TAIL, lo, hi)
        end do

    end procedure cosmology_age_tail

    module procedure cosmology_sound_tail

        real(real64) :: v_top, lo, hi
        integer      :: i, panels

        if (zeta /= zeta) then
            ! every public entry screens its redshift through `x_of_z` or `zeta_of_z` before reaching
            ! here, and the quadrature evaluates only at nodes interior to a finite panel, so this
            ! argument is never a NaN. The screen stays because an ordered comparison against one
            ! raises `IEEE_INVALID`, which is fatal under nagfor's default `-ieee=stop`.
            ! GCOVR_EXCL_START
            v = zeta
            return
            ! GCOVR_EXCL_STOP
        end if
        ! `b_top = e^(-zeta/2)` is the scale factor's square root at `zeta`, and `v_top` the `v`
        ! it sits at. The ratio cannot overflow: `b_top` is at most `1e5` over the domain and
        ! `sound_c` at least about `1e-161`, because a non-zero `Ogamma0` is at least the smallest
        ! subnormal.
        v_top = asinh(exp(-0.5_real64 * zeta) / d%sound_c)
        panels = max(1, min(PFC_SOUND_MAX_PANELS, ceiling(v_top / PFC_SOUND_PANEL)))
        v = 0.0_real64
        do i = 1, panels
            lo = v_top * real(i - 1, real64) / real(panels, real64)
            hi = v_top * real(i, real64) / real(panels, real64)
            v = v + cosmology_panel(p, d, PFC_INT_SOUND, lo, hi)
        end do

    end procedure cosmology_sound_tail

    ! =========================================================================================
    ! Domain screens and the table readers
    ! =========================================================================================

    !> `x = 1 + z` for a redshift inside the domain, or a quiet NaN for one outside it.
    !!
    !! **The closed-form bindings screen the caller's `z` and never take a logarithm.** They need
    !! `x`, not `zeta`: every term of `E^2` is a power of `x`, the neutrino fit is a function of
    !! `x`, and only the CPL factor wants `ln x` -- which is every model a caller writes down
    !! except the ones that set `w0` or `wa`. The round trip `z -> zeta -> x` costs a `log` and an
    !! `exp` for a quantity that is one addition away, and it is not free of error either: `exp`
    !! of a rounded logarithm is a few ulp from `1 + z`.
    !!
    !! The domain is the SAME domain the table screens against, stated in `z` instead of in
    !! `zeta`: see `PFC_Z_FLOOR`.
    pure function x_of_z(z) result(x)
        real(real64), intent(in) :: z !! redshift
        real(real64)             :: x !! `1 + z`, or NaN outside the domain

        if (z /= z) then
            x = z
            return
        end if
        if (z <= -1.0_real64 .or. z > PFC_Z_CEILING .or. z <= PFC_Z_FLOOR) then
            x = ieee_value(x, ieee_quiet_nan)
            return
        end if
        x = 1.0_real64 + z

    end function x_of_z

    module procedure cosmology_e2_x

        real(real64) :: de, nu

        call cosmology_e2_split(p, d, x, de, nu, v)

    end procedure cosmology_e2_x

    module procedure cosmology_e2_split

        de = de_term_of_x(p, x)
        nu = 0.0_real64
        if (de > huge(de)) then
            v = ieee_value(v, ieee_positive_inf)
            return
        end if
        v = p%om0 * x ** 3 + d%ok0 * x ** 2 + de
        if (d%ogamma0 /= 0.0_real64) then
            nu = cosmology_nu_rel(p, d, x)
            v = v + d%ogamma0 * x ** 4 * (1.0_real64 + nu)
        end if

    end procedure cosmology_e2_split

    !> `Ode0 f_DE` from `x`, taking `ln x` only where the CPL exponent needs it.
    pure function de_term_of_x(p, x) result(v)
        type(cosmology_params), intent(in) :: p !! the parameters
        real(real64), intent(in)           :: x !! `1 + z`
        real(real64)                       :: v !! `Ode0 f_DE`

        if (p%ode0 == 0.0_real64) then
            v = 0.0_real64
        else if (p%w0 == -1.0_real64 .and. p%wa == 0.0_real64) then
            v = p%ode0
        else
            v = p%ode0 * f_de_at(p, log(x), x)
        end if

    end function de_term_of_x

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

    !> One quintic Hermite piece of a forward table, and its derivative.
    !!
    !! **Six data, three nodes, one polynomial.** The table carries a value and an ANALYTIC first
    !! derivative at every node, so an interval has four data of its own and six with one
    !! neighbour -- enough to fix a quintic, whose interpolation error is
    !! `f^(6)(xi)/6! * (x - x_(i-1))^2 (x - x_i)^2 (x - x_(i+1))^2`, of order `h^6`. A cubic
    !! spline over the same nodes is `O(h^4)` and needs an end condition, which is what carried
    !! the module's worst error in the first interval above `z = 0`.
    !!
    !! It is `C^1` and not `C^2`: consecutive pieces share the node value and the node slope,
    !! both of which are data rather than something the interpolant chose. Nothing here needs a
    !! second derivative, and the one thing an end condition would have to invent is gone.
    !!
    !! **No abscissae and no search.** The nodes are the integer lattice times `PFC_H`, so the
    !! interval is one division and one `floor`; `zeta_0` is the table's bottom node. The stencil
    !! is the interval's LEFT neighbour, except in the first interval, where it is the right one
    !! -- the same polynomial degree, one-sided, and still matching value and slope at both ends
    !! of the interval it is used on.
    pure subroutine quintic_at(v, s, n, zeta_0, zeta, y, yp)
        real(real64), intent(in)  :: v(:)   !! the node values
        real(real64), intent(in)  :: s(:)   !! the node slopes, `d/dzeta`
        integer, intent(in)       :: n      !! how many nodes
        real(real64), intent(in)  :: zeta_0 !! the first node
        real(real64), intent(in)  :: zeta   !! where to evaluate, inside the table
        real(real64), intent(out) :: y      !! the value there
        real(real64), intent(out) :: yp     !! `dy/dzeta` there

        real(real64) :: t, ym, y0, y1, gm, g0, g1, aa, bb, cc, dd
        real(real64) :: c2, c3, c4, c5
        integer      :: i, mid

        ! The interval `[i, i+1]`, clamped so that an argument sitting exactly on either end of
        ! the table lands in a real interval rather than one past it.
        i = floor((zeta - zeta_0) / PFC_H) + 1
        if (i < 1) i = 1
        if (i > n - 1) i = n - 1
        ! The stencil's MIDDLE node. Taking the left neighbour puts the interval in `[0, 1]` of
        ! the local coordinate; the first interval has no left neighbour and sits in `[-1, 0]`.
        mid = i
        if (mid < 2) mid = 2
        t = (zeta - (zeta_0 + real(mid - 1, real64) * PFC_H)) / PFC_H

        ym = v(mid - 1)
        y0 = v(mid)
        y1 = v(mid + 1)
        gm = PFC_H * s(mid - 1)
        g0 = PFC_H * s(mid)
        g1 = PFC_H * s(mid + 1)
        aa = y1 - y0 - g0
        bb = ym - y0 + g0
        cc = g1 - g0
        dd = gm - g0
        c4 = 0.25_real64 * (cc - dd) - 0.5_real64 * (aa + bb)
        c2 = (aa + bb) - 0.25_real64 * (cc - dd)
        c5 = 0.25_real64 * (cc + dd) - 0.75_real64 * (aa - bb)
        c3 = 1.25_real64 * (aa - bb) - 0.25_real64 * (cc + dd)
        y = y0 + t * (g0 + t * (c2 + t * (c3 + t * (c4 + t * c5))))
        yp = (g0 + t * (2.0_real64 * c2 + t * (3.0_real64 * c3 &
             + t * (4.0_real64 * c4 + t * 5.0_real64 * c5)))) / PFC_H

    end subroutine quintic_at

    !> `ln D` and the growth rate `f` at one `zeta` inside the domain.
    !!
    !! The table covers the whole domain from above -- its top node is above `PFC_ZETA_CEILING`,
    !! whatever `zmax` is -- so the only fallback is DOWNWARD, below the bottom node `zmin` put
    !! there. That direction is the one the growing mode is an attractor in, so the fallback is
    !! the pass itself continued: the same substeps from the same tabulated pair, which makes the
    !! seam continuous by construction, exactly as the distance's panel walk is.
    !!
    !! It costs `PFC_GROWTH_SUBSTEPS` substeps per grid interval rather than one panel per unit,
    !! so a blueshift far below `zmin` is microseconds where a table read is nanoseconds. That is
    !! what `zmin=` is for.
    pure subroutine growth_at(this, zeta, lnd, f)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64), intent(out)       :: lnd  !! `ln D(zeta)`
        real(real64), intent(out)       :: f    !! `f(zeta) = dlnD/dlna`

        real(real64) :: slope, q0, s0, z0, hs, rem, u
        integer      :: i, k

        if (zeta >= this%d%zeta_gw_m) then
            call quintic_at(this%gwv, this%gwd, this%d%n_growth, this%d%zeta_gw_m, zeta, u, slope)
            lnd = u - zeta
            f = 1.0_real64 - slope
            return
        end if
        u = this%gwv(1)
        f = 1.0_real64 - this%gwd(1)
        z0 = this%d%zeta_gw_m
        hs = PFC_H / real(PFC_GROWTH_SUBSTEPS, real64)
        call cosmology_growth_coef(this%p, this%d, z0, q0, s0)
        ! Whole substeps first, then the remainder, so that the argument is landed on exactly and
        ! `z0` is formed from the bottom node rather than accumulated a step at a time.
        k = int((z0 - zeta) / hs)
        do i = 1, k
            call cosmology_growth_step(this%p, this%d, z0, -hs, q0, s0, f, u)
            z0 = this%d%zeta_gw_m - real(i, real64) * hs
        end do
        rem = zeta - z0
        if (rem < 0.0_real64) call cosmology_growth_step(this%p, this%d, z0, rem, q0, s0, f, u)
        lnd = u - zeta

    end subroutine growth_at

    !> `ln D` and `f` at a REDSHIFT, screened: the whole domain contract of the two bindings.
    !!
    !! A quiet NaN, raising no IEEE flag, outside the domain, for a model with no matter to grow
    !! and below the model's own floor. The floor is screened HERE rather than left to the
    !! arithmetic so that a redshift under it costs one comparison instead of several thousand
    !! substeps that all answer NaN.
    pure subroutine growth_pair(this, z, lnd, f)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: z    !! redshift, from the caller
        real(real64), intent(out)       :: lnd  !! `ln D(z)`, or NaN
        real(real64), intent(out)       :: f    !! `f(z)`, or NaN

        real(real64) :: zeta

        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            lnd = zeta
            f = zeta
            return
        end if
        if (.not. this%has_growth) then
            lnd = ieee_value(lnd, ieee_quiet_nan)
            f = lnd
            return
        end if
        if (this%d%zeta_floor > -PFC_ZETA_CEILING .and. zeta <= this%d%zeta_floor) then
            lnd = ieee_value(lnd, ieee_quiet_nan)
            f = lnd
            return
        end if
        call growth_at(this, zeta, lnd, f)

    end subroutine growth_pair

    !> `D_C(zeta)` in Mpc: the table inside its range, the panel rule outside it.
    !!
    !! The fallback starts from the TABULATED `D_N`, so the seam is continuous by construction.
    pure function dc_at(this, zeta) result(d)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64)                    :: d    !! `D_C` in Mpc

        real(real64) :: y, yp

        if (zeta > this%d%zeta_n) then
            d = this%d%d_n + this%d%dh * cosmology_walk(this%p, this%d, PFC_INT_DISTANCE, this%d%zeta_n, zeta)
        else if (zeta >= this%d%zeta_m) then
            call quintic_at(this%fv, this%fd, this%d%n_tab, this%d%zeta_m, zeta, y, yp)
            d = zeta * y
        else
            ! The downward fallback starts from the TABULATED bottom, exactly as the upward one
            ! starts from the tabulated top, so both seams are continuous by construction.
            d = this%d%d_m + this%d%dh &
                * cosmology_walk(this%p, this%d, PFC_INT_DISTANCE, this%d%zeta_m, zeta)
        end if

    end function dc_at

    !> `t_L(zeta)` in Gyr, the same way.
    pure function tl_at(this, zeta) result(t)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64)                    :: t    !! `t_L` in Gyr

        real(real64) :: y, yp

        if (zeta > this%d%zeta_n) then
            t = this%d%t_n + this%d%th * cosmology_walk(this%p, this%d, PFC_INT_TIME, this%d%zeta_n, zeta)
        else if (zeta >= this%d%zeta_m) then
            call quintic_at(this%gv, this%gd, this%d%n_tab, this%d%zeta_m, zeta, y, yp)
            t = zeta * y
        else
            t = this%d%t_m + this%d%th &
                * cosmology_walk(this%p, this%d, PFC_INT_TIME, this%d%zeta_m, zeta)
        end if

    end function tl_at

    !> The age at `zeta`, in Gyr.
    !!
    !! Three regimes, and the middle one is the whole point of the design: the age has its OWN
    !! table, so it is never `age(0) - t_L(z)` where that would cancel. Below the table the
    !! difference IS used, because `t_L` is negative at a blueshift and the two terms add.
    pure function age_at(this, zeta) result(t)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64)                    :: t    !! the age in Gyr

        real(real64) :: y, yp

        if (this%age_diverges) then
            t = ieee_value(t, ieee_positive_inf)
        else if (zeta > this%d%zeta_n) then
            t = this%d%th * cosmology_age_tail(this%p, this%d, zeta)
        else if (zeta >= this%d%zeta_m) then
            call quintic_at(this%av, this%ad, this%d%n_tab, this%d%zeta_m, zeta, y, yp)
            t = exp(y)
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
                ! `sqrt(Ok0) D_C / D_H` is bounded by the domain's own `zeta`, about 23, and
                ! `PFC_SINH_CEILING` is 700: wherever curvature dominates `E` the integrand is `1/sqrt(Ok0)`
                ! and the product is a logarithm of `1 + z`. Probed over the corner grid of
                ! `test_cosmology.f90`'s `build_extreme`, whose widest is about 24.
                ! GCOVR_EXCL_START
                dm = ieee_value(dm, ieee_positive_inf)
                ! GCOVR_EXCL_STOP
            else if (arg < -PFC_SINH_CEILING) then
                ! `sqrt(Ok0) D_C / D_H` is bounded by the domain's own `zeta`, about 23, and
                ! `PFC_SINH_CEILING` is 700: wherever curvature dominates `E` the integrand is `1/sqrt(Ok0)`
                ! and the product is a logarithm of `1 + z`. Probed over the corner grid of
                ! `test_cosmology.f90`'s `build_extreme`, whose widest is about 24.
                ! GCOVR_EXCL_START
                dm = ieee_value(dm, ieee_negative_inf)
                ! GCOVR_EXCL_STOP
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

    !> `D_C(z2) - D_C(z1)`: one integral for a close pair, the difference of two table reads for
    !! a distant one.
    !!
    !! **The WIDTH is formed from the redshifts, not from the two `zeta`s.** `zeta2 - zeta1`
    !! differences two rounded logarithms, so its relative error is `eps |zeta| / |zeta2 - zeta1|`
    !! -- `1.7e-12` for a pair a part in `1e4` apart, whatever is done with it afterwards.
    !! `ln(1 + (z2 - z1)/(1 + z1))` is the same width with no cancellation in it: for a close pair
    !! `z2 - z1` is EXACT by Sterbenz's lemma, and `pf_z2zeta` of a small argument is accurate to
    !! rounding. The panel is then laid from `zeta1` over that width, so the quantity the answer
    !! is proportional to carries every digit it has.
    pure function dc_pair(this, z1, z2, zeta1, zeta2) result(d)
        class(pf_cosmology), intent(in) :: this  !! the cosmology, known built
        real(real64), intent(in)        :: z1    !! the nearer redshift
        real(real64), intent(in)        :: z2    !! the farther one
        real(real64), intent(in)        :: zeta1 !! `zeta` of `z1`, known inside the domain
        real(real64), intent(in)        :: zeta2 !! `zeta` of `z2`, likewise
        real(real64)                    :: d     !! the comoving distance between them, in Mpc

        real(real64) :: w

        if (abs(zeta2 - zeta1) <= PFC_PAIR_DIRECT) then
            ! Directly: the difference of two tabulated distances keeps only the digits the pair's
            ! own span leaves, and a close pair spans almost none of it.
            w = pf_z2zeta((z2 - z1) / (1.0_real64 + z1))
            d = this%d%dh * panel_over(this%p, this%d, PFC_INT_DISTANCE, zeta1, w)
        else
            d = dc_at(this, zeta2) - dc_at(this, zeta1)
        end if

    end function dc_pair

    !> One 20-point Gauss-Legendre panel from `a` over a WIDTH, rather than to an upper bound.
    !!
    !! `cosmology_panel` takes two ends and forms `(b - a)/2`, which re-rounds a width that was
    !! accurate before it was added to `a`. Here the half-width is the caller's own number and
    !! only the ABSCISSAE carry `a`'s rounding, which shifts the sample points by an ulp of `a`
    !! and moves a smooth integrand by nothing.
    pure function panel_over(p, d, which, a, w) result(v)
        type(cosmology_params), intent(in)  :: p     !! the parameters
        type(cosmology_derived), intent(in) :: d     !! the derived values
        integer, intent(in)                 :: which !! which integrand
        real(real64), intent(in)            :: a     !! the panel's lower edge in `zeta`
        real(real64), intent(in)            :: w     !! its width, signed
        real(real64)                        :: v     !! the integral over it

        real(real64) :: half, mid, total
        integer      :: i

        half = 0.5_real64 * w
        mid = a + half
        total = 0.0_real64
        do i = 1, PFC_GL_N
            total = total + pfc_gl_w(i) * cosmology_integrand_at(p, d, which, mid + half * pfc_gl_x(i))
        end do
        v = half * total

    end function panel_over

    module procedure cosmology_comoving_distance_z1z2

        real(real64) :: zeta1, zeta2

        if (.not. this%ready) error stop "pf_cosmology%comoving_distance_z1z2: the cosmology is not initialised"
        zeta1 = zeta_of_z(z1)
        zeta2 = zeta_of_z(z2)
        if (zeta1 /= zeta1) then
            d = zeta1
        else if (zeta2 /= zeta2) then
            d = zeta2
        else
            d = dc_pair(this, z1, z2, zeta1, zeta2)
        end if

    end procedure cosmology_comoving_distance_z1z2

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
            ! distances: the latter is right only for a flat model. The difference itself comes
            ! from `dc_pair`, which is what makes a CLOSE pair accurate.
            d = dm_of_dc(this, dc_pair(this, z1, z2, zeta1, zeta2)) / exp(zeta2)
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
        ! The table needs `zeta`; `E^2` needs only `x`, which is one addition from the caller's
        ! own `z` rather than an `exp` of a rounded logarithm of it.
        e2 = cosmology_e2_x(this%p, this%d, 1.0_real64 + z)
        if (e2 /= e2) then
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            v = e2
            ! GCOVR_EXCL_STOP
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

        real(real64) :: x, v

        if (.not. this%ready) error stop "pf_cosmology%efunc: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            e = x
            return
        end if
        v = cosmology_e2_x(this%p, this%d, x)
        if (v /= v) then
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            e = v
            ! GCOVR_EXCL_STOP
        else if (v <= 0.0_real64) then
            e = ieee_value(e, ieee_quiet_nan)
        else
            e = sqrt(v)
        end if

    end procedure cosmology_efunc

    module procedure cosmology_inv_efunc

        real(real64) :: x, v

        if (.not. this%ready) error stop "pf_cosmology%inv_efunc: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            e = x
            return
        end if
        v = cosmology_e2_x(this%p, this%d, x)
        if (v /= v) then
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            e = v
            ! GCOVR_EXCL_STOP
        else if (v <= 0.0_real64) then
            e = ieee_value(e, ieee_quiet_nan)
        else
            e = 1.0_real64 / sqrt(v)
        end if

    end procedure cosmology_inv_efunc

    module procedure cosmology_hubble

        real(real64) :: x, v

        if (.not. this%ready) error stop "pf_cosmology%hubble: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            h = x
            return
        end if
        v = cosmology_e2_x(this%p, this%d, x)
        if (v /= v) then
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            h = v
            ! GCOVR_EXCL_STOP
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
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            v = e2
            ! GCOVR_EXCL_STOP
        else if (e2 <= 0.0_real64) then
            ! `E^2 <= 0` is "no big bang", which `%efunc` answers NaN for; so does every fraction
            ! of it.
            v = ieee_value(v, ieee_quiet_nan)
        else
            v = term / e2
        end if

    end function density_fraction

    module procedure cosmology_scale_factor

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%scale_factor: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            a = x
        else
            a = 1.0_real64 / x
        end if

    end procedure cosmology_scale_factor

    module procedure cosmology_otot

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%otot: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        ! `1 - Ok`, never the sum of the five: a flat model has `ok0` exactly zero by assignment,
        ! so this is exactly one, where the sum carries five roundings.
        v = 1.0_real64 - density_fraction(this%d%ok0 * x ** 2, cosmology_e2_x(this%p, this%d, x))

    end procedure cosmology_otot

    module procedure cosmology_ob

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%ob: the cosmology is not initialised"
        if (.not. this%has_ob0) then
            v = ieee_value(v, ieee_quiet_nan)
            return
        end if
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        v = density_fraction(this%p%ob0 * x ** 3, cosmology_e2_x(this%p, this%d, x))

    end procedure cosmology_ob

    module procedure cosmology_odm

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%odm: the cosmology is not initialised"
        if (.not. this%has_ob0) then
            v = ieee_value(v, ieee_quiet_nan)
            return
        end if
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        v = density_fraction((this%p%om0 - this%p%ob0) * x ** 3, cosmology_e2_x(this%p, this%d, x))

    end procedure cosmology_odm

    module procedure cosmology_nu_relative_density

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%nu_relative_density: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
        else
            v = cosmology_nu_rel(this%p, this%d, x)
        end if

    end procedure cosmology_nu_relative_density

    module procedure cosmology_onu_species

        real(real64) :: x, og, lead, u, y
        integer      :: i

        if (.not. this%ready) error stop "pf_cosmology%onu_species: the cosmology is not initialised"
        allocate (v(this%d%n_nu))
        if (this%d%n_nu == 0) return
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        ! `Ogamma(z)` first, so that the species sum to `%onu(z)` the way `%onu` forms it.
        og = density_fraction(this%d%ogamma0 * x ** 4, cosmology_e2_x(this%p, this%d, x))
        if (this%d%tnu0 == 0.0_real64) then
            ! No CMB switches neutrinos off entirely, a massive species included.
            v = 0.0_real64
            return
        end if
        lead = pfc_komatsu_a * (this%p%neff / real(this%d%n_nu, real64))
        do i = 1, this%d%n_nu
            if (this%p%m_nu(i) > 0.0_real64) then
                y = this%p%m_nu(i) / (pfc_k_b_ev * this%d%tnu0)
                u = (pfc_komatsu_c * y / x) ** pfc_komatsu_p
                v(i) = og * lead * (1.0_real64 + u) ** pfc_komatsu_invp
            else
                v(i) = og * lead
            end if
        end do

    end procedure cosmology_onu_species

    module procedure cosmology_tnu

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%tnu: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
        else
            v = this%d%tnu0 * x
        end if

    end procedure cosmology_tnu

    module procedure cosmology_om

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%om: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        v = density_fraction(this%p%om0 * x ** 3, cosmology_e2_x(this%p, this%d, x))

    end procedure cosmology_om

    module procedure cosmology_ode

        real(real64) :: x, e2, de, nu

        if (.not. this%ready) error stop "pf_cosmology%ode: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        ! The numerator comes back from the SAME call that built the denominator, so the ratio is
        ! one to a rounding wherever dark energy is all of `E^2` -- which is what keeps the five
        ! density parameters summing to one at a deep blueshift of a CPL model.
        call cosmology_e2_split(this%p, this%d, x, de, nu, e2)
        if (e2 /= e2) then
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            v = e2
            ! GCOVR_EXCL_STOP
        else if (e2 <= 0.0_real64) then
            v = ieee_value(v, ieee_quiet_nan)
        else if (e2 > huge(e2)) then
            ! The CPL factor overflowed, so dark energy is ALL of `E^2` and the limit is one.
            ! `Ode0 f_DE / E^2` would be `Infinity / Infinity` here: a NaN, and an `IEEE_INVALID`.
            v = 1.0_real64
        else
            v = de / e2
        end if

    end procedure cosmology_ode

    module procedure cosmology_ok

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%ok: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        ! A flat model has `ok0` exactly zero by assignment, so this is exactly zero too.
        v = density_fraction(this%d%ok0 * x ** 2, cosmology_e2_x(this%p, this%d, x))

    end procedure cosmology_ok

    module procedure cosmology_ogamma

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%ogamma: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        v = density_fraction(this%d%ogamma0 * x ** 4, cosmology_e2_x(this%p, this%d, x))

    end procedure cosmology_ogamma

    module procedure cosmology_onu

        real(real64) :: x, e2, de, nu

        if (.not. this%ready) error stop "pf_cosmology%onu: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        ! `Ogamma(z)` TIMES the fit, in that order, so that the five density parameters sum to one
        ! to a few ulp: the radiation term of `E^2` is `Ogamma0 x^4 (1 + nu_rel)`. The fit comes
        ! back from the same call that built `E^2`, for the reason `%ode` gives.
        call cosmology_e2_split(this%p, this%d, x, de, nu, e2)
        v = density_fraction(this%d%ogamma0 * x ** 4, e2) * nu

    end procedure cosmology_onu

    module procedure cosmology_tcmb

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%tcmb: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
        else
            v = this%p%tcmb0 * x
        end if

    end procedure cosmology_tcmb

    module procedure cosmology_w

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%w: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
        else if (this%p%wa == 0.0_real64) then
            ! Exactly `w0`, with no arithmetic to round it -- and it keeps `wa * z/(1 + z)` from
            ! being formed at all for the cosmological constant, where `z/(1 + z)` is `-1e5` at
            ! the blueshift end and `0 * (-1e5)` is only accidentally zero.
            v = this%p%w0
        else
            ! `z/(1 + z)` as astropy forms it, on the caller's own `z` rather than on `e^zeta`:
            ! it keeps its digits at a small `z`, where `1 - e^-zeta` would cancel. `x` IS
            ! `1 + z`, so this is that expression with the addition already done.
            v = this%p%w0 + this%p%wa * z / x
        end if

    end procedure cosmology_w

    module procedure cosmology_de_density_scale

        real(real64) :: x

        if (.not. this%ready) error stop "pf_cosmology%de_density_scale: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
        else if (this%p%w0 == -1.0_real64 .and. this%p%wa == 0.0_real64) then
            ! Exactly one for a cosmological constant, with no logarithm taken to find out.
            v = 1.0_real64
        else
            v = f_de_at(this%p, log(x), x)
        end if

    end procedure cosmology_de_density_scale

    module procedure cosmology_critical_density

        real(real64) :: x, e2

        if (.not. this%ready) error stop "pf_cosmology%critical_density: the cosmology is not initialised"
        x = x_of_z(z)
        if (x /= x) then
            v = x
            return
        end if
        e2 = cosmology_e2_x(this%p, this%d, x)
        if (e2 /= e2) then
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            v = e2
            ! GCOVR_EXCL_STOP
        else if (e2 <= 0.0_real64) then
            v = ieee_value(v, ieee_quiet_nan)
        else
            ! `rho_crit0` is already in M_sun/Mpc^3; `%init` did the conversion once.
            v = this%d%rho_crit0 * e2
        end if

    end procedure cosmology_critical_density

    !> The absorption distance at `zeta`: the table inside its range, the panel rule outside it.
    pure function xa_at(this, zeta) result(v)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64)                    :: v    !! the absorption distance there

        real(real64) :: y, yp

        if (zeta > this%d%zeta_n) then
            v = this%d%x_n + cosmology_walk(this%p, this%d, PFC_INT_ABSORPTION, this%d%zeta_n, zeta)
        else if (zeta >= this%d%zeta_m) then
            call quintic_at(this%xv, this%xd, this%d%n_tab, this%d%zeta_m, zeta, y, yp)
            v = zeta * y
        else
            v = this%d%x_m + cosmology_walk(this%p, this%d, PFC_INT_ABSORPTION, this%d%zeta_m, zeta)
        end if

    end function xa_at

    module procedure cosmology_absorption_distance

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%absorption_distance: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
        else
            v = xa_at(this, zeta)
        end if

    end procedure cosmology_absorption_distance

    module procedure cosmology_growth_factor

        real(real64) :: lnd, f

        if (.not. this%ready) error stop "pf_cosmology%growth_factor: the cosmology is not initialised"
        call growth_pair(this, z, lnd, f)
        ! `exp` of a quiet NaN is a quiet NaN and raises nothing, so the screens inside
        ! `growth_pair` are the whole of the domain contract for this binding.
        v = exp(lnd)

    end procedure cosmology_growth_factor

    module procedure cosmology_growth_rate

        real(real64) :: lnd

        if (.not. this%ready) error stop "pf_cosmology%growth_rate: the cosmology is not initialised"
        call growth_pair(this, z, lnd, v)

    end procedure cosmology_growth_rate

    !> `r_s(zeta)` in Mpc for a `zeta` known inside the domain: the panel rule, nothing tabulated.
    !!
    !! The whole of the sound horizon's domain contract in one place, so that `%sound_horizon` and
    !! the `%z_drag` solve cannot drift apart on what they admit.
    pure function rs_at(this, zeta) result(v)
        class(pf_cosmology), intent(in) :: this !! the cosmology, known built
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`, known inside the domain
        real(real64)                    :: v    !! `r_s` there, in Mpc

        if (.not. this%has_ob0) then
            ! No baryon density means no `R` and no sound speed, exactly as `%ob0()` itself is a
            ! NaN rather than a zero: a missing baryon fraction is not a zero one.
            v = ieee_value(v, ieee_quiet_nan)
        else if (this%d%ogamma0 <= 0.0_real64) then
            ! No photons, no sound. Screened BEFORE `R0` is formed, never by dividing by a zero
            ! `Ogamma0` and cleaning up the flag afterwards -- that division ends the process
            ! under nagfor's default `-ieee=stop`.
            v = 0.0_real64
        else if (this%d%zeta_floor > -PFC_ZETA_CEILING .and. zeta <= this%d%zeta_floor) then
            v = ieee_value(v, ieee_quiet_nan)
        else
            v = this%d%dh * cosmology_sound_tail(this%p, this%d, zeta)
        end if

    end function rs_at

    !> `d(-r_s)/dzeta = D_H e^zeta / (E sqrt(3 (1 + R0 e^(-zeta))))`, for the Newton step.
    !!
    !! The sound horizon FALLS with `zeta`, so its negative is the increasing quantity the solver
    !! inverts, and this is the analytic derivative OF THAT.
    pure function rs_neg_slope(this, zeta) result(s)
        class(pf_cosmology), intent(in) :: this !! the cosmology
        real(real64), intent(in)        :: zeta !! `ln(1 + z)`
        real(real64)                    :: s    !! `d(-r_s)/dzeta` in Mpc

        real(real64) :: v2

        v2 = cosmology_e2(this%p, this%d, zeta)
        if (v2 /= v2) then
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            s = v2
            ! GCOVR_EXCL_STOP
        else if (v2 <= 0.0_real64) then
            ! reached only from the solver, whose bracket is built from bounds `%init` stored at the
            ! model's own floor, so no iterate reaches a `zeta` at which `E^2` is not positive.
            ! GCOVR_EXCL_START
            s = ieee_value(s, ieee_quiet_nan)
            ! GCOVR_EXCL_STOP
        else
            s = this%d%dh * exp(zeta) / (sqrt(v2) * sqrt(3.0_real64) &
                * hypot(1.0_real64, this%d%sound_r0_root * exp(-0.5_real64 * zeta)))
        end if

    end function rs_neg_slope

    module procedure cosmology_sound_horizon

        real(real64) :: zeta

        if (.not. this%ready) error stop "pf_cosmology%sound_horizon: the cosmology is not initialised"
        zeta = zeta_of_z(z)
        if (zeta /= zeta) then
            v = zeta
        else
            v = rs_at(this, zeta)
        end if

    end procedure cosmology_sound_horizon

    module procedure cosmology_r_drag

        real(real64) :: h2, ocbh2, obh2, onuh2

        if (.not. this%ready) error stop "pf_cosmology%r_drag: the cosmology is not initialised"
        h2 = (this%p%h0 / 100.0_real64) ** 2
        ocbh2 = this%p%om0 * h2
        obh2 = this%p%ob0 * h2
        ! `obh2` is a quiet NaN when the caller gave no `ob0`, and an ordered comparison against
        ! one raises `IEEE_INVALID`, so the NaN is tested with `/=` as its own statement first.
        if (obh2 /= obh2) then
            v = obh2
            return
        end if
        ! The fit divides by a power of each, so a model with no baryons or no cold matter has no
        ! drag scale to report. `%sound_horizon` still answers for such a model: it is the FIT
        ! that has no value here, not the integral.
        if (obh2 <= 0.0_real64 .or. ocbh2 <= 0.0_real64) then
            v = ieee_value(v, ieee_quiet_nan)
            return
        end if
        ! `Onu h^2` is `SUM m_nu / 93.14 eV`, the MASSIVE species alone, which is what Aubourg et
        ! al. define. `%onu0() * h^2` is a different number -- it counts the relativistic species
        ! too, and is about `1.7e-05` where this is exactly zero -- and using it here would move
        ! `r_d` by about `1e-04`, which looks like the fit's own accuracy rather than a defect.
        onuh2 = sum(this%p%m_nu) / pfc_nu_mass_ev
        v = pfc_aubourg_a * exp(-pfc_aubourg_b * (onuh2 + pfc_aubourg_c) ** 2) &
            / (ocbh2 ** pfc_aubourg_p_cb * obh2 ** pfc_aubourg_p_b)

    end procedure cosmology_r_drag

    module procedure cosmology_z_drag

        real(real64) :: rd, lo, hi, r_lo, r_hi

        if (.not. this%ready) error stop "pf_cosmology%z_drag: the cosmology is not initialised"
        v = ieee_value(v, ieee_quiet_nan)
        rd = this%r_drag()
        if (rd /= rd) return
        ! A sound horizon that is identically zero never equals a finite `r_d`.
        if (this%d%ogamma0 <= 0.0_real64) return
        lo = pf_z2zeta(PFC_DRAG_Z_LO)
        hi = pf_z2zeta(PFC_DRAG_Z_HI)
        ! **The bracket is checked here and not left to the solver.** `solve_zeta` answers the END
        ! a target lies beyond, which is right for an inverse screened against its own stored
        ! bounds and wrong here: a model whose `r_d` is not reached between `z = 100` and `z = 1e5`
        ! has no drag epoch this bracket can name, and saying so is the honest answer.
        r_lo = rs_at(this, lo)
        r_hi = rs_at(this, hi)
        if (r_lo /= r_lo .or. r_hi /= r_hi) return
        if (rd > r_lo .or. rd < r_hi) return
        v = pf_zeta2z(solve_zeta(this, PFC_Q_SOUND, -rd, lo, hi))

    end procedure cosmology_z_drag

    module procedure cosmology_z_eq

        real(real64) :: or0

        if (.not. this%ready) error stop "pf_cosmology%z_eq: the cosmology is not initialised"
        if (this%d%ogamma0 <= 0.0_real64) then
            ! No radiation at all: matter dominates at every redshift and equality is never
            ! reached. Screened before the division rather than after the flag it would raise.
            v = ieee_value(v, ieee_positive_inf)
            return
        end if
        ! The RELATIVISTIC radiation density, because at equality every species is relativistic
        ! however massive it is today. `Ogamma0 + Onu0` is the other quantity and is a few per
        ! cent low for a model with a massive species -- silently, since the two agree exactly
        ! for a massless one.
        or0 = this%d%ogamma0 * (1.0_real64 + pfc_komatsu_a * this%p%neff)
        v = this%p%om0 / or0 - 1.0_real64

    end procedure cosmology_z_eq

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
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            s = v
            ! GCOVR_EXCL_STOP
        else if (v <= 0.0_real64) then
            ! reached only from the solver, whose bracket is built from bounds `%init` stored at the
            ! model's own floor, so no iterate reaches a `zeta` at which `E^2` is not positive.
            ! GCOVR_EXCL_START
            s = ieee_value(s, ieee_quiet_nan)
            ! GCOVR_EXCL_STOP
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
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
            ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
            ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
            ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
            ! because it is what keeps the ORDERED test beside it off a NaN.
            ! GCOVR_EXCL_START
            s = v
            ! GCOVR_EXCL_STOP
        else if (v <= 0.0_real64) then
            ! reached only from the solver, whose bracket is built from bounds `%init` stored at the
            ! model's own floor, so no iterate reaches a `zeta` at which `E^2` is not positive.
            ! GCOVR_EXCL_START
            s = ieee_value(s, ieee_quiet_nan)
            ! GCOVR_EXCL_STOP
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
            ! every public entry screens its redshift through `x_of_z` or `zeta_of_z` before reaching
            ! here, and the quadrature evaluates only at nodes interior to a finite panel, so this
            ! argument is never a NaN. The screen stays because an ordered comparison against one
            ! raises `IEEE_INVALID`, which is fatal under nagfor's default `-ieee=stop`.
            ! GCOVR_EXCL_START
            s = dc
            ! GCOVR_EXCL_STOP
        else if (this%flat) then
            s = 1.0_real64
        else if (this%d%ok0 > 0.0_real64) then
            arg = sqrt(this%d%ok0) * dc / this%d%dh
            if (abs(arg) > PFC_SINH_CEILING) then
                ! A NaN rather than an infinity, so that the solver BISECTS here instead of taking
                ! a zero-length Newton step and declaring itself converged where it is not.
                ! `sqrt(Ok0) D_C / D_H` is bounded by the domain's own `zeta`, about 23, and
                ! `PFC_SINH_CEILING` is 700: wherever curvature dominates `E` the integrand is `1/sqrt(Ok0)`
                ! and the product is a logarithm of `1 + z`. Probed over the corner grid of
                ! `test_cosmology.f90`'s `build_extreme`, whose widest is about 24.
                ! GCOVR_EXCL_START
                s = ieee_value(s, ieee_quiet_nan)
                ! GCOVR_EXCL_STOP
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

    !> Whether a stored screen bound may be compared against: it is a number, not a NaN.
    !!
    !! Every inverse asks this of each of its bounds, as its own statement, BEFORE the ordered
    !! comparison that screens the caller's argument. `%init` computes the bounds at the model's
    !! own floor, so none of them is a NaN today; this removes the class rather than the cause,
    !! because an ordered comparison against a quiet NaN raises `IEEE_INVALID` -- fatal under
    !! nagfor's default `-ieee=stop` -- and is false either way, so the screen would stop
    !! screening and hand an unreachable argument to the solver. `==` never raises on a NaN.
    pure function bound_usable(v) result(yes)
        real(real64), intent(in) :: v   !! a stored bound
        logical                  :: yes !! it is a number

        yes = v == v

    end function bound_usable

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

        real(real64) :: lo, hi, f_lo, f_hi, f_mid, slope, step, previous
        integer      :: i

        lo = lo_in
        hi = hi_in
        f_lo = value_at(this, which, lo) - target_value
        f_hi = value_at(this, which, hi) - target_value
        if (f_lo /= f_lo .or. f_hi /= f_hi) then
            ! An end of the bracket does not evaluate, so there is no bracket. Returning the
            ! midpoint of one that was never examined is how an unreachable argument became a
            ! redshift the model does not reach.
            ! every caller builds its bracket from stored bounds the model reaches, so neither
            ! end of it evaluates to a NaN.
            ! GCOVR_EXCL_START
            zeta = ieee_value(zeta, ieee_quiet_nan)
            return
            ! GCOVR_EXCL_STOP
        end if
        ! **The bracket is CHECKED, not assumed.** Every quantity here increases with `zeta`, so a
        ! bracket containing the target has `f_lo < 0 < f_hi`; when it does not, the target lies
        ! outside and the answer is the end it lies beyond, never the interior point a sign-blind
        ! iteration would walk to. The case that reaches this is a model whose `E^2` vanishes at a
        ! finite blueshift: the stored floor is taken with the adaptive rule and the forward
        ! quantity with the fixed panel rule, and the two disagree by the little the fixed rule
        ! loses to the inverse-square-root endpoint, which leaves a sliver of arguments the screen
        ! admits and the forward function does not quite reach.
        if (f_lo > 0.0_real64 .and. f_hi > 0.0_real64) then
            zeta = lo
            return
        end if
        if (f_lo < 0.0_real64 .and. f_hi < 0.0_real64) then
            zeta = hi
            return
        end if
        zeta = 0.5_real64 * (lo + hi)
        do i = 1, PFC_SOLVE_STEPS
            f_mid = value_at(this, which, zeta) - target_value
            if (f_mid /= f_mid) return
            if (f_mid == 0.0_real64) return
            ! The invariant the check above establishes is `f_lo <= 0 <= f_hi`, so an exact zero
            ! belongs with the LOW side. Filing it with the high side instead sends the bracket
            ! the wrong way whenever the target sits exactly on the lower end, and the solve then
            ! walks to the upper end: reachable both where the lower end is the model's own floor
            ! and where the quantity has saturated, which the blueshift end of a big-rip model's
            ! age does several units of `zeta` before the domain runs out.
            if ((f_lo <= 0.0_real64) .eqv. (f_mid <= 0.0_real64)) then
                lo = zeta
                f_lo = f_mid
            else
                hi = zeta
            end if
            slope = slope_at(this, which, zeta)
            previous = zeta
            if (slope /= slope) then
                ! `slope_at` answers a NaN only where `E^2` is not positive, which the bracket
                ! excludes; the bisection stays as the safe step for a slope that cannot be used.
                ! GCOVR_EXCL_START
                zeta = 0.5_real64 * (lo + hi)
                ! GCOVR_EXCL_STOP
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

        ! The loop returns on convergence, and the bracket halves at least every step, so the test
        ! above is met long before the count runs out; falling off the end is the shape that says
        ! `PFC_SOLVE_STEPS` was reached, which no model in the reference grid or in
        ! `build_extreme`'s corners does.
    end function solve_zeta   ! GCOVR_EXCL_LINE

    !> The quantity the solver is inverting, at `zeta`. Every one of the five INCREASES with
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
        case (PFC_Q_SOUND)
            ! The sound horizon falls as `zeta` rises, so its negative is the increasing one.
            v = -rs_at(this, zeta)
        case (PFC_Q_LOG_AGE)
            ! The age FALLS as `zeta` rises, so its negative logarithm is the increasing quantity.
            ! The logarithm is the point: at the top of the domain the age is `7.6e-18 Gyr` beside
            ! an `age(0)` of `13.8`, and only a relative measure of it has any digits left.
            t = age_at(this, zeta)
            if (t /= t) then
                ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns `+Infinity` as
                ! soon as the dark-energy term overflows, and inside the admitted parameter box (`|w0|` and
                ! `|wa|` at most 3, every density at most `1e6`, `x` at most `1e10`) no two terms of `E^2`
                ! can overflow with opposite signs, so `Infinity - Infinity` never forms. The screen stays
                ! because it is what keeps the ORDERED test beside it off a NaN.
                ! GCOVR_EXCL_START
                v = t
                ! GCOVR_EXCL_STOP
            else if (t <= 0.0_real64) then
                ! the age is positive throughout a model's own domain, and the bracket does not
                ! leave it.
                ! GCOVR_EXCL_START
                v = ieee_value(v, ieee_quiet_nan)
                ! GCOVR_EXCL_STOP
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
        case (PFC_Q_SOUND)
            s = rs_neg_slope(this, zeta)
        case (PFC_Q_LOG_AGE)
            ! `d(age)/dzeta` is exactly `-t_H/E`, because `age + t_L` is constant; so the slope of
            ! `-ln(age)` is the lookback time's own slope over the age.
            t = age_at(this, zeta)
            s = tl_slope(this, zeta)
            if (t /= t .or. s /= s) then
                ! reached only from the solver, whose bracket is built from bounds `%init` stored at the
                ! model's own floor, so no iterate reaches a `zeta` at which `E^2` is not positive.
                ! GCOVR_EXCL_START
                s = ieee_value(s, ieee_quiet_nan)
                ! GCOVR_EXCL_STOP
            else if (t <= 0.0_real64) then
                ! reached only from the solver, whose bracket is built from bounds `%init` stored at the
                ! model's own floor, so no iterate reaches a `zeta` at which `E^2` is not positive.
                ! GCOVR_EXCL_START
                s = ieee_value(s, ieee_quiet_nan)
                ! GCOVR_EXCL_STOP
            else
                s = s / t
            end if
        case default
            s = dc_slope(this, zeta)
        end select

    end function slope_at

    !> The `zeta` at a comoving distance INSIDE the inverse table, or `found = .false.`.
    !!
    !! The inverse table, then ONE Newton step on the FORWARD interpolant, which takes it from
    !! about `1e-9` to that interpolant's own inverse. Factored out because
    !! `%z_at_luminosity_distance` uses it for a BRACKET rather than for an answer: for
    !! `Ok0 >= 0` and `z >= 0` the luminosity distance is at least the comoving one, so the
    !! `zeta` at which `D_C` reaches `d` is an upper bound on the `zeta` at which `D_L` does.
    pure subroutine zeta_at_dc(this, d, zeta, found)
        class(pf_cosmology), intent(in) :: this  !! the cosmology
        real(real64), intent(in)        :: d     !! `D_C` in Mpc
        real(real64), intent(out)       :: zeta  !! the `zeta` there, or zero
        logical, intent(out)            :: found !! `d` was inside the inverse table

        real(real64) :: guess, slope, y, yp

        zeta = 0.0_real64
        found = this%d%has_d_inv
        if (found) found = d >= this%d%d_inv_bot .and. d <= this%d%d_inv_top
        if (.not. found) return
        guess = d * this%zd%eval(d)
        if (guess < this%d%zeta_d_bot) guess = this%d%zeta_d_bot
        if (guess > this%d%zeta_d_inv) guess = this%d%zeta_d_inv
        ! The step is taken on the INTERPOLANT, value and slope from one evaluation of it, so the
        ! answer is that interpolant's own inverse rather than an approximation to it.
        call quintic_at(this%fv, this%fd, this%d%n_tab, this%d%zeta_m, guess, y, yp)
        slope = y + guess * yp
        if (slope > 0.0_real64) then
            zeta = guess - (guess * y - d) / slope
        else
            zeta = guess
        end if

    end subroutine zeta_at_dc

    module procedure cosmology_z_at_distance

        real(real64) :: zeta, lo, hi, v_lo, v_hi
        logical      :: ok, found

        if (.not. this%ready) error stop "pf_cosmology%z_at_comoving_distance: the cosmology is not initialised"
        if (d /= d) then
            z = d
            return
        end if
        ! The bounds are tested for being NUMBERS before either is compared against; see
        ! `bound_usable`.
        if (.not. bound_usable(this%d%d_ceiling) .or. .not. bound_usable(this%d%d_floor)) then
            ! `%init` computes every stored bound at the model's own floor, so none of them is a NaN
            ! today; `bound_usable` removes the class rather than the cause, as its own doc-comment
            ! says. The screen stays because an ordered comparison against a NaN bound would stop
            ! screening and hand an unreachable argument to the solver.
            ! GCOVR_EXCL_START
            z = ieee_value(z, ieee_quiet_nan)
            return
            ! GCOVR_EXCL_STOP
        end if
        if (d > this%d%d_ceiling .or. d < this%d%d_floor) then
            z = ieee_value(z, ieee_quiet_nan)
            return
        end if
        call zeta_at_dc(this, d, zeta, found)
        if (.not. found) then
            call bracket_outside(this, PFC_Q_DC, d, lo, hi, v_lo, v_hi, ok)
            if (.not. ok) then
                ! `bracket_outside` stops its downward walk at `zeta_bottom` precisely so that no panel
                ! crosses the model's floor, which is the only thing that makes `cosmology_panel` answer a
                ! NaN; upward it stops at the domain's ceiling.
                ! GCOVR_EXCL_START
                z = ieee_value(z, ieee_quiet_nan)
                return
                ! GCOVR_EXCL_STOP
            end if
            zeta = solve_zeta(this, PFC_Q_DC, d, lo, hi)
        end if
        z = pf_zeta2z(zeta)

    end procedure cosmology_z_at_distance

    module procedure cosmology_z_at_lookback

        real(real64) :: zeta, guess, slope, lo, hi, v_lo, v_hi, y, yp
        logical      :: ok

        if (.not. this%ready) error stop "pf_cosmology%z_at_lookback_time: the cosmology is not initialised"
        if (t /= t) then
            z = t
            return
        end if
        if (.not. bound_usable(this%d%t_ceiling) .or. .not. bound_usable(this%d%t_floor)) then
            ! `%init` computes every stored bound at the model's own floor, so none of them is a NaN
            ! today; `bound_usable` removes the class rather than the cause, as its own doc-comment
            ! says. The screen stays because an ordered comparison against a NaN bound would stop
            ! screening and hand an unreachable argument to the solver.
            ! GCOVR_EXCL_START
            z = ieee_value(z, ieee_quiet_nan)
            return
            ! GCOVR_EXCL_STOP
        end if
        if (t > this%d%t_ceiling .or. t < this%d%t_floor) then
            z = ieee_value(z, ieee_quiet_nan)
            return
        end if
        if (this%d%has_t_inv .and. t >= this%d%t_inv_bot .and. t <= this%d%t_inv_top) then
            guess = t * this%zt%eval(t)
            if (guess < this%d%zeta_t_bot) guess = this%d%zeta_t_bot
            if (guess > this%d%zeta_t_inv) guess = this%d%zeta_t_inv
            call quintic_at(this%gv, this%gd, this%d%n_tab, this%d%zeta_m, guess, y, yp)
            slope = y + guess * yp
            if (slope > 0.0_real64) then
                zeta = guess - (guess * y - t) / slope
            else
                zeta = guess
            end if
        else
            call bracket_outside(this, PFC_Q_TL, t, lo, hi, v_lo, v_hi, ok)
            if (.not. ok) then
                ! `bracket_outside` stops its downward walk at `zeta_bottom` precisely so that no panel
                ! crosses the model's floor, which is the only thing that makes `cosmology_panel` answer a
                ! NaN; upward it stops at the domain's ceiling.
                ! GCOVR_EXCL_START
                z = ieee_value(z, ieee_quiet_nan)
                return
                ! GCOVR_EXCL_STOP
            end if
            zeta = solve_zeta(this, PFC_Q_TL, t, lo, hi)
        end if
        z = pf_zeta2z(zeta)

    end procedure cosmology_z_at_lookback

    module procedure cosmology_z_at_age

        real(real64) :: lo, hi, zeta, lt, guess, y, yp

        if (.not. this%ready) error stop "pf_cosmology%z_at_age: the cosmology is not initialised"
        if (t /= t) then
            z = t
            return
        end if
        if (.not. bound_usable(this%d%a_ceiling) .or. .not. bound_usable(this%d%a_floor)) then
            ! `%init` computes every stored bound at the model's own floor, so none of them is a NaN
            ! today; `bound_usable` removes the class rather than the cause, as its own doc-comment
            ! says. The screen stays because an ordered comparison against a NaN bound would stop
            ! screening and hand an unreachable argument to the solver.
            ! GCOVR_EXCL_START
            z = ieee_value(z, ieee_quiet_nan)
            return
            ! GCOVR_EXCL_STOP
        end if
        ! A model whose age integral diverges is infinitely old at EVERY redshift, so no age names
        ! one: the screen is the model's own flag rather than a comparison against an infinity.
        if (this%age_diverges .or. t <= 0.0_real64 .or. t < this%d%a_ceiling &
            .or. t > this%d%a_floor) then
            z = ieee_value(z, ieee_quiet_nan)
            return
        end if
        ! THE AGE'S OWN INVERSE TABLE, then one Newton step on the forward age table, whose
        ! slope is the analytic `d(ln age)/dzeta`. What is inverted is `-ln(age)`, which increases
        ! with `zeta`; the logarithm is the point, because at the top of the domain the age is
        ! `7.6e-18 Gyr` beside an `age(0)` of `13.8` and only a relative measure of it has any
        ! digits left.
        lt = log(t)
        if (this%d%has_age_inv .and. -lt >= this%d%a_inv_bot .and. -lt <= this%d%a_inv_top) then
            guess = this%za%eval(-lt)
            if (guess < this%d%zeta_a_bot) guess = this%d%zeta_a_bot
            if (guess > this%d%zeta_a_top) guess = this%d%zeta_a_top
            call quintic_at(this%av, this%ad, this%d%n_tab, this%d%zeta_m, guess, y, yp)
            if (yp < 0.0_real64) then
                zeta = guess - (y - lt) / yp
            else
                ! `yp` is the age table's own slope and the age decreases with `zeta` throughout,
                ! so it is negative at every node the inverse table reaches.
                ! GCOVR_EXCL_START
                zeta = guess
                ! GCOVR_EXCL_STOP
            end if
            z = pf_zeta2z(zeta)
            return
        end if
        ! The bracket comes from the three stored bounds, so no walk is needed to find one. The
        ! age DECREASES with `zeta`, which is why the tests read the way round they do.
        if (t >= this%d%age0) then
            ! `zeta_bottom`, not `-PFC_ZETA_CEILING`: below the model's own floor the age does not
            ! exist, and `a_floor` is the age AT that floor rather than at the domain's edge.
            lo = this%d%zeta_bottom
            hi = 0.0_real64
        else if (t >= this%d%a_n) then
            ! no model reached by the reference grid or by `build_extreme`'s corners, including
            ! one built with `zmax = 1e-6` whose table is a sliver, leaves the age inverse table
            ! without covering this bracket: the table is consulted first and answers.
            ! GCOVR_EXCL_START
            lo = 0.0_real64
            hi = this%d%zeta_n
            ! GCOVR_EXCL_STOP
        else
            lo = this%d%zeta_n
            hi = PFC_ZETA_CEILING
        end if
        ! On `-ln(age)`, never on `age(0) - t`: at the top of the domain that difference has no
        ! digit of the age left in it, and would answer an arbitrary redshift.
        zeta = solve_zeta(this, PFC_Q_LOG_AGE, -lt, lo, hi)
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

        real(real64) :: lo, hi, zeta, top, dm_top, dm_n
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
        if (.not. bound_usable(this%d%d_ceiling)) then
            ! `%init` computes every stored bound at the model's own floor, so none of them is a NaN
            ! today; `bound_usable` removes the class rather than the cause, as its own doc-comment
            ! says. The screen stays because an ordered comparison against a NaN bound would stop
            ! screening and hand an unreachable argument to the solver.
            ! GCOVR_EXCL_START
            z = ieee_value(z, ieee_quiet_nan)
            return
            ! GCOVR_EXCL_STOP
        end if
        if (this%d%ok0 >= 0.0_real64) then
            ! Flat or open: `D_L` is strictly increasing in `z >= 0`, since `e^zeta`, `D_C` and
            ! `sinh` all are. The domain itself is then a bracket, and the ceiling is read off
            ! the stored `D_C(zeta_ceiling)` rather than walked to.
            dm_top = dm_of_dc(this, this%d%d_ceiling)
            if (d > exp(PFC_ZETA_CEILING) * dm_top) then
                z = ieee_value(z, ieee_quiet_nan)
                return
            end if
            lo = 0.0_real64
            hi = PFC_ZETA_CEILING
            ! **A MUCH tighter bracket when the distance's inverse table reaches `d`.** For
            ! `Ok0 >= 0` and `z >= 0`, `D_M >= D_C` and `D_L = (1 + z) D_M >= D_C`, so `D_L`
            ! reaches `d` no later in `zeta` than `D_C` does: the `zeta` at which `D_C = d` is an
            ! upper bound, and it is one table read away.
            call zeta_at_dc(this, d, top, found)
            if (found .and. top > 0.0_real64 .and. top < hi) then
                hi = top
            else
                ! **Above the distance table the bracket comes from two logarithms instead.**
                ! `D_M` is squeezed between its value at the table's top and its value at the
                ! domain's ceiling -- `D_C` saturates towards the particle horizon, and `sinh` of
                ! a bounded argument is bounded -- and for a default table the two differ by about
                ! one per cent. So `D_L = e^zeta D_M` obeys
                !
                !   `D_L <= e^zeta D_M(ceiling)`   for every `zeta`, and
                !   `D_L >= e^zeta D_M(zeta_n)`    for `zeta >= zeta_n`,
                !
                ! which put the crossing between `ln(d / D_M(ceiling))` and
                ! `max(zeta_n, ln(d / D_M(zeta_n)))` -- a hundredth of a unit of `zeta` where the
                ! whole domain is twenty-three, and every step of a bisection across THAT is a
                ! panel walk past the table's edge. It is what makes `%z_at_distmod` usable above
                ! the comoving horizon, which is most of the range it is asked about: `D_L` is
                ! already twice the largest comoving distance by `z = 1`.
                if (d > dm_top) lo = log(d / dm_top)
                dm_n = dm_of_dc(this, this%d%d_n)
                if (dm_n > 0.0_real64) then
                    top = max(this%d%zeta_n, log(d / dm_n))
                    if (top < hi) hi = top
                end if
            end if
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
    pure subroutine bracket_outside(this, which, target_value, lo, hi, v_lo, v_hi, ok)
        class(pf_cosmology), intent(in) :: this         !! the cosmology
        integer, intent(in)             :: which        !! `PFC_Q_DC` or `PFC_Q_TL`, the two whose
                                                        !! quantity IS a tabulated integral
        real(real64), intent(in)        :: target_value !! the value to bracket
        real(real64), intent(out)       :: lo           !! the lower `zeta`
        real(real64), intent(out)       :: hi           !! the upper `zeta`
        real(real64), intent(out)       :: v_lo         !! the quantity at `lo`
        real(real64), intent(out)       :: v_hi         !! the quantity at `hi`
        logical, intent(out)            :: ok           !! a bracket was laid without a NaN in it

        real(real64) :: edge, v_edge, bot, v_bot, scale
        integer      :: i, panels, integrand

        ! The walk starts where the INVERSE table ends, which for a large `zmax` is short of the
        ! forward table's edge (see `strictly_increasing_prefix`). The quantity code is mapped to
        ! the INTEGRAND code here rather than being passed through, because the two vocabularies
        ! are not the same numbers.
        ! The walk starts where the INVERSE TABLE ends, at either end, and the two ends are
        ! whatever `increasing_run` reached. A model with no inverse table at all leaves both at
        ! the origin, where the quantity is zero: still a valid place to start walking from.
        if (which == PFC_Q_DC) then
            edge = this%d%zeta_d_inv
            v_edge = this%d%d_inv_top
            bot = this%d%zeta_d_bot
            v_bot = this%d%d_inv_bot
            scale = this%d%dh
            integrand = PFC_INT_DISTANCE
        else
            edge = this%d%zeta_t_inv
            v_edge = this%d%t_inv_top
            bot = this%d%zeta_t_bot
            v_bot = this%d%t_inv_bot
            scale = this%d%th
            integrand = PFC_INT_TIME
        end if
        panels = int(2.0_real64 * PFC_ZETA_CEILING / PFC_PANEL) + 2
        ok = .true.
        if (target_value > v_edge) then
            lo = edge
            v_lo = v_edge
            do i = 1, panels
                hi = min(lo + PFC_PANEL, PFC_ZETA_CEILING)
                v_hi = v_lo + scale * cosmology_panel(this%p, this%d, integrand, lo, hi)
                if (v_hi /= v_hi) then
                    ! `bracket_outside` stops its downward walk at `zeta_bottom` precisely so that no panel
                    ! crosses the model's floor, which is the only thing that makes `cosmology_panel` answer a
                    ! NaN; upward it stops at the domain's ceiling.
                    ! GCOVR_EXCL_START
                    ok = .false.
                    ! GCOVR_EXCL_STOP
                    return
                end if
                if (v_hi >= target_value .or. hi >= PFC_ZETA_CEILING) return
                lo = hi
                v_lo = v_hi
            end do
        else
            ! DOWNWARD the walk stops at `zeta_bottom`, the lowest `zeta` the stored floors reach,
            ! never at `-PFC_ZETA_CEILING`. A model whose `E^2` vanishes at a finite blueshift has
            ! no integrand below that point, and a panel laid across it sums to a NaN which the
            ! accumulator then carries to the domain's edge -- a bracket containing no crossing,
            ! handed to a solver that returned its midpoint.
            hi = bot
            v_hi = v_bot
            do i = 1, panels
                lo = max(hi - PFC_PANEL, this%d%zeta_bottom)
                v_lo = v_hi + scale * cosmology_panel(this%p, this%d, integrand, hi, lo)
                if (v_lo /= v_lo) then
                    ! `bracket_outside` stops its downward walk at `zeta_bottom` precisely so that no panel
                    ! crosses the model's floor, which is the only thing that makes `cosmology_panel` answer a
                    ! NaN; upward it stops at the domain's ceiling.
                    ! GCOVR_EXCL_START
                    ok = .false.
                    return
                    ! GCOVR_EXCL_STOP
                end if
                if (v_lo <= target_value .or. lo <= this%d%zeta_bottom) return
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

    module procedure cosmology_zmin
        if (.not. this%ready) error stop "pf_cosmology%zmin: the cosmology is not initialised"
        v = this%p%zmin
    end procedure cosmology_zmin

    module procedure cosmology_zeta_floor
        if (.not. this%ready) error stop "pf_cosmology%zeta_floor: the cosmology is not initialised"
        v = this%d%zeta_floor
    end procedure cosmology_zeta_floor

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
