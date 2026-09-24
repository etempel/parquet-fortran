!> Distances, times and volumes in an expanding universe: `pf_cosmology`, a cosmology built once
!! and evaluated any number of times, and the redshift conversions that go with it.
!!
!! One object carries a model and its tables:
!!
!! ```fortran
!! type(pf_cosmology) :: cosmo
!! call cosmo%init("Planck18")                          ! one of the eight named cosmologies
!! call cosmo%init(h0=70.0_real64, om0=0.3_real64)      ! flat LCDM, no radiation
!! dl = cosmo%luminosity_distance(z)                    ! Mpc, a whole column in one call
!! ```
!!
!! Six families, all over `real64`:
!!
!! * **Distances** -- `%comoving_distance`, `%comoving_transverse_distance`,
!!   `%luminosity_distance`, `%angular_diameter_distance`, `%comoving_distance_z1z2`,
!!   `%angular_diameter_distance_z1z2`, `%comoving_volume`, `%differential_comoving_volume`,
!!   `%lookback_distance`, `%absorption_distance` and `%distmod`, with
!!   `%comoving_distance_zeta` taking the table's own coordinate directly.
!! * **Times** -- `%lookback_time` and `%age`, in Gyr.
!! * **The expansion** -- `%efunc`, `%inv_efunc`, `%hubble`, `%scale_factor`, and the transverse
!!   scales `%kpc_proper_per_arcmin`, `%kpc_comoving_per_arcmin`, `%arcsec_per_kpc_proper` and
!!   `%arcsec_per_kpc_comoving`.
!! * **The contents of the universe at `z`** -- `%om`, `%ode`, `%ok`, `%ogamma`, `%onu`, which sum
!!   to one, with `%otot`, `%ob`, `%odm`, `%nu_relative_density`, `%onu_species`, `%tcmb`, `%tnu`,
!!   `%w`, `%de_density_scale` and `%critical_density`.
!! * **The growth of structure** -- `%growth_factor`, the linear growth factor `D(z)` normalised
!!   to `D(0) = 1`, and `%growth_rate`, `f = dlnD/dlna`.
!! * **The sound horizon** -- `%sound_horizon`, the comoving sound horizon at `z` in Mpc,
!!   `%r_drag`, the BAO scale, `%z_drag`, the drag epoch, and `%z_eq`, matter-radiation equality.
!! * **Inverses** -- `%z_at_comoving_distance`, `%z_at_lookback_time`, `%z_at_age`,
!!   `%z_at_luminosity_distance` and `%z_at_distmod`.
!!
!! plus the parameter queries (`%h0`, `%om0`, `%ode0`, `%ok0`, `%ogamma0`, `%onu0`, `%ob0`,
!! `%odm0`, `%tcmb0`, `%tnu0`, `%neff`, `%w0`, `%wa`, `%little_h`, `%hubble_distance`,
!! `%hubble_time`, `%zmax`, `%zmin`, `%zeta_floor`), the flags (`%is_flat`, `%has_massive_nu`,
!! `%is_initialised`), the four subroutines `%get_name`, `%describe`, `%m_nu` and `%clone`, and
!! `%clear`.
!!
!! Three free functions need no cosmology: `pf_z2zeta` and `pf_zeta2z` between a redshift and
!! `zeta = ln(1 + z)`, each exact to rounding at every argument where `log(1 + z)` and
!! `exp(zeta) - 1` are not, and `pf_z_combine`, which composes two redshifts.
!!
!! **The model is astropy's `w0waCDM`**, with every literal as astropy 8.0.1 writes it:
!! `E(z)^2 = Om0 x^3 + Ok0 x^2 + Ogamma0 x^4 [1 + nu_rel(z)] + Ode0 f_DE(z)` at `x = 1 + z`, the
!! CPL factor `f_DE = x^(3(1 + w0 + wa)) exp(-3 wa z/x)`, and Komatsu's fit for massive neutrinos.
!! `Om0` excludes massive neutrinos, which are counted in `Onu0`; `Tcmb0 = 0`, the default, switches
!! radiation and neutrinos off entirely, as astropy's `FlatLambdaCDM(H0, Om0)` does. So a
!! `"Planck18"` built here answers what astropy's `Planck18` answers, to about `1e-8`.
!! `tools/generate_cosmology_reference.py` derives the golden rows at 30 digits, and its
!! `--self-test` holds every literal below to the double nearest the derived value.
!!
!! **`%init` tabulates; every other binding reads.** Four integrals -- the comoving distance, the
!! lookback time, the age and the absorption distance -- are built over one uniform grid in
!! `zeta`, out of ONE set of interval integrals, and read back by a local quintic Hermite: a value
!! and an analytic slope at every node, so an interval has six data and the interpolation error is
!! `O(h^6)` with no end condition to get wrong. The grid is the integer lattice times `PFC_H`,
!! which puts a node at `zeta = 0` exactly and lets the interval be found by one division. A
!! redshift beyond either end of the table is answered by a fixed 20-point Gauss-Legendre rule
!! from that end, so **no redshift is ever refused and the table decides only how fast**: tens of
!! nanoseconds inside it, and microseconds to tens of microseconds outside, rising with the
!! distance past the edge. `zmax=` and `zmin=` move the two seams, and the answer by no more than
!! the table's own accuracy. `bench/benchmark_cosmology.sh` measures both.
!!
!! **The age is its own table, never `%age(0) - %lookback_time(z)`.** That difference cancels: at
!! `z = 1e10` the age is `7.6e-18 Gyr` while both terms are `13.7869 Gyr`, so not one digit of it
!! survives. Tabulated in its own right it is accurate to a few parts in `1e15` relative at every
!! redshift. A model whose age integral diverges -- de Sitter is one -- answers `+Infinity` at
!! every redshift, which is right rather than exceptional.
!!
!! **Total, not validating.** The redshift domain is `-1 < z <= 1e10`. Inside it every binding
!! answers a number, or a signed infinity where the mathematics says so. A NaN redshift answers
!! NaN; a redshift at or below `-1`, or above `1e10`, answers NaN quietly, raising no IEEE flag --
!! a catalogue's `-99` and `-1` sentinels are real inputs to an elemental call over a column, and
!! an abort there would take a program down for one row. A negative redshift above `-1` is a
!! blueshift and is answered with its sign. What aborts is a caller's mistake: a cosmology that was
!! never built, an unknown name, or a parameter outside its admitted range.
!!
!! **Growth is INTEGRATED rather than tabulated from a quadrature, and it is the one quantity
!! `zmax` does not move.** The growing mode is an attractor in the direction of time and only in
!! that direction, so the linear growth equation -- written as a Riccati equation for the rate `f`
!! alone, with `lnD` following by one quadrature -- is integrated DOWNWARD from
!! `PFC_GROWTH_TOP_NODE`, above the domain's ceiling, whatever the caller asked to tabulate. There
!! is no fallback above the table because there is no correct one to write: the same equation run
!! upward amplifies its own rounding by a factor of 25 by `z = 1e10`. The source term is `Om0`,
!! which excludes massive neutrinos, so for a model with a massive species this is the growth of
!! the cold matter in the scale-independent approximation; a universe with no matter at all has
!! nothing to grow and both bindings answer NaN for it.
!!
!! **A MODEL may end before the domain does.** Where `E(z)^2` reaches zero at a finite blueshift --
!! a recollapsing closed universe, or any negative `Ode0` -- there is no expansion below that
!! point, and every binding answers NaN there exactly as it does outside the domain. `%init`
!! builds such a model rather than refusing it, `%zeta_floor()` reports where it ends, and the
!! five inverses screen against bounds taken AT that floor, so an argument the model never reaches
!! answers NaN rather than a redshift at which the model itself answers NaN.
!!
!! **The no-flag guarantee is over `ieee_usual`** -- invalid, overflow and divide-by-zero -- and
!! not over `IEEE_UNDERFLOW`, which is outside that set. A denormal-adjacent argument such as
!! `z = 1e-300` with an extreme `h0` really does underflow something on the way to an answer that
!! is right, and screening for it would cost every ordinary call.
!!
!! **Three admitted inputs answer a signed infinity**, each through `ieee_value` and none raising a
!! flag: `%distmod(0)` is `-Infinity`, `%arcsec_per_kpc_*(0)` is `+Infinity`, and a divergent age,
!! an overflowing big-rip dark-energy factor or a `sinh` past its screen are `+Infinity`.
!!
!! **`real64` only**, like every numerical tier here; a `real32` caller converts at the call.
!!
!! **Arrow-free, settings-free and silent.** It reaches `parquet_integrate`, `parquet_interpolate`
!! and `parquet_utils` only (`check_parquet_cosmology_stays_arrow_free`), reads no knob and prints
!! nothing, so it re-exports no setting. `%init` and `%clear` are the only bindings that write an
!! object; every other one is `pure`, so a built cosmology may be evaluated from any number of
!! threads at once and two objects share nothing. The one hazard is the general one: a thread
!! reading an object another thread is re-`%init`ing is the caller's race, as it is for
!! `pf_interp_1d`.
module parquet_cosmology

    use iso_fortran_env, only : real64
    use parquet_integrate, only : pf_integrand, pf_tolerance, pf_integration_info, pf_integrate, &
                                  PF_INT_OK, PF_INT_DIVERGENT, PF_INT_BAD_VALUE, &
                                  PF_INT_ROUNDOFF, PF_INT_NO_CONVERGENCE
    use parquet_interpolate, only : pf_interp_1d
    use parquet_utils, only : pf_to_lower, pf_to_str

    implicit none
    private

    public :: pf_cosmology
    public :: pf_z2zeta, pf_zeta2z, pf_z_combine
    public :: parquet_debug_set_cosmology_max_neval, parquet_debug_cosmology_neval
    public :: parquet_debug_set_cosmology_exact_nu

    ! ---- The constants -----------------------------------------------------------------------
    !
    ! Private, and pinned by `tools/generate_cosmology_reference.py --self-test`, which holds each
    ! to the double nearest its own value. `c` appears once already as `skc_c_kms` in
    ! `parquet_skycoord_rotate.f90`; the two modules do not import each other and should not, so
    ! the value is repeated and each copy is pinned by its own generator.
    !
    ! THE RADIATION CHAIN IS ONLY RIGHT IN SI. `rho_gamma0 = 4 sigma_SB Tcmb0^4 / c^3` needs `c` in
    ! m/s and `rho_crit0 = 3 H0^2/(8 pi G)` needs `H0` in 1/s; substituting the km/s value of `c`
    ! is wrong by 1e27 and still produces a plausible small number, which is why the generator
    ! checks the derived `Ogamma0` and not only the literals below.

    real(real64), parameter :: pfc_c_kms = 299792.458_real64
        !! Speed of light in km/s, exact by definition. For `D_H = c / H0`.
    real(real64), parameter :: pfc_c_ms = 2.99792458e8_real64
        !! The same constant in m/s. For `rho_gamma0` and nothing else.
    real(real64), parameter :: pfc_g_si = 6.6743e-11_real64
        !! Newton's constant, CODATA 2022, m^3 kg^-1 s^-2. For `rho_crit0`.
    real(real64), parameter :: pfc_sigma_sb = 5.6703744191844314e-8_real64
        !! Stefan-Boltzmann constant, W m^-2 K^-4. For `rho_gamma0`.
    real(real64), parameter :: pfc_k_b_ev = 8.617333262145179e-5_real64
        !! Boltzmann's constant in eV/K. For the neutrino `y_i`.
    real(real64), parameter :: pfc_mpc_km = 3.0856775814913673e19_real64
        !! A megaparsec in km, IAU 2015. For `t_H` in Gyr.
    real(real64), parameter :: pfc_mpc_m = 3.0856775814913673e22_real64
        !! A megaparsec in m. For `H0` in 1/s, hence `rho_crit0`.
    real(real64), parameter :: pfc_gyr_s = 3.15576e16_real64
        !! A gigayear in seconds, on the Julian year. For `t_H`.
    real(real64), parameter :: pfc_nu_temp_ratio = 0.7137658555036082_real64
        !! `(4/11)^(1/3)`, the neutrino-to-photon temperature ratio. For `T_nu0`.
    real(real64), parameter :: pfc_gm_sun = 1.3271244e20_real64
        !! The IAU 2015 nominal solar mass parameter `GM_sun` in m^3 s^-2, exact as resolution B3
        !! writes it. The solar mass `%critical_density` reports in is this over `pfc_g_si`, which
        !! is how astropy derives its own `M_sun`; the choice is named in that binding's
        !! doc-comment because a critical density is quoted in three different units in the wild.

    real(real64), parameter :: pfc_komatsu_a = 0.22710731766_real64
        !! astropy's per-species relativistic neutrino density in units of the photon density.
    real(real64), parameter :: pfc_komatsu_p = 1.83_real64
        !! astropy's `KOMATSU_P`, the exponent inside the massive-neutrino fit.
    real(real64), parameter :: pfc_komatsu_invp = 0.54644808743_real64
        !! astropy's `KOMATSU_INVP`, AS WRITTEN: not `1/1.83`, so that the model IS astropy's.
    real(real64), parameter :: pfc_komatsu_c = 0.3173_real64
        !! astropy's scale inside the massive-neutrino fit.

    ! ---- The Aubourg et al. (2015) drag-scale fit, AS PUBLISHED ------------------------------
    !
    ! Physical Review D 92, 123516, equation (16). Every literal is the paper's own; the fit
    ! replaced Eisenstein and Hu's `z_drag` here after CAMB showed the older combination 2.5% out
    ! (`feature_cosmology_extra.md` section 3.2). `%r_drag` is the only binding that reads them,
    ! and `test/test_cosmology.f90` restates the whole formula from its own copies, so a
    ! transcribed exponent shows up as a disagreement rather than as a shared mistake.

    real(real64), parameter :: pfc_aubourg_a = 55.154_real64
        !! The fit's leading coefficient, in Mpc.
    real(real64), parameter :: pfc_aubourg_b = 72.3_real64
        !! The coefficient of the massive-neutrino exponential.
    real(real64), parameter :: pfc_aubourg_c = 0.0006_real64
        !! The offset inside it, in `Onu h^2`.
    real(real64), parameter :: pfc_aubourg_p_cb = 0.25351_real64
        !! The exponent on `Ocb h^2`, the cold dark matter and baryons together.
    real(real64), parameter :: pfc_aubourg_p_b = 0.12807_real64
        !! The exponent on `Ob h^2`.
    real(real64), parameter :: pfc_nu_mass_ev = 93.14_real64
        !! `SUM m_nu / 93.14 eV` is `Onu h^2`, the massive-neutrino density the fit is written in.
        !! It is NOT `%onu0() * h^2`, which counts the RELATIVISTIC species too and is about
        !! `1.7e-05 h^-2` for a massless model.

    ! ---- The eight named cosmologies, as astropy 8.0.1's realizations carry them ---------------

    integer, parameter :: PFC_N_NAMED = 8
        !! Named cosmologies `%init` accepts.
    integer, parameter :: PFC_NAMED_N_NU = 3
        !! `floor(neff)` for every one of them, hence the masses each carries.

    !> The canonical spellings, which `%get_name` returns; a caller's token is matched against
    !! these without regard to case.
    character(len=10), parameter :: pfc_named_name(PFC_N_NAMED) = [character(len=10) :: &
        "WMAP1", "WMAP3", "WMAP5", "WMAP7", "WMAP9", "Planck13", "Planck15", "Planck18"]
    !> `H0` in km/s/Mpc, in the order above.
    real(real64), parameter :: pfc_named_h0(PFC_N_NAMED) = [ &
        72.0_real64, 70.1_real64, 70.2_real64, 70.4_real64, 69.32_real64, 67.77_real64, &
        67.74_real64, 67.66_real64]
    !> `Om0`, excluding massive neutrinos, in the order above.
    real(real64), parameter :: pfc_named_om0(PFC_N_NAMED) = [ &
        0.257_real64, 0.276_real64, 0.277_real64, 0.272_real64, 0.2865_real64, 0.30712_real64, &
        0.3075_real64, 0.30966_real64]
    !> `Tcmb0` in K, in the order above.
    real(real64), parameter :: pfc_named_tcmb0(PFC_N_NAMED) = [ &
        2.725_real64, 2.725_real64, 2.725_real64, 2.725_real64, 2.725_real64, 2.7255_real64, &
        2.7255_real64, 2.7255_real64]
    !> `Neff`, in the order above.
    real(real64), parameter :: pfc_named_neff(PFC_N_NAMED) = [ &
        3.04_real64, 3.04_real64, 3.04_real64, 3.04_real64, 3.04_real64, 3.046_real64, &
        3.046_real64, 3.046_real64]
    !> `Ob0`, in the order above. Every named cosmology carries one.
    real(real64), parameter :: pfc_named_ob0(PFC_N_NAMED) = [ &
        0.0436_real64, 0.0454_real64, 0.0459_real64, 0.0455_real64, 0.04628_real64, &
        0.048252_real64, 0.0486_real64, 0.04897_real64]
    !> The species masses in eV, three per cosmology, model by model in the order above.
    real(real64), parameter :: pfc_named_mnu(PFC_NAMED_N_NU * PFC_N_NAMED) = [ &
        0.0_real64, 0.0_real64, 0.0_real64, &
        0.0_real64, 0.0_real64, 0.0_real64, &
        0.0_real64, 0.0_real64, 0.0_real64, &
        0.0_real64, 0.0_real64, 0.0_real64, &
        0.0_real64, 0.0_real64, 0.0_real64, &
        0.0_real64, 0.0_real64, 0.06_real64, &
        0.0_real64, 0.0_real64, 0.06_real64, &
        0.0_real64, 0.0_real64, 0.06_real64]

    ! ---- The admitted ranges -------------------------------------------------------------------
    !
    ! `E^2` is a sum of terms in bounded powers of `x = 1 + z`, so no evaluation order overflows
    ! once every density contribution and `h0` are bounded. With the ceilings below the largest
    ! admitted term is `1e6 * 1e40 = 1e46`, four decades below `huge`.

    real(real64), parameter :: PFC_Z_CEILING = 1.0e10_real64
        !! The largest redshift any binding answers; above it the answer is NaN.
    real(real64), parameter :: PFC_ZETA_CEILING = 23.02585093004047_real64
        !! `ln(1 + PFC_Z_CEILING)`: the domain's edge in the table's own coordinate. The true
        !! value is `23.02585093004045684`, and this is it rounded UP by a few units in the last
        !! place -- deliberately, so that `pf_z2zeta(PFC_Z_CEILING)` lands INSIDE the domain under
        !! any libm whose `log` is within an ulp of correct. Widening the admitted redshift by a
        !! relative `1e-14` costs nothing; a `z = 1e10` that answers NaN on one platform and a
        !! number on another would cost a great deal. (`23.025850929940457` is `ln(1e10)`, not
        !! `ln(1 + 1e10)`; the two differ by `1e-10`, which is exactly one panel edge out.)
    real(real64), parameter :: PFC_Z_FLOOR = -0.9999999999000001_real64
        !! The largest redshift the domain does NOT contain: every binding answers NaN at and
        !! below it. It is the largest double whose `pf_z2zeta` falls below `-PFC_ZETA_CEILING`,
        !! found by a one-ulp scan, so that the thirteen CLOSED-FORM bindings can screen the
        !! caller's `z` directly -- `z <= PFC_Z_FLOOR` -- and admit exactly the redshifts the
        !! table's own screen admits, without taking a logarithm to find out. The one double above
        !! it, `-0.9999999998999999917`, is inside. `the closed forms and the table share one
        !! domain` (`test/test_cosmology.f90`) asserts the two screens agree, which is also what
        !! would report a libm whose `log` at that point differs by an ulp from this one's.
    real(real64), parameter :: PFC_DENSITY_CEILING = 1.0e6_real64
        !! The largest admitted magnitude of any DERIVED density contribution.
    real(real64), parameter :: PFC_H0_MIN = 1.0e-10_real64
        !! The smallest admitted `h0`, which keeps `D_H` and `t_H` finite.
    real(real64), parameter :: PFC_H0_MAX = 1.0e10_real64
        !! The largest admitted `h0`.
    real(real64), parameter :: PFC_W_LIMIT = 3.0_real64
        !! `|w0|` and `|wa|` are admitted up to this, which bounds the CPL exponent by 15.
    real(real64), parameter :: PFC_EXP_CEILING = 709.0_real64
        !! Above this an `exp` would overflow, so the answer is `+Infinity` instead.
    real(real64), parameter :: PFC_DE_EXP_CEILING = 695.0_real64
        !! The CPL exponent's own ceiling, fourteen below `PFC_EXP_CEILING`. What has to stay
        !! finite is `Ode0 f_DE`, not `f_DE`, and `ode0` is admitted up to `PFC_DENSITY_CEILING`,
        !! whose logarithm is 13.8. Screening `f_DE` alone at 709 leaves the PRODUCT free to
        !! overflow, which answers the same `+Infinity` but raises `IEEE_OVERFLOW` on the way --
        !! fatal under nagfor's default `-ieee=stop`. No model reaches between the two ceilings by
        !! accident: the exponent passes both within a hair's breadth of `z = -1`.
    real(real64), parameter :: PFC_SINH_CEILING = 700.0_real64
        !! Above this a `sinh` would overflow, so the answer is `+Infinity` instead.
    real(real64), parameter :: PFC_Z1P_EXACT = 0.5_real64
        !! Above this `|z|`, `pf_z2zeta` answers `log(1 + z)`, which is correct to rounding there;
        !! below it, the `log1p` identity. The `2 atanh(z/(2 + z))` form is NOT uniformly accurate:
        !! `atanh`'s pole at 1 makes it `3.6e-9` out at `z = 1e10` and `2.3e-5` at `1e15`.
    real(real64), parameter :: PFC_VOLUME_SERIES = 1.0e-3_real64
        !! Below this `|Ok0| (D_M/D_H)^2` a curved `V_C` is formed by its series, not its closed
        !! form, which differences two quantities that agree to sixteen digits as `u -> 0`.

    ! ---- The tabulation ------------------------------------------------------------------------

    real(real64), parameter :: PFC_H = 0.008_real64
        !! The grid spacing in `zeta`. Fixed by design, not an argument.
    integer, parameter :: PFC_MIN_INTERVALS = 4
        !! The shortest table: `not_a_knot` needs four POINTS, and four intervals keeps two nodes
        !! strictly inside.
    real(real64), parameter :: PFC_DEFAULT_ZMAX = 1100.0_real64
        !! The default top of the table: recombination.
    real(real64), parameter :: PFC_DEFAULT_ZMIN = -0.9_real64
        !! The default bottom of the table. A blueshift OFF the table costs a panel walk, about
        !! twenty-seven times a table read, and the caller who never passes a negative redshift
        !! pays a third of the build time and a third of the memory for a floor they do not use.
        !! `-0.9` is the trade the review settled on: a millisecond, felt once per object, against
        !! a microsecond felt per row. `zmin=` is the knob for the caller who has measured.
    real(real64), parameter :: PFC_RTOL = 1.0e-12_real64
        !! The relative tolerance every interval integral is taken to.
    real(real64), parameter :: PFC_FLOOR_RTOL = 1.0e-8_real64
        !! The relative error estimate at which a panel of the two bounds AT THE DOMAIN'S FLOOR is
        !! accepted although `PFC_RTOL` was not certified. Those two are SCREENS rather than
        !! tabulated values -- they decide whether an argument is reachable at all, over distances
        !! of thousands of Mpc -- and the last panel of a model whose `E^2` vanishes carries an
        !! inverse-square-root endpoint on which the extrapolation runs out of double before it
        !! reaches `1e-12`: it returns an answer good to `1e-10` relative and says so in
        !! `info%abserr`, with a status that means "round-off, not misbehaviour". Refusing that
        !! panel costs the whole sliver of blueshifts below it; see `floor_bounds`.
    real(real64), parameter :: PFC_PANEL = 1.0_real64
        !! The width in `zeta` of one Gauss-Legendre panel beyond the table.
    integer, parameter :: PFC_GROWTH_SUBSTEPS = 2
        !! Runge-Kutta substeps per grid interval in the growth pass: the ODE step is
        !! `PFC_H / PFC_GROWTH_SUBSTEPS`.
        !!
        !! Measured against a 30-digit oracle over the whole domain, worst over four models and
        !! nine redshifts from `z = 1e10` to `z = -0.9`:
        !!
        !! | step | kernel evaluations | worst `D` | worst `f` |
        !! |---|---|---|---|
        !! | `PFC_H` | 5784 | 4.0e-11 | 1.8e-10 |
        !! | `PFC_H/2` | 11568 | 2.5e-12 | 1.1e-11 |
        !! | `PFC_H/4` | 23136 | 1.7e-13 | 7.1e-13 |
        !!
        !! Fourth order at `PFC_H/2` is six orders below the accuracy of the libraries a caller
        !! would otherwise reach for, and the `h^4` scaling above is clean to the last row, where
        !! accumulated rounding over 11568 steps starts to show.
    integer, parameter :: PFC_GROWTH_TOP_NODE = 2879
        !! The growth table's TOP node, as a multiple of `PFC_H`: the first lattice node above
        !! `PFC_ZETA_CEILING`, so the table covers the whole domain whatever `zmax` is.
        !!
        !! The growing mode is an attractor in the direction of time and only in that direction,
        !! so the pass runs DOWNWARD from here and there is no fallback above the table: the
        !! panels would have to run in the unstable direction, where the same equation amplifies
        !! its own rounding to `2.4e-07` by `z = 1100` and to `25` by `z = 1e10`.
    real(real64), parameter :: PFC_PAIR_DIRECT = 0.5_real64
        !! Below this separation in `zeta`, `%comoving_distance_z1z2` integrates the pair DIRECTLY
        !! instead of differencing two table reads.
        !!
        !! Measured against a 30-digit oracle, at `z1 = 5`: differencing loses the table's own
        !! relative error scaled by `D_C / (D_C2 - D_C1)`, so it is `2.5e-08` at a separation of
        !! `1e-8`, `5.3e-12` at `1e-4`, `2.3e-14` at `0.01` and `4.3e-15` at `0.5`, while the
        !! direct rule is `1e-16` to `3e-15` at every one of them. The direct rule is therefore
        !! the more accurate of the two at EVERY separation up to a panel's width, and the
        !! crossover is placed where the difference has fallen to a few parts in `1e15` rather
        !! than where it overtakes: past that point the extra decade costs twenty integrand
        !! evaluations against two table reads, and nothing asks for it.
        !!
        !! Below about `1e-6` neither form has digits to lose, because the SEPARATION does not:
        !! two redshifts a part in `1e8` apart pin `zeta2 - zeta1` to eight significant figures
        !! whatever is done with them afterwards.
    integer, parameter :: PFC_CONTEXT_CAP = 100
        !! Caller-supplied text is capped at this inside a message, as every module caps it.

    ! ---- Which integrand an integration is over ------------------------------------------------

    integer, parameter :: PFC_INT_DISTANCE = 1
        !! `e^zeta / E`, whose integral is `D_C / D_H`.
    integer, parameter :: PFC_INT_TIME = 2
        !! `1 / E`, whose integral is `t_L / t_H`.
    integer, parameter :: PFC_INT_AGE_TAIL = 3
        !! `2 / (b E(b^2))` in `b = sqrt(a)`, whose integral is the age past the table's edge.
    integer, parameter :: PFC_INT_ABSORPTION = 4
        !! `e^(3 zeta) / E`, whose integral is the absorption distance. In `z` it is
        !! `INT (1+z)^2/E dz`, and `dz = x dzeta` turns `x^2/E` into `x^3/E`.
    integer, parameter :: PFC_INT_SOUND = 5
        !! `2 c cosh(v) / (b^3 E(b^2) sqrt(3 (1 + R0 b^2)))` at `b = sound_c sinh(v)`, whose
        !! integral from the origin is the comoving sound horizon. See `cosmology_sound_tail` for
        !! why the variable is `v` and not `b` itself.

    !> The parameters a cosmology was built from, kept in their own component so that `%h0()`,
    !! `%om0()` and `%m_nu()` may be bindings: a binding may not share a name with a component.
    type :: cosmology_params
        real(real64) :: h0    = 0.0_real64   !! `H0` in km/s/Mpc
        real(real64) :: om0   = 0.0_real64   !! `Om0`, excluding massive neutrinos
        real(real64) :: ode0  = 0.0_real64   !! `Ode0`, derived when the caller omitted it
        real(real64) :: tcmb0 = 0.0_real64   !! `Tcmb0` in K; zero switches radiation off
        real(real64) :: neff  = 0.0_real64   !! `Neff`
        real(real64) :: ob0   = 0.0_real64   !! `Ob0`; NaN when the caller did not give one
        real(real64) :: w0    = -1.0_real64  !! `w0`
        real(real64) :: wa    = 0.0_real64   !! `wa`
        real(real64) :: zmax  = 0.0_real64   !! the `zmax` the CALLER asked for
        real(real64) :: zmin  = 0.0_real64   !! the `zmin` the CALLER asked for
        real(real64), allocatable :: m_nu(:) !! the species masses in eV, `floor(neff)` of them
    end type cosmology_params

    !> What `%init` derives, kept out of the bindings' way for the same reason.
    type :: cosmology_derived
        real(real64) :: ok0       = 0.0_real64 !! `Ok0`; exactly zero for a flat model
        real(real64) :: ogamma0   = 0.0_real64 !! `Ogamma0`
        real(real64) :: onu0      = 0.0_real64 !! `Onu0`
        real(real64) :: tnu0      = 0.0_real64 !! `T_nu0` in K
        real(real64) :: dh        = 0.0_real64 !! `D_H` in Mpc
        real(real64) :: th        = 0.0_real64 !! `t_H` in Gyr
        real(real64) :: age0      = 0.0_real64 !! `age(0)` in Gyr; `+Infinity` when it diverges
        real(real64) :: nu_rel0   = 0.0_real64 !! `nu_rel(0)`, the largest the fit reaches
        real(real64) :: zeta_n    = 0.0_real64 !! the table's top in `zeta`
        real(real64) :: zeta_m    = 0.0_real64 !! the table's BOTTOM in `zeta`, at or below zero
        real(real64) :: d_n       = 0.0_real64 !! `D_C(zeta_n)`, where the fallback starts
        real(real64) :: t_n       = 0.0_real64 !! `t_L(zeta_n)`, likewise
        real(real64) :: d_m       = 0.0_real64 !! `D_C(zeta_m)`, where the DOWNWARD fallback starts
        real(real64) :: t_m       = 0.0_real64 !! `t_L(zeta_m)`, likewise
        real(real64) :: x_n       = 0.0_real64 !! the absorption distance at `zeta_n`
        real(real64) :: x_m       = 0.0_real64 !! the absorption distance at `zeta_m`
        real(real64) :: d_ceiling = 0.0_real64 !! `D_C(+PFC_ZETA_CEILING)`, the inverse's top
        real(real64) :: d_floor   = 0.0_real64 !! `D_C(zeta_bottom)`, the inverse's bottom
        real(real64) :: t_ceiling = 0.0_real64 !! `t_L(+PFC_ZETA_CEILING)`
        real(real64) :: t_floor   = 0.0_real64 !! `t_L(zeta_bottom)`
        real(real64) :: zeta_floor = 0.0_real64 !! the largest `zeta < 0` where `E^2 <= 0`, else
                                                !! `-PFC_ZETA_CEILING`: the model's own bottom
        real(real64) :: zeta_bottom = 0.0_real64 !! the lowest `zeta` the three stored floors reach;
                                                 !! `zeta_floor` unless an integral stopped short
        real(real64) :: d_inv_top = 0.0_real64 !! the largest `D_C` the inverse TABLE covers
        real(real64) :: t_inv_top = 0.0_real64 !! the largest `t_L` the inverse table covers
        real(real64) :: zeta_d_inv = 0.0_real64 !! the `zeta` at `d_inv_top`
        real(real64) :: zeta_t_inv = 0.0_real64 !! the `zeta` at `t_inv_top`
        real(real64) :: d_inv_bot = 0.0_real64 !! the smallest `D_C` the inverse table covers
        real(real64) :: t_inv_bot = 0.0_real64 !! the smallest `t_L` the inverse table covers
        real(real64) :: zeta_d_bot = 0.0_real64 !! the `zeta` at `d_inv_bot`
        real(real64) :: zeta_t_bot = 0.0_real64 !! the `zeta` at `t_inv_bot`
        logical      :: has_d_inv = .false.    !! the distance inverse table was built
        logical      :: has_t_inv = .false.    !! the lookback time inverse table was built
        real(real64) :: a_inv_bot = 0.0_real64 !! the smallest `-ln(age)` the age inverse covers
        real(real64) :: a_inv_top = 0.0_real64 !! the largest one
        real(real64) :: zeta_a_bot = 0.0_real64 !! the `zeta` at `a_inv_bot`
        real(real64) :: zeta_a_top = 0.0_real64 !! the `zeta` at `a_inv_top`
        logical      :: has_age_inv = .false.  !! the age inverse table was built
        real(real64) :: rho_crit0 = 0.0_real64 !! `rho_crit(0)` in M_sun/Mpc^3
        real(real64) :: a_n       = 0.0_real64 !! `age(zeta_n)`, where the age's own fallback starts
        real(real64) :: a_ceiling = 0.0_real64 !! `age(+PFC_ZETA_CEILING)`: the SMALLEST age attained
        real(real64) :: a_floor   = 0.0_real64 !! `age(zeta_bottom)`: the LARGEST
        integer      :: n_nu      = 0          !! `floor(neff)`: the number of species
        integer      :: n_massless = 0         !! how many of them have zero mass
        integer      :: n_tab     = 0          !! nodes in the four forward tables
        integer      :: n_growth  = 0          !! nodes in the growth table, zero when there is none
        real(real64) :: zeta_gw_m = 0.0_real64 !! the growth table's BOTTOM node; its top is always
                                               !! `PFC_GROWTH_TOP_NODE * PFC_H`
        real(real64) :: sound_r0_root = 0.0_real64 !! `sqrt(R0)`, `R0 = 3 Ob0 / (4 Ogamma0)` the
                                               !! baryon-photon momentum ratio at `a = 1`. The
                                               !! ROOT and never `R0` itself, because `R0`
                                               !! overflows for an absurdly small `Ogamma0` while
                                               !! `sqrt(3 Ob0) / sqrt(4 Ogamma0)` does not, and
                                               !! because `sqrt(1 + R0 b^2)` is formed as
                                               !! `hypot(1, sqrt(R0) b)`. NaN without an `ob0`.
        real(real64) :: sound_c   = 1.0_real64 !! the sound horizon's integration scale in
                                               !! `b = sqrt(a)`: the tighter of `1/sqrt(R0)` and
                                               !! `sqrt(Or0/Om0)`. See `cosmology_sound_tail`.
        ! ---- the BUILD's own `nu_rel` table (R11) ----
        !
        ! Filled into the INTEGRAND's copy of this record and never into the object's, so that
        ! every per-query path evaluates the Komatsu fit exactly and `E(z)` stays astropy's
        ! formula rather than an interpolation of it. See `cosmology_e2`.
        real(real64), allocatable :: nu_tab(:)   !! `nu_rel` at the build grid's nodes
        real(real64), allocatable :: nu_slope(:) !! `dnu_rel/dzeta` there
        real(real64) :: nu_zeta0  = 0.0_real64 !! the first node of that grid
        real(real64) :: nu_zeta1  = 0.0_real64 !! the last one
        integer      :: nu_n      = 0          !! how many nodes
        logical      :: use_nu_tab = .false.   !! read the table instead of the fit
    end type cosmology_derived

    !> One of the three integrands, carrying the cosmology it is taken over.
    !!
    !! A `pf_integrand` extension rather than a bare function, because the integrand needs the
    !! model; `%eval`'s passed object is `intent(inout)`, as the abstract interface requires.
    type, extends(pf_integrand) :: cosmology_integrand
        type(cosmology_params)  :: p            !! the parameters
        type(cosmology_derived) :: d            !! the derived values
        integer :: which = PFC_INT_DISTANCE     !! `PFC_INT_DISTANCE`, `_TIME`, `_AGE_TAIL`,
                                                !! `_ABSORPTION` or `_SOUND`
    contains
        procedure :: eval => cosmology_integrand_eval !! The integrand at one point.
    end type cosmology_integrand

    !> A cosmology: a model, and the tables `%init` builds over it.
    !!
    !! Assignment deep-copies the tables, so `c2 = c1` is a clone. There is no finalizer: every
    !! allocatable component frees itself and `pf_interp_1d` has none.
    type :: pf_cosmology
        private
        type(cosmology_params)  :: p                    !! the parameters as given
        type(cosmology_derived) :: d                    !! what `%init` derived from them
        character(len=:), allocatable :: label          !! the canonical name, or the caller's
        ! The three FORWARD tables are quintic Hermite over the uniform grid: a value and an
        ! analytic slope at every node, and no abscissae, because the nodes are the integer
        ! lattice times `PFC_H` and the interval is found by one division. See `quintic_at`.
        real(real64), allocatable :: fv(:)              !! `D_C(zeta)/zeta` at the nodes
        real(real64), allocatable :: fd(:)              !! `d/dzeta` of it there
        real(real64), allocatable :: gv(:)              !! `t_L(zeta)/zeta` at the nodes
        real(real64), allocatable :: gd(:)              !! `d/dzeta` of it there
        real(real64), allocatable :: av(:)              !! `ln(age(zeta))` at the nodes
        real(real64), allocatable :: ad(:)              !! `d/dzeta` of it there
        real(real64), allocatable :: xv(:)              !! `X(zeta)/zeta` at the nodes, `X` the
                                                        !! absorption distance
        real(real64), allocatable :: xd(:)              !! `d/dzeta` of it there
        ! The growth table is the same shape and is read by the same quintic, but it is NOT on
        ! the same node range: it runs to `PFC_GROWTH_TOP_NODE` whatever `zmax` is.
        real(real64), allocatable :: gwv(:)             !! `ln D(zeta) + zeta` at the nodes, with
                                                        !! `D(0) = 1`, so the node at the origin is
                                                        !! exactly zero
        real(real64), allocatable :: gwd(:)             !! `d/dzeta` of it there, which is `1 - f`
        type(pf_interp_1d) :: zd                        !! `zeta / D` against `D`: the distance inverse
        type(pf_interp_1d) :: zt                        !! `zeta / t` against `t`: the time inverse
        type(pf_interp_1d) :: za                        !! `zeta` against `-ln(age)`: the age inverse
        logical :: flat         = .false.               !! `ok0 == 0` exactly, recorded
        logical :: massive_nu   = .false.               !! at least one species has a positive mass
        logical :: has_ob0      = .false.               !! the caller gave an `ob0`
        logical :: age_diverges = .false.               !! the age integral diverges
        logical :: has_growth   = .false.               !! the growth table was built; false when
                                                        !! `om0` is zero and there is nothing to grow
        logical :: ready        = .false.               !! `%init` has built this object
    contains
        procedure, private :: init_params => cosmology_init_params !! `%init` in the parameter form.
        procedure, private :: init_named  => cosmology_init_named  !! `%init` in the named form.
        generic :: init => init_params, init_named      !! Builds the cosmology; see the two bodies.
        procedure :: clone => cosmology_clone           !! A modified copy of this cosmology.
        procedure :: clear => cosmology_clear           !! Releases the tables; harmless on a fresh object.
        procedure :: comoving_distance => cosmology_comoving_distance !! `D_C` in Mpc.
        procedure :: comoving_distance_zeta => cosmology_comoving_distance_zeta !! `D_C` from `zeta`.
        procedure :: comoving_transverse_distance => cosmology_comoving_transverse !! `D_M` in Mpc.
        procedure :: luminosity_distance => cosmology_luminosity_distance !! `D_L` in Mpc.
        procedure :: angular_diameter_distance => cosmology_angular_diameter !! `D_A` in Mpc.
        procedure :: comoving_distance_z1z2 => cosmology_comoving_distance_z1z2
            !! `D_C` between two redshifts, in Mpc.
        procedure :: angular_diameter_distance_z1z2 => cosmology_angular_diameter_z1z2
                                                        !! `D_A` between two redshifts, in Mpc.
        procedure :: lookback_time => cosmology_lookback_time !! `t_L` in Gyr.
        procedure :: age => cosmology_age               !! The age of the universe at `z`, in Gyr.
        procedure :: efunc => cosmology_efunc           !! `E(z)`.
        procedure :: inv_efunc => cosmology_inv_efunc   !! `1 / E(z)`.
        procedure :: hubble => cosmology_hubble         !! `H(z)` in km/s/Mpc.
        procedure :: distmod => cosmology_distmod       !! The distance modulus in magnitudes.
        procedure :: comoving_volume => cosmology_comoving_volume !! `V_C` over the whole sky, Mpc^3.
        procedure :: differential_comoving_volume => cosmology_differential_volume
                                                        !! `dV_C/dz/dOmega` in Mpc^3 sr^-1.
        procedure :: kpc_proper_per_arcmin => cosmology_kpc_proper !! Proper kpc per arcminute.
        procedure :: kpc_comoving_per_arcmin => cosmology_kpc_comoving !! Comoving kpc per arcminute.
        procedure :: arcsec_per_kpc_proper => cosmology_arcsec_proper !! Arcseconds per proper kpc.
        procedure :: arcsec_per_kpc_comoving => cosmology_arcsec_comoving !! Arcseconds per comoving kpc.
        procedure :: scale_factor => cosmology_scale_factor !! `a(z) = 1/(1 + z)`.
        procedure :: om => cosmology_om                 !! `Om(z)`, the matter density parameter at `z`.
        procedure :: ode => cosmology_ode               !! `Ode(z)`, the dark-energy density parameter.
        procedure :: ok => cosmology_ok                 !! `Ok(z)`, the curvature density parameter.
        procedure :: ogamma => cosmology_ogamma         !! `Ogamma(z)`, the photon density parameter.
        procedure :: onu => cosmology_onu               !! `Onu(z)`, the neutrino density parameter.
        procedure :: otot => cosmology_otot             !! `Otot(z) = 1 - Ok(z)`.
        procedure :: ob => cosmology_ob                 !! `Ob(z)`, or NaN when no `ob0` was given.
        procedure :: odm => cosmology_odm               !! `Odm(z)`, or NaN when no `ob0` was given.
        procedure :: nu_relative_density => cosmology_nu_relative_density
            !! The Komatsu fit itself: `Onu(z)/Ogamma(z)`.
        procedure :: onu_species => cosmology_onu_species !! `Onu(z)` split by neutrino species.
        procedure :: tcmb => cosmology_tcmb             !! `T_CMB(z)` in K.
        procedure :: tnu => cosmology_tnu               !! `T_nu(z) = T_nu0 (1 + z)` in K.
        procedure :: w => cosmology_w                   !! `w(z)`, the dark-energy equation of state.
        procedure :: de_density_scale => cosmology_de_density_scale !! `f_DE(z)`, the CPL factor.
        procedure :: critical_density => cosmology_critical_density !! `rho_crit(z)` in M_sun/Mpc^3.
        procedure :: lookback_distance => cosmology_lookback_distance !! `c t_L(z)` in Mpc.
        procedure :: absorption_distance => cosmology_absorption_distance
            !! The dimensionless absorption distance out to `z`.
        procedure :: growth_factor => cosmology_growth_factor !! `D(z)`, normalised to `D(0) = 1`.
        procedure :: growth_rate => cosmology_growth_rate !! `f(z) = dlnD/dlna`.
        procedure :: sound_horizon => cosmology_sound_horizon !! `r_s(z)` in Mpc.
        procedure :: r_drag => cosmology_r_drag         !! The BAO scale `r_d` in Mpc, a fit.
        procedure :: z_drag => cosmology_z_drag         !! The drag epoch, where `r_s` is `r_d`.
        procedure :: z_eq => cosmology_z_eq             !! Matter-radiation equality.
        procedure :: z_at_comoving_distance => cosmology_z_at_distance !! The redshift at a `D_C`.
        procedure :: z_at_lookback_time => cosmology_z_at_lookback !! The redshift at a `t_L`.
        procedure :: z_at_age => cosmology_z_at_age     !! The redshift at which the universe was `t` old.
        procedure :: z_at_luminosity_distance => cosmology_z_at_luminosity !! The redshift at a `D_L`.
        procedure :: z_at_distmod => cosmology_z_at_distmod !! The redshift at a distance modulus.
        procedure :: hubble_distance => cosmology_hubble_distance !! `D_H` in Mpc.
        procedure :: hubble_time => cosmology_hubble_time !! `t_H` in Gyr.
        procedure :: h0 => cosmology_h0                 !! `H0` in km/s/Mpc.
        procedure :: little_h => cosmology_little_h     !! `H0 / 100`.
        procedure :: om0 => cosmology_om0               !! `Om0`.
        procedure :: ode0 => cosmology_ode0             !! `Ode0`.
        procedure :: ok0 => cosmology_ok0               !! `Ok0`.
        procedure :: ogamma0 => cosmology_ogamma0       !! `Ogamma0`.
        procedure :: onu0 => cosmology_onu0             !! `Onu0`.
        procedure :: ob0 => cosmology_ob0               !! `Ob0`, or NaN when none was given.
        procedure :: odm0 => cosmology_odm0             !! `Odm0 = Om0 - Ob0`, or NaN.
        procedure :: tcmb0 => cosmology_tcmb0           !! `Tcmb0` in K.
        procedure :: tnu0 => cosmology_tnu0             !! `T_nu0` in K.
        procedure :: neff => cosmology_neff             !! `Neff`.
        procedure :: w0 => cosmology_w0                 !! `w0`.
        procedure :: wa => cosmology_wa                 !! `wa`.
        procedure :: zmax => cosmology_zmax             !! The `zmax` the CALLER asked for.
        procedure :: zmin => cosmology_zmin             !! The `zmin` the CALLER asked for.
        procedure :: zeta_floor => cosmology_zeta_floor !! The `zeta` where `E^2` reaches zero below 0.
        procedure :: is_flat => cosmology_is_flat       !! `Ok0` is exactly zero.
        procedure :: has_massive_nu => cosmology_has_massive_nu !! A species has a positive mass.
        procedure :: is_initialised => cosmology_is_initialised !! Built; never aborts.
        procedure :: get_name => cosmology_get_name     !! The canonical name, or the caller's label.
        procedure :: describe => cosmology_describe     !! A one-line summary of the model.
        procedure :: m_nu => cosmology_m_nu             !! The species masses in eV.
    end type pf_cosmology

    ! ---- Test-only module state ------------------------------------------------------------
    !
    ! Both are written by `%init` alone and read by nothing the library computes with, so no
    ! `pure` binding touches them and the thread rule above is unaffected. Under concurrent
    ! builds the counter is the last build's, which is what its doc-comment says.

    logical, save :: pfc_exact_nu = .false.
        !! TEST-ONLY: `%init` evaluates the Komatsu fit exactly instead of tabulating it.
    integer, save :: pfc_max_neval = 0
        !! The `max_neval` `%init` passes each interval integral; 0 leaves the engine's own budget.
    integer, save :: pfc_neval = 0
        !! Integrand evaluations in the last `%init`, for `bench/benchmark_cosmology.f90`.

    ! ---- `%init` and the object's lifetime ----------------------------------------------------

    interface

        !> Builds a cosmology from its parameters.
        !!
        !! ```
        !! call cosmo%init(h0, om0, [ode0], [tcmb0], [neff], [m_nu], [ob0], [w0], [wa], [name], &
        !!                 [zmax], [zmin], [context])
        !! ```
        !!
        !! Optional arguments are in square brackets. `ode0` ABSENT means flat, and sets `Ok0 = 0`
        !! by assignment rather than by subtraction, so `%is_flat()` is exact. `tcmb0` defaults to
        !! zero, which switches radiation and neutrinos off entirely, as astropy does; `neff`
        !! defaults to `3.04`, and `m_nu` must carry `floor(neff)` masses when it is given. `name`
        !! is a free-text label the object carries for `%get_name` and `%describe`, default
        !! `"custom"`: it is NOT checked against the eight named cosmologies, so a caller may label
        !! a model anything. `zmax` sets the table's top, default `1100`, and `zmin` its bottom,
        !! default `-0.9` and admitted in `(-1, 0]`; nothing outside either is lost, only slower.
        !! `context` is appended to any abort message.
        !!
        !! A second `%init` on a built object replaces it entirely.
        module subroutine cosmology_init_params(this, h0, om0, ode0, tcmb0, neff, m_nu, ob0, w0, &
                                                wa, name, zmax, zmin, context)
            class(pf_cosmology), intent(out)       :: this    !! the cosmology to build
            real(real64),     intent(in)           :: h0      !! `H0` in km/s/Mpc, within `[1e-10, 1e10]`
            real(real64),     intent(in)           :: om0     !! `Om0`, finite and non-negative
            real(real64),     intent(in), optional :: ode0    !! `Ode0`; absent means flat
            real(real64),     intent(in), optional :: tcmb0   !! `Tcmb0` in K, non-negative; default 0
            real(real64),     intent(in), optional :: neff    !! `Neff`, non-negative; default 3.04
            real(real64),     intent(in), optional :: m_nu(:) !! species masses in eV, `floor(neff)` of them
            real(real64),     intent(in), optional :: ob0     !! `Ob0`, at most `om0`; absent means unknown
            real(real64),     intent(in), optional :: w0      !! `w0` within `[-3, 3]`; default -1
            real(real64),     intent(in), optional :: wa      !! `wa` within `[-3, 3]`; default 0
            character(len=*), intent(in), optional :: name    !! a label for the object; default "custom"
            real(real64),     intent(in), optional :: zmax    !! the table's top; default 1100
            real(real64),     intent(in), optional :: zmin    !! the table's bottom in `(-1, 0]`; default -0.9
            character(len=*), intent(in), optional :: context !! appended to any abort message
        end subroutine cosmology_init_params

        !> Builds one of the eight named cosmologies.
        !!
        !! ```
        !! call cosmo%init(name, [zmax], [zmin], [context])
        !! ```
        !!
        !! `name` is `Planck18`, `Planck15`, `Planck13`, `WMAP9`, `WMAP7`, `WMAP5`, `WMAP3` or
        !! `WMAP1`, matched without regard to case; `%get_name` returns the canonical spelling.
        !! The parameters are astropy 8.0.1's realizations. `zmax`, `zmin` and `context` are as
        !! they are for the parameter form.
        module subroutine cosmology_init_named(this, name, zmax, zmin, context)
            class(pf_cosmology), intent(out)       :: this    !! the cosmology to build
            character(len=*), intent(in)           :: name    !! one of the eight named cosmologies
            real(real64),     intent(in), optional :: zmax    !! the table's top; default 1100
            real(real64),     intent(in), optional :: zmin    !! the table's bottom in `(-1, 0]`; default -0.9
            character(len=*), intent(in), optional :: context !! appended to any abort message
        end subroutine cosmology_init_named

        !> A modified copy of this cosmology: every parameter the caller does not name is taken
        !! from this object, and the copy is built from scratch.
        !!
        !! ```
        !! call source%clone(out, [h0], [om0], [ode0], [tcmb0], [neff], [m_nu], [ob0], [w0], &
        !!                   [wa], [name], [zmax], [zmin], [context])
        !! ```
        !!
        !! Optional arguments are in square brackets. `call base%clone(c, h0=70.0_real64)` is the
        !! whole point: one line for "the same model with a different `H0`", where writing out
        !! `%init` again means reading eleven parameters back out of the source and passing them.
        !!
        !! **"Flat by omission" survives the copy.** `ode0` is passed on only when the source was
        !! built with one; a source that is flat because `ode0` was left out is cloned the same
        !! way, so `%is_flat()` stays a bit test that cannot fail and the clone of a flat model is
        !! flat whatever else changed. A flat source can be given a curvature by naming `ode0`
        !! here; a curved one cannot be made flat by omission, only by naming the `ode0` that
        !! makes it so.
        !!
        !! `neff` and `m_nu` travel together: `m_nu` must carry `floor(neff)` masses, so changing
        !! `neff` across a species boundary without giving new masses is refused by `%init` with
        !! its own message.
        module subroutine cosmology_clone(this, out, h0, om0, ode0, tcmb0, neff, m_nu, ob0, w0, &
                                          wa, name, zmax, zmin, context)
            class(pf_cosmology), intent(in)        :: this    !! the cosmology to copy
            type(pf_cosmology), intent(out)        :: out     !! the copy
            real(real64),     intent(in), optional :: h0      !! a new `H0`
            real(real64),     intent(in), optional :: om0     !! a new `Om0`
            real(real64),     intent(in), optional :: ode0    !! a new `Ode0`; makes the copy curved
            real(real64),     intent(in), optional :: tcmb0   !! a new `Tcmb0`
            real(real64),     intent(in), optional :: neff    !! a new `Neff`
            real(real64),     intent(in), optional :: m_nu(:) !! new species masses
            real(real64),     intent(in), optional :: ob0     !! a new `Ob0`
            real(real64),     intent(in), optional :: w0      !! a new `w0`
            real(real64),     intent(in), optional :: wa      !! a new `wa`
            character(len=*), intent(in), optional :: name    !! a new label
            real(real64),     intent(in), optional :: zmax    !! a new table top
            real(real64),     intent(in), optional :: zmin    !! a new table bottom
            character(len=*), intent(in), optional :: context !! appended to any abort message
        end subroutine cosmology_clone

        !> Releases the tables and returns the object to unbuilt. Harmless on a fresh object.
        module subroutine cosmology_clear(this)
            class(pf_cosmology), intent(inout) :: this !! the cosmology to release
        end subroutine cosmology_clear

        !> The canonical name of a named cosmology, or the label the caller gave, or `"custom"`.
        module subroutine cosmology_get_name(this, name)
            class(pf_cosmology), intent(in)            :: this !! the cosmology
            character(len=:), allocatable, intent(out) :: name !! the name
        end subroutine cosmology_get_name

        !> A one-line summary of the model, for a log or a plot title.
        !!
        !! The fields are always these, in this order, separated by `"; "` -- a semicolon, because
        !! `m_nu`'s own list uses commas:
        !!
        !! ```
        !! <name>; H0 = <h0> km/s/Mpc; Om0 = <om0>; Ode0 = <ode0>; Ok0 = <ok0>; Tcmb0 = <tcmb0> K;
        !! Neff = <neff>; m_nu = <m1>, <m2>, ... eV; Ob0 = <ob0>; w0 = <w0>; wa = <wa>
        !! ```
        !!
        !! `Ok0` reads `0 (flat)` for a flat model, `m_nu` reads `none` when there are no species,
        !! `Ob0` reads `unknown` when none was given, and the `w0`/`wa` pair is omitted entirely
        !! for a cosmological constant, which is every named cosmology.
        module subroutine cosmology_describe(this, text)
            class(pf_cosmology), intent(in)            :: this !! the cosmology
            character(len=:), allocatable, intent(out) :: text !! the summary
        end subroutine cosmology_describe

        !> The species masses in eV: `floor(neff)` of them, zeros where `m_nu` was not given.
        module subroutine cosmology_m_nu(this, masses)
            class(pf_cosmology), intent(in)         :: this      !! the cosmology
            real(real64), allocatable, intent(out)  :: masses(:) !! the masses
        end subroutine cosmology_m_nu

        !> Sets the evaluation budget each interval integral is given, for the table-not-converged
        !! scenario. TEST-ONLY: no library code calls it, and zero restores the engine's own budget.
        module subroutine parquet_debug_set_cosmology_max_neval(budget)
            integer, intent(in) :: budget !! evaluations per interval, or 0 for the engine's default
        end subroutine parquet_debug_set_cosmology_max_neval

        !> Makes `%init` evaluate the Komatsu neutrino fit EXACTLY inside its quadrature instead of
        !! reading the table it builds for it. TEST-ONLY: it exists so that one test can build the
        !! same model both ways and compare, which is the only way to see what the table costs.
        module subroutine parquet_debug_set_cosmology_exact_nu(exact)
            logical, intent(in) :: exact !! evaluate the fit exactly in the build's quadrature
        end subroutine parquet_debug_set_cosmology_exact_nu

        !> Integrand evaluations in the LAST `%init`, for `bench/benchmark_cosmology.f90`.
        !! Meaningless when several objects were built concurrently.
        module function parquet_debug_cosmology_neval() result(n)
            integer :: n !! the count
        end function parquet_debug_cosmology_neval

        !> The integrand at one point, selected by `%which`.
        module function cosmology_integrand_eval(this, x) result(f)
            class(cosmology_integrand), intent(inout) :: this !! the integrand and its cosmology
            real(real64), intent(in)                  :: x    !! `zeta`, or `b` for the age tail
            real(real64)                              :: f    !! the integrand there
        end function cosmology_integrand_eval

    end interface

    ! ---- The distances ---------------------------------------------------------------------

    interface

        !> The comoving distance to `z`, in Mpc. Hogg (1999) equation 15.
        pure elemental module function cosmology_comoving_distance(this, z) result(d)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: d    !! `D_C` in Mpc
        end function cosmology_comoving_distance

        !> The comoving distance at `zeta = ln(1 + z)`, the table's own coordinate, in Mpc.
        !! `%comoving_distance(z)` is this at `pf_z2zeta(z)`, to the bit.
        pure elemental module function cosmology_comoving_distance_zeta(this, zeta) result(d)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: zeta !! `ln(1 + z)`
            real(real64)                    :: d    !! `D_C` in Mpc
        end function cosmology_comoving_distance_zeta

        !> The comoving transverse distance to `z`, in Mpc. Hogg (1999) equation 16.
        pure elemental module function cosmology_comoving_transverse(this, z) result(d)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: d    !! `D_M` in Mpc
        end function cosmology_comoving_transverse

        !> The luminosity distance to `z`, in Mpc. Hogg (1999) equation 21.
        pure elemental module function cosmology_luminosity_distance(this, z) result(d)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: d    !! `D_L` in Mpc
        end function cosmology_luminosity_distance

        !> The angular diameter distance to `z`, in Mpc. Hogg (1999) equation 18.
        pure elemental module function cosmology_angular_diameter(this, z) result(d)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: d    !! `D_A` in Mpc
        end function cosmology_angular_diameter

        !> `D_C` between two redshifts, in Mpc: `D_C(z2) - D_C(z1)`, and NEGATIVE when `z2 < z1`.
        !!
        !! Not the same arithmetic at every separation. A CLOSE pair is integrated directly over
        !! `[zeta1, zeta2]`, because differencing two tabulated distances loses the table's own
        !! relative error scaled by how much of the distance the pair spans -- `2.5e-08` at a
        !! separation of `1e-8` in `zeta`, which is what an angular diameter distance between two
        !! galaxies in one group asks for. A distant pair is the difference of two table reads,
        !! which has stopped losing anything there and costs two reads instead of a walk.
        !! `PFC_PAIR_DIRECT` is the measured crossover.
        pure elemental module function cosmology_comoving_distance_z1z2(this, z1, z2) result(d)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z1   !! the nearer redshift
            real(real64), intent(in)        :: z2   !! the farther redshift
            real(real64)                    :: d    !! `D_C(z2) - D_C(z1)` in Mpc
        end function cosmology_comoving_distance_z1z2

        !> The angular diameter distance between two redshifts, in Mpc.
        !!
        !! `D_M(D_C(z2) - D_C(z1)) / (1 + z2)`, astropy's transverse-of-the-difference form, which
        !! is Hogg's equation 19 by the addition theorem and correct for a closed model too. It is
        !! NEGATIVE when `z2 < z1`, as astropy answers it, and exactly zero when they are equal.
        pure elemental module function cosmology_angular_diameter_z1z2(this, z1, z2) result(d)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z1   !! the nearer redshift
            real(real64), intent(in)        :: z2   !! the farther redshift
            real(real64)                    :: d    !! `D_A(z1, z2)` in Mpc
        end function cosmology_angular_diameter_z1z2

        !> The comoving volume out to `z`, over the whole sky, in Mpc^3. Hogg (1999) equation 29.
        !!
        !! A blueshift gives a signed volume, as the distances are signed there.
        pure elemental module function cosmology_comoving_volume(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `V_C` in Mpc^3
        end function cosmology_comoving_volume

        !> The comoving volume element `dV_C/dz/dOmega`, in Mpc^3 sr^-1. Hogg (1999) equation 28.
        pure elemental module function cosmology_differential_volume(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `dV_C/dz/dOmega` in Mpc^3 sr^-1
        end function cosmology_differential_volume

        !> The distance modulus at `z`, in magnitudes. Hogg (1999) equation 25.
        !!
        !! `5 log10(|D_L| / Mpc) + 25`, on the absolute value so that a blueshift has one.
        !! `%distmod(0)` is `-Infinity`, raising no flag.
        pure elemental module function cosmology_distmod(this, z) result(mu)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: mu   !! the distance modulus in magnitudes
        end function cosmology_distmod

    end interface

    ! ---- The times and the expansion --------------------------------------------------------

    interface

        !> The lookback time to `z`, in Gyr. Hogg (1999) equation 30.
        pure elemental module function cosmology_lookback_time(this, z) result(t)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: t    !! `t_L` in Gyr
        end function cosmology_lookback_time

        !> The age of the universe at `z`, in Gyr.
        !!
        !! From its own table, NEVER `%age(0) - %lookback_time(z)`, which cancels away every digit
        !! at high redshift. `+Infinity` at every `z` for a model whose age integral diverges.
        pure elemental module function cosmology_age(this, z) result(t)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: t    !! the age in Gyr
        end function cosmology_age

        !> `E(z)`, the expansion function: `H(z) / H0`.
        pure elemental module function cosmology_efunc(this, z) result(e)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: e    !! `E(z)`
        end function cosmology_efunc

        !> `1 / E(z)`.
        pure elemental module function cosmology_inv_efunc(this, z) result(e)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: e    !! `1 / E(z)`
        end function cosmology_inv_efunc

        !> `H(z) = H0 E(z)`, in km/s/Mpc. Named `%hubble` because Fortran cannot tell `%H` from `%h`.
        pure elemental module function cosmology_hubble(this, z) result(h)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: h    !! `H(z)` in km/s/Mpc
        end function cosmology_hubble

        !> Proper transverse kpc per arcminute at `z`.
        pure elemental module function cosmology_kpc_proper(this, z) result(s)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: s    !! kpc per arcminute
        end function cosmology_kpc_proper

        !> Comoving transverse kpc per arcminute at `z`.
        pure elemental module function cosmology_kpc_comoving(this, z) result(s)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: s    !! kpc per arcminute
        end function cosmology_kpc_comoving

        !> Arcseconds per proper transverse kpc at `z`. `+Infinity` at `z = 0`, raising no flag.
        pure elemental module function cosmology_arcsec_proper(this, z) result(s)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: s    !! arcseconds per kpc
        end function cosmology_arcsec_proper

        !> Arcseconds per comoving transverse kpc at `z`. `+Infinity` at `z = 0`.
        pure elemental module function cosmology_arcsec_comoving(this, z) result(s)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: s    !! arcseconds per kpc
        end function cosmology_arcsec_comoving

    end interface

    ! ---- The density parameters at a redshift, and what goes with them ------------------------
    !
    ! Each is a term of `E^2` over `E^2`, formed at the SAME `x = e^zeta` the kernel uses, so
    ! `%om + %ok + %ogamma + %onu + %ode` is one to within a few ulp at every redshift. Where the
    ! CPL factor has overflowed -- a big-rip model approaching `z = -1` -- `E^2` is `+Infinity` and
    ! the five answer the limit, `%ode(z) = 1` and the other four zero, rather than `Infinity /
    ! Infinity`, which would be a NaN and an `IEEE_INVALID`.

    interface

        !> The scale factor at `z`: `a = 1/(1 + z)`, normalised to one today.
        !!
        !! NaN outside the domain, as every binding is; `a` is `1e-10` at the ceiling and about
        !! `1e10` at the floor, and neither overflows.
        pure elemental module function cosmology_scale_factor(this, z) result(a)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: a    !! `1/(1 + z)`
        end function cosmology_scale_factor

        !> `Om(z) = Om0 (1+z)^3 / E(z)^2`, the matter density parameter at `z`.
        !!
        !! Massive neutrinos are NOT counted here; they are in `%onu`, as astropy counts them.
        pure elemental module function cosmology_om(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `Om(z)`
        end function cosmology_om

        !> `Ode(z) = Ode0 f_DE(z) / E(z)^2`, the dark-energy density parameter at `z`.
        pure elemental module function cosmology_ode(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `Ode(z)`
        end function cosmology_ode

        !> `Ok(z) = Ok0 (1+z)^2 / E(z)^2`. Exactly zero at every `z` for a flat model.
        pure elemental module function cosmology_ok(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `Ok(z)`
        end function cosmology_ok

        !> `Ogamma(z) = Ogamma0 (1+z)^4 / E(z)^2`, the PHOTON density parameter: neutrinos are
        !! `%onu`. Exactly zero at every `z` when `Tcmb0` is zero.
        pure elemental module function cosmology_ogamma(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `Ogamma(z)`
        end function cosmology_ogamma

        !> `Onu(z) = Ogamma(z) nu_rel(z)`, the neutrino density parameter at `z`.
        !!
        !! `nu_rel` is Komatsu's fit, so a massive species crosses over from `x^4` scaling to `x^3`
        !! as it becomes non-relativistic. Exactly zero at every `z` when `Tcmb0` is zero, which
        !! switches neutrinos off entirely whatever `m_nu` says.
        pure elemental module function cosmology_onu(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `Onu(z)`
        end function cosmology_onu

        !> `Otot(z) = 1 - Ok(z)`, the total density parameter at `z`.
        !!
        !! EXACTLY one at every `z` for a flat model, because `%ok` is exactly zero there by
        !! assignment rather than by subtraction. Formed as `1 - Ok` and never as the sum of the
        !! five, which would carry their rounding.
        pure elemental module function cosmology_otot(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `Otot(z)`
        end function cosmology_otot

        !> `Ob(z) = Ob0 (1+z)^3 / E(z)^2`, the baryon density parameter at `z`.
        !!
        !! NaN at every `z` when no `ob0` was given, exactly as `%ob0()` is: a missing baryon
        !! fraction is not a zero one. astropy 8 defaults `Ob0` to zero and answers a number.
        pure elemental module function cosmology_ob(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `Ob(z)`, or NaN
        end function cosmology_ob

        !> `Odm(z) = (Om0 - Ob0) (1+z)^3 / E(z)^2`, the cold-dark-matter density parameter.
        !!
        !! NaN at every `z` when no `ob0` was given, as `%odm0()` is.
        pure elemental module function cosmology_odm(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `Odm(z)`, or NaN
        end function cosmology_odm

        !> Komatsu's fit itself: the neutrino density in units of the PHOTON density at `z`.
        !!
        !! astropy spells it `nu_relative_density`. `%onu(z)` is `%ogamma(z)` times this, and for
        !! a massless species it is the constant `0.22710731766 Neff`. Zero when `Tcmb0` is zero.
        pure elemental module function cosmology_nu_relative_density(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `nu_rel(z)`
        end function cosmology_nu_relative_density

        !> `Onu(z)` split by species: `floor(neff)` of them, summing to `%onu(z)`.
        !!
        !! A SUBROUTINE with an allocatable result, because an `elemental` function cannot return
        !! `floor(neff)` values per element. `v` comes back with one entry per species, in the
        !! order `%m_nu` reports the masses, and a model with `neff < 1` gets a zero-length array
        !! rather than an unallocated one.
        module subroutine cosmology_onu_species(this, z, v)
            class(pf_cosmology), intent(in)        :: this !! the cosmology
            real(real64), intent(in)               :: z    !! redshift
            real(real64), allocatable, intent(out) :: v(:) !! `Onu` of each species at `z`
        end subroutine cosmology_onu_species

        !> `T_CMB(z) = Tcmb0 (1+z)`, in K. Zero at every `z` when `Tcmb0` is zero.
        pure elemental module function cosmology_tcmb(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `T_CMB(z)` in K
        end function cosmology_tcmb

        !> `T_nu(z) = T_nu0 (1 + z)`, the neutrino temperature in K.
        !!
        !! Zero at every `z` when `Tcmb0` is zero, which is what switches neutrinos off entirely.
        pure elemental module function cosmology_tnu(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `T_nu(z)` in K
        end function cosmology_tnu

        !> `w(z) = w0 + wa z / (1 + z)`, the dark-energy equation of state. Exactly `w0` at `z = 0`.
        !!
        !! Formed on `z/(1 + z)` as astropy forms it, which keeps its digits at a small `z` where
        !! `1 - e^-zeta` would not. It is LARGE at a deep blueshift -- `z/(1 + z)` is `-1e5` at
        !! `z = -0.99999` -- which is the equation of state that model really has there.
        pure elemental module function cosmology_w(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `w(z)`
        end function cosmology_w

        !> `f_DE(z) = (1+z)^(3(1 + w0 + wa)) exp(-3 wa z/(1+z))`: the dark-energy density in units
        !! of its value today. Exactly 1 at every `z` for a cosmological constant.
        !!
        !! `+Infinity` where the CPL exponent passes 709, which a big-rip model reaches as
        !! `z -> -1`; the density really does diverge there, and `%ode(z)` answers 1.
        pure elemental module function cosmology_de_density_scale(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `f_DE(z)`
        end function cosmology_de_density_scale

        !> `rho_crit(z) = rho_crit(0) E(z)^2`, in **solar masses per cubic megaparsec**.
        !!
        !! astropy reports this one in g/cm^3; M_sun/Mpc^3 is what a halo-mass calculation wants,
        !! so that is the unit here and the conversion is never implicit. The solar mass is the IAU
        !! 2015 nominal `GM_sun = 1.3271244e20 m^3 s^-2` divided by this module's own
        !! `G = 6.6743e-11`, which is how astropy derives `M_sun` too, so the two agree to rounding
        !! once the units are matched. `+Infinity` where `E^2` is.
        pure elemental module function cosmology_critical_density(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `rho_crit(z)` in M_sun/Mpc^3
        end function cosmology_critical_density

        !> `c t_L(z) = D_H t_L(z) / t_H`, in Mpc: the lookback time as a distance.
        !!
        !! NOT a distance to anything -- it is smaller than `D_C` at every positive redshift -- and
        !! NEGATIVE at a blueshift, where the lookback time is.
        pure elemental module function cosmology_lookback_distance(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `c t_L` in Mpc
        end function cosmology_lookback_distance

        !> The absorption distance out to `z`, dimensionless. astropy's `absorption_distance`.
        !!
        !! `X(z) = INT_0^z (1+z')^2 / E(z') dz'`, the path length a fixed comoving cross-section
        !! sweeps, which is what an absorber count per unit redshift is normalised by. In the
        !! table's own coordinate it is `INT e^(3 zeta)/E dzeta`, and it is a FOURTH tabulated
        !! integral: it is not a combination of the other three.
        !!
        !! Negative at a blueshift, as the other cumulative quantities are. It grows very fast --
        !! `e^(3 zeta)` is `1e30` at the domain's ceiling -- which is the quantity behaving, not
        !! an overflow.
        pure elemental module function cosmology_absorption_distance(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `X(z)`, dimensionless
        end function cosmology_absorption_distance

    end interface

    ! ---- The growth of structure --------------------------------------------------------------

    interface

        !> `D(z)`, the linear growth factor of matter perturbations, normalised to `D(0) = 1`.
        !!
        !! The growing solution of the linear growth equation for pressureless matter in a smooth
        !! background, written as a first-order equation for the growth rate and integrated on
        !! the module's own grid:
        !!
        !! ```
        !! df/dzeta   = f^2 + q f - (3/2) Om(zeta),   q = 2 - (1/2) dlnE^2/dzeta
        !! dlnD/dzeta = -f,                           Om = Om0 e^(3 zeta) / E^2
        !! ```
        !!
        !! **Scale-independent, and of the COLD matter.** The source is `Om0`, which excludes
        !! massive neutrinos: below their free-streaming length they contribute to `E^2` but not
        !! to the clustering, so for a model with massive neutrinos this is the growth of the cold
        !! matter in the scale-independent approximation. There is no `D(k, z)` here, no transfer
        !! function and no `sigma8`; the product a caller forms is
        !! `fs8 = sigma8 * %growth_factor(z) * %growth_rate(z)`.
        !!
        !! A quiet NaN outside the domain, below the model's own floor, and for a model with no
        !! matter at all -- de Sitter and Milne have nothing to grow, and answering `D = 1`
        !! everywhere would be a number that means nothing.
        pure elemental module function cosmology_growth_factor(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `D(z)`, dimensionless, `D(0) = 1`
        end function cosmology_growth_factor

        !> `f(z) = dlnD/dlna`, the linear growth rate.
        !!
        !! The variable the equation of `%growth_factor` is integrated in, so it is known at every
        !! node to the integrator's own accuracy rather than differentiated out of a table of
        !! something else. It is `O(1)` everywhere -- between 0 and 1 for every model tried --
        !! where `D` itself spans ten decades, and it tends to 1 deep in matter domination.
        !!
        !! The same NaNs as `%growth_factor`, for the same reasons.
        pure elemental module function cosmology_growth_rate(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `f(z)`, dimensionless
        end function cosmology_growth_rate

    end interface

    ! ---- The sound horizon ---------------------------------------------------------------------

    interface

        !> `r_s(z)`, the comoving sound horizon at `z`, in Mpc.
        !!
        !! How far a sound wave in the baryon-photon fluid has travelled by `z`, comoving:
        !!
        !! ```
        !! r_s(z) = D_H INT_z^inf  dz' / ( E(z') sqrt(3 (1 + R(z'))) ),   R = 3 Ob0 / (4 Ogamma0 (1 + z))
        !! ```
        !!
        !! **Exact for the model**, not a fit: it is `%r_drag()` that carries the per-mille, and
        !! the two exist side by side for exactly that reason. Measured against CAMB 2.0.4 at
        !! `1.8e-06` and CLASS 3.3.4.0 at `2.4e-07`.
        !!
        !! **It costs microseconds, not nanoseconds.** Nothing here is tabulated: each call lays
        !! panels of the 20-point Gauss rule over the whole sound-crossing history, twenty kernel
        !! evaluations each. It is declared `pure elemental` like every other redshift binding, so
        !! it will happily be handed a column of a million rows -- and will take a second over it.
        !! It is meant for the handful of scales a program forms once.
        !!
        !! A quiet NaN when the cosmology was built without an `ob0`, as `%ob0()` itself is: with
        !! no baryon density there is no `R` and no sound speed. Exactly zero when `Tcmb0` is
        !! zero, because a universe with no photons carries no sound.
        pure elemental module function cosmology_sound_horizon(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `r_s(z)` in Mpc
        end function cosmology_sound_horizon

        !> `r_d`, the BAO scale, in Mpc: the sound horizon at the baryon drag epoch.
        !!
        !! **A FIT, and the only fit in this module.** Aubourg et al. (2015), Physical Review D
        !! 92, 123516, equation (16):
        !!
        !! ```
        !! r_d = 55.154 exp[-72.3 (Onu h^2 + 0.0006)^2] / [ (Ocb h^2)^0.25351 (Ob h^2)^0.12807 ]  Mpc
        !! ```
        !!
        !! with `Ocb h^2 = Om0 h^2` -- cold dark matter and baryons, which is what this module's
        !! `Om0` already is -- and `Onu h^2 = SUM m_nu / 93.14 eV`, the MASSIVE species alone.
        !! Measured against CAMB 2.0.4 over the reference grid: `2.1e-04` for the three Planck
        !! cosmologies, `2.5e-04` to `3.6e-04` for the five WMAP ones -- and **`1.5e-01` for a model
        !! with six times the baryon density**, which is far outside the range the paper fits over.
        !! So a caller reading `147.09` off this binding should read three digits and not sixteen,
        !! and a caller whose model is nothing like a Planck cosmology should read `%sound_horizon`
        !! instead, which is exact for any model.
        !!
        !! The fit is blind to `Tcmb0`: it is `%sound_horizon(%z_drag())` that carries the model's
        !! own radiation, and the two agree to the fit's accuracy for a realistic model. A quiet
        !! NaN when there is no `ob0` or when it is exactly zero, since the formula divides by
        !! `(Ob h^2)^0.12807`, and likewise for a model with no matter.
        pure module function cosmology_r_drag(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `r_d` in Mpc
        end function cosmology_r_drag

        !> `z_drag`, the redshift at which `%sound_horizon(z)` equals `%r_drag()`.
        !!
        !! Solved, not fitted: the bracketed Newton iteration of the inverses, over `z` in
        !! `[100, 1e5]`, on a sound horizon that falls monotonically with redshift. Measured
        !! against CAMB 2.0.4's `zdrag` at `2.6e-04`, which is `%r_drag()`'s own accuracy carried
        !! through -- the redshift inherits the fit, not the integral.
        !!
        !! **This one costs tens of microseconds**: an integral that is itself microseconds, inside
        !! a solve. Measured at about fifty, against about ten for one `%sound_horizon` near `z = 0`
        !! -- the solve sits at `z ~ 1060`, where the interval is short and the rule lays two panels
        !! rather than five. A `pure` function cannot memoise, so keep it in a local if it is needed
        !! twice; `bench/benchmark_cosmology.sh sound` measures all four.
        !!
        !! A quiet NaN whenever `%r_drag()` is one, and also when `Tcmb0` is zero: a sound horizon
        !! that is identically zero never equals a finite `r_d`.
        pure module function cosmology_z_drag(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! the drag redshift
        end function cosmology_z_drag

        !> `z_eq`, matter-radiation equality: `Om0 / Or0 - 1`, exact from the parameters.
        !!
        !! `Or0 = Ogamma0 (1 + 0.22710731766 Neff)` is the RELATIVISTIC radiation density -- every
        !! neutrino species counted as massless, however massive it is today -- because at
        !! equality every one of them is. `%onu0()` is the other quantity, and using it here would
        !! be wrong by a few per cent for a model with a massive species, silently.
        !!
        !! `+Infinity` when `Tcmb0` is zero: with no radiation at all, matter dominates at every
        !! redshift and equality is never reached. `-1` for a model with no matter, which is the
        !! same statement the other way round.
        pure module function cosmology_z_eq(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! the equality redshift
        end function cosmology_z_eq

    end interface

    ! ---- The inverses -------------------------------------------------------------------------

    interface

        !> The redshift at which the comoving distance is `d`.
        !!
        !! NaN for a `d` outside `[D_C(-zeta_ceiling), D_C(zeta_ceiling)]`, the two bounds `%init`
        !! stores. The round trip is stable in the DISTANCE over the whole domain; in the redshift
        !! it is only as well conditioned as `dD_C/dz`, which falls like `z^(-3/2)`.
        pure elemental module function cosmology_z_at_distance(this, d) result(z)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: d    !! `D_C` in Mpc
            real(real64)                    :: z    !! the redshift there
        end function cosmology_z_at_distance

        !> The redshift `t` Gyr ago.
        !!
        !! NaN for a `t` outside `[t_L(-zeta_ceiling), t_L(zeta_ceiling)]`.
        pure elemental module function cosmology_z_at_lookback(this, t) result(z)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: t    !! lookback time in Gyr
            real(real64)                    :: z    !! the redshift there
        end function cosmology_z_at_lookback

        !> The redshift at which the universe was `t` Gyr old.
        !!
        !! Solved on `ln(age)`, never on `%age(0) - t`: the age is a fifth of an attosecond at the
        !! top of the domain while `%age(0)` is fourteen billion years, so the difference form
        !! answers an arbitrary redshift there while the logarithm keeps every digit.
        !!
        !! NaN for a `t` outside `[age(zeta_ceiling), age(-zeta_ceiling)]`, the two bounds `%init`
        !! stores, and NaN at every `t` for a model whose age integral diverges -- such a universe
        !! is infinitely old at every redshift, so no age names one.
        pure elemental module function cosmology_z_at_age(this, t) result(z)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: t    !! the age in Gyr
            real(real64)                    :: z    !! the redshift there
        end function cosmology_z_at_age

        !> The redshift at which the luminosity distance is `d`.
        !!
        !! **Answers a redshift at or above zero only**, and the SMALLEST one where `d` is reached.
        !! `D_L` is not one-to-one over the whole domain: it is strictly increasing in `z >= 0` for
        !! a flat or open model, but a blueshift carries it back to zero as `z -> -1` (the factor
        !! `1 + z` vanishes while `D_M` stays finite), and in a closed model `D_M` turns over at the
        !! antipode. So a negative `d` answers NaN rather than the blueshift that shares it, a `d`
        !! above `D_L(zeta_ceiling)` answers NaN, and a closed model is searched outward in unit
        !! panels of `zeta` from `z = 0` so that the first crossing is the one returned.
        pure elemental module function cosmology_z_at_luminosity(this, d) result(z)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: d    !! `D_L` in Mpc, at or above zero
            real(real64)                    :: z    !! the redshift there
        end function cosmology_z_at_luminosity

        !> The redshift at which the distance modulus is `mu`.
        !!
        !! `%z_at_luminosity_distance(10^((mu - 25)/5))`, so it carries that binding's contract
        !! exactly: a redshift at or above zero, the smallest one, NaN where there is none. A `mu`
        !! so large that `10^((mu - 25)/5)` would overflow answers NaN, and one so small that it
        !! would underflow answers zero.
        pure elemental module function cosmology_z_at_distmod(this, mu) result(z)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: mu   !! the distance modulus in magnitudes
            real(real64)                    :: z    !! the redshift there
        end function cosmology_z_at_distmod

    end interface

    ! ---- The parameters and the flags ---------------------------------------------------------

    interface

        !> `D_H = c / H0`, in Mpc.
        pure module function cosmology_hubble_distance(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `D_H` in Mpc
        end function cosmology_hubble_distance

        !> `t_H = 1 / H0`, in Gyr.
        pure module function cosmology_hubble_time(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `t_H` in Gyr
        end function cosmology_hubble_time

        !> `H0` in km/s/Mpc, as given.
        pure module function cosmology_h0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `H0`
        end function cosmology_h0

        !> `h = H0 / 100`.
        pure module function cosmology_little_h(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `H0 / 100`
        end function cosmology_little_h

        !> `Om0`, excluding massive neutrinos, as given.
        pure module function cosmology_om0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `Om0`
        end function cosmology_om0

        !> `Ode0`, as given or as derived for a flat model.
        pure module function cosmology_ode0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `Ode0`
        end function cosmology_ode0

        !> `Ok0`, derived. Exactly zero for a flat model.
        pure module function cosmology_ok0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `Ok0`
        end function cosmology_ok0

        !> `Ogamma0`, the photon density, derived from `Tcmb0` and `H0`.
        pure module function cosmology_ogamma0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `Ogamma0`
        end function cosmology_ogamma0

        !> `Onu0`, the neutrino density today, derived.
        pure module function cosmology_onu0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `Onu0`
        end function cosmology_onu0

        !> `Ob0`, as given, or NaN when the caller gave none.
        pure module function cosmology_ob0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `Ob0`, or NaN
        end function cosmology_ob0

        !> `Odm0 = Om0 - Ob0`, the cold dark matter density today, or NaN when no `ob0` was given.
        !!
        !! `Om0` here is astropy's: it excludes massive neutrinos, so this does too.
        pure module function cosmology_odm0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `Odm0`, or NaN
        end function cosmology_odm0

        !> `Tcmb0` in K, as given.
        pure module function cosmology_tcmb0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `Tcmb0`
        end function cosmology_tcmb0

        !> `T_nu0 = (4/11)^(1/3) Tcmb0`, in K.
        pure module function cosmology_tnu0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `T_nu0`
        end function cosmology_tnu0

        !> `Neff`, as given.
        pure module function cosmology_neff(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `Neff`
        end function cosmology_neff

        !> `w0`, as given.
        pure module function cosmology_w0(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `w0`
        end function cosmology_w0

        !> `wa`, as given.
        pure module function cosmology_wa(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! `wa`
        end function cosmology_wa

        !> The `zmax` the CALLER asked for.
        !!
        !! Not the table's own ceiling, which is rounded up to a node and never below `zeta_4`, and
        !! NOT a refusal boundary: every redshift in the domain is answered either way.
        pure module function cosmology_zmax(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! the requested `zmax`
        end function cosmology_zmax

        !> The `zmin` the CALLER asked for.
        !!
        !! Not the table's own bottom, which is rounded DOWN to a node and clamped by
        !! `%zeta_floor()` where the model itself ends, and NOT a refusal boundary: every redshift
        !! in the domain is answered either way, by the panel walk where the table does not reach.
        pure module function cosmology_zmin(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! the requested `zmin`
        end function cosmology_zmin

        !> The `zeta` at which this model's `E^2` reaches zero below `z = 0`, or
        !! `-PFC_ZETA_CEILING` (about `z = -0.9999999999`) when it stays positive all the way down.
        !!
        !! The BOTTOM OF THE DOMAIN, in the table's own coordinate: every binding answers NaN at
        !! and below it, and every inverse answers NaN for any value beyond what it reaches. A
        !! recollapsing closed universe (`om0 = 1.5, ode0 = 0`) has it at `ln(1/3)`, and any
        !! negative `ode0` puts it wherever the matter and curvature terms fall to `|ode0|`.
        !!
        !! The COMPANION of `%zmin()`: `%zmin()` is the floor the caller asked to tabulate to,
        !! this is the floor the model itself allows, and the table stops at whichever is higher.
        pure module function cosmology_zeta_floor(this) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64)                    :: v    !! the floor, in `zeta`
        end function cosmology_zeta_floor

        !> `Ok0` is exactly zero, which happens when `ode0` was omitted.
        pure module function cosmology_is_flat(this) result(ok)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            logical                         :: ok   !! flat
        end function cosmology_is_flat

        !> At least one neutrino species has a positive mass.
        pure module function cosmology_has_massive_nu(this) result(ok)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            logical                         :: ok   !! a species is massive
        end function cosmology_has_massive_nu

        !> `%init` has built this object. Never aborts, whatever the object holds.
        pure module function cosmology_is_initialised(this) result(ok)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            logical                         :: ok   !! built
        end function cosmology_is_initialised

    end interface

    ! ---- Private helpers the BUILD reaches across into the EVAL submodule for -------------------
    !
    ! `parquet_cosmology_build` and `parquet_cosmology_eval` are siblings, so neither can see the
    ! other's contained procedures. The kernel and the fallback rule are needed by both -- the
    ! build integrates over the kernel and lays panels to find its stored ceiling bounds -- so
    ! their interfaces are declared HERE and their bodies live in the eval submodule, which is the
    ! `parquet_core.f90` pattern for a cross-subtree private helper.

    interface

        !> `dE^2/dzeta`, the analytic derivative of the kernel, at `zeta`.
        !!
        !! Every term of `E^2` is a power of `x = e^zeta` times a constant or a factor whose
        !! logarithmic derivative is itself closed form, and `dx/dzeta = x`, so this is arithmetic
        !! on the same quantities `cosmology_e2` forms -- no second evaluation of anything
        !! transcendental, and no difference of two nearly equal numbers.
        !!
        !! * matter and curvature: `3 Om0 x^3` and `2 Ok0 x^2`;
        !! * dark energy: `Ode0 f_DE * 3(1 + w(z))`, since `dln f_DE/dzeta = 3(1 + w)`;
        !! * radiation: `Ogamma0 x^4 [4(1 + nu_rel) + dnu_rel/dzeta]`, with the Komatsu fit's own
        !!   derivative `dnu_rel/dzeta = A (Neff/N) SUM -u (1 + u)^(1/P - 1)` over the massive
        !!   species, `u = (C y / x)^P`.
        !!
        !! `+Infinity` where `E^2` itself has overflowed, and a quiet NaN for a NaN argument: the
        !! one caller is `%init`, which asks at `zeta = 0` where neither can happen for an
        !! admitted model, so the screens are there to keep the function total rather than to
        !! serve a path.
        pure module function cosmology_de2_dzeta(p, d, zeta) result(v)
            type(cosmology_params), intent(in)  :: p    !! the parameters
            type(cosmology_derived), intent(in) :: d    !! the derived values
            real(real64), intent(in)            :: zeta !! `ln(1 + z)`
            real(real64)                        :: v    !! `dE^2/dzeta` there
        end function cosmology_de2_dzeta

        !> `dnu_rel/dzeta`, the Komatsu fit's own derivative at `x = 1 + z`.
        !!
        !! `nu_rel` sums `(1 + u)^(1/P)` over the massive species with `u = (C y / x)^P`, and
        !! `du/dzeta = -P u` because `u` is a fixed power of `x` and `dx/dzeta = x`; each term
        !! therefore differentiates to `-u (1 + u)^(1/P - 1)`. Zero for a model with no massive
        !! species and for one with no CMB, where the fit is a constant.
        pure module function cosmology_dnu_rel(p, d, x) result(v)
            type(cosmology_params), intent(in)  :: p !! the parameters
            type(cosmology_derived), intent(in) :: d !! the derived values
            real(real64), intent(in)            :: x !! `1 + z`
            real(real64)                        :: v !! `dnu_rel/dzeta` there
        end function cosmology_dnu_rel

        !> `E^2` from `x = 1 + z` rather than from `zeta`: the kernel the CLOSED FORMS use.
        !!
        !! The same sum `cosmology_e2` forms, with the logarithm taken ONLY on the CPL branch,
        !! where `f_DE`'s exponent needs it. For a cosmological constant -- every named cosmology
        !! and every model that does not set `w0` or `wa` -- no logarithm is taken at all.
        !!
        !! **A separate module procedure and not a contained one**, so that the thirteen bindings
        !! over it reach ONE compiled copy rather than thirteen inlined ones optimised apart. At a
        !! deep blueshift of a CPL model the exponent is of order a few hundred, so its absolute
        !! rounding is `1e-14` of itself and two copies that group its terms differently answer
        !! `E^2` values `1e-14` apart -- enough to break the identity that the five density
        !! parameters sum to one, which ifx showed and gfortran did not.
        pure module function cosmology_e2_x(p, d, x) result(v)
            type(cosmology_params), intent(in)  :: p !! the parameters
            type(cosmology_derived), intent(in) :: d !! the derived values
            real(real64), intent(in)            :: x !! `1 + z`, known inside the domain
            real(real64)                        :: v !! `E^2` there
        end function cosmology_e2_x

        !> `E^2` from `x`, WITH the two terms a caller may want to divide by it.
        !!
        !! `%ode` and `%onu` are ratios whose numerator is one of `E^2`'s own terms, and the
        !! identity they keep -- that the five density parameters sum to one -- needs the numerator
        !! to be the SAME double the denominator was built from. Evaluating the term twice is what
        !! breaks it: see `cosmology_e2_x`.
        !> The growth equation's two coefficients at one `zeta`.
        !!
        !! `q = 2 - (1/2) dlnE^2/dzeta` and `src = (3/2) Om(zeta)`, which is everything in
        !! `df/dzeta = f^2 + q f - src` that does not depend on `f`. Both are a quiet NaN where
        !! `E^2` is not positive, so a redshift below the model's own floor answers NaN through
        !! the arithmetic rather than through a second screen.
        !!
        !! The Einstein-de Sitter check is a fixed point rather than an integration: `E^2 =
        !! e^(3 zeta)` gives `q = 1/2` and `src = 3/2`, whose root `f = 1` makes `df/dzeta`
        !! identically zero.
        pure module subroutine cosmology_growth_coef(p, d, zeta, q, src)
            type(cosmology_params), intent(in)  :: p    !! the parameters
            type(cosmology_derived), intent(in) :: d    !! the derived values
            real(real64), intent(in)            :: zeta !! `ln(1 + z)`
            real(real64), intent(out)           :: q    !! `2 - (1/2) dlnE^2/dzeta`
            real(real64), intent(out)           :: src  !! `(3/2) Om(zeta)`
        end subroutine cosmology_growth_coef

        !> One classical fourth-order Runge-Kutta substep of the growth pair, of width `h`.
        !!
        !! `h` is NEGATIVE for the downward pass, which is the only direction the equation may be
        !! integrated in ([`PFC_GROWTH_TOP_NODE`](../module/parquet_cosmology.html)). The
        !! coefficients at the step's START are passed in and the ones at its END come back, so a
        !! walk of many substeps costs TWO kernel evaluations per substep rather than four: the
        !! four stages need `zeta`, the midpoint twice, and `zeta + h`.
        !!
        !! **The second component is `u = ln D + zeta`, not `ln D` itself**, which is the same
        !! device as tabulating `D_C/zeta` and `ln(age)`: the known behaviour is divided out and
        !! what is summed is the departure from it. `du/dzeta = 1 - f`, so in matter domination
        !! the increments are small and in an Einstein-de Sitter universe they are exactly zero --
        !! `u` stays the zero it started at, and `lnD = -zeta` comes out of a single multiplication
        !! rather than six thousand additions. Summing `ln D` directly instead leaves the same
        !! identity `2.3e-12` from 1 at `z = 1100`, all of it the sum and none of it the method.
        pure module subroutine cosmology_growth_step(p, d, zeta, h, q0, s0, f, u)
            type(cosmology_params), intent(in)  :: p    !! the parameters
            type(cosmology_derived), intent(in) :: d    !! the derived values
            real(real64), intent(in)            :: zeta !! where the substep starts
            real(real64), intent(in)            :: h    !! its width, negative going downward
            real(real64), intent(inout)         :: q0   !! `q` at `zeta` in, at `zeta + h` out
            real(real64), intent(inout)         :: s0   !! `src` at `zeta` in, at `zeta + h` out
            real(real64), intent(inout)         :: f    !! the growth rate, advanced in place
            real(real64), intent(inout)         :: u    !! `ln D + zeta`, advanced in place
        end subroutine cosmology_growth_step

        pure module subroutine cosmology_e2_split(p, d, x, de, nu, v)
            type(cosmology_params), intent(in)  :: p  !! the parameters
            type(cosmology_derived), intent(in) :: d  !! the derived values
            real(real64), intent(in)            :: x  !! `1 + z`, known inside the domain
            real(real64), intent(out)           :: de !! `Ode0 f_DE` there
            real(real64), intent(out)           :: nu !! `nu_rel` there
            real(real64), intent(out)           :: v  !! `E^2` there
        end subroutine cosmology_e2_split

        !> `E(zeta)^2`, the pure kernel. Screened: `+Infinity` where the CPL factor has overflowed,
        !! and a quiet NaN where the sum is not positive, without taking a square root.
        pure module function cosmology_e2(p, d, zeta) result(v)
            type(cosmology_params), intent(in)  :: p    !! the parameters
            type(cosmology_derived), intent(in) :: d    !! the derived values
            real(real64), intent(in)            :: zeta !! `ln(1 + z)`
            real(real64)                        :: v    !! `E^2` there
        end function cosmology_e2

        !> The relativistic neutrino density in units of the photon density, at `x = 1 + z`.
        pure module function cosmology_nu_rel(p, d, x) result(v)
            type(cosmology_params), intent(in)  :: p !! the parameters
            type(cosmology_derived), intent(in) :: d !! the derived values
            real(real64), intent(in)            :: x !! `1 + z`
            real(real64)                        :: v !! `nu_rel`
        end function cosmology_nu_rel

        !> One of the three integrands at one point, selected by `which`.
        pure module function cosmology_integrand_at(p, d, which, x) result(v)
            type(cosmology_params), intent(in)  :: p     !! the parameters
            type(cosmology_derived), intent(in) :: d     !! the derived values
            integer, intent(in)                 :: which !! `PFC_INT_DISTANCE`, `_TIME`, `_AGE_TAIL`
            real(real64), intent(in)            :: x     !! `zeta`, or `b` for the age tail
            real(real64)                        :: v     !! the integrand there
        end function cosmology_integrand_at

        !> The 20-point Gauss-Legendre rule for one integrand over `[a, b]`, one panel.
        pure module function cosmology_panel(p, d, which, a, b) result(v)
            type(cosmology_params), intent(in)  :: p     !! the parameters
            type(cosmology_derived), intent(in) :: d     !! the derived values
            integer, intent(in)                 :: which !! which integrand
            real(real64), intent(in)            :: a     !! lower bound
            real(real64), intent(in)            :: b     !! upper bound
            real(real64)                        :: v     !! the panel's contribution
        end function cosmology_panel

        !> The integral of one integrand from `z0` to `z1` in `zeta`, laid out in unit panels.
        !!
        !! Panels of width `PFC_PANEL` with the last cut at the query, in either direction. The
        !! negative side is panelled too, and that is not a formality: a single panel over
        !! `[zeta, 0]` decays from `1.6e-14` at `z = -0.99` to `1.6e-9` at `z = -0.99999`, while
        !! unit panels hold `2e-16` throughout.
        pure module function cosmology_walk(p, d, which, z0, z1) result(v)
            type(cosmology_params), intent(in)  :: p     !! the parameters
            type(cosmology_derived), intent(in) :: d     !! the derived values
            integer, intent(in)                 :: which !! which integrand
            real(real64), intent(in)            :: z0    !! `zeta` to start from
            real(real64), intent(in)            :: z1    !! `zeta` to walk to
            real(real64)                        :: v     !! the integral between them
        end function cosmology_walk

        !> The age at `zeta` as its OWN integral: one 20-point rule in `b = sqrt(a)` over
        !! `[0, e^(-zeta/2)]`, where the substitution leaves an integrand behaving like `b^3` or
        !! `b^2` at the origin. Never a tabulated value minus anything.
        pure module function cosmology_age_tail(p, d, zeta) result(v)
            type(cosmology_params), intent(in)  :: p    !! the parameters
            type(cosmology_derived), intent(in) :: d    !! the derived values
            real(real64), intent(in)            :: zeta !! `ln(1 + z)`
            real(real64)                        :: v    !! the age there, in units of `t_H`
        end function cosmology_age_tail

        !> The comoving sound horizon at `zeta`, in units of `D_H`: panels of the 20-point rule
        !! over `[0, asinh(b_top / sound_c)]`, where `b = sound_c sinh(v)` and `b_top = e^(-zeta/2)`.
        pure module function cosmology_sound_tail(p, d, zeta) result(v)
            type(cosmology_params), intent(in)  :: p    !! the parameters
            type(cosmology_derived), intent(in) :: d    !! the derived values
            real(real64), intent(in)            :: zeta !! `ln(1 + z)`
            real(real64)                        :: v    !! `r_s / D_H` there
        end function cosmology_sound_tail

    end interface

    ! ---- The free functions ---------------------------------------------------------------

    interface

        !> `ln(1 + z)`, exact to rounding at every `z > -1`.
        !!
        !! TWO forms, because neither is accurate over the whole range. For `|z| <= 0.5` it is
        !! `2 atanh(z / (2 + z))`, the `log1p` identity `parquet_random` already carries, because
        !! `log(1.0 + z)` rounds `1 + z` before taking the logarithm and is `6e-9` out at
        !! `z = 1e-8` and `9e-5` out at `z = 1e-12`. Above `|z| = 0.5` it is `log(1 + z)`, because
        !! the `atanh` form has its own pole to contend with -- `z/(2 + z)` approaches 1 -- and is
        !! `3.6e-9` out at `z = 1e10`, `2.3e-5` out at `z = 1e15`, and raises
        !! `IEEE_DIVIDE_BY_ZERO` from about `z = 1e17`. Together they are within about two ulp
        !! everywhere: the worst of 400 000 random `z` from `1e-14` to `1e10`, of both signs, is
        !! `4.6e-16` relative. Total: a NaN or a `z <= -1` answers NaN, raising no flag, and an
        !! infinite `z` answers `+Infinity`.
        pure elemental module function pf_z2zeta(z) result(zeta)
            real(real64), intent(in) :: z    !! redshift
            real(real64)             :: zeta !! `ln(1 + z)`
        end function pf_z2zeta

        !> `exp(zeta) - 1`, exact to rounding at every `zeta`.
        !!
        !! Written `2 exp(zeta/2) sinh(zeta/2)` because Fortran has no `expm1`. Total: a NaN
        !! answers NaN, and a `zeta` above 709 answers `+Infinity` rather than overflowing.
        pure elemental module function pf_zeta2z(zeta) result(z)
            real(real64), intent(in) :: zeta !! `ln(1 + z)`
            real(real64)             :: z    !! redshift
        end function pf_zeta2z

        !> Two redshifts composed: `(1 + z1)(1 + z2) - 1`.
        !!
        !! Formed as `z1 + z2 + z1*z2`, which loses nothing at small redshifts where the product
        !! form cancels. Composes a cosmological redshift with a peculiar one --
        !! `pf_z_combine(z_cos, v_los / c)` for a mock catalogue in redshift space. Total: a NaN
        !! argument answers NaN.
        pure elemental module function pf_z_combine(z1, z2) result(z)
            real(real64), intent(in) :: z1 !! the first redshift
            real(real64), intent(in) :: z2 !! the second redshift
            real(real64)             :: z  !! the composed redshift
        end function pf_z_combine

    end interface

end module parquet_cosmology
