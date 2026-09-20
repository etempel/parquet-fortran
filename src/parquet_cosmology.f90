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
!! Five families, all over `real64`:
!!
!! * **Distances** -- `%comoving_distance`, `%comoving_transverse_distance`,
!!   `%luminosity_distance`, `%angular_diameter_distance`, `%angular_diameter_distance_z1z2`,
!!   `%comoving_volume`, `%differential_comoving_volume`, `%lookback_distance` and `%distmod`,
!!   with `%comoving_distance_zeta` taking the table's own coordinate directly.
!! * **Times** -- `%lookback_time` and `%age`, in Gyr.
!! * **The expansion** -- `%efunc`, `%inv_efunc`, `%hubble`, and the transverse scales
!!   `%kpc_proper_per_arcmin`, `%kpc_comoving_per_arcmin`, `%arcsec_per_kpc_proper` and
!!   `%arcsec_per_kpc_comoving`.
!! * **The contents of the universe at `z`** -- `%om`, `%ode`, `%ok`, `%ogamma`, `%onu`, which sum
!!   to one, with `%tcmb`, `%w`, `%de_density_scale` and `%critical_density`.
!! * **Inverses** -- `%z_at_comoving_distance`, `%z_at_lookback_time`, `%z_at_age`,
!!   `%z_at_luminosity_distance` and `%z_at_distmod`.
!!
!! plus the parameter queries (`%h0`, `%om0`, `%ode0`, `%ok0`, `%ogamma0`, `%onu0`, `%ob0`,
!! `%odm0`, `%tcmb0`, `%tnu0`, `%neff`, `%w0`, `%wa`, `%little_h`, `%hubble_distance`,
!! `%hubble_time`, `%zmax`), the flags (`%is_flat`, `%has_massive_nu`, `%is_initialised`), the
!! three subroutines `%get_name`, `%describe` and `%m_nu`, and `%clear`.
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
!! **`%init` tabulates; every other binding reads.** Three integrals -- the comoving distance, the
!! lookback time and the age -- are built over one uniform grid in `zeta`, out of ONE set of
!! interval integrals, and interpolated by `pf_interp_1d`. A redshift beyond the table is answered
!! by a fixed 20-point Gauss-Legendre rule from the table's edge, so **no redshift is ever refused
!! and the table decides only how fast**: tens of nanoseconds inside it, and microseconds to tens
!! of microseconds outside, rising with the distance past the edge. `zmax=` moves the seam, never
!! the answer. `bench/benchmark_cosmology.sh` measures both.
!!
!! **The age is its own table, never `%age(0) - %lookback_time(z)`.** That difference cancels: at
!! `z = 1e10` the age is `7.6e-18 Gyr` while both terms are `13.7869 Gyr`, so not one digit of it
!! survives. Tabulated in its own right it is accurate to about `1e-10` relative at every redshift.
!! A model whose age integral diverges -- de Sitter is one -- answers `+Infinity` at every
!! redshift, which is right rather than exceptional.
!!
!! **Total, not validating.** The redshift domain is `-1 < z <= 1e10`. Inside it every binding
!! answers a number, or a signed infinity where the mathematics says so. A NaN redshift answers
!! NaN; a redshift at or below `-1`, or above `1e10`, answers NaN quietly, raising no IEEE flag --
!! a catalogue's `-99` and `-1` sentinels are real inputs to an elemental call over a column, and
!! an abort there would take a program down for one row. A negative redshift above `-1` is a
!! blueshift and is answered with its sign. What aborts is a caller's mistake: a cosmology that was
!! never built, an unknown name, or a parameter outside its admitted range.
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
                                  PF_INT_OK, PF_INT_DIVERGENT, PF_INT_BAD_VALUE
    use parquet_interpolate, only : pf_interp_1d
    use parquet_utils, only : pf_to_lower, pf_to_str

    implicit none
    private

    public :: pf_cosmology
    public :: pf_z2zeta, pf_zeta2z, pf_z_combine
    public :: parquet_debug_set_cosmology_max_neval, parquet_debug_cosmology_neval

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
    real(real64), parameter :: PFC_RTOL = 1.0e-12_real64
        !! The relative tolerance every interval integral is taken to.
    real(real64), parameter :: PFC_PANEL = 1.0_real64
        !! The width in `zeta` of one Gauss-Legendre panel beyond the table.
    integer, parameter :: PFC_CONTEXT_CAP = 100
        !! Caller-supplied text is capped at this inside a message, as every module caps it.

    ! ---- Which integrand an integration is over ------------------------------------------------

    integer, parameter :: PFC_INT_DISTANCE = 1
        !! `e^zeta / E`, whose integral is `D_C / D_H`.
    integer, parameter :: PFC_INT_TIME = 2
        !! `1 / E`, whose integral is `t_L / t_H`.
    integer, parameter :: PFC_INT_AGE_TAIL = 3
        !! `2 / (b E(b^2))` in `b = sqrt(a)`, whose integral is the age past the table's edge.

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
        real(real64) :: d_n       = 0.0_real64 !! `D_C(zeta_n)`, where the fallback starts
        real(real64) :: t_n       = 0.0_real64 !! `t_L(zeta_n)`, likewise
        real(real64) :: d_ceiling = 0.0_real64 !! `D_C(+PFC_ZETA_CEILING)`, the inverse's top
        real(real64) :: d_floor   = 0.0_real64 !! `D_C(-PFC_ZETA_CEILING)`, the inverse's bottom
        real(real64) :: t_ceiling = 0.0_real64 !! `t_L(+PFC_ZETA_CEILING)`
        real(real64) :: t_floor   = 0.0_real64 !! `t_L(-PFC_ZETA_CEILING)`
        real(real64) :: d_inv_top = 0.0_real64 !! the largest `D_C` the inverse TABLE covers
        real(real64) :: t_inv_top = 0.0_real64 !! the largest `t_L` the inverse table covers
        real(real64) :: zeta_d_inv = 0.0_real64 !! the `zeta` at `d_inv_top`
        real(real64) :: zeta_t_inv = 0.0_real64 !! the `zeta` at `t_inv_top`
        real(real64) :: rho_crit0 = 0.0_real64 !! `rho_crit(0)` in M_sun/Mpc^3
        real(real64) :: a_n       = 0.0_real64 !! `age(zeta_n)`, where the age's own fallback starts
        real(real64) :: a_ceiling = 0.0_real64 !! `age(+PFC_ZETA_CEILING)`: the SMALLEST age attained
        real(real64) :: a_floor   = 0.0_real64 !! `age(-PFC_ZETA_CEILING)`: the LARGEST
        integer      :: n_nu      = 0          !! `floor(neff)`: the number of species
        integer      :: n_massless = 0         !! how many of them have zero mass
    end type cosmology_derived

    !> One of the three integrands, carrying the cosmology it is taken over.
    !!
    !! A `pf_integrand` extension rather than a bare function, because the integrand needs the
    !! model; `%eval`'s passed object is `intent(inout)`, as the abstract interface requires.
    type, extends(pf_integrand) :: cosmology_integrand
        type(cosmology_params)  :: p            !! the parameters
        type(cosmology_derived) :: d            !! the derived values
        integer :: which = PFC_INT_DISTANCE     !! `PFC_INT_DISTANCE`, `_TIME` or `_AGE_TAIL`
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
        type(pf_interp_1d) :: f                         !! `D_C(zeta) / zeta` against `zeta`
        type(pf_interp_1d) :: g                         !! `t_L(zeta) / zeta` against `zeta`
        type(pf_interp_1d) :: a                         !! `ln(age(zeta))` against `zeta`
        type(pf_interp_1d) :: zd                        !! `zeta / D` against `D`: the distance inverse
        type(pf_interp_1d) :: zt                        !! `zeta / t` against `t`: the time inverse
        logical :: flat         = .false.               !! `ok0 == 0` exactly, recorded
        logical :: massive_nu   = .false.               !! at least one species has a positive mass
        logical :: has_ob0      = .false.               !! the caller gave an `ob0`
        logical :: age_diverges = .false.               !! the age integral diverges
        logical :: ready        = .false.               !! `%init` has built this object
    contains
        procedure, private :: init_params => cosmology_init_params !! `%init` in the parameter form.
        procedure, private :: init_named  => cosmology_init_named  !! `%init` in the named form.
        generic :: init => init_params, init_named      !! Builds the cosmology; see the two bodies.
        procedure :: clear => cosmology_clear           !! Releases the tables; harmless on a fresh object.
        procedure :: comoving_distance => cosmology_comoving_distance !! `D_C` in Mpc.
        procedure :: comoving_distance_zeta => cosmology_comoving_distance_zeta !! `D_C` from `zeta`.
        procedure :: comoving_transverse_distance => cosmology_comoving_transverse !! `D_M` in Mpc.
        procedure :: luminosity_distance => cosmology_luminosity_distance !! `D_L` in Mpc.
        procedure :: angular_diameter_distance => cosmology_angular_diameter !! `D_A` in Mpc.
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
        procedure :: om => cosmology_om                 !! `Om(z)`, the matter density parameter at `z`.
        procedure :: ode => cosmology_ode               !! `Ode(z)`, the dark-energy density parameter.
        procedure :: ok => cosmology_ok                 !! `Ok(z)`, the curvature density parameter.
        procedure :: ogamma => cosmology_ogamma         !! `Ogamma(z)`, the photon density parameter.
        procedure :: onu => cosmology_onu               !! `Onu(z)`, the neutrino density parameter.
        procedure :: tcmb => cosmology_tcmb             !! `T_CMB(z)` in K.
        procedure :: w => cosmology_w                   !! `w(z)`, the dark-energy equation of state.
        procedure :: de_density_scale => cosmology_de_density_scale !! `f_DE(z)`, the CPL factor.
        procedure :: critical_density => cosmology_critical_density !! `rho_crit(z)` in M_sun/Mpc^3.
        procedure :: lookback_distance => cosmology_lookback_distance !! `c t_L(z)` in Mpc.
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
        !!                 [zmax], [context])
        !! ```
        !!
        !! Optional arguments are in square brackets. `ode0` ABSENT means flat, and sets `Ok0 = 0`
        !! by assignment rather than by subtraction, so `%is_flat()` is exact. `tcmb0` defaults to
        !! zero, which switches radiation and neutrinos off entirely, as astropy does; `neff`
        !! defaults to `3.04`, and `m_nu` must carry `floor(neff)` masses when it is given. `name`
        !! is a free-text label the object carries for `%get_name` and `%describe`, default
        !! `"custom"`: it is NOT checked against the eight named cosmologies, so a caller may label
        !! a model anything. `zmax` sets the table's top, default `1100`; nothing above it is lost,
        !! only slower. `context` is appended to any abort message.
        !!
        !! A second `%init` on a built object replaces it entirely.
        module subroutine cosmology_init_params(this, h0, om0, ode0, tcmb0, neff, m_nu, ob0, w0, &
                                                wa, name, zmax, context)
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
            character(len=*), intent(in), optional :: context !! appended to any abort message
        end subroutine cosmology_init_params

        !> Builds one of the eight named cosmologies.
        !!
        !! ```
        !! call cosmo%init(name, [zmax], [context])
        !! ```
        !!
        !! `name` is `Planck18`, `Planck15`, `Planck13`, `WMAP9`, `WMAP7`, `WMAP5`, `WMAP3` or
        !! `WMAP1`, matched without regard to case; `%get_name` returns the canonical spelling.
        !! The parameters are astropy 8.0.1's realizations. `zmax` and `context` are as they are
        !! for the parameter form.
        module subroutine cosmology_init_named(this, name, zmax, context)
            class(pf_cosmology), intent(out)       :: this    !! the cosmology to build
            character(len=*), intent(in)           :: name    !! one of the eight named cosmologies
            real(real64),     intent(in), optional :: zmax    !! the table's top; default 1100
            character(len=*), intent(in), optional :: context !! appended to any abort message
        end subroutine cosmology_init_named

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

        !> `T_CMB(z) = Tcmb0 (1+z)`, in K. Zero at every `z` when `Tcmb0` is zero.
        pure elemental module function cosmology_tcmb(this, z) result(v)
            class(pf_cosmology), intent(in) :: this !! the cosmology
            real(real64), intent(in)        :: z    !! redshift
            real(real64)                    :: v    !! `T_CMB(z)` in K
        end function cosmology_tcmb

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
