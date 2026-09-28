!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Named constants: pi and the angle factors built on it, the speed of light, Newton's constant,
!> the Sun's mass parameter and mass, the Stefan-Boltzmann and Boltzmann constants, the megaparsec,
!> the gigayear, the neutrino-to-photon temperature ratio and the AB zero point, each a `real64`
!> parameter.
!!
!! **Every name carries its unit.** A constant is `PF_<symbol>[_<unit>]`: the symbol as astropy
!! spells it, upper-cased (`C`, `G`, `GM_SUN`, `M_SUN`, `SIGMA_SB`, `K_B`), then the unit of a
!! dimensional constant, spelled out where it is short (`KMS` for km/s, `MS`, `KG`, `M`, `KM`, `S`,
!! `JY`, `EV_K` for eV/K) and `SI` where it is a compound SI unit (`PF_G_SI`, `PF_GM_SUN_SI`,
!! `PF_SIGMA_SB_SI`). A pure number carries none (`PF_PI`, `PF_TNU_OVER_TGAMMA`). The angle factors
!! name what they convert, so the units cancel on sight: `angle_deg * PF_RAD_PER_DEG` is in
!! radians. They are not `PF_DEG2RAD` and `PF_RAD2DEG`, because Fortran names are
!! case-insensitive: those are the names of `parquet_utils`' conversion functions `pf_deg2rad` and
!! `pf_rad2deg`, and a program reaching both modules through `use parquet` could not call them.
!!
!! **These are the doubles the library computes with.** Every procedure of this library that needs
!! one of these values reads it from here, so a program using the same names computes with the
!! same doubles. A value the definitions make exact is the double nearest it, with the exceptions
!! each doc-comment names: where astropy 8.0.1 writes a different double, and this library's
!! agreement with astropy depends on it, the double is astropy's (`PF_SIGMA_SB_SI`, `PF_K_B_EV_K`
!! and `PF_M_SUN_KG`). A new edition of a measured value (CODATA, IAU) changes what the library
!! answers, and is made as a change of its own.
!!
!! **`real64` only.** A `real32` caller converts at the use: `real(PF_PI, real32)`.
!!
!! **A constant joins this module when both hold**: its meaning does not depend on any one
!! algorithm (a mathematical or physical constant, a unit, an astronomical convention), and a
!! procedure of this library uses it, or a program built on the library needs it, in the change
!! that adds it. A tuning value, a threshold, a fit coefficient, a model's table or the default of
!! one procedure's argument never joins: each is documented at the procedure it serves. Every
!! constant is pinned bit for bit by `test/test_constants.f90` (`check_constants_are_pinned`), no
!! public name elsewhere in `src/` may equal one of these names case-insensitively
!! (`check_constants_names_are_unique`), and no other source file may carry a private copy of one
!! of these values (`check_constants_have_one_home`), all three in
!! `tools/check_source_conventions.py`.
!!
!! **A leaf of values.** The module imports the intrinsic module `iso_fortran_env` and nothing else,
!! and declares parameters only: no variable, no procedure. So `use parquet_constants` compiles one
!! Fortran file, reads no setting, prints nothing and holds no state for threads to share.
!! `check_parquet_constants_stays_leaf` keeps it so, which is what lets the leaf tiers
!! `parquet_utils`, `parquet_random` and `parquet_transform` import it and stay leaves.
module parquet_constants
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    private

    public :: PF_PI, PF_TWOPI, PF_HALFPI
    public :: PF_RAD_PER_DEG, PF_DEG_PER_RAD, PF_RAD_PER_ARCMIN, PF_RAD_PER_ARCSEC
    public :: PF_C_KMS, PF_C_MS
    public :: PF_MPC_M, PF_MPC_KM, PF_GYR_S
    public :: PF_G_SI, PF_GM_SUN_SI, PF_M_SUN_KG, PF_G_MPC_MSUN_KMS2
    public :: PF_SIGMA_SB_SI, PF_K_B_EV_K, PF_TNU_OVER_TGAMMA
    public :: PF_AB_ZERO_POINT_JY

    ! ---- Pi and the angle factors ----

    !> pi, the double nearest it.
    real(real64), parameter :: PF_PI = 3.14159265358979323846264338327950288_real64
    !> 2 pi, the double nearest it.
    real(real64), parameter :: PF_TWOPI = 2.0_real64 * PF_PI
    !> pi / 2, the double nearest it.
    real(real64), parameter :: PF_HALFPI = 0.5_real64 * PF_PI
    !> Radians per degree, `pi / 180`, the double nearest it.
    real(real64), parameter :: PF_RAD_PER_DEG = PF_PI / 180.0_real64
    !> Degrees per radian, `180 / pi`, the double nearest it.
    real(real64), parameter :: PF_DEG_PER_RAD = 180.0_real64 / PF_PI
    !> Radians per arcminute, `pi / 10800`, the double nearest it.
    real(real64), parameter :: PF_RAD_PER_ARCMIN = PF_PI / 10800.0_real64
    !> Radians per arcsecond, `pi / 648000`, the double nearest it.
    real(real64), parameter :: PF_RAD_PER_ARCSEC = PF_PI / 648000.0_real64

    ! ---- The speed of light ----

    !> The speed of light in km/s, 299 792.458 exactly by the SI definition of the metre; the double
    !! nearest it.
    real(real64), parameter :: PF_C_KMS = 299792.458_real64
    !> The speed of light in m/s, exact by the SI definition of the metre, and `1000 * PF_C_KMS`
    !! exactly.
    real(real64), parameter :: PF_C_MS = 2.99792458e8_real64

    ! ---- Distance and time ----

    !> A megaparsec in metres: a million parsecs of `648000 / pi` astronomical units (IAU 2015
    !! Resolution B2), the unit 149 597 870 700 m exactly (IAU 2012 Resolution B2); the double
    !! nearest it.
    real(real64), parameter :: PF_MPC_M = 3.0856775814913673e22_real64
    !> A megaparsec in kilometres, the double nearest it; `1000 * PF_MPC_KM` is `PF_MPC_M` exactly.
    real(real64), parameter :: PF_MPC_KM = 3.0856775814913673e19_real64
    !> A gigayear in seconds: a thousand million Julian years of 365.25 days of 86 400 s, exact.
    real(real64), parameter :: PF_GYR_S = 3.15576e16_real64

    ! ---- Gravitation and the Sun ----

    !> Newton's constant `G` in m^3 kg^-1 s^-2, the CODATA 2022 value `6.67430(15)e-11`: the double
    !! nearest it, and astropy 8.0.1's `G`.
    real(real64), parameter :: PF_G_SI = 6.6743e-11_real64
    !> The Sun's mass parameter `GM_sun` in m^3 s^-2, the IAU 2015 nominal value (Resolution B3),
    !! exact as written.
    real(real64), parameter :: PF_GM_SUN_SI = 1.3271244e20_real64
    !> The Sun's mass in kg, `PF_GM_SUN_SI / PF_G_SI`, as astropy derives its `M_sun`: astropy
    !! 8.0.1's double bit for bit, which is 1 ulp above the double nearest the quotient of the two
    !! values. It carries the uncertainty of `G`, 2.2e-5 relative.
    real(real64), parameter :: PF_M_SUN_KG = PF_GM_SUN_SI / PF_G_SI
    !> Newton's constant in Mpc (km/s)^2 per solar mass, the unit of a circular speed at a radius in
    !! megaparsecs: `PF_GM_SUN_SI / (1e6 PF_MPC_M)`, since `G M_sun` is `GM_sun`. The double
    !! nearest that quotient, and astropy 8.0.1's `G` in that unit bit for bit. `G` cancels out of
    !! it, so it carries none of the uncertainty of `G`.
    real(real64), parameter :: PF_G_MPC_MSUN_KMS2 = PF_GM_SUN_SI / (1.0e6_real64 * PF_MPC_M)

    ! ---- Radiation ----

    !> The Stefan-Boltzmann constant in W m^-2 K^-4, exact in the 2019 SI as
    !! `2 pi^5 k^4 / (15 h^3 c^2)`. This is astropy 8.0.1's double, 3 ulp above the double nearest
    !! the exact value, so that a radiation density computed with it is astropy's.
    real(real64), parameter :: PF_SIGMA_SB_SI = 5.6703744191844314e-8_real64
    !> Boltzmann's constant in eV/K, `k / e`, exact in the 2019 SI. This is astropy 8.0.1's double,
    !! 1 ulp above the double nearest the exact value, so that a neutrino temperature computed with
    !! it is astropy's.
    real(real64), parameter :: PF_K_B_EV_K = 8.617333262145179e-5_real64
    !> The ratio of the neutrino to the photon temperature after electron-positron annihilation,
    !! `(4/11)^(1/3)`; the double nearest it.
    real(real64), parameter :: PF_TNU_OVER_TGAMMA = 0.7137658555036082_real64

    ! ---- Photometry ----

    !> The flux density of AB magnitude zero, in janskys: `m_AB = -2.5 log10(f_nu) - 48.60`, with
    !! `f_nu` in erg s^-1 cm^-2 Hz^-1, puts `m_AB = 0` at `10^(-19.44)` of that unit, which is
    !! `10^3.56` Jy. The double nearest it; astropy 8.0.1's `ABflux` is 22 ulp lower, and the
    !! rounded 3631 Jy is 6.0e-5 higher.
    real(real64), parameter :: PF_AB_ZERO_POINT_JY = 3630.7805477010133_real64

end module parquet_constants
