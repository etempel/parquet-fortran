---
title: Physical and mathematical constants with parquet_constants
---

`parquet_constants` holds the named constants this library computes with: pi and the angle factors
built on it, and the physical constants and units its cosmology, coordinate and sky tiers need. Each
is a `real64` parameter, so it costs nothing at run time and may appear in your own constant
expressions. `use parquet` brings them in; on its own, `use parquet_constants` compiles one of this
library's Fortran files, reads no setting and prints nothing. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

## Quick example

```fortran
use parquet_constants, only : PF_C_KMS, PF_RAD_PER_DEG
use iso_fortran_env, only : real64

real(real64) :: v_kms, z, dec_deg, dec_rad

v_kms = 600.0_real64
z = v_kms / PF_C_KMS                 ! a recession velocity as a redshift, to first order
dec_deg = -30.0_real64
dec_rad = dec_deg * PF_RAD_PER_DEG   ! degrees to radians
```

## What it holds

Six families. The module's own page in the API reference, reached from the
[modules listing](../../lists/modules.html), lists every constant with its unit and where its value
comes from; the families are:

- **Pi and the angle factors**: `PF_PI`, a turn and a quarter turn of it, and the factors between
  radians and degrees, arcminutes and arcseconds (`PF_RAD_PER_DEG`, `PF_DEG_PER_RAD`, ...).
- **The speed of light**, in km/s and in m/s.
- **Distance and time**: the megaparsec in metres and in kilometres, and the gigayear in seconds.
- **Gravitation and the Sun**: Newton's constant in SI units, the Sun's mass parameter `GM_sun` and
  its mass, and Newton's constant in `Mpc (km/s)^2` per solar mass, the unit of a circular speed at
  a radius in megaparsecs.
- **Radiation**: the Stefan-Boltzmann constant, Boltzmann's constant in eV/K, and the ratio of the
  neutrino to the photon temperature.
- **Photometry**: the flux density of AB magnitude zero, in janskys.

## Every name carries its unit

A constant is named `PF_<symbol>[_<unit>]`. The symbol is astropy's, upper-cased: `C`, `G`,
`GM_SUN`, `M_SUN`, `SIGMA_SB`, `K_B`. A dimensional constant always carries its unit, spelled out
where it is short (`KMS` for km/s, `MS`, `KG`, `M`, `KM`, `S`, `JY`, `EV_K` for eV/K) and `SI` where
it is a compound SI unit, so `PF_C_KMS` and `PF_C_MS` are the same constant in two units and
`PF_G_SI` is in m^3 kg^-1 s^-2. A pure number carries none: `PF_PI`, `PF_TNU_OVER_TGAMMA`.

The angle factors say what they convert, so the units cancel on sight: `angle_deg * PF_RAD_PER_DEG`
is in radians, and `angle_rad * PF_DEG_PER_RAD` in degrees. They are not called `PF_DEG2RAD` and
`PF_RAD2DEG`: Fortran names are case-insensitive, so those would be the names of the conversion
functions `pf_deg2rad` and `pf_rad2deg` of [`parquet_utils`](utils.html), which `use parquet` brings
in too, and no program could then call either function through `use parquet`.

## Where the values come from

**Each value is the double nearest its definition**, with three exceptions below. Pi is the double
nearest pi, and the angle factors are folded from it, each landing on the double nearest its own
exact value. The speed of light is exact by the SI definition of the metre. The megaparsec is a
million parsecs of `648000 / pi` astronomical units (IAU 2015 Resolution B2), the astronomical unit
exactly 149 597 870 700 m (IAU 2012 Resolution B2). The gigayear is a thousand million Julian years
of 365.25 days. `GM_sun` is the IAU 2015 nominal value (Resolution B3), exact as written. Newton's
constant is the CODATA 2022 value, the only measured one here. `(4/11)^(1/3)` is the ratio of the
neutrino to the photon temperature after electron-positron annihilation.

**Three are astropy's doubles instead**, because this library's cosmology agrees with astropy
8.0.1 and that agreement runs through them:

- the Stefan-Boltzmann constant, exact in the 2019 SI, is 3 ulp above the double nearest its exact
  value;
- Boltzmann's constant in eV/K, `k / e`, also exact in the 2019 SI, is 1 ulp above;
- the solar mass is `GM_sun / G` folded in double arithmetic, which is how astropy derives its
  `M_sun`, and lands 1 ulp above the double nearest that quotient. It carries the uncertainty of
  `G`, 2.2e-5 relative, where Newton's constant in `Mpc (km/s)^2` per solar mass, being
  `GM_sun / (1e6 Mpc)`, carries none.

**The AB zero point is `10^3.56` Jy**: `m_AB = -2.5 log10(f_nu) - 48.60`, with `f_nu` in
erg s^-1 cm^-2 Hz^-1, puts `m_AB = 0` at `10^(-19.44)` of that unit. The constant is the double
nearest it, `3630.7805477010133`; astropy's `ABflux` is 22 ulp lower, and the rounded 3631 Jy often
quoted is 6.0e-5 higher.

Every constant is held to its bit pattern by the library's test suite, so none of these doubles
moves by accident. A new edition of a measured value, a new CODATA `G` say, changes what the
library answers, and would be released as a change of its own.

## `real64` only

A `real32` program converts at the use, `real(PF_PI, real32)`, which gives the `real32` nearest the
constant.

## What it does not hold

**A value that belongs to one procedure stays with it**, documented there:

- the CMB dipole, which is the default of the optional apex and speed arguments of `pf_zhel2zcmb`
  and `pf_zcmb2zhel` — see [Celestial coordinate systems](skycoord.html);
- the angles defining the coordinate systems of [`parquet_skycoord`](skycoord.html);
- the Julian-date offset `parquet_timestamp` converts with;
- the normal distribution's constants, `sqrt(2 pi)` and its kin, kept by the modules that use them;
- every tuning value, threshold, fit coefficient and default of an argument.

**A constant nothing in the library uses is not here either**: no Planck constant, parsec or light
year. A constant joins when a procedure of this library needs it.

## Thread safety

The module holds nothing but parameters, so there is nothing to share and nothing to guard.
