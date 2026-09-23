---
title: Distances and times in an expanding universe with parquet_cosmology
---

`parquet_cosmology` turns a redshift into a distance, a time, a volume or an angular scale — and
back again. Build a cosmology once, then evaluate it over whole columns. It reaches no reader, no
writer and no setting: `use parquet_cosmology` compiles eleven Fortran files and nothing of the
Arrow stack. `use parquet` brings it in too, so nothing here needs a second import. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

The model is astropy's `w0waCDM`, with every constant and literal as astropy 8.0.1 writes them, so
a `"Planck18"` built here answers what astropy's `Planck18` answers to about one part in a hundred
million. Eight named cosmologies come ready; any flat, open, closed, `wCDM` or `w0waCDM` model of
your own is a call away.

## Quick example

```fortran
use parquet_cosmology
use iso_fortran_env, only : real64

type(pf_cosmology) :: cosmo
real(real64), allocatable :: z(:), dl(:), mabs(:), vol(:)

call cosmo%init("Planck18")
dl   = cosmo%luminosity_distance(z)                 ! Mpc, the whole column in one call
mabs = mag - cosmo%distmod(z)                       ! absolute magnitude, K-correction aside
vol  = cosmo%comoving_volume(z) * area_sr / (4*pi)  ! V_max for a survey of area_sr steradians
```

Every binding that takes a redshift is `elemental`: a scalar or an array of any rank goes in and
the same shape comes out, so a whole catalogue converts in one call — inside your own
`!$omp parallel do` if you like, since a built cosmology is read-only.

## Building a cosmology

Two call forms. Optional arguments are shown in square brackets, with the comma outside; you never
type the brackets.

```fortran
call cosmo%init(name, [zmax], [zmin], [context])
call cosmo%init(h0, om0, [ode0], [tcmb0], [neff], [m_nu], [ob0], [w0], [wa], [name], [zmax], &
                [zmin], [context])
```

The named cosmologies are `Planck18`, `Planck15`, `Planck13`, `WMAP9`, `WMAP7`, `WMAP5`, `WMAP3`
and `WMAP1`, matched without regard to case. `%get_name` gives back the canonical spelling and
`%describe` a one-line summary of the whole model.

```fortran
call cosmo%init("planck18")        ! the canonical name comes back from %get_name
call sim%init(h0=67.66_real64, om0=0.30966_real64, name="my_sim")
```

In the parameter form:

- **`h0`** is `H0` in km/s/Mpc and **`om0`** is `Om0`, which EXCLUDES massive neutrinos — they are
  counted in `Onu0`, as astropy counts them.
- **`ode0` absent means flat**, and sets `Ok0` to exactly zero rather than deriving it by
  subtraction, so `%is_flat()` is a test that cannot fail. Supply an `ode0` and you get whatever
  curvature it implies, including a curvature of exactly zero.
- **`tcmb0` defaults to zero**, which switches radiation and neutrinos off entirely. That is
  astropy's default too, and it is what most cosmology code silently assumes; pass `tcmb0=2.7255`
  (with `neff=` and `m_nu=`) when you want a radiation content, or use a named cosmology, which
  carries one.
- **`m_nu` carries one mass in eV per species**, and there are `floor(neff)` species. A single
  shared mass is written `[m, m, m]`; `floor(neff) == 0` takes a zero-length array or no argument
  at all.
- **`ob0` is optional and unvalidated against the rest**: absent, `%ob0()` answers NaN and
  `%describe` says `unknown`.
- **`name` in the parameter form is a free-text label**, not a check. Nothing stops you labelling
  your own model `"Planck18"`; the label is yours to choose and the library reports it back
  unaltered.
- **`zmax` sets how far the table reaches upward**, default 1100, and **`zmin` how far downward**,
  default `-0.9` and admitted anywhere in `(-1, 0]`. Both change how FAST a query is answered, and
  the answer itself by no more than the table's own accuracy: see
  [Beyond the table](#beyond-the-table). `zmin = 0` tabulates no blueshift at all and builds
  fastest; a model whose `E(z)²` reaches zero at a finite blueshift tabulates only as far as it
  exists, whatever `zmin` asked for.

A built cosmology can be copied with one parameter changed rather than written out again:

```fortran
call base%clone(shifted, h0=70.0_real64)          ! the same model, a different H0
```

`%clone` takes the same optional arguments `%init` does and fills every one you leave out from the
source. A source that is flat because `ode0` was omitted is cloned the same way, so the copy of a
flat model is flat whatever else changed.

## What it answers

Distances in Mpc, times in Gyr, volumes in Mpc³, `H(z)` in km/s/Mpc, the distance modulus in
magnitudes. The names are astropy's method names wherever astropy has one, so a script moving
across finds the same words; the two exceptions are forced by Fortran, which cannot tell `%H` from
`%h` — they are `%hubble` and `%little_h`.

| Binding | Answers |
|---|---|
| `%comoving_distance(z)` | the comoving distance `D_C` |
| `%comoving_distance_zeta(zeta)` | the same, from `zeta = ln(1+z)` directly |
| `%comoving_transverse_distance(z)` | `D_M`, which differs from `D_C` only in a curved model |
| `%luminosity_distance(z)` | `D_L` |
| `%angular_diameter_distance(z)` | `D_A` |
| `%comoving_distance_z1z2(z1, z2)` | `D_C` between two redshifts; NEGATIVE when `z2 < z1` |
| `%angular_diameter_distance_z1z2(z1, z2)` | `D_A` between two redshifts; NEGATIVE when `z2 < z1` |
| `%lookback_time(z)` | how long ago light left `z` |
| `%age(z)` | the age of the universe at `z` |
| `%efunc(z)`, `%inv_efunc(z)` | `E(z) = H(z)/H0`, and its reciprocal |
| `%hubble(z)` | `H(z)` |
| `%distmod(z)` | the distance modulus |
| `%comoving_volume(z)` | the comoving volume out to `z`, over the whole sky |
| `%differential_comoving_volume(z)` | `dV_C/dz/dOmega` |
| `%kpc_proper_per_arcmin(z)`, `%kpc_comoving_per_arcmin(z)` | transverse scale |
| `%arcsec_per_kpc_proper(z)`, `%arcsec_per_kpc_comoving(z)` | its inverse |
| `%lookback_distance(z)` | the lookback time as a distance, `c t_L` |
| `%absorption_distance(z)` | the dimensionless absorption distance, `INT (1+z)²/E dz` |
| `%scale_factor(z)` | `a = 1/(1+z)` |
| `%om(z)`, `%ode(z)`, `%ok(z)`, `%ogamma(z)`, `%onu(z)` | what the universe is made of at `z`; the five sum to one |
| `%otot(z)` | `1 - Ok(z)`; EXACTLY one at every `z` for a flat model |
| `%ob(z)`, `%odm(z)` | the baryon and cold-dark-matter density parameters; NaN when no `ob0` was given |
| `%nu_relative_density(z)` | Komatsu's fit itself, `Onu(z)/Ogamma(z)` |
| `%onu_species(z, v)` | `Onu(z)` split by species, summing to `%onu(z)` |
| `%tcmb(z)` | the CMB temperature at `z`, in K |
| `%tnu(z)` | the neutrino temperature at `z`, in K |
| `%w(z)`, `%de_density_scale(z)` | the dark-energy equation of state, and its density in units of today's |
| `%critical_density(z)` | `rho_crit(z)` in **M_sun/Mpc³** |
| `%z_at_comoving_distance(d)` | the redshift at which `D_C` is `d` |
| `%z_at_lookback_time(t)` | the redshift `t` Gyr ago |
| `%z_at_age(t)` | the redshift at which the universe was `t` Gyr old |
| `%z_at_luminosity_distance(d)`, `%z_at_distmod(mu)` | the redshift at a `D_L` or a distance modulus |

and the model itself: `%h0()`, `%little_h()`, `%om0()`, `%ode0()`, `%ok0()`, `%ogamma0()`,
`%onu0()`, `%ob0()`, `%odm0()`, `%tcmb0()`, `%tnu0()`, `%neff()`, `%w0()`, `%wa()`,
`%hubble_distance()`, `%hubble_time()`, `%zmax()`, `%zmin()`, `%zeta_floor()`, `%is_flat()`,
`%has_massive_nu()`, `%is_initialised()`, and the four subroutines `%get_name`, `%describe`,
`%m_nu` and `%clone`, each handing back an allocatable result or a built copy.

Three of these repay a closer look:

- **The five density parameters sum to one**, because each is a term of `E(z)²` over `E(z)²`
  formed at the same `1 + z`. `%om` excludes massive neutrinos, which are counted in `%onu`, as
  astropy counts them; `%ogamma` is the photons alone. Setting `tcmb0` to zero — the default —
  switches radiation and neutrinos off entirely, so `%ogamma`, `%onu` and `%tcmb` are then exactly
  zero at every redshift whatever `m_nu` says.
- **`%critical_density` answers in solar masses per cubic megaparsec**, not astropy's g/cm³,
  because that is the unit a halo-mass calculation wants. The solar mass is the IAU 2015 nominal
  `GM_sun` divided by the same `G`, which is how astropy derives its own, so the two agree once
  the conversion is asked for.
- **`%z_at_luminosity_distance` and `%z_at_distmod` answer a redshift at or above zero**, and the
  smallest one at which the distance is reached. `D_L` is not one-to-one over the whole domain: a
  blueshift carries it back towards zero as `z` approaches `-1`, and in a closed model `D_M` turns
  over at the antipode. A negative distance therefore answers NaN rather than the blueshift that
  shares it. `%z_at_age` has no such trouble — the age falls monotonically with redshift — and it
  is solved on `ln(age)`, which is what lets it work at the top of the domain, where the age is
  a fifth of an attosecond and `%age(0)` minus a lookback time would keep no digit of it.

`%clear` releases the tables and returns the object to unbuilt; it is harmless on a fresh one. A
second `%init` simply replaces the object, which is how a program switches cosmology. Intrinsic
assignment deep-copies, so `c2 = c1` is an independent clone.

## Redshifts, and composing them

Three free functions need no cosmology at all:

```fortran
zeta  = pf_z2zeta(z)          ! ln(1 + z)
z     = pf_zeta2z(zeta)       ! exp(zeta) - 1
z_obs = pf_z_combine(z1, z2)  ! (1 + z1)(1 + z2) - 1
```

Each is written to keep its digits where the obvious form loses them. `log(1.0 + z)` is wrong in
its ninth digit at `z = 1e-8`, because `1 + z` is rounded before the logarithm; `pf_z2zeta` is
within about two units in the last place everywhere from `1e-14` to `1e10`. `pf_z_combine` is
formed as `z1 + z2 + z1*z2`, which loses nothing where `(1+z1)(1+z2) - 1` would cancel. Use it to
put a peculiar velocity on a cosmological redshift for a mock catalogue:

```fortran
zobs = pf_z_combine(zred, vlos / 299792.458_real64)
d    = sim%comoving_distance(zred)            ! Mpc; divide by sim%little_h() for Mpc/h
zz   = sim%z_at_comoving_distance(d)          ! and back again, to rounding
```

## Beyond the table

`%init` tabulates four integrals — the comoving distance, the lookback time, the age and the
absorption distance — on a uniform grid in `zeta = ln(1+z)` that runs from `zmin` to `zmax` and has
a node at `zeta = 0` exactly. Every other answer is arithmetic over them. A redshift beyond either
end is answered by a fixed quadrature rule from that end instead, so **no redshift is ever
refused, and `zmax` and `zmin` decide how fast — and the answer itself by never more than the
table's own accuracy**: tens of nanoseconds inside the table, and microseconds to tens of
microseconds outside it, rising with how far outside you go —
`bench/benchmark_cosmology.sh` measures both. A program that queries beyond `z = 1100` in a hot
loop passes a larger `zmax=`; a program that never leaves `z < 1` may pass a smaller one and build
faster, and one that never passes a negative redshift may pass `zmin=0` and skip about a third of
the build. Either way the answers agree to the accuracy below.

The fourth integral is the absorption distance's, and it is built whether or not you ask for that
binding, because the object is fixed once `%init` returns. It is about half the build's cost
again; `zmin=0` gives back rather more than it takes.

`%zmax()` and `%zmin()` report what you asked for. Neither is a boundary: nothing is refused at
either. `%zeta_floor()` reports something different — the `zeta` at which this model's `E(z)²`
reaches zero, below which the universe does not exist and every binding answers NaN. For almost
every model that is the domain's own edge.

## The domain, and what is not a number

**Data is total; a mistake in your code aborts.** The redshift domain runs from just above `z = -1`
to `z = 1e10`. Inside it every binding answers a number, or a signed infinity where the
mathematics gives one. Outside it — and for a NaN redshift — every binding answers NaN, quietly,
without raising an IEEE flag. That is deliberate: a catalogue's `-99` and `-1` sentinels for "no
redshift measured" are real inputs to an elemental call over a column, and a NaN is the answer the
[statistics family](statistics.html) already treats as a null, where an abort would take your
program down for one row.

A negative redshift above `-1` is a blueshift, and is answered with its sign: the comoving
distance and the lookback time come back negative, and the universe is older there than it is now.

**Some models stop existing before `z = -1`.** A recollapsing closed universe (`om0 = 1.5` with
`ode0 = 0`) has `E(z)² = 0` at `z = -2/3`, and any model with a negative `Ode0` has one somewhere;
below that point there is no expansion to measure and every binding answers NaN, as it does
outside the domain. `%init` does not refuse such a model — it is a perfectly good universe as far
down as it goes — and `%zeta_floor()` says where it ends. The five inverses know about the floor
too: an argument the model never reaches answers NaN rather than a redshift at which the model
itself answers NaN.

Three admitted inputs answer a signed infinity rather than a number:

- `%distmod(0)` is `-Infinity`, and `%arcsec_per_kpc_proper(0)` and its comoving twin are
  `+Infinity`. Zero distance has no modulus and subtends no angle.
- `%age(z)` is `+Infinity` at every redshift for a model whose age integral diverges — de Sitter
  (`om0 = 0`, `ode0 = 1`) is one. Such a universe really is infinitely old throughout, so this is
  an answer rather than an error, and `%z_at_age` then answers NaN for every age, because none of
  them names a redshift.
- A "big rip" model (`wa > 0`) has a dark-energy density that diverges as `z` approaches `-1`, and
  a model with a large negative `ode0` can overflow the transverse distance. Both answer
  `+Infinity` there, and the distances built on them stop growing. `%de_density_scale(z)` is the
  diverging density itself, and where it has, `%ode(z)` is exactly one while the other four
  density parameters are zero — the limit, rather than one infinity divided by another.

What aborts is a mistake with no sensible reading: evaluating a cosmology that was never built or
has been cleared, a name that is not one of the eight, a parameter outside its admitted range
(`h0` within `[1e-10, 1e10]`, `|w0|` and `|wa|` at most 3, `om0`, `tcmb0` and `neff` non-negative,
`ob0` at most `om0`, `m_nu` of size `floor(neff)`, `zmax` positive and at most `1e10`), a density
parameter absurd enough to overflow the arithmetic, or a model with no big bang — one whose
`E(z)²` goes negative, which `%init` finds while it tabulates and reports with the redshift at
which it happened. Pass `context=` to `%init` to have your own call site named in any such message.

## Accuracy

Every redshift in the domain is answered to about fourteen significant digits or better. The
tables are quintic Hermite over the grid — a value and an analytic slope at every node — and carry
a few parts in `1e15` over `z` from `1e-8` to `100`, checked against a thirty-digit model; the
quadrature rule beyond them is exact to rounding and inherits the table edge's error. The age is
built in its own right, never as `%age(0)` minus `%lookback_time(z)`, which would have no correct
digit left by `z = 1e10`.

`zmax` and `zmin` move the seam between the table and the quadrature, and the answer either side
of the seam differs by no more than the table's own accuracy. Both seams are continuous: the
quadrature starts from the tabulated edge value rather than from zero.

Three accuracy notes worth knowing before you rely on an answer:

- **An inverse is only as well conditioned as the function it inverts.** `dD_C/dz` falls off
  steeply, so at the very top of the domain a distance carrying one unit in the last place fixes
  the redshift only to about one part in ten. That is the mathematics, not the implementation:
  `%comoving_distance(%z_at_comoving_distance(d))` recovers `d` to about `1e-15` over the whole
  domain, while the redshift itself round-trips that well only up to `z` of order a thousand.
  **`%z_at_lookback_time` runs out four decades sooner**, around `z = 1e6`: the lookback time
  saturates towards the age of the universe long before the comoving distance saturates towards
  the horizon, so there is less left in it to name a redshift with. Both still recover the
  QUANTITY that was asked for; it is the redshift that stops being determined.
- **`%comoving_distance_z1z2` and `%angular_diameter_distance_z1z2` of a CLOSE pair are integrated
  directly** rather than differenced, because differencing two tabulated distances keeps only the
  digits the pair's own span leaves. Two redshifts a part in `1e4` apart still pin their
  separation to about twelve digits, and nothing can do better: that is what a `real64` redshift
  carries, not what the library does with it.
- **At a deep blueshift a `real64` redshift cannot carry its own precision.** At `z = -0.99995`
  the quantity `1 + z` is `5e-5`, so `z` pins it to only about eleven digits, and an inverse that
  answers in `z` loses the rest. Work in `zeta` — `%comoving_distance_zeta` — if you need that
  corner.

One deliberate difference from astropy, worth knowing when a script moves across: **astropy 8
defaults `Ob0` to zero, and this module answers NaN.** `%ob0()`, `%odm0()`, `%ob(z)` and `%odm(z)`
are NaN unless an `ob0=` was given, because a missing baryon fraction is not a zero one. Give
`ob0=` and they answer numbers.

## Threads

`%init` and `%clear` are the only bindings that write an object. Every other one is `pure`, so
**one built cosmology may be evaluated from any number of threads at once**, and two cosmologies
share nothing. The module has no variable a user's program can reach that is not a constant.

```fortran
call cosmo%init("Planck18")        ! once, before the region
!$omp parallel do
do i = 1, n
    d(i) = cosmo%comoving_distance(z(i))
end do
!$omp end parallel do
```

Building inside a region is fine too — each thread builds its own object — but a `pf_cosmology`
holds allocatable storage, so give each thread a slot of an array allocated before the region
rather than an OpenMP `private()` copy, exactly as [interpolation](interpolation.html) describes
for `pf_interp_1d`.

The one hazard is the general one: a thread reading a cosmology while another thread is
re-`%init`ing it is a race in your program, and the library cannot see it.

## Limitations

- `real64` only. A `real32` caller converts at the call.
- Scalar `Om0`, `Ode0` and the CPL pair only: no arbitrary `w(z)`, no early dark energy, no
  perturbations, no growth factor and no power spectrum.
- No comoving Cartesian coordinates. They are a sky direction times a comoving distance, three
  lines with [`parquet_skycoord`](skycoord.html) or [`parquet_sphere`](sphere.html), and importing
  either here would add its whole graph to every consumer of this one.
