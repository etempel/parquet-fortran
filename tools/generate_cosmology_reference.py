#!/usr/bin/env python3
"""Generate the reference vectors that pin `parquet_cosmology`'s contract.

`parquet_cosmology` answers distances, times, volumes and their inverses for a FLRW universe. Every
one of them is a plausible WRONG ANSWER rather than an abort: drop the neutrino term from `E(z)` and
`Planck18`'s comoving distance moves by about a part in a thousand, which is far too little to look
like a bug and far too much to be rounding. So the vectors below are derived HERE, from a 30-digit
`mpmath` model of astropy 8.0.1's `w0waCDM` definition, rather than read back out of a Fortran run,
which could only ever confirm that the implementation agrees with itself.

Emitted (COMMITTED to the repository, like every other generator's output):

  test/test_cosmology_vectors.f90  module `test_cosmology_vectors`: the model grid (the eight named
                                   cosmologies and twelve user-defined ones, with their derived
                                   parameters); every quantity of `CQ_NAMES` below at
                                   twenty-five redshifts of each; and a block of
                                   `D_A(z1, z2)` PAIRS, the one stage-1 binding no single-redshift
                                   row reaches.

Usage:  tools/generate_cosmology_reference.py [--check] [--self-test] [--verify-oracle]

  --check          regenerate into memory and compare with the committed file; exit 1 on any
                   difference. Needs `mpmath` and nothing else, so CI's lint job can run it.
  --self-test      re-derive the published anchors -- the constants and the named-cosmology table
                   against src/parquet_cosmology.f90, the twenty Gauss-Legendre abscissae and
                   weights against src/parquet_cosmology_eval.f90, `Planck18`'s six derived values
                   including the `Ogamma0` that catches the SI-versus-km/s slip, the three analytic
                   universes against their closed forms, and every emitted literal's round trip --
                   and exit 1 if any of them fails.
  --verify-oracle  cross-check the emitted rows against astropy 8.0.1 in the workspace `astro`
                   environment, at 1e-6 -- ASTROPY's accuracy, not this model's, and measured
                   rather than chosen: at z = 1e-8 its `comoving_distance` carries about eight
                   significant digits, because its `quad` works to an absolute tolerance on an
                   integral of size 1e-8. Four classes of value are NOT compared, each counted and
                   named in the output rather than dropped quietly: every row above z = 1e5, where
                   astropy's quadrature breaks down (`lookback_time(1e8)` answers 2.0e-9 Gyr
                   against a true 13.7869); `age` above z = 1000, the one quantity astropy
                   integrates to infinity (97% out at z = 1e5); a curved model's `comoving_volume`
                   where astropy's own closed form has cancelled away; and, at a blueshift, the
                   quantities whose sign convention this document fixes rather than inherits.
                   Where astropy raises instead of answering -- it cannot evaluate a big-rip
                   `w0waCDM` at z = -0.99999 at all -- the input and its message are printed.

NEVER HAND-EDIT A VECTOR. A contract change is an edit to the model below plus a regeneration.

--------------------------------------------------------------------------------------------
THE MODEL, AS ASTROPY DEFINES IT

With `x = 1 + z` and `zeta = ln x`:

  E(z)^2    = Om0 x^3 + Ok0 x^2 + Ogamma0 x^4 [1 + nu_rel(z)] + Ode0 f_DE(z)
  f_DE(z)   = x^(3(1 + w0 + wa)) exp(-3 wa z / x)              CPL; exactly 1 when w0 = -1, wa = 0
  nu_rel(z) = 0.22710731766 Neff                               no massive species
  nu_rel(z) = 0.22710731766 (Neff/N_nu) [n_massless + SUM (1 + (0.3173 y_i/x)^1.83)^0.54644808743]
  y_i       = m_nu_i / (k_B[eV/K] T_nu0),  T_nu0 = (4/11)^(1/3) Tcmb0,  N_nu = floor(Neff)

`0.54644808743` is astropy's `KOMATSU_INVP` as written, not `1/1.83`, so that this model IS
astropy's rather than a rounding of it. `Tcmb0 = 0` switches radiation AND neutrinos off entirely,
which is astropy's default for `FlatLambdaCDM(H0, Om0)`; the radiation term is then not formed at
all, so no `0 * inf` arises from an undefined `T_nu0`.

THE RADIATION CHAIN IS ONLY RIGHT IN SI. `rho_gamma0 = 4 sigma_SB Tcmb0^4 / c^3` with `c` in m/s,
`rho_crit0 = 3 H0^2/(8 pi G)` with `H0` in 1/s as `h0 * 1000 / (Mpc in m)`. Substituting the km/s
value of `c` is wrong by 1e27 and still produces a plausible small number, which is why
`--self-test` checks the derived `Ogamma0` and not only the literals.

THE INTEGRALS ARE TAKEN IN zeta, WHICH IS WHAT THE LIBRARY TABULATES:

  D_C(zeta) = D_H INT_0^zeta e^t / E dt
  t_L(zeta) = t_H INT_0^zeta dt / E
  age(zeta) = t_H INT_0^b 2 db' / (b' E(b'^2)),  b = e^(-zeta/2),  b' = sqrt(scale factor)

The age is its own integral, NEVER `age(0) - t_L(z)`: that difference has no correct digit left by
z = 1e10, where `age` is 7.6e-18 Gyr and both terms are 13.7869 Gyr.

THE SCREENS ARE PART OF THE CONTRACT, so the model applies them where the library does. `f_DE` is
`+Infinity` once `L = 3(1 + w0 + wa) ln x - 3 wa z/x` passes 709 (a big-rip model as `z -> -1`),
and `sinh`'s argument is screened at 700. Two places deliberately do NOT screen: the quadratures
read an UNSCREENED `1/E`, because mpmath's exponent range is unbounded and `1/E` there is below
1e-150 -- smaller than the rounding of the integral it sits in -- so keeping it finite avoids a
kink in the integrand while changing no emitted digit.

WHAT IS EXACT AND WHAT IS NOT. Every value is computed at 30 significant digits and correctly
rounded to double, so `--check` gives the same answer on every host. The library reaches the same
numbers through a spline over a Gauss-Kronrod tabulation and libm, so the suite asserts each row to
a tolerance it states rather than bit for bit.
"""

import argparse
import pathlib
import struct
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

import generate_random_golden_vectors as rgv  # noqa: E402
from gen_parallel import map_cases  # noqa: E402  the shared case map

try:
    import mpmath as mp
except ImportError:  # pragma: no cover - reported, not raised
    print("generate_cosmology_reference.py needs mpmath (pip install mpmath).", file=sys.stderr)
    raise

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent
OUT_PATH = REPO_ROOT / "test" / "test_cosmology_vectors.f90"
SPEC_PATH = REPO_ROOT / "src" / "parquet_cosmology.f90"
EVAL_PATH = REPO_ROOT / "src" / "parquet_cosmology_eval.f90"

#: Working precision. A double needs 17 digits; 30 leaves thirteen of margin, and the emitted
#: doubles are unchanged at 25 and at 40, which is the evidence that the margin is real.
PREC = 30
mp.mp.dps = PREC

array = rgv.array
int_literal = rgv.int_literal


def as_double(z):
    """A redshift as the DOUBLE the generated file carries it as, exactly.

    **The row must be the value AT THE ARGUMENT THE TEST PASSES.** `cz_bits` emits the double
    nearest each decimal above, and the Fortran side reads that double back and hands it to the
    library; evaluating the 30-digit model at the decimal instead compares two different
    redshifts. It costs nothing anywhere the two are close in RELATIVE terms, and everything at a
    deep blueshift, where they are not: `1 + z` at `z = -0.99999` is `1e-5`, so half an ulp of
    `z` is a relative `1.1e-12` of `1 + z`, and every quantity built on `1 + z` inherits it --
    including `%tcmb`, which is a multiplication and nothing else. That artifact, not the table,
    was the largest residual in the whole grid.
    """
    return mp.mpf(float(mp.mpf(z)))


def bits64(x):
    """The `transfer` bit pattern of a double (or an mpf rounded to one), as an `int64` literal.

    **A NaN is emitted as the canonical quiet NaN**, `0x7FF8000000000000`, rather than as whatever
    `float(mp.nan)` happens to hand over. IEEE 754 does not specify a NaN's sign, and the two are
    not the same literal: this file has been written on a platform where that sign came out SET
    (`0xFFF8000000000000`) and regenerated on one where it came out clear, which makes `--check`
    fail on whichever machine did not write it, over a difference that pins nothing. The Fortran
    side reads every pattern back through `transfer` and compares VALUES -- a NaN through
    `ieee_is_nan` -- so the stored sign is never part of what is certified.
    """
    v = float(x)
    if v != v:
        return int_literal(0x7FF8000000000000, "int64")
    return int_literal(struct.unpack("<q", struct.pack("<d", v))[0], "int64")


# ---------------------------------------------------------------------------------------------
# The constants. Each is the value the module carries, and
# `--self-test` holds the module's literal to the double nearest the value here.
# ---------------------------------------------------------------------------------------------

C_KMS = mp.mpf("299792.458")                    # exact by definition
C_MS = mp.mpf("2.99792458e8")                   # the same constant, in SI
G_SI = mp.mpf("6.6743e-11")                     # CODATA 2022, m^3 kg^-1 s^-2
SIGMA_SB = mp.mpf("5.6703744191844314e-8")      # W m^-2 K^-4
K_B_EV = mp.mpf("8.617333262145179e-5")         # eV/K
MPC_KM = mp.mpf("3.0856775814913673e19")        # IAU 2015 parsec
MPC_M = mp.mpf("3.0856775814913673e22")
GYR_S = mp.mpf("3.15576e16")                    # Julian year
NU_TEMP_RATIO = mp.mpf("0.7137658555036082")    # (4/11)^(1/3)
GM_SUN = mp.mpf("1.3271244e20")                 # IAU 2015 nominal GM_sun, m^3 s^-2

KOM_A = mp.mpf("0.22710731766")                 # astropy's per-species relativistic density
KOM_B = mp.mpf("1.83")                          # KOMATSU_P
KOM_INVP = mp.mpf("0.54644808743")              # KOMATSU_INVP, as written, not 1/1.83
KOM_C = mp.mpf("0.3173")                        # the fit's scale

# The Aubourg et al. (2015) drag-scale fit, Physical Review D 92, 123516 equation (16). These are
# transcribed from the paper INDEPENDENTLY of `src/parquet_cosmology.f90`'s own copies, and
# `--self-test` compares the two: a digit dropped on either side shows up as a disagreement
# rather than as a shared mistake.
AUB_A = mp.mpf("55.154")                        # Mpc
AUB_B = mp.mpf("72.3")                          # the massive-neutrino exponential
AUB_C = mp.mpf("0.0006")                        # its offset in Onu h^2
AUB_P_CB = mp.mpf("0.25351")                    # the exponent on Ocb h^2
AUB_P_B = mp.mpf("0.12807")                     # the exponent on Ob h^2
NU_MASS_EV = mp.mpf("93.14")                    # SUM m_nu / 93.14 eV is Onu h^2

#: The named constants as the Fortran spec spells them, for `--self-test`.
SPEC_CONSTANTS = {
    "pfc_c_kms": C_KMS,
    "pfc_c_ms": C_MS,
    "pfc_g_si": G_SI,
    "pfc_sigma_sb": SIGMA_SB,
    "pfc_k_b_ev": K_B_EV,
    "pfc_mpc_km": MPC_KM,
    "pfc_mpc_m": MPC_M,
    "pfc_gyr_s": GYR_S,
    "pfc_nu_temp_ratio": NU_TEMP_RATIO,
    "pfc_gm_sun": GM_SUN,
    "pfc_komatsu_a": KOM_A,
    "pfc_komatsu_p": KOM_B,
    "pfc_komatsu_invp": KOM_INVP,
    "pfc_komatsu_c": KOM_C,
    "pfc_aubourg_a": AUB_A,
    "pfc_aubourg_b": AUB_B,
    "pfc_aubourg_c": AUB_C,
    "pfc_aubourg_p_cb": AUB_P_CB,
    "pfc_aubourg_p_b": AUB_P_B,
    "pfc_nu_mass_ev": NU_MASS_EV,
}

#: `%z_drag`'s bracket in `z`, as `src/parquet_cosmology_eval.f90` carries it.
DRAG_Z_LO = mp.mpf("100")
DRAG_Z_HI = mp.mpf("1e5")

#: `exp(L)` is `+Infinity` above this, and `sinh` is screened one unit lower (section 4.8).
#: The CPL exponent is screened FOURTEEN lower again, because what has to stay finite is
#: `Ode0 f_DE` and `Ode0` is admitted up to 1e6, whose logarithm is 13.8: screening `f_DE` alone at
#: 709 leaves the product free to overflow, which raises `IEEE_OVERFLOW` on its way to the same
#: `+Infinity`. No model in the grid has an exponent between the two ceilings.
EXP_CEILING = mp.mpf(709)
DE_EXP_CEILING = mp.mpf(695)
SINH_CEILING = mp.mpf(700)
#: `ln(1 + 1e10)` as the module rounds it: the bottom of the domain the floor scan walks down to.
ZETA_CEILING = mp.mpf("23.02585093004047")

#: The growth ODE's starting point: `PFC_GROWTH_TOP_NODE * PFC_H`, the library's own top node.
#: Its exact value does not matter -- moving it from `z = 1e10` to `z = 1e14` moves the normalised
#: `D` by at most `7.1e-13` anywhere in the domain -- but using the library's makes the comparison
#: one of METHOD rather than of where each side chose to start.
GROWTH_TOP = mp.mpf(2879) * mp.mpf("0.008")
#: The macro-step of the growth integration, and the relative accuracy each one is taken to. The
#: extrapolation table IS the error estimate, so nothing here is asserted: a step that does not
#: reach the tolerance in `GROWTH_ORDERS` refinements returns the best it has and the caller's
#: own convergence test -- `--self-test`'s Einstein-de Sitter anchor -- would see it.
GROWTH_MACRO = mp.mpf("0.5")
GROWTH_TOL = mp.mpf("1e-25")
GROWTH_ORDERS = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
#: How many times a macro-step may be HALVED before the solve gives up and says so. A step that
#: does not converge is halved, never accepted: a CPL model at a deep blueshift has `q` near 46
#: and `f` falling through twenty decades, and a fixed macro-step returns a plausible wrong
#: number there rather than a visibly wrong one -- `f` at `z = -0.99` of `w0wacdm` came out
#: `1.1e-5` against a true `6.1e-23`, which no eye and no tolerance would have caught.
GROWTH_SPLITS = 24


# ---------------------------------------------------------------------------------------------
# The model
# ---------------------------------------------------------------------------------------------

class Cosmology(object):
    """One FLRW cosmology, at 30 digits.

    `ode0=None` means flat: `Ok0` is zero by ASSIGNMENT rather than by subtraction, which is what
    `flat_by_omission_is_exactly_flat` asserts of the library.
    """

    def __init__(self, label, h0, om0, ode0=None, tcmb0=0, neff="3.04", m_nu=None,
                 ob0=None, w0=-1, wa=0):
        self.label = label
        self.h0 = mp.mpf(h0)
        self.om0 = mp.mpf(om0)
        self.tcmb0 = mp.mpf(tcmb0)
        self.neff = mp.mpf(neff)
        self.w0 = mp.mpf(w0)
        self.wa = mp.mpf(wa)
        self.ob0 = None if ob0 is None else mp.mpf(ob0)
        self.flat = ode0 is None

        self.n_nu = int(mp.floor(self.neff))
        if m_nu is None:
            self.m_nu = [mp.mpf(0)] * self.n_nu
        else:
            self.m_nu = [mp.mpf(m) for m in m_nu]
        if len(self.m_nu) != self.n_nu:
            raise SystemExit("%s: m_nu has %d entries, floor(neff) is %d"
                             % (label, len(self.m_nu), self.n_nu))
        self.n_massless = sum(1 for m in self.m_nu if m == 0)
        self.has_massive_nu = self.n_massless != self.n_nu

        self.tnu0 = NU_TEMP_RATIO * self.tcmb0
        h0_si = self.h0 * 1000 / MPC_M
        rho_crit0 = 3 * h0_si ** 2 / (8 * mp.pi * G_SI)
        # In M_sun/Mpc^3, which is `%critical_density`'s unit: the solar mass is the IAU 2015
        # nominal `GM_sun` over this model's own `G`, which is how astropy derives `M_sun` too.
        self.rho_crit0 = rho_crit0 * MPC_M ** 3 / (GM_SUN / G_SI)
        if self.tcmb0 == 0:
            self.ogamma0 = mp.mpf(0)
        else:
            rho_gamma0 = 4 * SIGMA_SB * self.tcmb0 ** 4 / C_MS ** 3
            self.ogamma0 = rho_gamma0 / rho_crit0
        self.onu0 = self.ogamma0 * self.nu_rel(mp.mpf(1))

        if self.flat:
            self.ode0 = 1 - self.om0 - self.ogamma0 - self.onu0
            self.ok0 = mp.mpf(0)
        else:
            self.ode0 = mp.mpf(ode0)
            self.ok0 = 1 - self.om0 - self.ode0 - self.ogamma0 - self.onu0

        self.dh = C_KMS / self.h0                       # Mpc
        self.th = MPC_KM / GYR_S / self.h0              # Gyr
        self.is_lambda = self.w0 == -1 and self.wa == 0

        # The age integral diverges only when nothing grows as the scale factor shrinks: no matter,
        # no curvature, no radiation, and a dark-energy term that does not rise towards a -> 0.
        # de Sitter is the case in the grid.
        self.age_diverges = (self.om0 == 0 and self.ok0 == 0 and self.ogamma0 == 0
                             and 3 * (1 + self.w0 + self.wa) <= 0)
        # The bottom of the model's own domain, found before any quadrature: below it `E^2` is
        # not positive, so the universe does not exist there and every quantity is a NaN.
        self.zeta_floor = self.find_floor()
        self.age0 = mp.inf if self.age_diverges else self.age_of_zeta(mp.mpf(0))

    def find_floor(self):
        """The largest `zeta < 0` at which `E^2 <= 0`, or `-ZETA_CEILING` when there is none.

        The same walk-and-bisect the library does in `cosmology_find_floor`, at 30 digits: unit
        steps down from `zeta = 0`, twenty points inside the first step that is not positive, and
        bisection between the last positive point and the first that is not. Two models in the
        grid have such a floor; for every other one this returns the domain's edge and nothing
        below changes.
        """
        edge = -ZETA_CEILING
        hi = mp.mpf(0)
        lo = mp.mpf(0)
        found = False
        for _ in range(int(ZETA_CEILING) + 2):
            lo = max(hi - 1, edge)
            if self.e2(lo, screened=False) <= 0:
                found = True
                break
            if lo <= edge:
                break
            hi = lo
        if not found:
            return edge
        top, bottom = hi, lo
        for j in range(1, 20):
            mid = hi + (lo - hi) * mp.mpf(j) / 20
            if self.e2(mid, screened=False) <= 0:
                bottom = mid
                break
            top = mid
        for _ in range(200):
            if top - bottom <= mp.mpf("1e-25"):
                break
            mid = (top + bottom) / 2
            if self.e2(mid, screened=False) > 0:
                top = mid
            else:
                bottom = mid
        return bottom

    # -- the expansion function ----------------------------------------------------------------

    def nu_rel(self, x):
        """The relativistic neutrino density in units of the photon density, at `x = 1 + z`.

        `Tcmb0 = 0` switches radiation AND NEUTRINOS off entirely, as astropy does; without that
        first arm a massive species divides by a zero neutrino temperature.
        """
        if self.tnu0 == 0:
            return mp.mpf(0)
        if self.n_nu == 0 or not self.has_massive_nu:
            return KOM_A * self.neff
        total = mp.mpf(self.n_massless)
        for m in self.m_nu:
            if m > 0:
                y = m / (K_B_EV * self.tnu0)
                total += (1 + (KOM_C * y / x) ** KOM_B) ** KOM_INVP
        return KOM_A * (self.neff / self.n_nu) * total

    def dnu_rel(self, x):
        """`dnu_rel/dzeta`, the Komatsu fit's own derivative at `x = 1 + z`.

        `u = (C y / x)^P` is a fixed power of `x` and `dx/dzeta = x`, so `du/dzeta = -P u` and
        each term `(1 + u)^Q` contributes `-P Q u (1 + u)^(Q - 1)`. `P Q` is written as the
        product of the two published constants rather than as 1, for the same reason `KOM_INVP`
        is `0.54644808743` rather than `1/1.83`. Zero for a massless model and for `Tcmb0 = 0`,
        which are the two branches `nu_rel` itself takes.
        """
        if self.tnu0 == 0 or self.n_nu == 0 or not self.has_massive_nu:
            return mp.mpf(0)
        total = mp.mpf(0)
        for m in self.m_nu:
            if m > 0:
                y = m / (K_B_EV * self.tnu0)
                u = (KOM_C * y / x) ** KOM_B
                total -= KOM_B * KOM_INVP * u * (1 + u) ** (KOM_INVP - 1)
        return KOM_A * (self.neff / self.n_nu) * total

    def de2_dzeta(self, zeta):
        """`dE^2/dzeta`, analytic: what the library's `cosmology_de2_dzeta` forms.

        The dark-energy term is written through `w(z)`, since `dln f_DE/dzeta = 3(1 + w(z))`,
        rather than by differentiating the CPL exponent a second time.
        """
        x = mp.exp(zeta)
        de = self.de_term(x)
        w_z = self.w0 + self.wa * (x - 1) / x
        v = 3 * self.om0 * x ** 3 + 2 * self.ok0 * x ** 2 + 3 * (1 + w_z) * de
        if self.ogamma0 == 0:
            return v
        return v + self.ogamma0 * x ** 4 * (4 * (1 + self.nu_rel(x)) + self.dnu_rel(x))

    def f_de_log(self, x):
        """`L`, the logarithm of the CPL factor; `None` for a cosmological constant."""
        if self.is_lambda:
            return None
        return 3 * (1 + self.w0 + self.wa) * mp.log(x) - 3 * self.wa * (x - 1) / x

    def f_de(self, x, screened=True):
        """The CPL dark-energy factor. Screened at `exp(709)`, as the library is (section 4.8)."""
        ell = self.f_de_log(x)
        if ell is None:
            return mp.mpf(1)
        if screened and ell > DE_EXP_CEILING:
            return mp.inf
        if screened and ell < -DE_EXP_CEILING:
            # Symmetric with the ceiling. mpmath's exponent range is unbounded, so nothing here
            # underflows; the floor is the library's contract restated, where `exp(ell)` would
            # otherwise run down through the subnormals and raise `IEEE_UNDERFLOW`.
            return mp.mpf(0)
        return mp.exp(ell)

    def de_term(self, x, screened=True):
        """`Ode0 f_DE`, the dark-energy TERM of `E^2`.

        The product, never the two factors separately: past the CPL screen `f_DE` is `+Infinity`,
        and a model with no dark energy at all would form `0 * Infinity` for a term that is zero.
        """
        if self.ode0 == 0:
            return mp.mpf(0)
        return self.ode0 * self.f_de(x, screened)

    def e2(self, zeta, screened=True):
        """`E^2` at `zeta = ln(1 + z)`."""
        return self.e2_at_x(mp.exp(zeta), screened)

    def e2_at_x(self, x, screened=True):
        """`E^2` from `x = 1 + z`, which is how the CLOSED FORMS form it.

        The library's closed-form bindings screen the caller's `z` and use `1 + z` directly; only
        the quadratures work in `zeta`. Forming the reference's own `E^2` from `exp(ln(1 + z))`
        instead leaves the two a rounding apart, which is invisible in every quantity but the
        ones that CANCEL: `Otot = 1 - Ok` of the Milne universe is exactly zero when both sides
        come from one `x` and an ulp of the working precision when they do not.
        """
        total = self.om0 * x ** 3 + self.ok0 * x ** 2 + self.de_term(x, screened)
        if self.ogamma0 != 0:
            total += self.ogamma0 * x ** 4 * (1 + self.nu_rel(x))
        return total

    def efunc_at_x(self, x):
        """`E` from `x`, screened as `efunc` is."""
        v = self.e2_at_x(x)
        if v == mp.inf:
            return mp.inf
        if v <= 0:
            return mp.nan
        return mp.sqrt(v)

    def efunc(self, zeta):
        """`E(zeta)`, screened; `+Infinity` where the dark-energy factor has overflowed."""
        v = self.e2(zeta)
        if v == mp.inf:
            return mp.inf
        if v <= 0:
            return mp.nan
        return mp.sqrt(v)

    def inv_efunc(self, zeta):
        """`1/E`, UNSCREENED: see the module docstring. Never `0/0`, never a kink."""
        return 1 / mp.sqrt(self.e2(zeta, screened=False))

    # -- the three quadratures -----------------------------------------------------------------

    def comoving_distance_zeta(self, zeta):
        if zeta == 0:
            return mp.mpf(0)
        return self.dh * mp.quad(lambda t: mp.exp(t) * self.inv_efunc(t), [0, zeta])

    def lookback_time_zeta(self, zeta):
        if zeta == 0:
            return mp.mpf(0)
        return self.th * mp.quad(self.inv_efunc, [0, zeta])

    def absorption_distance_zeta(self, zeta):
        """`INT (1+z)^2/E dz` written in `zeta`: `dz = x dzeta` turns `x^2/E` into `x^3/E`."""
        if zeta == 0:
            return mp.mpf(0)
        return mp.quad(lambda t: mp.exp(3 * t) * self.inv_efunc(t), [0, zeta])

    def age_of_zeta(self, zeta):
        """`t_H INT_0^b 2 db/(b E(b^2))` with `b = e^(-zeta/2)`: the age's OWN integral."""
        if self.age_diverges:
            return mp.inf
        b_top = mp.exp(-zeta / 2)

        def integrand(b):
            if b == 0:
                return mp.mpf(0)
            return 2 / (b * mp.sqrt(self.e2(-2 * mp.log(b), screened=False)))

        return self.th * mp.quad(integrand, [0, b_top])

    # -- the sound horizon ---------------------------------------------------------------------

    def sound_r0(self):
        """`R0 = 3 Ob0 / (4 Ogamma0)`, or None where the model has no sound speed at all."""
        if self.ob0 is None or self.ogamma0 == 0:
            return None
        return 3 * self.ob0 / (4 * self.ogamma0)

    def sound_c(self):
        """The tighter of the two crowding scales in `b`, exactly as `sound_scales` picks it.

        The integrand carries a factor `1/sqrt(1 + (b/s)^2)` at `s = 1/sqrt(R0)` and another at
        `s = sqrt(Or0/Om0)`, each with a branch point at `b = i s`. This is only a change of
        variable for the oracle -- `mp.quad` would reach the same number over `b` with the same
        points named -- and it is kept identical to the library's so that the two are comparable
        step for step.
        """
        r0 = self.sound_r0()
        or0 = self.ogamma0 * (1 + KOM_A * self.neff)
        c = None
        if r0 is not None and r0 > 0:
            c = 1 / mp.sqrt(r0)
        if self.om0 > 0 and or0 > 0:
            b_eq = mp.sqrt(or0 / self.om0)
            if c is None or b_eq < c:
                c = b_eq
        return mp.mpf(1) if c is None or not (c > 0) else c

    def sound_horizon_zeta(self, zeta):
        """`r_s(zeta) = D_H INT_0^b_top 2 db / (b^3 E(b^2) sqrt(3 (1 + R0 b^2)))`, `b = sqrt(a)`.

        NaN without an `ob0`, because there is no `R`; exactly zero without photons, because the
        sound speed is then zero at every redshift.
        """
        if self.ob0 is None:
            return mp.nan
        if self.ogamma0 == 0:
            return mp.mpf(0)
        if zeta <= self.zeta_floor:
            return mp.nan
        r0, c = self.sound_r0(), self.sound_c()
        v_top = mp.asinh(mp.exp(-zeta / 2) / c)

        def integrand(v):
            if v == 0:
                return mp.mpf(0)
            b = c * mp.sinh(v)
            return (2 * c * mp.cosh(v)
                    / (b ** 3 * mp.sqrt(self.e2(-2 * mp.log(b), screened=False))
                       * mp.sqrt(3 * (1 + r0 * b * b))))

        n = max(1, int(mp.ceil(v_top)))
        return self.dh * mp.quad(integrand, [v_top * mp.mpf(i) / n for i in range(n + 1)])

    def r_drag(self):
        """The Aubourg et al. (2015) fit, transcribed here independently of the Fortran."""
        if self.ob0 is None or self.ob0 == 0 or self.om0 == 0:
            return mp.nan
        h2 = (self.h0 / 100) ** 2
        onuh2 = sum(self.m_nu, mp.mpf(0)) / NU_MASS_EV
        return (AUB_A * mp.exp(-AUB_B * (onuh2 + AUB_C) ** 2)
                / ((self.om0 * h2) ** AUB_P_CB * (self.ob0 * h2) ** AUB_P_B))

    def z_drag(self):
        """The redshift at which `r_s` equals `r_drag`, solved in `zeta` and NOT on a double grid.

        NaN where `r_drag` is one, where the sound horizon is identically zero, and where `r_d`
        is not attained between `z = 100` and `z = 1e5` -- the same three refusals the library
        makes, so a disagreement is about the root and never about the contract.
        """
        rd = self.r_drag()
        if rd != rd or self.ogamma0 == 0:
            return mp.nan
        lo, hi = mp.log(1 + DRAG_Z_LO), mp.log(1 + DRAG_Z_HI)
        f = lambda t: self.sound_horizon_zeta(t) / rd - 1
        if f(lo) < 0 or f(hi) > 0:
            return mp.nan
        return mp.expm1(mp.findroot(f, (lo, hi), solver="anderson",
                                    tol=mp.mpf(10) ** (-2 * PREC + 8)))

    def z_eq(self):
        """`Om0/Or0 - 1` with the RELATIVISTIC `Or0`: at equality every species is relativistic."""
        if self.ogamma0 == 0:
            return mp.inf
        return self.om0 / (self.ogamma0 * (1 + KOM_A * self.neff)) - 1

    # -- the closed forms over them ------------------------------------------------------------

    def comoving_transverse(self, dc):
        if self.ok0 == 0:
            return dc
        if self.ok0 > 0:
            s = mp.sqrt(self.ok0)
            arg = s * dc / self.dh
            if arg > SINH_CEILING:
                return mp.inf
            return self.dh / s * mp.sinh(arg)
        s = mp.sqrt(-self.ok0)
        return self.dh / s * mp.sin(s * dc / self.dh)

    def comoving_volume(self, dm):
        if self.ok0 == 0:
            return 4 * mp.pi / 3 * dm ** 3
        q = dm / self.dh
        lead = 4 * mp.pi * self.dh ** 3 / (2 * self.ok0)
        if self.ok0 > 0:
            s = mp.sqrt(self.ok0)
            return lead * (q * mp.sqrt(1 + self.ok0 * q ** 2) - mp.asinh(s * q) / s)
        s = mp.sqrt(-self.ok0)
        return lead * (q * mp.sqrt(1 + self.ok0 * q ** 2) - mp.asin(s * q) / s)

    # -- the growth ODE --------------------------------------------------------------------

    def growth_rhs(self, zeta, y):
        """`(df/dzeta, du/dzeta)` at `zeta`, with `y = (f, u)` and `u = ln D + zeta`.

        `df/dzeta = f^2 + q f - (3/2) Om` with `q = 2 - (1/2) dlnE^2/dzeta`, and `du/dzeta` is
        `1 - f`. The second component is `ln D + zeta` rather than `ln D` because that is what the
        library tabulates, and because in an Einstein-de Sitter universe its every increment is
        exactly zero -- which is what makes `D = 1/(1 + z)` an identity on both sides rather than
        an agreement to some number of digits.
        """
        e2 = self.e2(zeta)
        # `E^2 <= 0` is a redshift the model does not reach, and an INFINITE `E^2` -- a CPL
        # model's dark-energy term past the `exp` screen at a deep blueshift -- would make
        # `dlnE^2/dzeta` an `Infinity/Infinity`. Both are the NaN the library answers, and the
        # caller stops there rather than integrating a NaN through several thousand steps.
        if e2 != e2 or e2 <= 0 or e2 == mp.inf:
            return [mp.nan, mp.nan]
        q = 2 - self.de2_dzeta(zeta) / (2 * e2)
        src = 3 * self.om0 * mp.exp(3 * zeta) / (2 * e2)
        return [y[0] * y[0] + q * y[0] - src, 1 - y[0]]

    def _growth_mmid(self, zeta, y, h, n):
        """The modified midpoint rule: `n` substeps of `h/n`, whose error is EVEN in the substep.

        That is what makes Richardson extrapolation in `h^2` gain two orders per refinement
        rather than one, and it is why this rule rather than a Runge-Kutta is the base of the
        extrapolation.
        """
        hs = h / n
        f0 = self.growth_rhs(zeta, y)
        y0 = y
        y1 = [y[k] + hs * f0[k] for k in (0, 1)]
        for m in range(1, n):
            fm = self.growth_rhs(zeta + m * hs, y1)
            y0, y1 = y1, [y0[k] + 2 * hs * fm[k] for k in (0, 1)]
        fe = self.growth_rhs(zeta + h, y1)
        return [(y0[k] + y1[k] + hs * fe[k]) / 2 for k in (0, 1)]

    def _growth_step(self, zeta, y, h, depth=0):
        """One macro-step of width `h`, extrapolated to `GROWTH_TOL`; HALVED when it will not.

        The Neville table's last two diagonal entries differ by the error of the second-to-last,
        so the table is its own estimate and nothing is asserted from outside. A step that does
        not reach the tolerance is split in two rather than returned: the error estimate exists
        precisely so that it can be acted on, and a CPL model at a deep blueshift -- where `q`
        reaches 46 and the step is far outside the extrapolation's reach -- is the case that says
        so. The convergence test is against `max(1, |y|)`, so `f` is judged ABSOLUTELY where it
        is small, which is the only accuracy it has there: `f` at `z = -0.99` of `w0wacdm` is
        `6e-23`, the residue of `exp(-INT q dzeta)` over an integral of about 50.
        """
        table = []
        for j, n in enumerate(GROWTH_ORDERS):
            row = [self._growth_mmid(zeta, y, h, n)]
            # A NaN anywhere in the step means the model stopped existing inside it. Splitting
            # would only find the same NaN at half the width, so it is answered at once.
            if row[0][0] != row[0][0] or row[0][1] != row[0][1]:
                return [mp.nan, mp.nan]
            for k in range(1, j + 1):
                ratio = (mp.mpf(GROWTH_ORDERS[j]) / GROWTH_ORDERS[j - k]) ** 2
                row.append([row[k - 1][c] + (row[k - 1][c] - table[j - 1][k - 1][c]) / (ratio - 1)
                            for c in (0, 1)])
            table.append(row)
            if j >= 2:
                err = max(abs(row[j][c] - row[j - 1][c]) for c in (0, 1))
                scale = max(mp.mpf(1), max(abs(row[j][c]) for c in (0, 1)))
                if err < GROWTH_TOL * scale:
                    return row[j]
        if depth >= GROWTH_SPLITS:
            raise SystemExit("generate_cosmology_reference.py: the growth ODE did not converge "
                             "at zeta = %s with a step of %s" % (mp.nstr(zeta, 12), mp.nstr(h, 6)))
        half = h / 2
        return self._growth_step(zeta + half, self._growth_step(zeta, y, half, depth + 1),
                                 half, depth + 1)

    def growth_solve(self, zetas):
        """`{zeta: (ln D, f)}` at each of `zetas`, normalised to `D(0) = 1`.

        Integrated DOWNWARD from the Meszaros (1974) growing mode at `GROWTH_TOP`, which is the
        one direction the growing mode is an attractor in. `Or0` there is the RELATIVISTIC
        radiation density, `Ogamma0 (1 + 0.2271 Neff)`, because at `a = 1e-10` every species is
        relativistic however massive it is today.

        A model with no matter has nothing to grow and answers NaN everywhere; a redshift at or
        below the model's own floor answers NaN, as every other quantity there does.
        """
        want = sorted(set(list(zetas) + [mp.mpf(0)]), reverse=True)
        nan_all = dict((t, (mp.nan, mp.nan)) for t in want)
        if self.om0 <= 0:
            return nan_all
        or0 = self.ogamma0 * (1 + KOM_A * self.neff)
        if or0 > 0:
            y32 = 3 * (self.om0 / or0) * mp.exp(-GROWTH_TOP) / 2
            f = y32 / (1 + y32)
        else:
            f = mp.mpf(1)                       # the same expression's limit, without the division
        y = [f, mp.mpf(0)]
        zeta = GROWTH_TOP
        out = {}
        for target in want:
            if target <= self.zeta_floor or y[0] != y[0]:
                out[target] = (mp.nan, mp.nan)
                continue
            while zeta > target:
                if y[0] != y[0]:
                    break
                step = min(GROWTH_MACRO, zeta - target)
                y = self._growth_step(zeta, y, -step)
                zeta -= step
            out[target] = (y[1], y[0])
        anchor = out[mp.mpf(0)][0]
        if anchor != anchor:
            return nan_all
        return dict((t, (mp.nan, mp.nan) if v[0] != v[0] else (v[0] - anchor - t, v[1]))
                    for t, v in out.items())

    def growth_at(self, zetas):
        """`{zeta: (D, f)}`: `growth_solve` with the exponential taken."""
        return dict((t, (mp.nan if v[0] != v[0] else mp.exp(v[0]), v[1]))
                    for t, v in self.growth_solve(zetas).items())

    def growth_pair(self, z):
        """`(D, f)` at one redshift, solving the whole grid once and keeping it."""
        if not hasattr(self, "_growth_cache"):
            zs = [mp.log(1 + as_double(t)) for t in REDSHIFTS]
            self._growth_cache = self.growth_at(zs)
        zeta = mp.log(1 + as_double(z))
        if zeta not in self._growth_cache:
            self._growth_cache.update(self.growth_at([zeta]))
        return self._growth_cache[zeta]

    def row(self, z, growth=None):
        """Every stage-1 quantity at one redshift, in the order `CQ_NAMES` fixes."""
        if growth is None:
            growth = self.growth_pair(z)
        z = as_double(z)
        x = 1 + z
        zeta = mp.log(x)
        if zeta <= self.zeta_floor:
            return self.row_below_the_floor(x, z)
        dc = self.comoving_distance_zeta(zeta)
        dm = self.comoving_transverse(dc)
        dl = x * dm
        da = dm / x
        tl = self.lookback_time_zeta(zeta)
        age = self.age_of_zeta(zeta)
        vc = self.comoving_volume(dm)
        ev = self.efunc_at_x(x)
        dv = mp.mpf(0) if ev == mp.inf else self.dh * dm ** 2 / ev
        mu = -mp.inf if dl == 0 else 5 * mp.log10(abs(dl)) + 25
        rad_min = mp.pi / 10800          # one arcminute in radians
        rad_sec = mp.pi / 648000         # one arcsecond in radians
        kpc_p = 1000 * da * rad_min
        kpc_c = 1000 * dm * rad_min
        arc_p = mp.inf if da == 0 else 1 / (1000 * da * rad_sec)
        arc_c = mp.inf if dm == 0 else 1 / (1000 * dm * rad_sec)

        # The density parameters are terms of `E^2` over `E^2`, formed at the same `x`, so the
        # five of them sum to one. Where the CPL factor has overflowed, `E^2` is infinite and the
        # LIMIT is taken -- dark energy is all of it -- rather than `Infinity / Infinity`.
        e2 = self.e2_at_x(x)
        if e2 == mp.inf:
            om_z, ok_z, og_z, onu_z, ode_z = (mp.mpf(0),) * 4 + (mp.mpf(1),)
            rho_z = mp.inf
        elif e2 != e2 or e2 <= 0:
            om_z = ok_z = og_z = onu_z = ode_z = rho_z = mp.nan
        else:
            om_z = self.om0 * x ** 3 / e2
            ok_z = self.ok0 * x ** 2 / e2
            og_z = self.ogamma0 * x ** 4 / e2
            onu_z = og_z * self.nu_rel(x)
            ode_z = self.de_term(x) / e2
            rho_z = self.rho_crit0 * e2
        w_z = self.w0 if self.wa == 0 else self.w0 + self.wa * z / x
        # `Otot`, `Ob` and `Odm` follow the same three regimes the five above do, and `Ob`/`Odm`
        # are NaN at every redshift for a model built without an `ob0`: a missing baryon fraction
        # is not a zero one, which is the one place this module differs from astropy 8 on purpose.
        if e2 == mp.inf:
            otot_z = mp.mpf(1)
            ob_z = odm_z = mp.nan if self.ob0 is None else mp.mpf(0)
        elif e2 != e2 or e2 <= 0:
            otot_z = ob_z = odm_z = mp.nan
        else:
            otot_z = 1 - ok_z
            ob_z = mp.nan if self.ob0 is None else self.ob0 * x ** 3 / e2
            odm_z = mp.nan if self.ob0 is None else (self.om0 - self.ob0) * x ** 3 / e2
        return [dc, dm, dl, da, tl, age, vc, dv, mu, kpc_p, kpc_c, arc_p, arc_c, ev, self.h0 * ev,
                om_z, ode_z, ok_z, og_z, onu_z, self.tcmb0 * x, w_z, self.f_de(x), rho_z,
                self.dh * tl / self.th,
                1 / x, otot_z, ob_z, odm_z, self.tnu0 * x,
                self.absorption_distance_zeta(zeta), self.nu_rel(x), growth[0], growth[1],
                self.sound_horizon_zeta(zeta)]

    def row_below_the_floor(self, x, z):
        """The row at a `zeta` at or below `zeta_floor`, where `E^2` is not positive.

        Everything that needs `E` is a NaN, which is what the library answers there: the walk
        that would form it sums a quiet NaN, and `%efunc` refuses to take a square root of a
        non-positive `E^2`. THREE quantities survive, because none of them touches `E` at all --
        the CMB temperature, the equation of state and the dark-energy scale factor are functions
        of the redshift alone -- and the library answers all three there, so the reference must
        carry them rather than a blanket NaN.
        """
        nan = mp.nan
        w_z = self.w0 if self.wa == 0 else self.w0 + self.wa * z / x
        return [nan] * 20 + [self.tcmb0 * x, w_z, self.f_de(x), nan, nan,
                             1 / x, nan, nan, nan, self.tnu0 * x, nan, self.nu_rel(x), nan, nan,
                             nan]

    def angular_diameter_z1z2(self, z1, z2):
        """astropy's transverse-of-the-difference form; NEGATIVE when `z2 < z1`."""
        z1, z2 = as_double(z1), as_double(z2)
        dc1 = self.comoving_distance_zeta(mp.log(1 + z1))
        dc2 = self.comoving_distance_zeta(mp.log(1 + z2))
        return self.comoving_transverse(dc2 - dc1) / (1 + z2)


#: The quantities of each row, in the order the emitted arrays carry them.
CQ_NAMES = ["dc", "dm", "dl", "da", "tl", "age", "vc", "dv", "mu",
            "kpc_proper", "kpc_comoving", "arcsec_proper", "arcsec_comoving", "efunc", "hubble",
            "om", "ode", "ok", "ogamma", "onu", "tcmb", "w", "de_density_scale",
            "critical_density", "lookback_distance",
            "scale_factor", "otot", "ob", "odm", "tnu", "absorption_distance",
            "nu_relative_density", "growth", "growth_rate", "sound_horizon"]

#: The quantities that do not touch `E` at all, and so survive below a model's own floor.
CQ_NO_EFUNC = {"tcmb", "w", "de_density_scale", "scale_factor", "tnu", "nu_relative_density"}


# ---------------------------------------------------------------------------------------------
# The models and the redshifts
# ---------------------------------------------------------------------------------------------

#: astropy 8.0.1's realizations, read from the installed package. Every one is flat.
NAMED = [
    ("WMAP1", "72.0", "0.257", "2.725", "3.04", ["0", "0", "0"], "0.0436"),
    ("WMAP3", "70.1", "0.276", "2.725", "3.04", ["0", "0", "0"], "0.0454"),
    ("WMAP5", "70.2", "0.277", "2.725", "3.04", ["0", "0", "0"], "0.0459"),
    ("WMAP7", "70.4", "0.272", "2.725", "3.04", ["0", "0", "0"], "0.0455"),
    ("WMAP9", "69.32", "0.2865", "2.725", "3.04", ["0", "0", "0"], "0.04628"),
    ("Planck13", "67.77", "0.30712", "2.7255", "3.046", ["0", "0", "0.06"], "0.048252"),
    ("Planck15", "67.74", "0.3075", "2.7255", "3.046", ["0", "0", "0.06"], "0.0486"),
    ("Planck18", "67.66", "0.30966", "2.7255", "3.046", ["0", "0", "0.06"], "0.04897"),
]

#: The redshifts every model is tabulated at (section 9.2).
REDSHIFTS = ["1e-8", "1e-6", "1e-4", "1e-3", "0.01", "0.1", "0.3", "0.5", "1", "2", "3", "5",
             "7", "10", "20", "50", "100", "1100", "2000", "1e5", "1e10",
             "-0.001", "-0.5", "-0.99", "-0.99999"]

#: The redshift above which `--verify-oracle` stops comparing: astropy's quadrature breaks down.
ORACLE_CEILING = mp.mpf("1e5")


def models():
    """The eight named cosmologies, then the user-defined grid of section 9.2."""
    out = []
    for name, h0, om0, tcmb0, neff, m_nu, ob0 in NAMED:
        out.append(Cosmology(name, h0, om0, tcmb0=tcmb0, neff=neff, m_nu=m_nu, ob0=ob0))
    out.append(Cosmology("flat_no_rad", "70", "0.3"))
    out.append(Cosmology("open", "70", "0.3", ode0="0.6"))
    out.append(Cosmology("closed", "70", "0.3", ode0="0.8"))
    out.append(Cosmology("wcdm", "70", "0.3", w0="-0.9"))
    out.append(Cosmology("w0wacdm", "70", "0.3", w0="-0.9", wa="0.3"))
    out.append(Cosmology("rad_massless", "70", "0.3", tcmb0="2.7255", neff="3.046"))
    out.append(Cosmology("one_massive", "70", "0.3", tcmb0="2.7255", neff="3.046",
                         m_nu=["0", "0", "0.06"]))
    out.append(Cosmology("two_massive", "70", "0.3", tcmb0="2.7255", neff="3.046",
                         m_nu=["0", "0.05", "0.1"]))
    out.append(Cosmology("neff_zero", "70", "0.3", tcmb0="2.7255", neff="0", m_nu=[]))
    out.append(Cosmology("einstein_de_sitter", "70", "1", ode0="0"))
    out.append(Cosmology("milne", "70", "0", ode0="0"))
    out.append(Cosmology("de_sitter", "70", "0", ode0="1"))
    # The two models whose domain is TRUNCATED: `E^2` reaches zero at a finite blueshift, so the
    # universe does not exist below it and the rows there are NaN. They are the only models in
    # the grid with that property, which is exactly why they are pinned at 30 digits -- the
    # inverses screen against bounds taken AT that floor, and nothing else here would move if
    # those bounds went wrong.
    # The eight named cosmologies are the only ones above with an `ob0`, and all eight are flat
    # LCDM with a realistic baryon fraction. These two carry a baryon density into the parts of
    # the parameter space they do not reach: curvature and CPL dark energy inside the sound
    # integral, and a baryon fraction six times a realistic one, which is what decides WHICH of
    # the two crowding scales `sound_c` picks (`1/sqrt(R0)` here, `sqrt(Or0/Om0)` for the eight).
    out.append(Cosmology("bao_open_cpl", "70", "0.3", ode0="0.6", tcmb0="2.7255", neff="3.046",
                         m_nu=["0", "0", "0"], ob0="0.05", w0="-0.9", wa="0.3"))
    out.append(Cosmology("bao_baryon_rich", "70", "0.3", tcmb0="2.7255", neff="3.046",
                         m_nu=["0", "0", "0.06"], ob0="0.29"))
    out.append(Cosmology("recollapse_closed", "70", "1.5", ode0="0"))
    out.append(Cosmology("negative_ode0", "70", "0.3", ode0="-0.5"))
    return out


#: The `D_A(z1, z2)` pairs, and the three models they are taken over.
PAIRS = [("0.1", "0.5"), ("0.5", "2"), ("2", "1100"), ("0.5", "0.5"), ("2", "0.5"),
         ("0.5", "0.5001"), ("2", "2.000002"), ("0.1", "0.6")]
PAIR_MODELS = ["flat_no_rad", "open", "closed"]


# ---------------------------------------------------------------------------------------------
# The twenty-point Gauss-Legendre rule the fallback uses (section 4.4)
# ---------------------------------------------------------------------------------------------

def gauss_legendre_20():
    """The 20 abscissae and weights on `[-1, 1]`, at the working precision.

    Newton from the Chebyshev guess, which is within `1e-2` of every root, on the Legendre
    polynomial evaluated by its own three-term recurrence -- `mp.legendre` would do, but the
    recurrence gives `P'` in the same pass and is what `--self-test` re-derives.
    """
    n = 20
    nodes, weights = [], []
    for i in range(1, n + 1):
        t = mp.cos(mp.pi * (i - mp.mpf(1) / 4) / (n + mp.mpf(1) / 2))
        for _ in range(100):
            p_prev, p = mp.mpf(1), t
            for k in range(2, n + 1):
                p_prev, p = p, ((2 * k - 1) * t * p - (k - 1) * p_prev) / k
            dp = n * (t * p - p_prev) / (t * t - 1)
            step = p / dp
            t -= step
            # The threshold must stay ABOVE the working precision's own floor, or the step
            # oscillates in the last digits and the loop never ends.
            if step == 0 or abs(step) < mp.mpf(10) ** (-(PREC - 2)):
                break
        else:  # pragma: no cover - a root that will not converge is a bug, not an input
            raise SystemExit("gauss_legendre_20: Newton did not converge for root %d" % i)
        p_prev, p = mp.mpf(1), t
        for k in range(2, n + 1):
            p_prev, p = p, ((2 * k - 1) * t * p - (k - 1) * p_prev) / k
        dp = n * (t * p - p_prev) / (t * t - 1)
        nodes.append(t)
        weights.append(2 / ((1 - t * t) * dp * dp))
    order = sorted(range(n), key=lambda j: nodes[j])
    return [nodes[j] for j in order], [weights[j] for j in order]


# ---------------------------------------------------------------------------------------------
# Emission
# ---------------------------------------------------------------------------------------------

BANNER = """!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_cosmology_reference.py
! The contract model lives in that script; edit it there, not here. Never hand-edit a vector.
!"""

LABEL_WIDTH = 18


def char_array(name, size_expr, items, width):
    """`character(len=width), parameter :: name(size_expr) = [...]`, one literal per line."""
    for t in items:
        if '"' in t or len(t) > width:
            raise SystemExit("text %r cannot be emitted into %s" % (t, name))
    out = ['    character(len=%d), parameter :: %s(%s) = [character(len=%d) :: &'
           % (width, name, size_expr, width)]
    for n, t in enumerate(items):
        out.append('        "%s"%s' % (t, ", &" if n < len(items) - 1 else "]"))
    return out


def logical_array(name, size_expr, flags):
    return array("logical", name, size_expr, [".true." if f else ".false." for f in flags])


#: Entries per part of a split table. A statement may carry at most 255 continuation lines (F2008
#: C1003), which ONLY nagfor enforces -- an over-long one builds clean under gfortran, ifx and
#: flang and breaks a nagfor run alone. Four `int64` literals fit on a line here, so 800 entries is
#: about 200 continuations, with margin.
PART_ENTRIES = 800


def big_array(decl, name, size_expr, items):
    """`name` as a `parameter`, split into parts and rejoined, so no statement runs too long."""
    if len(items) <= PART_ENTRIES:
        return array(decl, name, size_expr, items)
    out, parts = [], []
    for n, start in enumerate(range(0, len(items), PART_ENTRIES), start=1):
        chunk = items[start:start + PART_ENTRIES]
        part = "%s_p%d" % (name, n)
        parts.append(part)
        out.append("    !> Part %d of `%s`; see the note on `PART_ENTRIES` in the generator."
                   % (n, name))
        out += array(decl, part, "%d" % len(chunk), chunk)
    out.append("    !> The parts above, rejoined.")
    out += array(decl, name, size_expr, parts)
    return out


def _row_of(job):
    """One `(model, z)` job's stage-1 row. Module-level so that `map_cases` can pickle it."""
    model, z, growth = job
    return model.row(z, growth)


def _growth_of(model):
    """One model's whole growth solve, in redshift order. Its own job, for its own reason.

    The ODE runs from `GROWTH_TOP` down through every redshift in ONE pass, so it is a job per
    MODEL where everything else here is a job per (model, redshift): fanning it out per row would
    solve the same equation twenty-five times.
    """
    got = model.growth_at([mp.log(1 + as_double(z)) for z in REDSHIFTS])
    return ([got[mp.log(1 + as_double(z))] for z in REDSHIFTS], got[mp.mpf(0)][1])
    # `D(0)` is exactly one by construction and needs no reference; `f(0)` is the value a caller
    # forming `f sigma8` at the present day reads, and the redshift grid has no zero in it.


def _scalars_of(model):
    """One model's three sound-horizon scalars. Its own job: `z_drag` is a root solve."""
    return (model.r_drag(), model.z_drag(), model.z_eq())


def _pair_of(job):
    """One `(model, z1, z2)` job's angular diameter distance. Module-level for the same reason."""
    model, z1, z2 = job
    return model.angular_diameter_z1z2(z1, z2)


def _pair_dc_of(job):
    """The same pair's COMOVING separation, `D_C(z2) - D_C(z1)`, signed."""
    model, z1, z2 = job
    return (model.comoving_distance_zeta(mp.log(1 + as_double(z2)))
            - model.comoving_distance_zeta(mp.log(1 + as_double(z1))))


def gen_module():
    ms = models()
    growth = map_cases(_growth_of, ms)
    scalars = map_cases(_scalars_of, ms)
    row_jobs = [(m, z, g) for m, gs in zip(ms, growth) for z, g in zip(REDSHIFTS, gs[0])]
    by_label = {m.label: m for m in ms}
    pair_jobs = [(by_label[label], z1, z2) for label in PAIR_MODELS for z1, z2 in PAIRS]
    # Every row and every pair is an independent quadrature over its own model, and together they
    # are effectively this generator's whole run time. `map_cases` keeps the order, so `rows` and
    # `pairs` are assembled exactly as the comprehensions they replace built them.
    row_values = map_cases(_row_of, row_jobs)
    pair_values = map_cases(_pair_of, pair_jobs)
    pair_dc_values = map_cases(_pair_dc_of, pair_jobs)
    rows = [(m, z, value) for (m, z, _), value in zip(row_jobs, row_values)]
    pairs = [(model.label, z1, z2, value)
             for (model, z1, z2), value in zip(pair_jobs, pair_values)]
    pair_dcs = list(pair_dc_values)

    L = [BANNER]
    L.append("!> Golden rows for `parquet_cosmology`, derived from a %d-digit mpmath model of"
             " astropy 8.0.1's" % PREC)
    L.append("!! `w0waCDM` definition. Nothing here is read back out of a Fortran run.")
    L.append("!!")
    L.append("!! Reals are stored as `transfer` bit patterns of the correctly rounded double, so no"
             " value is lost to")
    L.append("!! a decimal literal. The library reaches the same numbers through a spline over a"
             " Gauss-Kronrod")
    L.append("!! tabulation and libm, so `test/test_cosmology.f90` asserts each row to a tolerance"
             " it states.")
    L.append("!!")
    L.append("!! Row `(i, j)` is model `i` at redshift `j`; quantity `q` of it is")
    L.append("!! `crow_bits(((i - 1) * n_cz + (j - 1)) * n_cquantity + q)`.")
    L.append("module test_cosmology_vectors")
    L.append("    use iso_fortran_env, only: int64")
    L.append("    implicit none")
    L.append("    public")
    L.append("")

    L.append("    ! ---- The quantity order of every row ----")
    L.append("    integer, parameter :: n_cquantity = %d !! Quantities per row." % len(CQ_NAMES))
    for n, name in enumerate(CQ_NAMES, start=1):
        L.append("    integer, parameter :: cq_%s = %d !! `%s` in a row." % (name, n, name))
    L.append("")

    L.append("    ! ---- The models: the eight named cosmologies, then the user-defined grid ----")
    L.append("    integer, parameter :: n_cmodel = %d !! Models in the grid." % len(ms))
    L.append("    integer, parameter :: c_label_len = %d !! Length of a model label." % LABEL_WIDTH)
    L.append("    !> The label of each model; the first %d are the canonical names `%%init` takes."
             % len(NAMED))
    L += char_array("cmodel_label", "n_cmodel", [m.label for m in ms], LABEL_WIDTH)
    L.append("    !> `.true.` where the model is one of the eight named cosmologies.")
    L += logical_array("cmodel_named", "n_cmodel", [n < len(NAMED) for n in range(len(ms))])
    L.append("    !> `.true.` where `ode0` was ABSENT, so `%init` must be called without it.")
    L += logical_array("cmodel_flat", "n_cmodel", [m.flat for m in ms])
    L.append("    !> `.true.` where `ob0` was given; `%ob0()` is NaN for the rest.")
    L += logical_array("cmodel_has_ob0", "n_cmodel", [m.ob0 is not None for m in ms])
    L.append("    !> `.true.` where at least one species has a positive mass.")
    L += logical_array("cmodel_has_massive_nu", "n_cmodel", [m.has_massive_nu for m in ms])
    L.append("    !> `.true.` where the age integral diverges, so `%age` is `+Infinity` throughout.")
    L += logical_array("cmodel_age_diverges", "n_cmodel", [m.age_diverges for m in ms])
    L.append("    !> `floor(neff)`: the number of neutrino species, and the size `m_nu` must have.")
    L += array("integer", "cmodel_nnu", "n_cmodel", ["%d" % m.n_nu for m in ms])
    L.append("")

    offsets, flat_mnu = [1], []
    for m in ms:
        flat_mnu.extend(m.m_nu)
        offsets.append(offsets[-1] + len(m.m_nu))
    L.append("    !> Model `i`'s masses are `cmodel_mnu_bits(cmodel_mnu_off(i) : cmodel_mnu_off(i"
             " + 1) - 1)`.")
    L += array("integer", "cmodel_mnu_off", "n_cmodel + 1", ["%d" % o for o in offsets])
    L.append("    integer, parameter :: n_cmnu = %d !! Masses over the whole grid." % len(flat_mnu))
    L.append("    !> Every species mass in eV, model by model.")
    L += big_array("integer(int64)", "cmodel_mnu_bits", "max(n_cmnu, 1)",
               [bits64(v) for v in flat_mnu] or ["0_int64"])
    L.append("")

    par_names = ["h0", "om0", "ode0", "tcmb0", "neff", "ob0", "w0", "wa"]
    L.append("    ! ---- The parameters of each model, in the order `%init` takes them ----")
    L.append("    integer, parameter :: n_cparam = %d !! Parameters per model." % len(par_names))
    for n, name in enumerate(par_names, start=1):
        L.append("    integer, parameter :: cp_%s = %d !! `%s` of a model." % (name, n, name))
    L.append("    !> `ode0` is the DERIVED value for a flat model, which is what `%ode0()` answers;")
    L.append("    !! `ob0` is NaN where `cmodel_has_ob0` is `.false.`.")
    par_values = []
    for m in ms:
        par_values += [m.h0, m.om0, m.ode0, m.tcmb0, m.neff,
                       mp.nan if m.ob0 is None else m.ob0, m.w0, m.wa]
    L += big_array("integer(int64)", "cmodel_par_bits", "n_cparam * n_cmodel",
               [bits64(v) for v in par_values])
    L.append("")

    der_names = ["ok0", "ogamma0", "onu0", "tnu0", "dh", "th", "age0", "little_h", "odm0",
                 "growth_rate0", "r_drag", "z_drag", "z_eq"]
    L.append("    ! ---- The values `%init` must derive from them ----")
    L.append("    integer, parameter :: n_cderived = %d !! Derived values per model." % len(der_names))
    for n, name in enumerate(der_names, start=1):
        L.append("    integer, parameter :: cd_%s = %d !! Derived `%s`." % (name, n, name))
    L.append("    !> `age0` is `+Infinity` where `cmodel_age_diverges` is `.true.`; `odm0` is NaN")
    L.append("    !! where `cmodel_has_ob0` is `.false.`; `growth_rate0` is NaN where `om0` is zero.")
    L.append("    !! `r_drag` and `z_drag` are NaN without an `ob0`, and `z_drag` also where")
    L.append("    !! `Tcmb0` is zero; `z_eq` is `+Infinity` there and a number everywhere else.")
    der_values = []
    for m, g, s in zip(ms, growth, scalars):
        der_values += [m.ok0, m.ogamma0, m.onu0, m.tnu0, m.dh, m.th, m.age0, m.h0 / 100,
                       mp.nan if m.ob0 is None else m.om0 - m.ob0, g[1], s[0], s[1], s[2]]
    L += big_array("integer(int64)", "cmodel_derived_bits", "n_cderived * n_cmodel",
               [bits64(v) for v in der_values])
    L.append("")

    L.append("    ! ---- The redshifts every model is tabulated at ----")
    L.append("    integer, parameter :: n_cz = %d !! Redshifts per model." % len(REDSHIFTS))
    L.append("    !> The redshifts, in the order the rows carry them.")
    L += big_array("integer(int64)", "cz_bits", "n_cz", [bits64(mp.mpf(z)) for z in REDSHIFTS])
    L.append("    !> `.true.` where astropy's own quadrature is trustworthy, so"
             " `--verify-oracle` compares.")
    L += logical_array("cz_oracle_ok", "n_cz", [mp.mpf(z) <= ORACLE_CEILING for z in REDSHIFTS])
    L.append("")

    L.append("    ! ---- The rows ----")
    L.append("    integer, parameter :: n_crow = n_cmodel * n_cz !! Rows in the grid.")
    L.append("    !> Every quantity of every row; see the module's own note for the index rule.")
    L += big_array("integer(int64)", "crow_bits", "n_cquantity * n_crow",
               [bits64(v) for _, _, r in rows for v in r])
    L.append("")

    L.append("    ! ---- `D_A(z1, z2)`: the one stage-1 binding no single-redshift row reaches ----")
    L.append("    integer, parameter :: n_cpair = %d !! Redshift pairs." % len(pairs))
    L.append("    !> The model each pair is taken over, as an index into the grid above.")
    L += array("integer", "cpair_model", "n_cpair",
               ["%d" % ([m.label for m in ms].index(label) + 1) for label, _, _, _ in pairs])
    L.append("    !> `z1` then `z2` of each pair.")
    L += big_array("integer(int64)", "cpair_z_bits", "2 * n_cpair",
               [bits64(mp.mpf(v)) for _, z1, z2, _ in pairs for v in (z1, z2)])
    L.append("    !> `D_A(z1, z2)` in Mpc; NEGATIVE where `z2 < z1`, as astropy answers it.")
    L += big_array("integer(int64)", "cpair_da_bits", "n_cpair", [bits64(v) for _, _, _, v in pairs])
    L.append("    !> `D_C(z2) - D_C(z1)` of each pair, in Mpc, signed. The CLOSE pairs are what")
    L.append("    !! `%comoving_distance_z1z2` exists for: differencing two tabulated distances")
    L.append("    !! keeps only the digits the pair's own span leaves.")
    L += big_array("integer(int64)", "cpair_dc_bits", "n_cpair", [bits64(v) for v in pair_dcs])
    L.append("")
    L.append("end module test_cosmology_vectors ! GCOVR_EXCL_LINE")
    return "\n".join(L) + "\n"


# ---------------------------------------------------------------------------------------------
# Reading the published anchors back out of the Fortran
# ---------------------------------------------------------------------------------------------

import re  # noqa: E402  (used only by the self-test below)

#: A Fortran `real64` literal: optional sign, digits, optional fraction, optional exponent.
_REAL = r"[-+]?[0-9]+(?:\.[0-9]*)?(?:[eEdD][-+]?[0-9]+)?_real64"


def _to_float(text):
    return float(text.replace("_real64", "").replace("d", "e").replace("D", "e"))


def source_real(src, name):
    """The value of `real(real64), parameter :: <name> = <literal>`, or None."""
    m = re.search(r"\b%s\s*=\s*(%s)" % (re.escape(name), _REAL), src)
    return None if m is None else _to_float(m.group(1))


def source_real_array(src, name):
    """The values of `real(real64), parameter :: <name>(...) = [ ... ]`, continuations joined."""
    m = re.search(r"\b%s\s*\([^)]*\)\s*=\s*\[" % re.escape(name), src)
    if m is None:
        return None
    depth, i = 1, m.end()
    while i < len(src) and depth:
        if src[i] == "[":
            depth += 1
        elif src[i] == "]":
            depth -= 1
        i += 1
    body = src[m.end():i - 1]
    return [_to_float(t) for t in re.findall(_REAL, body)]


def source_text_array(src, name):
    """The double-quoted items of `character(len=*), parameter :: <name>(...) = [...]`."""
    m = re.search(r"\b%s\s*\([^)]*\)\s*=\s*\[" % re.escape(name), src)
    if m is None:
        return None
    depth, i = 1, m.end()
    while i < len(src) and depth:
        if src[i] == "[":
            depth += 1
        elif src[i] == "]":
            depth -= 1
        i += 1
    return [t.strip() for t in re.findall(r'"([^"]*)"', src[m.end():i - 1])]


def self_test():
    bad = []
    skipped = []

    def check(ok, what):
        if not ok:
            bad.append(what)

    def exact(got, want, what):
        """The published literal must BE the double nearest the model's value."""
        if got is None:
            bad.append("%s: not found in the source" % what)
        elif got != float(want):
            bad.append("%s: source has %r, the double nearest the model's value is %r"
                       % (what, got, float(want)))

    def close(got, want, tol, what):
        if not abs(float(got) - float(want)) <= tol * abs(float(want)):
            bad.append("%s: got %r want %r (relative tolerance %g)"
                       % (what, float(got), float(want), tol))

    ms = models()
    by_label = {m.label: m for m in ms}
    # The growth solves are most of this function's run time and each is independent of every
    # other, so they are fanned out exactly as the emitted rows are and seeded into the caches
    # `row` and `growth_pair` read. Serially they cost 45 seconds; this is the same 3 the rest of
    # the generator takes.
    for m, g in zip(ms, map_cases(_growth_of, ms)):
        m._growth_cache = dict(zip([mp.log(1 + as_double(z)) for z in REDSHIFTS], g[0]))

    # -- the constants and the named table, against the spec ---------------------------------
    if SPEC_PATH.exists():
        src = SPEC_PATH.read_text()
        for name, value in SPEC_CONSTANTS.items():
            exact(source_real(src, name), value, "%s in %s" % (name, SPEC_PATH.name))
        names = source_text_array(src, "pfc_named_name")
        check(names == [n for n, *_ in NAMED],
              "pfc_named_name in %s is %r, not astropy's eight" % (SPEC_PATH.name, names))
        fields = {"h0": 1, "om0": 2, "tcmb0": 3, "neff": 4, "ob0": 6}
        for field, col in fields.items():
            got = source_real_array(src, "pfc_named_" + field)
            want = [float(mp.mpf(row[col])) for row in NAMED]
            if got is None:
                bad.append("pfc_named_%s not found in %s" % (field, SPEC_PATH.name))
            elif got != want:
                bad.append("pfc_named_%s in %s is %r, not astropy's %r"
                           % (field, SPEC_PATH.name, got, want))
        got = source_real_array(src, "pfc_named_mnu")
        want = [float(mp.mpf(m)) for row in NAMED for m in row[5]]
        if got is None:
            bad.append("pfc_named_mnu not found in %s" % SPEC_PATH.name)
        elif got != want:
            bad.append("pfc_named_mnu in %s is %r, not astropy's %r" % (SPEC_PATH.name, got, want))
    else:
        skipped.append("%s does not exist yet: the constants and the named table are unchecked"
                       % SPEC_PATH.relative_to(REPO_ROOT))

    # -- the Gauss-Legendre rule, against the eval submodule ----------------------------------
    nodes, weights = gauss_legendre_20()
    if EVAL_PATH.exists():
        src = EVAL_PATH.read_text()
        for name, want in (("pfc_gl_x", nodes), ("pfc_gl_w", weights)):
            got = source_real_array(src, name)
            if got is None:
                bad.append("%s not found in %s" % (name, EVAL_PATH.name))
            elif len(got) != len(want):
                bad.append("%s in %s has %d entries, not %d"
                           % (name, EVAL_PATH.name, len(got), len(want)))
            else:
                for n, (g, w) in enumerate(zip(got, want), start=1):
                    exact(g, w, "%s(%d) in %s" % (name, n, EVAL_PATH.name))
    else:
        skipped.append("%s does not exist yet: the Gauss-Legendre rule is unchecked"
                       % EVAL_PATH.relative_to(REPO_ROOT))

    # The rule must integrate every polynomial it claims to: degree 2n - 1 = 39.
    for degree in (0, 1, 7, 38, 39):
        got = sum(w * t ** degree for t, w in zip(nodes, weights))
        want = mp.mpf(0) if degree % 2 else mp.mpf(2) / (degree + 1)
        check(abs(got - want) < mp.mpf(10) ** (-(PREC - 6)),
              "the 20-point rule is wrong at degree %d: %s against %s"
              % (degree, mp.nstr(got, 12), mp.nstr(want, 12)))

    # -- Planck18's derived values, the Ogamma0 among them ------------------------------------
    p = by_label["Planck18"]
    for field, want in (("ode0", "0.6888463055445441"), ("ogamma0", "5.402015137139353e-5"),
                        ("onu0", "0.0014396743040845244"), ("tnu0", "1.9453688391750839"),
                        ("dh", "4430.866952409105"), ("th", "14.451555153425794")):
        close(getattr(p, field), mp.mpf(want), 1e-14, "Planck18 %s against astropy" % field)
    close(p.age0, mp.mpf("13.786885302009706"), 1e-12, "Planck18 age(0) against astropy")

    # The SI-versus-km/s slip: the km/s value of `c` is wrong by 1e27 here and still small.
    wrong = 4 * SIGMA_SB * p.tcmb0 ** 4 / C_KMS ** 3 / (3 * (p.h0 * 1000 / MPC_M) ** 2
                                                        / (8 * mp.pi * G_SI))
    check(abs(float(wrong) - float(p.ogamma0)) > 1e-10,
          "the km/s value of c would give the same Ogamma0, so this check proves nothing")

    # -- the three analytic universes ---------------------------------------------------------
    for label, dc_form, age_form in (
        ("einstein_de_sitter",
         lambda m, z: 2 * m.dh * (1 - 1 / mp.sqrt(1 + z)),
         lambda m, z: mp.mpf(2) / 3 * m.th * (1 + z) ** mp.mpf("-1.5")),
        ("milne",
         lambda m, z: m.dh * mp.log(1 + z),
         lambda m, z: m.th / (1 + z)),
    ):
        m = by_label[label]
        for z in ("0.01", "0.5", "3", "100", "1100"):
            z = mp.mpf(z)
            close(m.comoving_distance_zeta(mp.log(1 + z)), dc_form(m, z), 1e-22,
                  "%s D_C(%s) against its closed form" % (label, z))
            close(m.age_of_zeta(mp.log(1 + z)), age_form(m, z), 1e-22,
                  "%s age(%s) against its closed form" % (label, z))
    ds = by_label["de_sitter"]
    for z in ("0.01", "0.5", "3", "100", "1100"):
        z = mp.mpf(z)
        close(ds.comoving_distance_zeta(mp.log(1 + z)), ds.dh * z, 1e-22,
              "de Sitter D_C(%s) against its closed form" % z)
    check(ds.age_diverges and ds.age0 == mp.inf, "de Sitter's age must be +Infinity")
    check(not by_label["milne"].age_diverges, "Milne's age must be finite")

    # -- the identity the two tables are built to keep ----------------------------------------
    for label in ("Planck18", "flat_no_rad", "closed"):
        m = by_label[label]
        for z in ("0.5", "10", "1100"):
            zeta = mp.log(1 + mp.mpf(z))
            close(m.age_of_zeta(zeta) + m.lookback_time_zeta(zeta), m.age0, 1e-24,
                  "%s age(%s) + t_L(%s) against age(0)" % (label, z, z))

    # -- the identity the density family is built to keep -------------------------------------
    for label in ("Planck18", "flat_no_rad", "open", "closed", "w0wacdm", "two_massive"):
        m = by_label[label]
        for z in ("0", "0.5", "10", "1100", "-0.5"):
            row = dict(zip(CQ_NAMES, m.row(z)))
            total = row["om"] + row["ode"] + row["ok"] + row["ogamma"] + row["onu"]
            close(total, mp.mpf(1), 1e-26,
                  "%s Om + Ode + Ok + Ogamma + Onu at z=%s" % (label, z))
        # `Onu` is `Ogamma` times the fit, so the radiation term of `E^2` is their sum.
        row = dict(zip(CQ_NAMES, m.row("3")))
        if m.ogamma0 != 0:
            close(row["onu"] / row["ogamma"], m.nu_rel(mp.mpf(4)), 1e-26,
                  "%s Onu/Ogamma is nu_rel at z=3" % label)
    for label in ("Planck18", "einstein_de_sitter"):
        m = by_label[label]
        for z in ("0.5", "1100"):
            row = dict(zip(CQ_NAMES, m.row(z)))
            check(row["de_density_scale"] == 1 and row["w"] == -1,
                  "%s is a cosmological constant, so f_DE is 1 and w is -1 at z=%s" % (label, z))

    # -- the growth oracle, against three things that are not it ------------------------------
    #
    # The ODE is the one quantity here with no quadrature to check it, so it is checked against
    # the two closed forms it must reproduce and against a numerical derivative of the kernel it
    # is built on. Together these catch a wrong ODE in the ORACLE, which no Fortran test could:
    # the library and the reference would simply agree on the same wrong equation.
    eds = by_label["einstein_de_sitter"]
    for z in REDSHIFTS:
        zeta = mp.log(1 + as_double(z))
        d, f = eds.growth_pair(z)
        close(d * (1 + as_double(z)), 1, mp.mpf("1e-25"),
              "the Einstein-de Sitter growth at z=%s is 1/(1+z)" % z)
        close(f, 1, mp.mpf("1e-25"), "the Einstein-de Sitter growth rate at z=%s is 1" % z)
        del zeta

    # The integral form `D = (5 Om0/2) E INT_zeta^inf e^(2t)/E^3 dt` is the EXACT growing solution
    # when -- and only when -- `E^2` is matter, curvature and a cosmological constant: substituting
    # `D = E` leaves a residual that vanishes for no other composition. Radiation and CPL dark
    # energy both break it, which is why the library solves the ODE; where it does hold it is an
    # independent check on the ODE that shares no arithmetic with it.
    exact_form = [m for m in ms if m.tcmb0 == 0 and m.is_lambda and m.om0 > 0]
    check(len(exact_form) >= 5, "only %d models have a closed-form growth to check the ODE against"
          % len(exact_form))
    for m in exact_form:
        i0 = mp.quad(lambda t: mp.exp(2 * t) / m.efunc(t) ** 3, [0, 1, 5, mp.inf])
        for z in ["0.5", "2", "10", "1100"]:
            zeta = mp.log(1 + as_double(z))
            iz = mp.quad(lambda t: mp.exp(2 * t) / m.efunc(t) ** 3, [zeta, zeta + 1, zeta + 5, mp.inf])
            want = m.efunc(zeta) * iz / (m.efunc(mp.mpf(0)) * i0)
            close(m.growth_pair(z)[0], want, mp.mpf("1e-20"),
                  "%s: the ODE growth at z=%s is the closed form" % (m.label, z))

    # `dE^2/dzeta` is written out by hand, term by term, including the Komatsu fit's own
    # derivative -- which is zero for every massless model, so a missing term would be invisible
    # in most of the grid. `two_massive` and the named cosmologies are where it is not.
    for label in ["Planck18", "two_massive", "w0wacdm", "open", "rad_massless"]:
        m = by_label[label]
        for zt in ["-0.5", "0", "1", "7", "20"]:
            zeta = mp.mpf(zt)
            close(m.de2_dzeta(zeta), mp.diff(lambda t: m.e2(t), zeta), mp.mpf("1e-20"),
                  "%s: dE^2/dzeta at zeta=%s is the derivative of E^2" % (label, zt))

    # -- the sound horizon, against a closed form and against its own root --------------------
    #
    # For a FLAT universe of matter and radiation alone the integral has an elementary
    # antiderivative -- `INT da / sqrt(3 (Or0 + Om0 a)(1 + R0 a))` is a logarithm -- and no model
    # in the emitted grid is that universe, so the anchor is built here for the purpose. It shares
    # no arithmetic with `sound_horizon_zeta`: no substitution, no quadrature, no panels.
    probe = Cosmology("rs_probe", "70", "0.3", ode0="0", tcmb0="2.7255", neff="3.046",
                      m_nu=["0", "0", "0"], ob0="0.05")
    anchor = Cosmology("rs_anchor", "70", 1 - probe.ogamma0 - probe.onu0, ode0="0",
                       tcmb0="2.7255", neff="3.046", m_nu=["0", "0", "0"], ob0="0.05")
    check(abs(anchor.ok0) < mp.mpf("1e-25"),
          "the sound-horizon anchor is flat to 1e-25, not %s" % mp.nstr(anchor.ok0, 6))
    a_or0 = anchor.ogamma0 * (1 + KOM_A * anchor.neff)
    a_r0 = anchor.sound_r0()
    for zt in ["0", "10", "1100", "1e5"]:
        zeta = mp.log(1 + mp.mpf(zt))
        a = mp.exp(-zeta)
        want = anchor.dh * 2 / (mp.sqrt(3) * mp.sqrt(anchor.om0 * a_r0)) * (
            mp.log(mp.sqrt(a_r0 * (a_or0 + anchor.om0 * a)) + mp.sqrt(anchor.om0 * (1 + a_r0 * a)))
            - mp.log(mp.sqrt(a_r0 * a_or0) + mp.sqrt(anchor.om0)))
        close(anchor.sound_horizon_zeta(zeta), want, mp.mpf("1e-22"),
              "the matter-radiation sound horizon at z=%s is the closed form" % zt)

    # `z_drag` is the root of `r_s(z) = r_drag`, so the one thing it must satisfy is that equality;
    # and `z_eq` is where the matter term of `E^2` meets the relativistic radiation term, which for
    # a massless model is the model's OWN radiation term and so is checkable against `e2`'s parts.
    for m in ms:
        zd = m.z_drag()
        if zd == zd:
            close(m.sound_horizon_zeta(mp.log(1 + zd)), m.r_drag(), mp.mpf("1e-24"),
                  "%s: the sound horizon at z_drag is r_drag" % m.label)
            check(DRAG_Z_LO <= zd <= DRAG_Z_HI,
                  "%s: z_drag is %s, outside the bracket the library uses" % (m.label, mp.nstr(zd, 8)))
        elif m.ob0 is not None and m.ob0 > 0 and m.om0 > 0 and m.ogamma0 > 0:
            bad.append("%s: z_drag is NaN although the model has baryons, matter and photons"
                       % m.label)
        if m.ogamma0 > 0 and not m.has_massive_nu and m.om0 > 0:
            x = 1 + m.z_eq()
            close(m.om0 * x ** 3, m.ogamma0 * x ** 4 * (1 + m.nu_rel(x)), mp.mpf("1e-25"),
                  "%s: matter equals radiation at z_eq" % m.label)

    # -- every emitted literal round-trips, and every NaN is one the model owes ---------------
    #
    # A NaN appears in exactly one place: at a redshift at or below a model's `zeta_floor`, where
    # `E^2` is not positive and the universe does not exist. Three quantities survive there,
    # because none of them touches `E`. Asserting the pattern in BOTH directions is what keeps a
    # quadrature that quietly returned a NaN from being emitted as a reference row.
    survivors = CQ_NO_EFUNC
    for m in ms:
        no_baryons = m.ob0 is None
        for z in REDSHIFTS:
            zeta = mp.log(1 + as_double(z))
            below = zeta <= m.zeta_floor
            # The growth pair has two NaNs of its own beyond the floor: a universe with no matter
            # has nothing to grow, and a model whose `E^2` has passed the `exp` screen and become
            # infinite cannot be integrated through. Both are the library's answers too.
            no_growth = m.om0 <= 0 or m.e2(zeta) == mp.inf
            for name, value in zip(CQ_NAMES, m.row(z)):
                unknown = no_baryons and name in ("ob", "odm", "sound_horizon")
                if name in ("growth", "growth_rate") and no_growth:
                    unknown = True
                packed = struct.unpack("<d", struct.pack("<d", float(value)))[0]
                if packed != packed:                       # NaN never round-trips by equality
                    if unknown:
                        pass
                    elif not below:
                        bad.append("%s of %s at z=%s is NaN above the model's own floor"
                                   % (name, m.label, z))
                    elif name in survivors:
                        bad.append("%s of %s at z=%s is NaN, although it never touches E"
                                   % (name, m.label, z))
                elif unknown:
                    bad.append("%s of %s at z=%s is a number, but the model owes a NaN there"
                               % (name, m.label, z))
                elif below and name not in survivors:
                    bad.append("%s of %s at z=%s is a number below the model's own floor"
                               % (name, m.label, z))
                elif packed != float(value):
                    bad.append("%s of %s at z=%s does not round-trip" % (name, m.label, z))

    return bad, skipped


# ---------------------------------------------------------------------------------------------
# The astropy oracle
# ---------------------------------------------------------------------------------------------

#: Quantities astropy defines the same way at a BLUESHIFT. `distmod` and the angular scales are
#: left out: this model fixes their sign conventions itself rather than inheriting them.
NEGATIVE_SAFE = {"dc", "dm", "dl", "da", "tl", "age", "efunc", "hubble",
                 "om", "ode", "ok", "ogamma", "onu", "tcmb", "w", "de_density_scale",
                 "critical_density", "lookback_distance"}

#: The tolerance each quantity is compared at. These are ASTROPY's accuracy, not this model's: the
#: model is exact to 30 digits and astropy integrates in double precision, so every number here was
#: MEASURED rather than chosen. The worst offender is a tiny integral -- at `z = 1e-8` astropy's
#: `comoving_distance` carries about 8 significant digits, because its `quad` is working to an
#: absolute tolerance on an integral of size 1e-8.
ORACLE_TOL = 1e-6

#: `age` is the one quantity astropy integrates to INFINITY (`quad(integrand, z, inf)`, which its
#: own runtime warns "is probably divergent, or slowly convergent"). Measured against this model for
#: `Planck18`: 5.2e-12 at z = 100, 5.2e-7 at 1100, 1.4e-2 at 1e4 and 97% at 1e5 -- not monotone,
#: because it is quadrature luck rather than a bound. So the age is compared only up to here. A
#: `Tcmb0 = 0` model, where astropy has a closed form instead, agrees to 2e-16 at every redshift.
AGE_ORACLE_CEILING = mp.mpf(1000)

#: Below this `|Ok0| (D_M/D_H)^2`, ASTROPY's own `comoving_volume` has cancelled away: its closed
#: form differences two numbers that agree to sixteen digits, and at `z = 1e-8` it retains none.
#: This model computes the same expression at 30 digits and is right, so the disagreement is
#: astropy's; the rows are kept and simply not compared here. The library reaches them through the
#: series `comoving_volume` uses below, for exactly the same reason.
VOLUME_ORACLE_FLOOR = 1e-3


def astropy_model(m, cosmo_module, units):
    """`m` as an astropy cosmology, in whichever of the four classes fits it."""
    kw = dict(H0=float(m.h0) * units.km / units.s / units.Mpc, Om0=float(m.om0),
              Tcmb0=float(m.tcmb0) * units.K, Neff=float(m.neff),
              m_nu=[float(v) for v in m.m_nu] * units.eV)
    if m.ob0 is not None:
        kw["Ob0"] = float(m.ob0)
    if m.n_nu == 0:
        kw["m_nu"] = None
    if m.is_lambda:
        if m.flat:
            return cosmo_module.FlatLambdaCDM(**kw)
        return cosmo_module.LambdaCDM(Ode0=float(m.ode0), **kw)
    kw.update(w0=float(m.w0), wa=float(m.wa))
    if m.flat:
        return cosmo_module.Flatw0waCDM(**kw)
    return cosmo_module.w0waCDM(Ode0=float(m.ode0), **kw)


#: The redshift above which `--verify-oracle` stops comparing GROWTH with CCL. Above it the two
#: part company by CCL's own initial condition, not by either implementation: CCL starts the
#: equation from the pure-matter growing mode `D = a` at `a = 1e-6`, which is inside the radiation
#: era for any model with a CMB, and the transient that leaves is still worth `1.3e-03` at
#: `z = 1100`. This module starts from the exact matter-radiation growing mode, whose answer does
#: not move when the starting redshift is moved four decades.
GROWTH_ORACLE_CEILING = mp.mpf(100)
#: colossus refuses a redshift above this: it is the ceiling of its own interpolation table.
COLOSSUS_CEILING = mp.mpf(500)


def verify_growth_libraries(ms):
    """The growth ODE against CCL and colossus, with every exclusion named and counted.

    Neither library is a reference for this module's accuracy -- both solve the same equation to
    six or seven digits where this one carries twelve -- so what is checked is that the module
    answers the SAME quantity, not that it answers it to the same precision. Two classes of row
    are excluded by name rather than dropped quietly, exactly as the astropy comparison excludes
    four: a model with a massive neutrino, where CCL's growth is a different DEFINITION and no
    arrangement of parameters removes the difference, and every redshift above
    `GROWTH_ORACLE_CEILING`.
    """
    try:
        import warnings
        warnings.filterwarnings("ignore")
        import pyccl as ccl
    except ImportError:
        print("  growth: pyccl is not installed; CCL not compared")
        return [], 0
    bad, compared = [], 0
    # **Measured, then chosen.** Up to `z = 10` CCL and this module agree to `4.0e-07` on `D` and
    # `1.3e-07` on `f`; by `z = 100` -- the last redshift compared -- that has grown to `4.7e-06`
    # and `1.5e-05`, which is CCL's own initial-condition transient beginning to show rather than
    # either implementation drifting. The bound below is set to catch "this is a different
    # quantity", not to chase CCL's accuracy, and every run prints the worst it saw.
    tol = 1e-4
    dropped = {"a massive neutrino: CCL's growth is a different definition": 0,
               "z above %s: CCL's own initial condition" % mp.nstr(GROWTH_ORACLE_CEILING, 6): 0,
               "no Ob0 or no matter: CCL will not build the model": 0,
               "a blueshift: CCL refuses a scale factor above one": 0}
    worst_d, worst_f, where = 0.0, 0.0, ""
    near_d, near_f = 0.0, 0.0
    print("  growth: CCL %s" % ccl.__version__)
    for m in ms:
        if m.ob0 is None or m.om0 <= 0 or m.ob0 >= m.om0:
            dropped["no Ob0 or no matter: CCL will not build the model"] += 2 * len(REDSHIFTS)
            continue
        if m.has_massive_nu:
            dropped["a massive neutrino: CCL's growth is a different definition"] += 2 * len(REDSHIFTS)
            continue
        try:
            cos = ccl.Cosmology(Omega_c=float(m.om0 - m.ob0), Omega_b=float(m.ob0),
                                h=float(m.h0 / 100), n_s=0.96, sigma8=0.81,
                                Omega_k=float(m.ok0), w0=float(m.w0), wa=float(m.wa),
                                T_CMB=float(m.tcmb0), Neff=float(m.neff), m_nu=0.0,
                                transfer_function="bbks")
        except Exception as exc:
            print("  growth: CCL refused %s (%s)" % (m.label, exc))
            continue
        for z in REDSHIFTS:
            zf = float(as_double(z))
            if zf < 0:
                dropped["a blueshift: CCL refuses a scale factor above one"] += 2
                continue
            if mp.mpf(z) > GROWTH_ORACLE_CEILING:
                dropped["z above %s: CCL's own initial condition"
                        % mp.nstr(GROWTH_ORACLE_CEILING, 6)] += 2
                continue
            want_d, want_f = m.growth_pair(z)
            a = 1.0 / (1.0 + zf)
            got_d = float(ccl.growth_factor(cos, a))
            got_f = float(ccl.growth_rate(cos, a))
            rel_d = abs(got_d - float(want_d)) / abs(float(want_d))
            gap_f = abs(got_f - float(want_f))
            compared += 2
            if rel_d > worst_d:
                worst_d, where = rel_d, "%s at z=%s" % (m.label, z)
            worst_f = max(worst_f, gap_f)
            if mp.mpf(z) <= 10:
                near_d, near_f = max(near_d, rel_d), max(near_f, gap_f)
            if rel_d > tol or gap_f > tol:
                bad.append("%s growth at z=%s: CCL %r / %r, model %r / %r"
                           % (m.label, z, got_d, got_f, float(want_d), float(want_f)))
    print("  growth: worst D relative %.3g (%s), worst f absolute %.3g, %d values compared at %g"
          % (worst_d, where, worst_f, compared, tol))
    print("  growth: up to z = 10 the same two are %.3g and %.3g" % (near_d, near_f))
    for reason, n in sorted(dropped.items()):
        if n:
            print("  %6d not compared -- %s" % (n, reason))

    try:
        from colossus.cosmology import cosmology as col
    except ImportError:
        print("  growth: colossus is not installed; not compared")
        return bad, compared
    # colossus is asked ABOVE its own ceiling on purpose, so that the refusal is recorded rather
    # than skipped: its interpolation table stops at `z = 500`, which is the same ceiling the
    # review found for its distances, re-measured here on `growthFactor`.
    worst_c, refused, refusal = 0.0, 0, ""
    for m in ms:
        if m.ob0 is None or m.om0 <= 0 or m.has_massive_nu or m.tcmb0 == 0 or not m.is_lambda:
            continue
        col.setCosmology("pf_check", {"flat": m.ok0 == 0, "H0": float(m.h0), "Om0": float(m.om0),
                                      "Ob0": float(m.ob0), "sigma8": 0.81, "ns": 0.96,
                                      "Tcmb0": float(m.tcmb0), "Neff": float(m.neff)})
        cos = col.getCurrent()
        for z in REDSHIFTS:
            zf = float(as_double(z))
            if zf < 0:
                continue
            try:
                got_d = float(cos.growthFactor(zf))
            except Exception as exc:
                refused += 1
                refusal = str(exc)
                continue
            if mp.mpf(z) > GROWTH_ORACLE_CEILING:
                continue
            want_d = float(m.growth_pair(z)[0])
            worst_c = max(worst_c, abs(got_d - want_d) / abs(want_d))
            compared += 1
    print("  growth: colossus worst D relative %.3g up to z = %s; %d rows refused"
          % (worst_c, mp.nstr(GROWTH_ORACLE_CEILING, 6), refused))
    if refusal:
        print("  growth: colossus refuses -- %s" % refusal)
    return bad, compared


def verify_sound_libraries(ms):
    """The sound horizon, `r_drag`, `z_drag` and `z_eq` against CAMB and CLASS.

    The split here is the point of the pair: `%sound_horizon` is the model's own integral and is
    compared TIGHTLY, while `%r_drag` is a published fit and is compared at the accuracy the fit
    claims. CAMB's `rdrag` is an integral over its own recombination history, so
    `r_s(CAMB's zdrag)` against `CAMB's rdrag` isolates the integral from the fit; comparing our
    FIT against `rdrag` then measures the fit alone.

    **Two exclusions, both measured before they were chosen, both named and counted** rather than
    dropped quietly -- the shape `verify_growth_libraries` uses for CCL:

    * A MASSIVE neutrino costs an order of magnitude. Massless models agree to `1.8e-06` against
      CAMB and `2.5e-07` against CLASS; one 0.06 eV species takes those to `9.0e-06` and `1.1e-05`.
      That is astropy's Komatsu fit for the massive density against each code's exact Fermi-Dirac
      integration, not either integral drifting, and it is why the headline numbers quoted on the
      guide page -- measured on five massless models -- are the massless ones. Both
      classes are compared; the bound is set from the massive one and both worsts are printed.
    * The Aubourg FIT is only compared where the model is inside the range it was calibrated on.
      `Ob h^2` near `0.0224` and `Ocb h^2` near `0.1417` is what the paper fits over; a model six
      times outside -- `bao_baryon_rich`, at `Ob h^2 = 0.142` -- puts the fit `1.5e-01` from CAMB
      and `z_drag` `2.4e-01`, while its INTEGRAL is still `6.4e-06`. Excluding it from the fit's
      comparison and printing what it does there is the honest report; including it would either
      fail the arm or loosen the bound until the fit's real accuracy stopped being checked.

    CLASS refuses several of the grid's models outright, including that one, and every refusal is
    named.
    """
    try:
        import warnings
        warnings.filterwarnings("ignore")
        import camb
    except ImportError:
        print("  sound: camb is not installed; not compared")
        return [], 0
    bad, compared = [], 0
    print("  sound: CAMB %s" % camb.__version__)
    # Measured, then chosen. The integral: `1.8e-06` massless and `9.0e-06` with one massive
    # species against CAMB, `2.5e-07` and `1.1e-05` against CLASS -- so `2e-05` catches "this is a
    # different quantity" with a factor of two to spare and nothing else. The fit: `2.1e-04` to
    # `3.6e-04` against CAMB over the realistic models, and `z_drag` `3.0e-04` to `5.6e-04`, so
    # `1e-03` is the same kind of bound on the fit's own claim.
    tol_rs, tol_fit = 2.0e-5, 1.0e-3
    # The fit's calibration range, as a factor either side of Planck's parameters.
    fit_centre_b, fit_centre_cb, fit_span = 0.0224, 0.1417, 2.0
    worst = {"integral, massless": 0.0, "integral, one or more massive": 0.0,
             "the fit": 0.0, "z_drag": 0.0, "z_eq": 0.0}
    where = dict.fromkeys(worst, "")
    outside = []
    skipped_models = []
    for m in ms:
        if m.ob0 is None or m.ob0 <= 0 or m.om0 <= m.ob0 or m.tcmb0 == 0 or not m.is_lambda:
            skipped_models.append(m.label)
            continue
        h2 = float((m.h0 / 100) ** 2)
        massive = [x for x in m.m_nu if x > 0]
        obh2, ocbh2 = float(m.ob0) * h2, float(m.om0) * h2
        try:
            pars = camb.set_params(H0=float(m.h0), ombh2=obh2,
                                   omch2=float(m.om0 - m.ob0) * h2,
                                   mnu=float(sum(massive, mp.mpf(0))),
                                   num_massive_neutrinos=len(massive),
                                   nnu=float(m.neff), TCMB=float(m.tcmb0),
                                   omk=float(m.ok0), tau=0.054)
            der = camb.get_background(pars).get_derived_params()
        except Exception as exc:
            print("  sound: CAMB refused %s (%s)" % (m.label, exc))
            continue
        # The integral, at CAMB's OWN drag redshift, against CAMB's own drag-epoch sound horizon.
        got = float(m.sound_horizon_zeta(mp.log(1 + mp.mpf(repr(der["zdrag"])))))
        rel = abs(got - der["rdrag"]) / der["rdrag"]
        key = "integral, one or more massive" if massive else "integral, massless"
        if rel > worst[key]:
            worst[key], where[key] = rel, m.label
        compared += 1
        if rel > tol_rs:
            bad.append("%s: r_s at CAMB's zdrag is %r, CAMB's rdrag is %r"
                       % (m.label, got, der["rdrag"]))
        # `z_eq` is exact from the parameters and is compared for every model.
        rel = abs(float(m.z_eq()) - der["zeq"]) / der["zeq"]
        if rel > worst["z_eq"]:
            worst["z_eq"], where["z_eq"] = rel, m.label
        compared += 1
        if rel > 1.0e-2:
            bad.append("%s: z_eq is %r, CAMB's zeq is %r" % (m.label, float(m.z_eq()), der["zeq"]))
        # The fit, and the redshift the fit implies -- inside the fit's own range only.
        in_range = (fit_centre_b / fit_span <= obh2 <= fit_centre_b * fit_span
                    and fit_centre_cb / fit_span <= ocbh2 <= fit_centre_cb * fit_span)
        rel_fit = abs(float(m.r_drag()) - der["rdrag"]) / der["rdrag"]
        rel_zd = abs(float(m.z_drag()) - der["zdrag"]) / der["zdrag"]
        if not in_range:
            outside.append("%s (Ob h^2 = %.5f, Ocb h^2 = %.5f): the fit is %.3g from CAMB and "
                           "z_drag %.3g, while the INTEGRAL is %.3g"
                           % (m.label, obh2, ocbh2, rel_fit, rel_zd,
                              abs(got - der["rdrag"]) / der["rdrag"]))
            continue
        if rel_fit > worst["the fit"]:
            worst["the fit"], where["the fit"] = rel_fit, m.label
        if rel_zd > worst["z_drag"]:
            worst["z_drag"], where["z_drag"] = rel_zd, m.label
        compared += 2
        if rel_fit > tol_fit:
            bad.append("%s: the Aubourg fit is %r, CAMB's rdrag is %r"
                       % (m.label, float(m.r_drag()), der["rdrag"]))
        if rel_zd > tol_fit:
            bad.append("%s: z_drag is %r, CAMB's zdrag is %r"
                       % (m.label, float(m.z_drag()), der["zdrag"]))
    for name in ("integral, massless", "integral, one or more massive", "the fit", "z_drag",
                 "z_eq"):
        print("  sound: CAMB, %-30s worst %.3g (%s)" % (name, worst[name], where[name] or "none"))
    for line in outside:
        print("  sound: OUTSIDE the fit's calibration range, not compared -- %s" % line)
    if skipped_models:
        print("  sound: no Ob0, no photons, or not a cosmological constant -- %s"
              % ", ".join(skipped_models))

    try:
        from classy import Class
    except ImportError:
        print("  sound: classy is not installed; not compared")
        return bad, compared
    worst_cl = {"massless": 0.0, "one or more massive": 0.0}
    refused = []
    for m in ms:
        if m.ob0 is None or m.ob0 <= 0 or m.om0 <= m.ob0 or m.tcmb0 == 0 or not m.is_lambda:
            continue
        h2 = float((m.h0 / 100) ** 2)
        massive = [x for x in m.m_nu if x > 0]
        cfg = {"H0": float(m.h0), "omega_b": float(m.ob0) * h2,
               "omega_cdm": float(m.om0 - m.ob0) * h2, "T_cmb": float(m.tcmb0),
               "Omega_k": float(m.ok0)}
        if massive:
            cfg.update({"N_ur": float(m.neff) - len(massive) * 1.0132, "N_ncdm": len(massive),
                        "m_ncdm": ",".join(repr(float(x)) for x in massive)})
        else:
            cfg["N_ur"] = float(m.neff)
        cl = Class()
        try:
            cl.set(cfg)
            cl.compute()
            d = cl.get_current_derived_parameters(["rs_d", "z_d"])
        except Exception as exc:
            refused.append("%s (%s)" % (m.label, str(exc).strip().splitlines()[0][:90]))
            cl.struct_cleanup()
            continue
        got = float(m.sound_horizon_zeta(mp.log(1 + mp.mpf(repr(d["z_d"])))))
        rel = abs(got - d["rs_d"]) / d["rs_d"]
        key = "one or more massive" if massive else "massless"
        worst_cl[key] = max(worst_cl[key], rel)
        compared += 1
        if rel > tol_rs:
            bad.append("%s: r_s at CLASS's z_d is %r, CLASS's rs_d is %r"
                       % (m.label, got, d["rs_d"]))
        cl.struct_cleanup()
    for key in ("massless", "one or more massive"):
        print("  sound: CLASS, r_s at its own z_d, %-20s worst %.3g" % (key, worst_cl[key]))
    for line in refused:
        print("  sound: CLASS refused -- %s" % line)
    return bad, compared


def verify_oracle():
    try:
        import astropy
        import astropy.cosmology as ac
        import astropy.units as u
    except ImportError:
        print("--verify-oracle needs astropy (the workspace `astro` environment).", file=sys.stderr)
        return 1

    import warnings
    warnings.filterwarnings("ignore")

    print("generate_cosmology_reference.py --verify-oracle: astropy %s" % astropy.__version__)
    worst, compared, bad, refused = {}, 0, [], []
    dropped = {"z above %s" % mp.nstr(ORACLE_CEILING, 6): 0,
               "age above %s" % mp.nstr(AGE_ORACLE_CEILING, 6): 0,
               "curved volume astropy cannot form": 0,
               "blueshift, convention not inherited": 0,
               "infinite or undefined here": 0}

    for m in models():
        try:
            ap = astropy_model(m, ac, u)
        except Exception as exc:                            # a model astropy will not build
            bad.append("%s: astropy refused the model (%s)" % (m.label, exc))
            continue
        for z in REDSHIFTS:
            zf = float(mp.mpf(z))
            if mp.mpf(z) > ORACLE_CEILING:
                dropped["z above %s" % mp.nstr(ORACLE_CEILING, 6)] += len(CQ_NAMES)
                continue
            row = dict(zip(CQ_NAMES, m.row(z)))
            try:
                got = {
                    "dc": ap.comoving_distance(zf).to_value(u.Mpc),
                    "dm": ap.comoving_transverse_distance(zf).to_value(u.Mpc),
                    "dl": ap.luminosity_distance(zf).to_value(u.Mpc),
                    "da": ap.angular_diameter_distance(zf).to_value(u.Mpc),
                    "tl": ap.lookback_time(zf).to_value(u.Gyr),
                    "age": ap.age(zf).to_value(u.Gyr),
                    "vc": ap.comoving_volume(zf).to_value(u.Mpc ** 3),
                    "dv": ap.differential_comoving_volume(zf).to_value(u.Mpc ** 3 / u.sr),
                    "efunc": ap.efunc(zf),
                    "hubble": ap.H(zf).to_value(u.km / u.s / u.Mpc),
                    "om": ap.Om(zf),
                    "ode": ap.Ode(zf),
                    "ok": ap.Ok(zf),
                    "ogamma": ap.Ogamma(zf),
                    "onu": ap.Onu(zf),
                    "tcmb": ap.Tcmb(zf).to_value(u.K),
                    "w": ap.w(zf),
                    "de_density_scale": ap.de_density_scale(zf),
                    # astropy reports this one in g/cm^3; M_sun/Mpc^3 is the unit the library
                    # answers in, and astropy's own `M_sun` is the IAU 2015 `GM_sun` over `G`,
                    # so the two agree once the conversion is asked for explicitly.
                    "critical_density": ap.critical_density(zf).to_value(u.Msun / u.Mpc ** 3),
                    "lookback_distance": ap.lookback_distance(zf).to_value(u.Mpc),
                }
                if zf > 0:
                    got["mu"] = ap.distmod(zf).to_value(u.mag)
                    got["kpc_proper"] = ap.kpc_proper_per_arcmin(zf).to_value(u.kpc / u.arcmin)
                    got["kpc_comoving"] = ap.kpc_comoving_per_arcmin(zf).to_value(u.kpc / u.arcmin)
                    got["arcsec_proper"] = ap.arcsec_per_kpc_proper(zf).to_value(u.arcsec / u.kpc)
                    got["arcsec_comoving"] = ap.arcsec_per_kpc_comoving(zf).to_value(
                        u.arcsec / u.kpc)
            except Exception as exc:
                # astropy itself giving up on an extreme input is not a disagreement about a
                # value, so it is REPORTED rather than counted as a failure -- but it is never
                # swallowed: every one is printed with the model, the redshift and the message.
                refused.append("%s at z=%s: astropy raised %s"
                               % (m.label, z, str(exc).split("(this most likely")[0].strip()))
                continue
            for name, value in got.items():
                want = float(row[name])
                if zf < 0 and name not in NEGATIVE_SAFE:
                    dropped["blueshift, convention not inherited"] += 1
                    continue
                if name == "age" and mp.mpf(z) > AGE_ORACLE_CEILING:
                    dropped["age above %s" % mp.nstr(AGE_ORACLE_CEILING, 6)] += 1
                    continue
                if name == "vc" and m.ok0 != 0:
                    q = float(row["dm"]) / float(m.dh)
                    if abs(float(m.ok0)) * q * q < VOLUME_ORACLE_FLOOR:
                        dropped["curved volume astropy cannot form"] += 1
                        continue
                if want != want or abs(want) == mp.inf or value != value or abs(value) == mp.inf:
                    dropped["infinite or undefined here"] += 1
                    continue
                scale = abs(want) if want else 1.0
                rel = abs(value - want) / scale
                compared += 1
                if rel > worst.get(name, (0.0, ""))[0]:
                    worst[name] = (rel, "%s at z=%s" % (m.label, z))
                if rel > ORACLE_TOL:
                    bad.append("%s %s at z=%s: astropy %r, model %r (relative %.3g > %g)"
                               % (m.label, name, z, value, want, rel, ORACLE_TOL))

    for name in CQ_NAMES:
        if name in worst:
            print("  %-16s worst relative %-10.3g at %s" % (name, worst[name][0], worst[name][1]))
    print("  %d values compared against astropy at %g" % (compared, ORACLE_TOL))
    for reason, n in sorted(dropped.items()):
        if n:
            print("  %6d not compared -- %s" % (n, reason))
    for line in refused:
        print("  astropy could not evaluate -- %s" % line)
    if bad:
        print("--verify-oracle FAILED (%d):" % len(bad), file=sys.stderr)
        for line in bad[:40]:
            print("  " + line, file=sys.stderr)
        return 1
    gbad, gn = verify_growth_libraries(models())
    if gbad:
        print("--verify-oracle FAILED (%d growth):" % len(gbad), file=sys.stderr)
        for line in gbad[:20]:
            print("  " + line, file=sys.stderr)
        return 1
    sbad, sn = verify_sound_libraries(models())
    if sbad:
        print("--verify-oracle FAILED (%d sound):" % len(sbad), file=sys.stderr)
        for line in sbad[:20]:
            print("  " + line, file=sys.stderr)
        return 1
    print("--verify-oracle: every compared value agrees with astropy, %d growth values with CCL"
          " and %d sound-horizon values with CAMB and CLASS" % (gn, sn))
    return 0


# ---------------------------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------------------------

def print_gauss():
    """The Fortran the eval submodule carries, so that the rule is transcribed and not retyped."""
    nodes, weights = gauss_legendre_20()
    for name, values, what in (("pfc_gl_x", nodes, "abscissae"), ("pfc_gl_w", weights, "weights")):
        print("    !> The 20-point Gauss-Legendre %s on `[-1, 1]`." % what)
        for line in array("real(real64)", name, "PFC_GL_N",
                          [rgv.real_literal(float(v), "real64") for v in values]):
            print(line)


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--check", action="store_true",
                    help="regenerate into memory and compare with the committed file")
    ap.add_argument("--self-test", action="store_true",
                    help="re-derive the published anchors; exit 1 if any of them fails")
    ap.add_argument("--verify-oracle", action="store_true",
                    help="cross-check the rows against astropy")
    ap.add_argument("--print-gauss", action="store_true",
                    help="print the Gauss-Legendre block src/parquet_cosmology_eval.f90 carries")
    args = ap.parse_args()

    if args.print_gauss:
        print_gauss()
        return 0
    if args.self_test:
        bad, skipped = self_test()
        for line in skipped:
            print("generate_cosmology_reference.py --self-test: SKIPPED -- %s" % line)
        if bad:
            print("generate_cosmology_reference.py --self-test FAILED (%d):" % len(bad),
                  file=sys.stderr)
            for line in bad[:40]:
                print("  " + line, file=sys.stderr)
            return 1
        print("generate_cosmology_reference.py --self-test: every anchor reproduced")
        return 0
    if args.verify_oracle:
        return verify_oracle()

    text = gen_module()
    over = [n for n, line in enumerate(text.split("\n"), start=1) if len(line) > 132]
    if over:
        print("generate_cosmology_reference.py: emitted line(s) exceed 132 columns: %s"
              % over[:10], file=sys.stderr)
        return 1
    if args.check:
        if not OUT_PATH.exists() or OUT_PATH.read_text() != text:
            print("generate_cosmology_reference.py --check: %s differs from a fresh generation."
                  % OUT_PATH.relative_to(REPO_ROOT), file=sys.stderr)
            print("Re-run tools/generate_cosmology_reference.py to regenerate.", file=sys.stderr)
            return 1
        print("generate_cosmology_reference.py --check: %s is current."
              % OUT_PATH.relative_to(REPO_ROOT))
        return 0
    OUT_PATH.write_text(text)
    print("generate_cosmology_reference.py: wrote %s (%d lines)."
          % (OUT_PATH.relative_to(REPO_ROOT), text.count("\n")))
    return 0


if __name__ == "__main__":
    sys.exit(main())
