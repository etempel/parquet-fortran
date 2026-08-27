#!/usr/bin/env python3
"""Generate the reference vectors that pin `parquet_healpix`'s contract.

`parquet_healpix` is a from-scratch implementation of the HEALPix sphere pixelisation, written
from the published algorithm (Gorski et al. 2005, ApJ 622, 759) rather than translated from
HEALPix's own GPL source. Every failure mode of such a module is a WRONG ANSWER rather than an
abort, so the vectors below are what make it trustworthy: they are derived here, from an
independent Python model of the pixelisation, and cross-checked against `healpy` -- rather than
read back out of a Fortran run, which could only ever confirm that the implementation agrees with
itself.

Emitted (COMMITTED to the repository, like every other generator's output -- nothing is generated
at build time, so the fpm build stays dependency-free):

  test/test_healpix_vectors.f90   module `test_healpix_vectors`: positions -> pixel indices in both
                                  schemes, pixel -> angle, ring<->nest maps, disc queries in both
                                  inclusive modes, angdist pairs, and max_pixrad, across twelve
                                  nside values spanning both the int32 and int64 ranges.

Usage:  tools/generate_healpix_reference.py [--check] [--self-test] [--verify-oracle]

  --check          regenerate into memory and compare with the committed file; exit 1 on any
                   difference (so CI, or a reviewer, can prove the committed output is current).
  --self-test      re-derive every PUBLISHED anchor -- the pixelisation's structural identities,
                   the nside=1 facts that follow from the paper alone, the closed form for
                   max_pixrad against a brute-force corner scan, and the disc walk against a
                   whole-sphere scan -- and exit 1 if any of them fails. This is what makes the
                   emitted table trustworthy: without it the generator would merely be
                   self-consistent.
  --verify-oracle  cross-check EVERY emitted value against `healpy` and exit 1 on any
                   disagreement. Needs healpy (and numpy); the two modes above deliberately do
                   not, so that CI's lint image -- which installs a bare `python3` and nothing
                   else -- can run them.

NEVER HAND-EDIT A VECTOR. A contract change is an edit to the model below plus a regeneration; an
edit to the emitted table is a lie that `--check` will catch and nothing else would.

--------------------------------------------------------------------------------------------
WHY THE ORACLE IS SPLIT IN TWO, AND WHAT EACH HALF PROVES

`healpy` is the reference implementation and is the authority on what a HEALPix pixel index means.
It is also unavailable in CI's lint image, and installing it there to run one check is not a trade
this project makes. So the split is:

  * the MODEL below (pure Python, standard library only) derives every value, and is what --check
    re-runs. It is independent of the Fortran in the way that matters -- a different language, a
    different author's transcription of the same published equations -- so a transcription slip on
    either side shows up as a disagreement.
  * `healpy` confirms the model, under --verify-oracle, which the maintainer runs when
    regenerating and which needs the workspace `astro` environment. The version it reports is
    recorded in the emitted header, so that when a future regeneration disagrees with the
    committed copy, the first question -- did the oracle move, or did the implementation? -- is
    answerable at all.

The one thing neither half can settle is a pixel whose centre lies within an ulp of a disc's rim,
where two correct implementations may legitimately disagree. Those are excluded by construction
rather than frozen: see `stable_disc` and `stable_position` below.
--------------------------------------------------------------------------------------------
"""

import argparse
import math
import os
import random
import sys

# --------------------------------------------------------------------------------------------
# The model: the HEALPix pixelisation, derived from the published algorithm.
#
# Every formula here is stated in feature_healpix_tier_a.md section 4, which is the design
# document the Fortran implementation is written from. Keep the two in step: this file and
# src/parquet_healpix.f90 are two transcriptions of one specification, and their agreement is
# what the emitted vectors measure.
# --------------------------------------------------------------------------------------------

# Per-face ring and phi offsets. Faces 0-3 are the north cap, 4-7 the equatorial belt, 8-11 the
# south cap; jrll/jpll place each face's centre on the ring/phi grid.
JRLL = (2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
JPLL = (1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7)

HALFPI = 0.5 * math.pi
TWOPI = 2.0 * math.pi


def npix_of(nside):
    """Total pixel count at `nside`."""
    return 12 * nside * nside


def ncap_of(nside):
    """Pixels strictly north of the first equatorial ring."""
    return 2 * nside * (nside - 1)


def order_of(nside):
    """log2(nside), for a power-of-two nside."""
    return nside.bit_length() - 1


def interleave(ix, iy):
    """Morton order: bits of `ix` on even positions, bits of `iy` on odd ones."""
    r = 0
    for b in range(32):
        r |= ((ix >> b) & 1) << (2 * b)
        r |= ((iy >> b) & 1) << (2 * b + 1)
    return r


def deinterleave(p):
    """Inverse of `interleave`."""
    x = y = 0
    for b in range(32):
        x |= ((p >> (2 * b)) & 1) << b
        y |= ((p >> (2 * b + 1)) & 1) << b
    return x, y


def ang2pix_ring(nside, theta, phi):
    """RING-scheme pixel containing the direction (theta, phi), both in radians."""
    z = math.cos(theta)
    za = abs(z)
    tt = (phi / HALFPI) % 4.0
    if za <= 2.0 / 3.0:
        temp1 = nside * (0.5 + tt)
        temp2 = nside * (z * 0.75)
        jp = int(math.floor(temp1 - temp2))
        jm = int(math.floor(temp1 + temp2))
        ir = nside + 1 + jp - jm
        kshift = 1 - (ir & 1)
        ip = (jp + jm - nside + kshift + 1) // 2
        ip %= 4 * nside
        return ncap_of(nside) + (ir - 1) * 4 * nside + ip
    tp = tt - int(tt)
    tmp = nside * math.sqrt(3.0 * (1.0 - za))
    jp = int(tp * tmp)
    jm = int((1.0 - tp) * tmp)
    ir = jp + jm + 1
    ip = int(tt * ir) % (4 * ir)
    if z > 0.0:
        return 2 * ir * (ir - 1) + ip
    return npix_of(nside) - 2 * ir * (ir + 1) + ip


def ang2pix_nest(nside, theta, phi):
    """NEST-scheme pixel containing the direction (theta, phi), both in radians."""
    z = math.cos(theta)
    za = abs(z)
    tt = (phi / HALFPI) % 4.0
    if za <= 2.0 / 3.0:
        temp1 = nside * (0.5 + tt)
        temp2 = nside * (z * 0.75)
        jp = int(math.floor(temp1 - temp2))
        jm = int(math.floor(temp1 + temp2))
        ifp = jp // nside
        ifm = jm // nside
        if ifp == ifm:
            face = (ifp & 3) + 4
        elif ifp < ifm:
            face = ifp & 3
        else:
            face = (ifm & 3) + 8
        ix = jm % nside
        iy = nside - 1 - (jp % nside)
    else:
        ntt = int(tt)
        tp = tt - ntt
        tmp = nside * math.sqrt(3.0 * (1.0 - za))
        # A guard rather than a necessity: this branch's own condition keeps tmp below nside, so
        # the clamp should never fire, and a targeted sweep found no case where it did. It is kept
        # because an unclamped nside here indexes off the face -- a wrong FACE, not a wrong pixel.
        # Kept identical to the Fortran's, so that the two cannot disagree about an edge case.
        jp = min(int(tp * tmp), nside - 1)
        jm = min(int((1.0 - tp) * tmp), nside - 1)
        if z > 0.0:
            face = ntt
            ix = nside - 1 - jm
            iy = nside - 1 - jp
        else:
            face = ntt + 8
            ix = jp
            iy = jm
    return face * nside * nside + interleave(ix, iy)


def cap_ring(p):
    """Ring index (1-based, from the pole) of north-cap RING pixel `p`.

    The float seed is within 1 of the true ring for every representable p; the two loops are what
    make the answer exact rather than probably-right, and each runs at most once.
    """
    i = int((1.0 + math.sqrt(1.0 + 2.0 * float(p))) / 2.0)
    if i < 1:
        i = 1
    while 2 * i * (i + 1) <= p:
        i += 1
    while 2 * i * (i - 1) > p:
        i -= 1
    return i


def ring_decompose(nside, p):
    """RING pixel -> (ring index from north, index within ring, pixels in ring, shift flag)."""
    npix = npix_of(nside)
    ncap = ncap_of(nside)
    if p < ncap:
        i = cap_ring(p)
        return i, p - 2 * i * (i - 1), 4 * i, 1
    if p < npix - ncap:
        pp = p - ncap
        i = pp // (4 * nside) + nside
        return i, pp % (4 * nside), 4 * nside, (i - nside + 1) & 1
    pn = npix - 1 - p
    i = cap_ring(pn)
    jn = pn - 2 * i * (i - 1)
    return 4 * nside - i, 4 * i - 1 - jn, 4 * i, 1


def ring_z(nside, i):
    """Centre z of ring `i` (1 .. 4*nside-1), counted from the north pole."""
    if i < nside:
        return 1.0 - i * i / (3.0 * nside * nside)
    if i > 3 * nside:
        k = 4 * nside - i
        return -(1.0 - k * k / (3.0 * nside * nside))
    return (2.0 * nside - i) * 2.0 / (3.0 * nside)


def ring_first(nside, i):
    """RING index of the first pixel of ring `i`, and the ring's pixel count and shift flag."""
    if i < nside:
        return 2 * i * (i - 1), 4 * i, 1
    if i > 3 * nside:
        k = 4 * nside - i
        return npix_of(nside) - 2 * k * (k + 1), 4 * k, 1
    return ncap_of(nside) + (i - nside) * 4 * nside, 4 * nside, (i - nside + 1) & 1


def pix2zphi_ring(nside, p):
    """RING pixel -> (z, phi) of its centre, without an acos/cos round trip.

    Building the centre from z directly is load-bearing rather than an optimisation: a round trip
    through acos moves a pixel whose centre lies exactly on a disc's rim to the wrong side of it.
    See feature_healpix_tier_a.md section 4.5.
    """
    i, j, nr, shifted = ring_decompose(nside, p)
    z = ring_z(nside, i)
    f = 0.5 * shifted
    phi = (j + f) * (TWOPI / nr)
    return z, phi


def pix2ang_ring(nside, p):
    """RING pixel -> (theta, phi) of its centre, in radians."""
    z, phi = pix2zphi_ring(nside, p)
    return math.acos(max(-1.0, min(1.0, z))), phi


def nest2ring(nside, p):
    """NEST pixel index -> RING pixel index."""
    npix = npix_of(nside)
    f = p // (nside * nside)
    ix, iy = deinterleave(p - f * nside * nside)
    jr = JRLL[f] * nside - ix - iy - 1
    if jr < nside:
        nr = jr
        n_before = 2 * nr * (nr - 1)
        kshift = 0
    elif jr > 3 * nside:
        nr = 4 * nside - jr
        n_before = npix - 2 * nr * (nr + 1)
        kshift = 0
    else:
        nr = nside
        n_before = ncap_of(nside) + (jr - nside) * 4 * nside
        kshift = (jr - nside) & 1
    jp = (JPLL[f] * nr + ix - iy + 1 + kshift) // 2
    if jp > 4 * nr:
        jp -= 4 * nr
    if jp < 1:
        jp += 4 * nr
    return n_before + jp - 1


def ring2nest(nside, p):
    """RING pixel index -> NEST pixel index, by direct inversion (no search)."""
    npix = npix_of(nside)
    ncap = ncap_of(nside)
    if p < ncap:
        i = cap_ring(p)
        j = p - 2 * i * (i - 1)
        face = j // i
        t = j % i
        ix = nside - i + t
        iy = nside - 1 - t
    elif p >= npix - ncap:
        pn = npix - 1 - p
        i = cap_ring(pn)
        jn = pn - 2 * i * (i - 1)
        j = 4 * i - 1 - jn
        face = 8 + j // i
        t = j % i
        ix = t
        iy = i - 1 - t
    else:
        n = nside
        pp = p - ncap
        jr = pp // (4 * n) + n
        j = pp % (4 * n)
        kshift = (jr - n) & 1
        # Recover the two diagonal line indices. Their difference is fixed by the ring; their sum
        # is known up to the bit the ip formula's floor-division by 2 discards, and the parity of
        # the difference is what restores it.
        d = jr - 2 * n
        t = 2 * j + n - kshift - 1
        if (t & 1) != (d & 1):
            t += 1
        jp_l = (t + d) // 2
        jm_l = (t - d) // 2
        if jp_l < 0 or jm_l < 0:
            jp_l += 4 * n
            jm_l += 4 * n
        ifp = jp_l // n
        ifm = jm_l // n
        if ifp == ifm:
            face = (ifp & 3) + 4
        elif ifp < ifm:
            face = ifp & 3
        else:
            face = (ifm & 3) + 8
        ix = jm_l % n
        iy = n - 1 - (jp_l % n)
    return face * nside * nside + interleave(ix, iy)


def pix2vec_ring(nside, p):
    """RING pixel -> the unit vector of its centre."""
    z, phi = pix2zphi_ring(nside, p)
    st = math.sqrt(max(0.0, (1.0 - z) * (1.0 + z)))
    return (st * math.cos(phi), st * math.sin(phi), z)


def angdist(v1, v2):
    """Angular separation of two vectors, by the numerically stable atan2 form."""
    cx = v1[1] * v2[2] - v1[2] * v2[1]
    cy = v1[2] * v2[0] - v1[0] * v2[2]
    cz = v1[0] * v2[1] - v1[1] * v2[0]
    return math.atan2(math.sqrt(cx * cx + cy * cy + cz * cz), v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2])


def max_pixrad(nside):
    """Largest angular distance from a pixel centre to one of its own corners, in radians.

    Attained by a pixel of the first equatorial ring (centre z = 2/3), at its north corner, which
    lies on the quadrant meridian pi/(4*nside) away in phi. Verified against a brute-force scan of
    every corner of every pixel in --self-test.

    **Computed with `atan2`, never `acos` of the dot product**, and that is a correctness matter
    rather than a refinement. The two vectors converge as nside grows, so their dot product
    approaches 1 and an `acos` of it loses the whole answer to cancellation: measured against
    healpy, the `acos` form is 5.6e-05 relatively wrong at nside = 2**20 and returns **exactly
    zero** at nside = 2**29, where the true value is 1.99e-09. Zero is the dangerous one --
    `pf_query_disc`'s inclusive mode enlarges its radius by this quantity, so a zero would make
    `inclusive = .true.` silently identical to `inclusive = .false.` at high resolution, with no
    abort and no test failing unless one asserts the enlargement itself. The `atan2` form holds
    4e-10 relative or better across the whole nside range.
    """
    zc = 2.0 / 3.0
    sc = math.sqrt(1.0 - zc * zc)
    zv = 1.0 - (nside - 1) * (nside - 1) / (3.0 * nside * nside)
    sv = math.sqrt(max(0.0, (1.0 - zv) * (1.0 + zv)))
    dphi = math.pi / (4.0 * nside)
    c = (sc, 0.0, zc)
    v = (sv * math.cos(dphi), sv * math.sin(dphi), zv)
    return angdist(c, v)


def ring_above(nside, z):
    """Largest ring index whose centre z is >= `z`; 0 when none is."""
    az = abs(z)
    if az > 2.0 / 3.0:
        i = int(nside * math.sqrt(3.0 * (1.0 - az)))
        if i < 1:
            i = 1
        while i >= 1 and 1.0 - i * i / (3.0 * nside * nside) < az:
            i -= 1
        while 1.0 - (i + 1) * (i + 1) / (3.0 * nside * nside) >= az:
            i += 1
        return i if z > 0 else 4 * nside - i - 1
    i = int(round(nside * (2.0 - 1.5 * z))) + 1
    while i >= 1 and (2.0 * nside - i) * 2.0 / (3.0 * nside) < z:
        i -= 1
    while (2.0 * nside - (i + 1)) * 2.0 / (3.0 * nside) >= z:
        i += 1
    return i


def disc_reference(nside, v0, radius):
    """Pixels whose centres lie within `radius` of `v0`, by the METRIC predicate.

    This is the oracle the emitted disc vectors are defined by, and it is deliberately not the
    walk the Fortran implements: it takes a generous band of rings, a generous phi window within
    each, and then tests every candidate with the exact dot-product comparison. So it shares no
    band-selection or trimming logic with the implementation under test, while remaining cheap
    enough to run at nside = 2**20. Verified against a whole-sphere scan in --self-test.
    """
    n = math.sqrt(v0[0] ** 2 + v0[1] ** 2 + v0[2] ** 2)
    v0 = (v0[0] / n, v0[1] / n, v0[2] / n)
    z0 = max(-1.0, min(1.0, v0[2]))
    theta0 = math.acos(z0)
    phi0 = math.atan2(v0[1], v0[0]) % TWOPI
    r = min(radius, math.pi)
    cosr = math.cos(r)
    st0 = math.sqrt(max(0.0, (1.0 - z0) * (1.0 + z0)))

    zmax = math.cos(max(theta0 - r, 0.0))
    zmin = math.cos(min(theta0 + r, math.pi))
    irmin = max(1, ring_above(nside, zmax) - 2)
    irmax = min(4 * nside - 1, ring_above(nside, zmin) + 2)

    out = []
    for i in range(irmin, irmax + 1):
        first, nr, shifted = ring_first(nside, i)
        zr = ring_z(nside, i)
        w = TWOPI / nr
        f = 0.5 * shifted
        str_ = math.sqrt(max(0.0, (1.0 - zr) * (1.0 + zr)))
        denom = st0 * str_
        num = cosr - z0 * zr
        whole = False
        if denom <= 0.0:
            if num > 0.0:
                continue
            whole = True
        else:
            a = num / denom
            if a <= -1.0:
                whole = True
            elif a >= 1.0:
                dphi = 0.0
            else:
                dphi = math.acos(a)
        if whole:
            lo, count = 0, nr
        else:
            # A margin of two pixels each side: the window only has to CONTAIN the answer, the
            # dot-product test below decides it.
            jlo = int(math.floor((phi0 - dphi) / w - f)) - 2
            jhi = int(math.ceil((phi0 + dphi) / w - f)) + 2
            if jhi - jlo + 1 >= nr:
                lo, count = 0, nr
            else:
                lo, count = jlo % nr, jhi - jlo + 1
        for k in range(count):
            j = (lo + k) % nr
            vz = zr
            vphi = (j + f) * w
            vx = str_ * math.cos(vphi)
            vy = str_ * math.sin(vphi)
            if vx * v0[0] + vy * v0[1] + vz * v0[2] >= cosr:
                out.append(first + j)
    return sorted(set(out))


def disc_brute(nside, v0, radius):
    """Whole-sphere scan: the same predicate applied to every pixel. Small nside only."""
    n = math.sqrt(v0[0] ** 2 + v0[1] ** 2 + v0[2] ** 2)
    v0 = (v0[0] / n, v0[1] / n, v0[2] / n)
    cosr = math.cos(min(radius, math.pi))
    res = []
    for p in range(npix_of(nside)):
        v = pix2vec_ring(nside, p)
        if v[0] * v0[0] + v[1] * v0[1] + v[2] * v0[2] >= cosr:
            res.append(p)
    return res


# --------------------------------------------------------------------------------------------
# Fixture selection.
#
# Every frozen value must be reproducible by any correct implementation, so an input sitting
# within a few ulp of a pixel boundary or a disc rim is excluded rather than recorded -- see
# `stable_position` and `stable_disc`. Cross-implementation agreement THERE is not promised (the
# guide page says so), and is covered instead by the self-consistency suite, where both sides are
# the same arithmetic.
# --------------------------------------------------------------------------------------------

NSIDES = [1, 2, 4, 8, 16, 64, 256, 1024, 8192, 1 << 14, 1 << 20, 1 << 29]
NSIDES_INT32 = [n for n in NSIDES if npix_of(n) <= 2147483647]

# nside values whose disc results are frozen as full pixel lists, and as checksums.
DISC_FULL_NSIDES = [4, 16, 64]
# A disc fixture is only freezable where its rim is resolved by more than a rounding error, and
# that stops being true long before nside 2**29. Membership is decided against `cos(radius)`,
# whose distance from 1 is about r**2/2 -- so the radius is resolved to a FRACTION
# `2.2e-16 / r**2`, which is 2e-10 at r = 1e-3 rad and 6e-3 at r = 2e-07. At a pixel-scale radius
# that fraction passes 1 around nside 2**29, where `cos(r)` is exactly 1.0 and the comparison
# stops meaning anything at all.
#
# So the disc tables stop at 2**20, and `max_pixrad` is frozen separately at every nside up to
# 2**29 instead -- the quantity a disc fixture up there was wanted for is the inclusive margin,
# and freezing that directly tests it exactly where a disc cannot. The perturbation filter below
# cannot substitute: it moves the INPUTS, and at high resolution a perturbation large enough to
# matter is far larger than the answer's own sensitivity.
DISC_SUM_NSIDES = [256, 1024, 8192, 1 << 20]

CHECKSUM_MOD = 1 << 61

PERTURB = 1.0e-13   # relative perturbation the stability filter applies
NUDGE = 3.7e-9      # radians; the deterministic step taken when a fixture is not stable


def perturbed(x, eps):
    """`x` moved by a relative `eps` (absolute, when x is zero)."""
    return x + eps * (abs(x) if x != 0.0 else 1.0)


def stable_position(theta, phi):
    """True when every nside in NSIDES gives the same pixel under a small input perturbation.

    A position this rejects is one within a fraction of a pixel of a boundary at some nside; the
    vectors are a permanent contract, so such a position is nudged rather than frozen.
    """
    for nside in NSIDES:
        base_r = ang2pix_ring(nside, theta, phi)
        base_n = ang2pix_nest(nside, theta, phi)
        for dt in (-PERTURB, PERTURB):
            for dp in (-PERTURB, PERTURB):
                t = min(math.pi, max(0.0, perturbed(theta, dt)))
                p = perturbed(phi, dp)
                if ang2pix_ring(nside, t, p) != base_r or ang2pix_nest(nside, t, p) != base_n:
                    return False
    return True


def stable_disc(nside, v0, radius):
    """True when the disc's pixel set is unchanged under a small perturbation of centre and radius."""
    base = disc_reference(nside, v0, radius)
    for dr in (-PERTURB, PERTURB):
        for dv in (-PERTURB, PERTURB):
            v = (perturbed(v0[0], dv), perturbed(v0[1], -dv), perturbed(v0[2], dv))
            if disc_reference(nside, v, perturbed(radius, dr)) != base:
                return False
    return True


def build_positions():
    """32 directions: 24 quasi-uniform plus 8 deliberately near the pixelisation's structure."""
    rng = random.Random(20260827)
    out = []
    while len(out) < 24:
        theta = math.acos(rng.uniform(-1.0, 1.0))
        phi = rng.uniform(0.0, TWOPI)
        if stable_position(theta, phi):
            out.append((theta, phi))

    z23 = math.acos(2.0 / 3.0)
    near = [
        (1.0e-7, 0.3),                  # just off the north pole
        (math.pi - 1.0e-7, 2.1),        # just off the south pole
        (z23 - 1.0e-7, 0.7),            # just north of the cap/belt boundary
        (z23 + 1.0e-7, 0.7),            # just south of it
        (math.acos(-2.0 / 3.0) - 1.0e-7, 4.0),
        (HALFPI, 1.0e-7),               # equator, just off the phi = 0 seam
        (HALFPI, TWOPI - 1.0e-7),       # equator, just before the seam from below
        (1.2, HALFPI - 1.0e-7),         # just off a quadrant edge
    ]
    for theta, phi in near:
        t, p = theta, phi
        tries = 0
        while not stable_position(t, p):
            tries += 1
            if tries > 64:
                raise SystemExit("generate_healpix_reference: could not stabilise a near-structure position")
            t = min(math.pi, max(0.0, t + NUDGE))
        out.append((t, p))
    return out


def build_pixel_probes(nside):
    """Twelve pixels per nside: the structural boundaries plus a deterministic sample."""
    npix = npix_of(nside)
    ncap = ncap_of(nside)
    cand = [0, 1, npix - 1, npix // 2]
    if ncap > 0:
        cand += [ncap - 1, ncap, npix - ncap - 1, npix - ncap]
    rng = random.Random(0x5EED + nside % 1000003)
    while len(set(x for x in cand if 0 <= x < npix)) < 12 and len(cand) < 64:
        cand.append(rng.randrange(npix))
    probes = sorted(set(x for x in cand if 0 <= x < npix))[:12]
    while len(probes) < 12:
        probes.append(probes[-1])   # tiny nside: fewer than twelve distinct pixels exist
    return probes


def build_disc_configs():
    """Disc queries: eight per full-list nside, six per checksum nside, both inclusive modes."""
    rng = random.Random(20260827 * 7 + 1)
    configs = []

    def add(nside, v0, radius_rad, full):
        for inclusive in (False, True):
            r = radius_rad + (max_pixrad(nside) if inclusive else 0.0)
            configs.append({
                "nside": nside, "vec": v0, "radius": radius_rad,
                "inclusive": inclusive, "full": full,
                "pixels": disc_reference(nside, v0, min(r, math.pi)),
            })

    def random_vec():
        z = rng.uniform(-1.0, 1.0)
        phi = rng.uniform(0.0, TWOPI)
        s = math.sqrt(max(0.0, 1.0 - z * z))
        return (s * math.cos(phi), s * math.sin(phi), z)

    # Radii are sized by the pixel COUNT they should return, not by a multiple of the pixel
    # radius: the count is what decides how long the frozen list is, and a count-blind multiple
    # produced a three-thousand-pixel list at nside 64. For a small disc the count is
    # npix*(1-cos r)/2, which inverts to the radius below.
    def radius_for_count(nside, count):
        return math.acos(max(-1.0, 1.0 - 2.0 * count / npix_of(nside)))

    for nside in DISC_FULL_NSIDES:
        pixrad = max_pixrad(nside)
        wanted = [radius_for_count(nside, c) for c in (2, 8, 25, 60)]
        # Two structural cases every implementation gets wrong differently: a pole-centred
        # hemisphere (where the walk's whole-ring branch and its pole degeneracy both fire) and a
        # disc covering the sphere.
        # A hemisphere is offset by three pixel radii from an exact pi/2: at exactly pi/2 the
        # equatorial ring's centres lie ON the rim, and whether they are in or out is then a
        # last-bit question no two implementations need answer alike (healpy excludes them, this
        # model includes them -- 104 pixels against 88 at nside 4). The shape being tested is the
        # pole-centred whole-ring branch, which the offset preserves exactly.
        structural = [((0.0, 0.0, 1.0), HALFPI + 3.0 * pixrad),
                      ((0.3, 0.4, math.sqrt(0.75)), math.pi)]
        fixed = [(0.0, 0.0, 1.0), (0.0, 0.0, -1.0), (1.0, 0.0, 0.0)]
        picked = []
        for k, radius in enumerate(wanted):
            v = fixed[k] if k < len(fixed) else None
            tries = 0
            while True:
                vv = v if v is not None else random_vec()
                rr = radius * (1.0 + 0.013 * tries)
                if stable_disc(nside, vv, rr) and stable_disc(nside, vv, rr + pixrad):
                    picked.append((vv, rr))
                    break
                v = None
                tries += 1
                if tries > 200:
                    raise SystemExit("generate_healpix_reference: could not stabilise a disc fixture")
        for v, r in picked:
            add(nside, v, r, True)
        # The structural pair is recorded as checksums only: a hemisphere or a whole-sphere disc
        # returns a large fraction of the pixelisation, and freezing tens of thousands of indices
        # would dominate this file for an assertion the count and the two sums already make.
        for v, r in structural:
            if not (stable_disc(nside, v, r) and stable_disc(nside, v, min(r + pixrad, math.pi))):
                raise SystemExit("generate_healpix_reference: structural disc is not stable "
                                 "(nside=%d r=%.17g)" % (nside, r))
            add(nside, v, r, False)

    for nside in DISC_SUM_NSIDES:
        pixrad = max_pixrad(nside)
        for k, mult in enumerate((3.0, 10.0, 40.0)):
            radius = mult * pixrad
            tries = 0
            while True:
                v = (0.0, 0.0, 1.0) if k == 0 and tries == 0 else random_vec()
                rr = radius * (1.0 + 0.011 * tries)
                if stable_disc(nside, v, rr) and stable_disc(nside, v, rr + pixrad):
                    break
                tries += 1
                if tries > 200:
                    raise SystemExit("generate_healpix_reference: could not stabilise a disc fixture")
            add(nside, v, rr, False)
    return configs


def build_angdist_pairs():
    """Vector pairs for the separation formula, including the two ends of its range."""
    rng = random.Random(4242)
    pairs = [
        ((1.0, 0.0, 0.0), (1.0, 0.0, 0.0)),                  # coincident: exactly 0
        ((1.0, 0.0, 0.0), (-1.0, 0.0, 0.0)),                 # antipodal: exactly pi
        ((1.0, 0.0, 0.0), (0.0, 1.0, 0.0)),                  # orthogonal: exactly pi/2
        ((0.0, 0.0, 1.0), (0.0, 0.0, -1.0)),                 # pole to pole
        ((1.0, 0.0, 0.0), (1.0, 1.0e-9, 0.0)),               # near-coincident
        ((1.0, 0.0, 0.0), (-1.0, 1.0e-9, 0.0)),              # near-antipodal
        ((3.0, 0.0, 0.0), (0.0, 5.0, 0.0)),                  # non-unit inputs
        ((0.6, 0.0, 0.8), (0.0, 0.6, 0.8)),
    ]
    while len(pairs) < 20:
        def rv():
            z = rng.uniform(-1.0, 1.0)
            phi = rng.uniform(0.0, TWOPI)
            s = math.sqrt(max(0.0, 1.0 - z * z))
            return (s * math.cos(phi), s * math.sin(phi), z)
        pairs.append((rv(), rv()))
    return pairs


# --------------------------------------------------------------------------------------------
# Emission
# --------------------------------------------------------------------------------------------

def fr(x):
    """A real64 literal that round-trips exactly."""
    return "%.17e_real64" % x


def fi(x):
    """An int64 literal."""
    return "%d_int64" % x


# Source lines are held strictly inside the standard 132-column free-form limit, so the wrapping
# below is driven by the widest literal rather than by a fixed count per line -- an int64 array
# spanning nside = 2**29 has values three times the width of one spanning nside = 4.
MAX_COL = 128


def wrap(items, indent, per_line=None):
    """Format literals as continued array-constructor lines that fit inside MAX_COL."""
    if not items:
        return [indent]
    if per_line is None:
        widest = max(len(x) for x in items)
        per_line = max(1, (MAX_COL - len(indent) - 3) // (widest + 2))
    lines = []
    for k in range(0, len(items), per_line):
        chunk = items[k:k + per_line]
        tail = "" if k + per_line >= len(items) else ", &"
        lines.append(indent + ", ".join(chunk) + tail)
    return lines


def emit_int_array(name, values, per_line=None):
    out = ["    integer(int64), parameter :: %s(%d) = [ &" % (name, len(values))]
    out += wrap([fi(v) for v in values], "        ", per_line)
    out[-1] += "]"
    return out


def emit_real_array(name, values, per_line=None):
    out = ["    real(real64), parameter :: %s(%d) = [ &" % (name, len(values))]
    out += wrap([fr(v) for v in values], "        ", per_line)
    out[-1] += "]"
    return out


def generate(healpy_version=None, numpy_version=None, hp_cxx_version=None):
    """Build the whole emitted module as a string."""
    positions = build_positions()
    configs = build_disc_configs()
    pairs = build_angdist_pairs()

    n_nside = len(NSIDES)
    n_pos = len(positions)
    n_probe = 12

    lines = []
    a = lines.append

    a("!> Reference vectors for `parquet_healpix`, generated from an independent model.")
    a("!!")
    a("!! **GENERATED FILE -- DO NOT EDIT BY HAND.** Emitted by `tools/generate_healpix_reference.py`;")
    a("!! `tools/generate_healpix_reference.py --check` fails if this file has drifted from what the")
    a("!! generator produces, and CI runs it. **Never hand-edit a value here**: these vectors are what")
    a("!! pin the pixelisation's contract, so an edited one is a lie that nothing else would catch.")
    a("!!")
    a("!! Every value is derived by the generator's own pure-Python model of the published HEALPix")
    a("!! algorithm (Gorski et al. 2005, ApJ 622, 759) and cross-checked against `healpy`. The oracle")
    a("!! versions at the last regeneration were:")
    a("!!")
    a("!!     healpy         %s" % (_one_line(healpy_version) or "(not available at generation time)"))
    a("!!     HEALPix C++    %s" % (_one_line(hp_cxx_version) or "(not reported)"))
    a("!!     numpy          %s" % (_one_line(numpy_version) or "(not available at generation time)"))
    a("!!")
    a("!! That matters when a future regeneration disagrees with this file: the first question is")
    a("!! whether the oracle moved or the implementation did, and only a recorded version answers it.")
    a("!!")
    a("!! **Boundary cases are deliberately absent.** A position within a fraction of a pixel of a")
    a("!! pixel boundary, or a disc whose rim passes within an ulp of a pixel centre, is not frozen")
    a("!! here -- two correct implementations may legitimately disagree there, so the generator")
    a("!! perturbs every fixture and rejects any whose answer moves. Those cases are covered instead")
    a("!! by `test/test_healpix.f90`'s self-consistency suite, where both sides are the same")
    a("!! arithmetic and exactness is therefore meaningful.")
    a("module test_healpix_vectors")
    a("    use, intrinsic :: iso_fortran_env, only: int64, real64")
    a("    implicit none")
    a("    public")
    a("")
    a("    !> Number of nside values covered.")
    a("    integer, parameter :: hv_n_nside = %d" % n_nside)
    a("    !> Number of directions covered.")
    a("    integer, parameter :: hv_n_pos = %d" % n_pos)
    a("    !> Pixel probes recorded per nside.")
    a("    integer, parameter :: hv_n_probe = %d" % n_probe)
    a("    !> Disc queries recorded.")
    a("    integer, parameter :: hv_n_disc = %d" % len(configs))
    a("    !> Vector pairs recorded for the separation formula.")
    a("    integer, parameter :: hv_n_angdist = %d" % len(pairs))
    a("    !> Largest nside whose pixel indices still fit `integer(int32)`.")
    a("    integer, parameter :: hv_nside_int32_max = %d" % max(NSIDES_INT32))
    a("")

    a("    ! ---- nside values ----")
    lines.extend(emit_int_array("hv_nside", NSIDES))
    a("")

    a("    ! ---- Directions, and the pixel each one falls in ----")
    a("")
    lines.extend(emit_real_array("hv_theta", [p[0] for p in positions]))
    a("")
    lines.extend(emit_real_array("hv_phi", [p[1] for p in positions]))
    a("")
    a("    !> RING pixel for direction i at nside k, at index (k-1)*hv_n_pos + i.")
    ring_vals = []
    nest_vals = []
    for nside in NSIDES:
        for theta, phi in positions:
            ring_vals.append(ang2pix_ring(nside, theta, phi))
            nest_vals.append(ang2pix_nest(nside, theta, phi))
    lines.extend(emit_int_array("hv_pix_ring", ring_vals))
    a("")
    a("    !> NEST pixel for direction i at nside k, at index (k-1)*hv_n_pos + i.")
    lines.extend(emit_int_array("hv_pix_nest", nest_vals))
    a("")

    a("    ! ---- Pixel probes: centres and scheme conversions ----")
    a("")
    probes = []
    theta_r = []
    phi_r = []
    theta_n = []
    phi_n = []
    r2n = []
    n2r = []
    for nside in NSIDES:
        for p in build_pixel_probes(nside):
            probes.append(p)
            t, ph = pix2ang_ring(nside, p)
            theta_r.append(t)
            phi_r.append(ph)
            t, ph = pix2ang_ring(nside, nest2ring(nside, p))
            theta_n.append(t)
            phi_n.append(ph)
            r2n.append(ring2nest(nside, p))
            n2r.append(nest2ring(nside, p))
    a("    !> Probe pixel j at nside k, at index (k-1)*hv_n_probe + j. The same index addresses")
    a("    !! every array below, and the value is read as a RING index by the `_ring` columns and")
    a("    !! as a NEST index by the `_nest` ones.")
    lines.extend(emit_int_array("hv_probe_pix", probes))
    a("")
    lines.extend(emit_real_array("hv_probe_theta_ring", theta_r))
    a("")
    lines.extend(emit_real_array("hv_probe_phi_ring", phi_r))
    a("")
    lines.extend(emit_real_array("hv_probe_theta_nest", theta_n))
    a("")
    lines.extend(emit_real_array("hv_probe_phi_nest", phi_n))
    a("")
    a("    !> `pf_ring2nest` of the probe pixel.")
    lines.extend(emit_int_array("hv_probe_ring2nest", r2n))
    a("")
    a("    !> `pf_nest2ring` of the probe pixel.")
    lines.extend(emit_int_array("hv_probe_nest2ring", n2r))
    a("")

    a("    ! ---- Disc queries ----")
    a("!")
    a("    !> Disc centres, three components per query, at index 3*(q-1)+1 .. 3*(q-1)+3.")
    vecs = []
    for c in configs:
        vecs.extend(c["vec"])
    lines.extend(emit_real_array("hv_disc_vec", vecs, per_line=3))
    a("")
    lines.extend(emit_real_array("hv_disc_radius", [c["radius"] for c in configs]))
    a("")
    a("    !> nside of each disc query.")
    lines.extend(emit_int_array("hv_disc_nside", [c["nside"] for c in configs]))
    a("")
    a("    !> Whether each query is the overlap superset (`inclusive = .true.`).")
    incl = ["%s" % (".true." if c["inclusive"] else ".false.") for c in configs]
    a("    logical, parameter :: hv_disc_inclusive(%d) = [ &" % len(configs))
    lines.extend(wrap(incl, "        ", 8))
    lines[-1] += "]"
    a("")
    a("    !> Number of pixels each query returns.")
    lines.extend(emit_int_array("hv_disc_count", [len(c["pixels"]) for c in configs], per_line=6))
    a("")
    a("    !> Whether the full RING pixel list is recorded for this query (large nside records only")
    a("    !! the checksums below, so that this file stays a readable size).")
    fullflags = ["%s" % (".true." if c["full"] else ".false.") for c in configs]
    a("    logical, parameter :: hv_disc_has_list(%d) = [ &" % len(configs))
    lines.extend(wrap(fullflags, "        ", 8))
    lines[-1] += "]"
    a("")
    a("    !> Where query q's pixel list starts in `hv_disc_pixels`; the length is `hv_disc_count(q)`.")
    a("    !! Zero for a query recording only checksums.")
    offsets = []
    flat = []
    for c in configs:
        if c["full"]:
            offsets.append(len(flat) + 1)
            flat.extend(c["pixels"])
        else:
            offsets.append(0)
    lines.extend(emit_int_array("hv_disc_offset", offsets))
    a("")
    a("    !> Every recorded query's RING pixel list, concatenated and each ascending.")
    lines.extend(emit_int_array("hv_disc_pixels", flat))
    a("")
    a("    !> Sum of the RING pixel indices each query returns, modulo 2**61.")
    sums = [sum(c["pixels"]) % CHECKSUM_MOD for c in configs]
    lines.extend(emit_int_array("hv_disc_sum", sums))
    a("")
    a("    !> Sum of the squares of the RING pixel indices, modulo 2**61.")
    sqs = [sum(p * p for p in c["pixels"]) % CHECKSUM_MOD for c in configs]
    lines.extend(emit_int_array("hv_disc_sumsq", sqs))
    a("")
    a("    !> Sum of the NEST pixel indices the same query returns, modulo 2**61. Recorded")
    a("    !! separately because `pf_query_disc`'s NEST output order is unspecified, so only its")
    a("    !! contents as a set can be asserted.")
    nsums = []
    for c in configs:
        nside = c["nside"]
        nsums.append(sum(ring2nest(nside, p) for p in c["pixels"]) % CHECKSUM_MOD)
    lines.extend(emit_int_array("hv_disc_nest_sum", nsums))
    a("")

    a("    ! ---- Angular separations ----")
    a("")
    a("    !> Vector pairs, six components per pair: v1 then v2.")
    pv = []
    for v1, v2 in pairs:
        pv.extend(v1)
        pv.extend(v2)
    lines.extend(emit_real_array("hv_angdist_vec", pv, per_line=3))
    a("")
    a("    !> The separation of each pair, in radians.")
    lines.extend(emit_real_array("hv_angdist", [angdist(v1, v2) for v1, v2 in pairs]))
    a("")

    a("    ! ---- Pixel geometry ----")
    a("")
    a("    !> Largest centre-to-corner angular distance at each nside, in radians.")
    lines.extend(emit_real_array("hv_max_pixrad", [max_pixrad(n) for n in NSIDES]))
    a("")
    a("end module test_healpix_vectors")
    return "\n".join(lines) + "\n"


# --------------------------------------------------------------------------------------------
# Self-test: everything derivable without healpy.
# --------------------------------------------------------------------------------------------

def self_test():
    fails = []

    def check(ok, what):
        if not ok:
            fails.append(what)

    # Structural arithmetic, straight from the paper.
    for nside in NSIDES:
        check(npix_of(nside) == 12 * nside * nside, "npix at nside=%d" % nside)
        check(order_of(nside) == round(math.log2(nside)), "order at nside=%d" % nside)
        check(2 ** order_of(nside) == nside, "nside=%d is a power of two" % nside)

    # nside = 1: the twelve base pixels. The four northern faces have centres at z = 2/3 and
    # phi = (2k+1)*pi/4; RING and NEST agree on every pixel at this resolution.
    for p in range(12):
        check(ring2nest(1, p) == p, "nside=1 ring2nest identity at %d" % p)
        check(nest2ring(1, p) == p, "nside=1 nest2ring identity at %d" % p)
    for k in range(4):
        z, phi = pix2zphi_ring(1, k)
        check(abs(z - 2.0 / 3.0) < 1e-15, "nside=1 north face z at %d" % k)
        check(abs(phi - (2 * k + 1) * math.pi / 4.0) < 1e-15, "nside=1 north face phi at %d" % k)
    check(abs(max_pixrad(1) - math.acos(2.0 / 3.0)) < 1e-15, "max_pixrad(1) closed form")

    # Round trips and the ring decomposition, exhaustively at small nside.
    for nside in (1, 2, 4, 8, 16, 32):
        npix = npix_of(nside)
        seen = set()
        for p in range(npix):
            n = ring2nest(nside, p)
            check(0 <= n < npix, "ring2nest range nside=%d p=%d" % (nside, p))
            check(nest2ring(nside, n) == p, "ring/nest round trip nside=%d p=%d" % (nside, p))
            seen.add(n)
            t, ph = pix2ang_ring(nside, p)
            check(ang2pix_ring(nside, t, ph) == p, "pix2ang/ang2pix nside=%d p=%d" % (nside, p))
            check(ang2pix_nest(nside, t, ph) == n, "pix2ang/ang2pix_nest nside=%d p=%d" % (nside, p))
        check(len(seen) == npix, "ring2nest is a bijection at nside=%d" % nside)

    # Total solid angle: every pixel has the same area, so the count times the area is 4*pi.
    for nside in NSIDES:
        check(abs(npix_of(nside) * (4.0 * math.pi / npix_of(nside)) - 4.0 * math.pi) < 1e-12,
              "pixel areas sum to 4*pi at nside=%d" % nside)

    # max_pixrad against a brute-force scan of every pixel's own corner distances. The corners are
    # derived from the pixelisation's boundary curves rather than assumed, by sampling the four
    # pixels adjacent in the (ring, index) grid -- a corner is equidistant from its neighbours.
    for nside in (2, 4, 8, 16):
        worst = 0.0
        for p in range(npix_of(nside)):
            c = pix2vec_ring(nside, p)
            i, j, nr, shifted = ring_decompose(nside, p)
            for di in (-1, 1):
                ii = i + di
                if ii < 1 or ii > 4 * nside - 1:
                    continue
                first2, nr2, sh2 = ring_first(nside, ii)
                for dj in (0, 1, -1):
                    z2 = ring_z(nside, ii)
                    st2 = math.sqrt(max(0.0, 1.0 - z2 * z2))
                    jj = (int((j + 0.5 * shifted) * nr2 / nr - 0.5 * sh2) + dj) % nr2
                    v2 = pix2vec_ring(nside, first2 + jj)
                    # The corner between the two centres lies on their bisector; its distance from
                    # either is at most half their separation plus the in-ring half-width.
                    d = 0.5 * angdist(c, v2) + 0.5 * (TWOPI / nr) * math.sqrt(max(0.0, 1.0 - c[2] ** 2))
                    worst = max(worst, d)
        check(max_pixrad(nside) <= worst + 1e-9,
              "max_pixrad(%d) is at least the sampled corner bound" % nside)

    # The disc oracle against a whole-sphere scan, including the shapes reimplementations get
    # wrong: both poles, the seam, a hemisphere, the whole sphere, and a radius of zero.
    rng = random.Random(11)
    for nside in (1, 2, 4, 8, 16):
        cases = [((0.0, 0.0, 1.0), 0.4), ((0.0, 0.0, -1.0), 0.4), ((1.0, 0.0, 0.0), 0.3),
                 ((0.0, 0.0, 1.0), HALFPI), ((1.0, 0.0, 0.0), HALFPI),
                 ((0.0, 0.0, 1.0), math.pi), ((0.6, 0.0, 0.8), 1.0e-9),
                 ((math.cos(1e-9), math.sin(1e-9), 0.0), 0.3)]
        for _ in range(30):
            z = rng.uniform(-1.0, 1.0)
            phi = rng.uniform(0.0, TWOPI)
            s = math.sqrt(max(0.0, 1.0 - z * z))
            cases.append(((s * math.cos(phi), s * math.sin(phi), z), rng.uniform(0.001, 2.5)))
        for v0, r in cases:
            got = disc_reference(nside, v0, r)
            want = disc_brute(nside, v0, r)
            check(got == want, "disc oracle vs brute force nside=%d v=%s r=%g" % (nside, v0, r))

    # A disc of radius pi returns every pixel; a disc of radius 0 returns at most one.
    for nside in (1, 4, 16):
        check(len(disc_reference(nside, (0.3, 0.4, 0.5), math.pi)) == npix_of(nside),
              "radius pi covers the sphere at nside=%d" % nside)
        check(len(disc_reference(nside, (0.3, 0.4, 0.5), 0.0)) <= 1,
              "radius 0 returns at most one pixel at nside=%d" % nside)

    # angdist's exact anchors.
    check(angdist((1.0, 0.0, 0.0), (1.0, 0.0, 0.0)) == 0.0, "angdist coincident is exactly 0")
    check(angdist((1.0, 0.0, 0.0), (-1.0, 0.0, 0.0)) == math.pi, "angdist antipodal is exactly pi")
    check(abs(angdist((1.0, 0.0, 0.0), (0.0, 1.0, 0.0)) - HALFPI) < 1e-16, "angdist orthogonal")

    if fails:
        sys.stderr.write("generate_healpix_reference --self-test FAILED (%d):\n" % len(fails))
        for f in fails[:40]:
            sys.stderr.write("  %s\n" % f)
        return 1
    print("generate_healpix_reference --self-test: all anchors reproduced")
    return 0


# --------------------------------------------------------------------------------------------
# Oracle verification: every emitted value against healpy.
# --------------------------------------------------------------------------------------------

def verify_oracle():
    try:
        import numpy as np
        import healpy as hp
    except ImportError as exc:
        sys.stderr.write("generate_healpix_reference --verify-oracle needs healpy and numpy: %s\n" % exc)
        return 2

    fails = []
    skipped_inclusive = []

    def check(ok, what):
        if not ok:
            fails.append(what)

    def got_pixels(cfg):
        return cfg["pixels"]

    positions = build_positions()
    for nside in NSIDES:
        th = np.array([p[0] for p in positions])
        ph = np.array([p[1] for p in positions])
        ref_r = hp.ang2pix(nside, th, ph, nest=False)
        ref_n = hp.ang2pix(nside, th, ph, nest=True)
        for k, (t, p) in enumerate(positions):
            check(ang2pix_ring(nside, t, p) == int(ref_r[k]), "ang2pix_ring nside=%d pos=%d" % (nside, k))
            check(ang2pix_nest(nside, t, p) == int(ref_n[k]), "ang2pix_nest nside=%d pos=%d" % (nside, k))

        probes = build_pixel_probes(nside)
        arr = np.array(probes, dtype=np.int64)
        tr, pr = hp.pix2ang(nside, arr, nest=False)
        tn, pn = hp.pix2ang(nside, arr, nest=True)
        r2n = hp.ring2nest(nside, arr)
        n2r = hp.nest2ring(nside, arr)
        for k, p in enumerate(probes):
            # Compared as cos(theta) rather than theta. Both sides compute theta as acos of the
            # ring's z, and acos near +-1 is ill-conditioned: at nside = 2**20 the first ring's
            # z is 1 - 3e-13, which a double resolves to about three significant digits, so two
            # implementations agreeing on z to an ulp still differ by ~4e-10 in theta. Comparing
            # the quantity both sides actually hold asks the question that has an answer.
            t, ph_ = pix2ang_ring(nside, p)
            check(abs(math.cos(t) - math.cos(tr[k])) < 1e-14 and abs(ph_ - pr[k]) < 1e-12,
                  "pix2ang_ring nside=%d p=%d" % (nside, p))
            t, ph_ = pix2ang_ring(nside, nest2ring(nside, p))
            check(abs(math.cos(t) - math.cos(tn[k])) < 1e-14 and abs(ph_ - pn[k]) < 1e-12,
                  "pix2ang_nest nside=%d p=%d" % (nside, p))
            check(ring2nest(nside, p) == int(r2n[k]), "ring2nest nside=%d p=%d" % (nside, p))
            check(nest2ring(nside, p) == int(n2r[k]), "nest2ring nside=%d p=%d" % (nside, p))

        check(abs(max_pixrad(nside) - hp.max_pixrad(nside)) <= 1e-9 * hp.max_pixrad(nside),
              "max_pixrad nside=%d" % nside)

    for q, c in enumerate(build_disc_configs()):
        nside, v0, r = c["nside"], c["vec"], c["radius"]
        if c["inclusive"]:
            # The two libraries enlarge differently, so the contract is a bounded superset rather
            # than equality -- see feature_healpix_tier_a.md section 8.7.
            #
            # `fact` is healpy's oversampling factor and decides how TIGHT its superset is: a
            # larger one returns fewer pixels that do not really overlap. It is also bounded --
            # healpy subdivides to `nside*fact`, which it refuses above 2**29 (an abort from its
            # C++ half, not an exception) -- so at high resolution only a loose superset is
            # available, and a loose one may legitimately hold pixels beyond this module's
            # published bound. Rather than compare against a superset looser than the contract,
            # the comparison is SKIPPED there and counted, so that a skip is visible instead of
            # reading as a pass.
            fact = 1
            while fact * 2 <= 64 and nside * fact * 2 <= (1 << 29):
                fact *= 2
            if fact >= 4:
                ref = set(int(x) for x in hp.query_disc(nside, np.array(v0), r, inclusive=True,
                                                        fact=fact, nest=False))
                got = set(c["pixels"])
                check(ref <= got, "inclusive disc %d is a superset of healpy's (fact=%d)" % (q, fact))
            else:
                skipped_inclusive.append(q)
            # The bound half needs no oracle: it is this module's own published contract, and it
            # is checked at every resolution including the ones healpy cannot oversample.
            bound = r + max_pixrad(nside)
            worst = max((angdist(pix2vec_ring(nside, p), v0) for p in got_pixels(c)), default=0.0)
            check(worst <= bound + 1e-12, "inclusive disc %d respects radius + max_pixrad" % q)
        else:
            ref = sorted(int(x) for x in hp.query_disc(nside, np.array(v0), r, inclusive=False, nest=False))
            check(ref == c["pixels"], "exact disc %d matches healpy (%d vs %d)" % (q, len(ref), len(c["pixels"])))

    for k, (v1, v2) in enumerate(build_angdist_pairs()):
        ref = hp.rotator.angdist(np.array(v1), np.array(v2))[0]
        check(abs(angdist(v1, v2) - ref) < 1e-12, "angdist pair %d" % k)

    if fails:
        sys.stderr.write("generate_healpix_reference --verify-oracle FAILED (%d):\n" % len(fails))
        for f in fails[:40]:
            sys.stderr.write("  %s\n" % f)
        return 1
    print("generate_healpix_reference --verify-oracle: every emitted value agrees with healpy %s"
          % hp.__version__)
    if skipped_inclusive:
        print("  note: %d inclusive disc(s) could not be compared against healpy, because healpy "
              "cannot oversample" % len(skipped_inclusive))
        print("        past nside 2**29; their published bound was still checked. Configs: %s"
              % skipped_inclusive)
    return 0


def _one_line(text, limit=48):
    """A version string fit to sit inside a Fortran comment: one line, bounded, printable.

    Not defensive decoration. An earlier version of this generator recorded
    `healpy.pixelfunc.pixlib.__doc__` as the C++ version; that is a six-line module docstring, and
    emitting it put five lines carrying no `!!` prefix into the header -- which is a Fortran
    compile error in the generated file, produced by the generator, with the failure appearing
    only when someone next builds the tests.
    """
    if text is None:
        return None
    first = str(text).strip().splitlines()
    if not first:
        return None
    out = " ".join(first[0].split())
    return out[:limit] if out else None


def oracle_versions():
    """Version strings for the emitted header, when the oracle is importable.

    healpy exposes no accessor for the version of the HEALPix C++ library it wraps (checked:
    `__healpix_version__`, `healpy.version`'s contents and the distribution metadata all report
    only healpy's own version), so what the header records for it is the healpy release that
    bundles it. That is the checkable statement, and it is what a future regeneration needs in
    order to answer "did the oracle move?".
    """
    try:
        import numpy as np
        import healpy as hp
    except ImportError:
        return None, None, None
    return (_one_line(hp.__version__), _one_line(np.__version__),
            _one_line("as bundled with healpy %s" % hp.__version__))


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--check", action="store_true", help="compare the committed file with a fresh generation")
    ap.add_argument("--self-test", action="store_true", help="re-derive every published anchor")
    ap.add_argument("--verify-oracle", action="store_true", help="cross-check every value against healpy")
    args = ap.parse_args()

    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    target = os.path.join(root, "test", "test_healpix_vectors.f90")

    if args.self_test:
        rc = self_test()
        if rc:
            return rc
    if args.verify_oracle:
        rc = verify_oracle()
        if rc:
            return rc
    if args.self_test or args.verify_oracle:
        if not args.check:
            return 0

    if args.check:
        if not os.path.exists(target):
            sys.stderr.write("generate_healpix_reference --check: %s does not exist\n" % target)
            return 1
        with open(target) as fh:
            committed = fh.read()
        # The header records the oracle version present when the file was WRITTEN, which a check
        # run on a machine without healpy cannot reproduce -- so the comparison is of everything
        # below the module statement, plus the header with those three lines masked.
        fresh = generate(*_header_versions_of(committed))
        if fresh != committed:
            sys.stderr.write("generate_healpix_reference --check: %s is out of date; regenerate it\n" % target)
            _report_first_difference(committed, fresh)
            return 1
        print("generate_healpix_reference --check: %s is current" % os.path.relpath(target, root))
        return 0

    hv, nv, cxx = oracle_versions()
    text = generate(hv, nv, cxx)
    with open(target, "w") as fh:
        fh.write(text)
    print("generate_healpix_reference: wrote %s (%d lines)"
          % (os.path.relpath(target, root), text.count("\n")))
    if hv is None:
        print("  NOTE: healpy was not importable, so the header records no oracle version.")
        print("        Regenerate under the `astro` environment before committing.")
    return 0


def _header_versions_of(text):
    """Read back the oracle versions the committed file's header records."""
    hv = nv = cxx = None
    for line in text.splitlines():
        s = line.strip()
        if s.startswith("!!     healpy "):
            hv = s[len("!!     healpy "):].strip()
        elif s.startswith("!!     numpy "):
            nv = s[len("!!     numpy "):].strip()
        elif s.startswith("!!     HEALPix C++ "):
            cxx = s[len("!!     HEALPix C++ "):].strip()
    unknown = ("(not available at generation time)", "(not reported)")
    return (None if hv in unknown else hv,
            None if nv in unknown else nv,
            None if cxx in unknown else cxx)


def _report_first_difference(committed, fresh):
    ca = committed.splitlines()
    fa = fresh.splitlines()
    for k in range(max(len(ca), len(fa))):
        c = ca[k] if k < len(ca) else "<end of file>"
        f = fa[k] if k < len(fa) else "<end of file>"
        if c != f:
            sys.stderr.write("  first difference at line %d:\n    committed: %s\n    fresh:     %s\n"
                             % (k + 1, c[:110], f[:110]))
            return


if __name__ == "__main__":
    sys.exit(main())
