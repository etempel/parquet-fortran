!> The pixelisation itself: ring geometry, the Morton codec, and every position/pixel conversion.
!!
!! Derived from Gorski et al. 2005, ApJ 622, 759; see `parquet_healpix`'s own header for the
!! provenance rule this file is written under. Each procedure's contract is documented on its
!! interface in `src/parquet_healpix.f90`, which is the canonical place for it -- what the comments
!! here add is why a particular arrangement of the arithmetic was chosen, which is the part a
!! future reader cannot recover from the formula.
!!
!! **Every procedure here is `pure`, and the two message helpers at the end are the only
!! exceptions** (they perform an internal write). Nothing in this file validates its arguments or
!! aborts: the conversions are the per-element hot path, and validation belongs in the
!! once-per-query entry points -- see `parquet_healpix_query`.
submodule(parquet_healpix) parquet_healpix_core
    implicit none

contains

    ! ---- Small shared helpers ----

    module procedure hpx_spread_bits
        s = iand(v, hpx_m32)
        s = iand(ior(s, ishft(s, 16)), hpx_m16)
        s = iand(ior(s, ishft(s, 8)), hpx_m8)
        s = iand(ior(s, ishft(s, 4)), hpx_m4)
        s = iand(ior(s, ishft(s, 2)), hpx_m2)
        s = iand(ior(s, ishft(s, 1)), hpx_m1)
    end procedure hpx_spread_bits

    module procedure hpx_compact_bits
        c = iand(v, hpx_m1)
        c = iand(ior(c, ishft(c, -1)), hpx_m2)
        c = iand(ior(c, ishft(c, -2)), hpx_m4)
        c = iand(ior(c, ishft(c, -4)), hpx_m8)
        c = iand(ior(c, ishft(c, -8)), hpx_m16)
        c = iand(ior(c, ishft(c, -16)), hpx_m32)
    end procedure hpx_compact_bits

    module procedure hpx_nside_ok
        ! A positive power of two has exactly one bit set, so clearing its lowest set bit leaves
        ! zero. The range test comes first because it is what rejects 0 before the bit test, where
        ! `iand(0, -1)` would otherwise report 0 as a power of two.
        ok = nside >= 1_int64 .and. nside <= limit
        if (ok) ok = iand(nside, nside - 1_int64) == 0_int64
    end procedure hpx_nside_ok

    ! ---- Angular separation ----

    module procedure pf_angdist
        real(real64) :: cx, cy, cz, dot

        cx = vec1(2) * vec2(3) - vec1(3) * vec2(2)
        cy = vec1(3) * vec2(1) - vec1(1) * vec2(3)
        cz = vec1(1) * vec2(2) - vec1(2) * vec2(1)
        dot = vec1(1) * vec2(1) + vec1(2) * vec2(2) + vec1(3) * vec2(3)
        ! atan2 rather than acos(dot): the cross product carries the small angles and the dot
        ! product the large ones, so this form keeps about an ulp across the whole range where
        ! acos(dot) loses half its digits near 0 and near pi. It is also scale-invariant, so the
        ! inputs need not be normalised, and it raises nothing on any finite input.
        dist = atan2(sqrt(cx * cx + cy * cy + cz * cz), dot)
    end procedure pf_angdist

    ! ---- Ring geometry ----

    module procedure hpx_ring_z
        real(real64) :: rn, ri
        integer(int64) :: k

        rn = real(nside, real64)
        if (i < nside) then
            ri = real(i, real64)
            z = 1.0_real64 - ri * ri / (3.0_real64 * rn * rn)
        else if (i > 3_int64 * nside) then
            k = 4_int64 * nside - i
            ri = real(k, real64)
            z = -(1.0_real64 - ri * ri / (3.0_real64 * rn * rn))
        else
            z = (2.0_real64 * rn - real(i, real64)) * 2.0_real64 / (3.0_real64 * rn)
        end if
    end procedure hpx_ring_z

    module procedure hpx_ring_first
        integer(int64) :: k

        if (i < nside) then
            first = 2_int64 * i * (i - 1_int64)
            nr = 4_int64 * i
            shifted = 1_int64
        else if (i > 3_int64 * nside) then
            k = 4_int64 * nside - i
            first = 12_int64 * nside * nside - 2_int64 * k * (k + 1_int64)
            nr = 4_int64 * k
            shifted = 1_int64
        else
            first = 2_int64 * nside * (nside - 1_int64) + (i - nside) * 4_int64 * nside
            nr = 4_int64 * nside
            shifted = iand(i - nside + 1_int64, 1_int64)
        end if
    end procedure hpx_ring_first

    module procedure hpx_ring_above
        real(real64) :: az, rn
        integer(int64) :: guard

        az = abs(z)
        rn = real(nside, real64)
        if (az > hpx_twothird) then
            ! Cap: invert z = 1 - i^2/(3*nside^2). The float seed lands within a ring of the
            ! answer; the two corrections make it exact. Both are bounded rather than open loops --
            ! the walk widens its band by a ring on each side anyway, so an answer off by one costs
            ! three membership tests and never a wrong result, and a bound is cheaper than trusting
            ! the seed on an input nobody has thought about.
            i = int(rn * sqrt(3.0_real64 * (1.0_real64 - az)), int64)
            if (i < 1_int64) i = 1_int64
            guard = 0_int64
            do while (i >= 1_int64 .and. hpx_ring_z(nside, i) < az .and. guard < 8_int64)
                i = i - 1_int64
                guard = guard + 1_int64
            end do
            guard = 0_int64
            do while (i + 1_int64 < nside .and. hpx_ring_z(nside, i + 1_int64) >= az .and. guard < 8_int64)
                i = i + 1_int64
                guard = guard + 1_int64
            end do
            if (z <= 0.0_real64) i = 4_int64 * nside - i - 1_int64
        else
            ! Belt: z is linear in the ring index, so the seed is exact but for rounding.
            i = nint(rn * (2.0_real64 - 1.5_real64 * z), int64) + 1_int64
            if (i < 1_int64) i = 1_int64
            guard = 0_int64
            do while (i >= 1_int64 .and. hpx_ring_z(nside, i) < z .and. guard < 8_int64)
                i = i - 1_int64
                guard = guard + 1_int64
            end do
            guard = 0_int64
            do while (i + 1_int64 <= 4_int64 * nside - 1_int64 .and. &
                      hpx_ring_z(nside, i + 1_int64) >= z .and. guard < 8_int64)
                i = i + 1_int64
                guard = guard + 1_int64
            end do
        end if
    end procedure hpx_ring_above

    module procedure hpx_cap_ring
        ! Inverts p = 2*i*(i-1) + j, i.e. i = (1 + sqrt(1 + 2p))/2 before the truncation. The
        ! square root's relative error of about 1e-16 is worth less than a thousandth of a ring
        ! even at nside = 2**29, where the argument reaches 7e18 -- so each correction runs at most
        ! once, and they are what make the result exact rather than probably right.
        i = int((1.0_real64 + sqrt(1.0_real64 + 2.0_real64 * real(p, real64))) * 0.5_real64, int64)
        if (i < 1_int64) i = 1_int64
        do while (2_int64 * i * (i + 1_int64) <= p)
            i = i + 1_int64
        end do
        do while (i > 1_int64 .and. 2_int64 * i * (i - 1_int64) > p)
            i = i - 1_int64
        end do
    end procedure hpx_cap_ring

    module procedure hpx_ring_decompose
        integer(int64) :: npix, ncap, pp, pn, jn, k

        npix = 12_int64 * nside * nside
        ncap = 2_int64 * nside * (nside - 1_int64)
        if (p < ncap) then
            i = hpx_cap_ring(p)
            j = p - 2_int64 * i * (i - 1_int64)
            nr = 4_int64 * i
            shifted = 1_int64
        else if (p < npix - ncap) then
            pp = p - ncap
            nr = 4_int64 * nside
            i = pp / nr + nside
            j = modulo(pp, nr)
            shifted = iand(i - nside + 1_int64, 1_int64)
        else
            ! The south cap is the north cap reflected, so it is decomposed by reflecting the
            ! index rather than by a second set of formulas that could drift from the first.
            pn = npix - 1_int64 - p
            k = hpx_cap_ring(pn)
            jn = pn - 2_int64 * k * (k - 1_int64)
            i = 4_int64 * nside - k
            j = 4_int64 * k - 1_int64 - jn
            nr = 4_int64 * k
            shifted = 1_int64
        end if
    end procedure hpx_ring_decompose

    module procedure hpx_pix2zphi_ring
        integer(int64) :: i, j, nr, shifted

        call hpx_ring_decompose(nside, p, i, j, nr, shifted)
        z = hpx_ring_z(nside, i)
        ! One expression for all three regions: a ring's centres are equally spaced in longitude,
        ! offset by half a step when the ring is shifted.
        phi = (real(j, real64) + 0.5_real64 * real(shifted, real64)) * (hpx_twopi / real(nr, real64))
    end procedure hpx_pix2zphi_ring

    module procedure hpx_ringij2nest
        integer(int64) :: i, face, t, ix, iy, kshift, d, tsum, jp_l, jm_l, ifp, ifm

        if (jr < nside) then
            ! North cap: the ring's four quadrants are the four northern faces, and the position
            ! within a quadrant runs diagonally across the face.
            i = jr
            face = j / i
            t = modulo(j, i)
            ix = nside - i + t
            iy = nside - 1_int64 - t
        else if (jr > 3_int64 * nside) then
            i = 4_int64 * nside - jr
            face = 8_int64 + j / i
            t = modulo(j, i)
            ix = t
            iy = i - 1_int64 - t
        else
            ! Belt: recover the two diagonal line indices the forward transform floors. Their
            ! difference is fixed by the ring; their sum is known up to the single bit that
            ! forward's division by two discarded, and the parity of the difference restores it.
            kshift = iand(jr - nside, 1_int64)
            d = jr - 2_int64 * nside
            tsum = 2_int64 * j + nside - kshift - 1_int64
            if (iand(tsum, 1_int64) /= iand(d, 1_int64)) tsum = tsum + 1_int64
            jp_l = (tsum + d) / 2_int64
            jm_l = (tsum - d) / 2_int64
            if (jp_l < 0_int64 .or. jm_l < 0_int64) then
                ! The phi = 0 seam: the pair wrapped below zero and is restored a whole revolution
                ! up, which leaves the face determination below unchanged.
                jp_l = jp_l + 4_int64 * nside
                jm_l = jm_l + 4_int64 * nside
            end if
            ifp = jp_l / nside
            ifm = jm_l / nside
            if (ifp == ifm) then
                face = iand(ifp, 3_int64) + 4_int64
            else if (ifp < ifm) then
                face = iand(ifp, 3_int64)
            else
                face = iand(ifm, 3_int64) + 8_int64
            end if
            ix = modulo(jm_l, nside)
            iy = nside - 1_int64 - modulo(jp_l, nside)
        end if
        ipnest = face * nside * nside + ior(hpx_spread_bits(ix), ishft(hpx_spread_bits(iy), 1))
    end procedure hpx_ringij2nest

    module procedure hpx_max_pixrad
        real(real64) :: zc, sc, zv, sv, dphi, rn, c(3), v(3)

        ! The most elongated pixels are those of the first equatorial ring, whose centres sit at
        ! z = 2/3; the farthest corner of one is its northern corner, on the quadrant meridian
        ! pi/(4*nside) away in longitude, at the z of the last cap ring.
        rn = real(nside, real64)
        zc = hpx_twothird
        sc = sqrt(1.0_real64 - zc * zc)
        zv = 1.0_real64 - (rn - 1.0_real64) * (rn - 1.0_real64) / (3.0_real64 * rn * rn)
        sv = sqrt(max(0.0_real64, (1.0_real64 - zv) * (1.0_real64 + zv)))
        dphi = hpx_pi / (4.0_real64 * rn)
        c = [sc, 0.0_real64, zc]
        v = [sv * cos(dphi), sv * sin(dphi), zv]
        ! Measured with atan2, never with acos of the dot product, and that is a correctness
        ! matter rather than a refinement: the two vectors converge as nside grows, so their dot
        ! product approaches 1 and an acos of it loses the whole answer to cancellation. Measured
        ! against healpy, the acos form is 5.6e-05 relatively wrong at nside = 2**20 and returns
        ! EXACTLY ZERO at nside = 2**29, where the true value is 1.99e-09. Zero is the dangerous
        ! one: pf_query_disc's inclusive mode enlarges its radius by this quantity, so a zero makes
        ! inclusive = .true. silently identical to inclusive = .false. at high resolution, with no
        ! abort and no test failing unless one asserts the enlargement itself. This form holds
        ! 4e-10 relative or better across the whole nside range.
        call pf_angdist(c, v, r)
    end procedure hpx_max_pixrad

    module procedure hpx_max_pixrad_i64
        r = hpx_max_pixrad(nside)
    end procedure hpx_max_pixrad_i64

    module procedure hpx_max_pixrad_i32
        r = hpx_max_pixrad(int(nside, int64))
    end procedure hpx_max_pixrad_i32

    ! ---- Position -> pixel ----

    module procedure hpx_ang2pix_ring_i64
        real(real64) :: z, za, tt, temp1, temp2, tp, tmp, rn
        integer(int64) :: jp, jm, ir, kshift, ip

        rn = real(nside, real64)
        z = cos(theta)
        za = abs(z)
        tt = modulo(phi / hpx_halfpi, 4.0_real64)
        if (za <= hpx_twothird) then
            ! Equatorial belt. jp and jm index the two families of diagonal lines the belt's
            ! pixels are bounded by, so their difference names the ring and their sum the position
            ! along it.
            temp1 = rn * (0.5_real64 + tt)
            temp2 = rn * (z * 0.75_real64)
            jp = floor(temp1 - temp2, int64)
            jm = floor(temp1 + temp2, int64)
            ir = nside + 1_int64 + jp - jm
            kshift = 1_int64 - iand(ir, 1_int64)
            ip = (jp + jm - nside + kshift + 1_int64) / 2_int64
            ip = modulo(ip, 4_int64 * nside)
            ipix = 2_int64 * nside * (nside - 1_int64) + (ir - 1_int64) * 4_int64 * nside + ip
        else
            ! Polar cap. The two formulas continue each other exactly at |z| = 2/3, so the
            ! boundary needs no special case: ir = nside there lands on the first equatorial ring
            ! with the same longitude convention.
            tp = tt - aint(tt)
            tmp = rn * sqrt(3.0_real64 * (1.0_real64 - za))
            jp = int(tp * tmp, int64)
            jm = int((1.0_real64 - tp) * tmp, int64)
            ir = jp + jm + 1_int64
            ip = modulo(int(tt * real(ir, real64), int64), 4_int64 * ir)
            if (z > 0.0_real64) then
                ipix = 2_int64 * ir * (ir - 1_int64) + ip
            else
                ipix = 12_int64 * nside * nside - 2_int64 * ir * (ir + 1_int64) + ip
            end if
        end if
    end procedure hpx_ang2pix_ring_i64

    module procedure hpx_ang2pix_ring_i32
        integer(int64) :: p

        call hpx_ang2pix_ring_i64(int(nside, int64), theta, phi, p)
        ipix = int(p, int32)
    end procedure hpx_ang2pix_ring_i32

    module procedure hpx_ang2pix_nest_i64
        real(real64) :: z, za, tt, temp1, temp2, tp, tmp, rn
        integer(int64) :: jp, jm, ifp, ifm, face, ix, iy, ntt

        rn = real(nside, real64)
        z = cos(theta)
        za = abs(z)
        tt = modulo(phi / hpx_halfpi, 4.0_real64)
        if (za <= hpx_twothird) then
            temp1 = rn * (0.5_real64 + tt)
            temp2 = rn * (z * 0.75_real64)
            jp = floor(temp1 - temp2, int64)
            jm = floor(temp1 + temp2, int64)
            ! Which face a belt position falls on is decided by which face-width band each
            ! diagonal index lands in: equal bands means an equatorial face, otherwise the smaller
            ! one names a northern or southern one.
            ifp = jp / nside
            ifm = jm / nside
            if (ifp == ifm) then
                face = iand(ifp, 3_int64) + 4_int64
            else if (ifp < ifm) then
                face = iand(ifp, 3_int64)
            else
                face = iand(ifm, 3_int64) + 8_int64
            end if
            ix = modulo(jm, nside)
            iy = nside - 1_int64 - modulo(jp, nside)
        else
            ntt = int(tt, int64)
            tp = tt - real(ntt, real64)
            tmp = rn * sqrt(3.0_real64 * (1.0_real64 - za))
            ! A guard, and described as one rather than as a necessity: `tmp` is
            ! `nside*sqrt(3*(1-|z|))`, which this branch's own condition keeps below `nside`, so
            ! the clamp should never fire. It is kept because the alternative is a face-crossing
            ! wrong answer rather than a slightly wrong one -- an unclamped `nside` here indexes
            ! off the face and produces a pixel belonging to a different one entirely -- and
            ! because the bound is only guaranteed up to how `sqrt` rounds within an ulp of the
            ! boundary. A sweep of 200000 positions within 1e-9 of `|z| = 2/3` at ten resolutions,
            ! and an ulp-by-ulp walk upward from the boundary itself, produced no case where it
            ! changed the answer; do not read that as licence to remove it, and do not read the
            ! `min` as evidence that the unclamped value is reachable.
            jp = min(int(tp * tmp, int64), nside - 1_int64)
            jm = min(int((1.0_real64 - tp) * tmp, int64), nside - 1_int64)
            if (z > 0.0_real64) then
                face = ntt
                ix = nside - 1_int64 - jm
                iy = nside - 1_int64 - jp
            else
                face = ntt + 8_int64
                ix = jp
                iy = jm
            end if
        end if
        ipix = face * nside * nside + ior(hpx_spread_bits(ix), ishft(hpx_spread_bits(iy), 1))
    end procedure hpx_ang2pix_nest_i64

    module procedure hpx_ang2pix_nest_i32
        integer(int64) :: p

        call hpx_ang2pix_nest_i64(int(nside, int64), theta, phi, p)
        ipix = int(p, int32)
    end procedure hpx_ang2pix_nest_i32

    ! ---- Scheme conversion ----

    module procedure hpx_nest2ring_i64
        integer(int64) :: npix, f, raw, ix, iy, jr, nr, n_before, kshift, jp

        npix = 12_int64 * nside * nside
        f = ipnest / (nside * nside)
        raw = ipnest - f * nside * nside
        ix = hpx_compact_bits(raw)
        iy = hpx_compact_bits(ishft(raw, -1))
        jr = hpx_jrll(int(f)) * nside - ix - iy - 1_int64
        if (jr < nside) then
            nr = jr
            n_before = 2_int64 * nr * (nr - 1_int64)
            kshift = 0_int64
        else if (jr > 3_int64 * nside) then
            nr = 4_int64 * nside - jr
            n_before = npix - 2_int64 * nr * (nr + 1_int64)
            kshift = 0_int64
        else
            nr = nside
            n_before = 2_int64 * nside * (nside - 1_int64) + (jr - nside) * 4_int64 * nside
            kshift = iand(jr - nside, 1_int64)
        end if
        jp = (hpx_jpll(int(f)) * nr + ix - iy + 1_int64 + kshift) / 2_int64
        ! One revolution either way is all that is ever needed: the face offset and the within-face
        ! coordinates each span less than a full ring.
        if (jp > 4_int64 * nr) jp = jp - 4_int64 * nr
        if (jp < 1_int64) jp = jp + 4_int64 * nr
        ipring = n_before + jp - 1_int64
    end procedure hpx_nest2ring_i64

    module procedure hpx_nest2ring_i32
        integer(int64) :: p

        call hpx_nest2ring_i64(int(nside, int64), int(ipnest, int64), p)
        ipring = int(p, int32)
    end procedure hpx_nest2ring_i32

    module procedure hpx_ring2nest_i64
        integer(int64) :: i, j, nr, shifted

        call hpx_ring_decompose(nside, ipring, i, j, nr, shifted)
        ipnest = hpx_ringij2nest(nside, i, j)
    end procedure hpx_ring2nest_i64

    module procedure hpx_ring2nest_i32
        integer(int64) :: p

        call hpx_ring2nest_i64(int(nside, int64), int(ipring, int64), p)
        ipnest = int(p, int32)
    end procedure hpx_ring2nest_i32

    ! ---- Pixel -> position ----

    module procedure hpx_pix2ang_ring_i64
        real(real64) :: z

        call hpx_pix2zphi_ring(nside, ipix, z, phi)
        ! Clamped before the acos although the ring formulas cannot mathematically exceed 1: the
        ! clamp costs nothing and discharges this module's no-IEEE-exception promise locally,
        ! rather than resting it on an argument about the formula.
        theta = acos(max(-1.0_real64, min(1.0_real64, z)))
    end procedure hpx_pix2ang_ring_i64

    module procedure hpx_pix2ang_ring_i32
        real(real64) :: t, p

        call hpx_pix2ang_ring_i64(int(nside, int64), int(ipix, int64), t, p)
        theta = t
        phi = p
    end procedure hpx_pix2ang_ring_i32

    module procedure hpx_pix2ang_nest_i64
        integer(int64) :: ipring

        call hpx_nest2ring_i64(nside, ipix, ipring)
        call hpx_pix2ang_ring_i64(nside, ipring, theta, phi)
    end procedure hpx_pix2ang_nest_i64

    module procedure hpx_pix2ang_nest_i32
        real(real64) :: t, p

        call hpx_pix2ang_nest_i64(int(nside, int64), int(ipix, int64), t, p)
        theta = t
        phi = p
    end procedure hpx_pix2ang_nest_i32

    module procedure hpx_pix2vec_ring_i64
        real(real64) :: z, phi, st

        call hpx_pix2zphi_ring(nside, ipix, z, phi)
        ! Built from z directly rather than through theta = acos(z) and back through cos(theta).
        ! The difference is small -- at most 3e-16 rad -- and the honest account of why it matters
        ! is worth more than the size: it is what makes this procedure usable as a TEST ORACLE. A
        ! whole-sphere scan judging disc membership by the dot product of each centre with the
        ! query direction was measured disagreeing with the walk on exactly one shape, a
        ! pole-centred disc of radius pi/2, where the equator ring's z came back from the round
        ! trip as 6.1e-17 instead of 0 and the entire ring flipped sides. The z-built form has no
        ! such case and costs nothing.
        !
        ! Note this is NOT load-bearing for pf_query_disc, which never calls it: the walk derives
        ! each ring's z from hpx_ring_z directly. The difference is also not observable through
        ! this module's own public surface, because the only route from a vector back to a pixel
        ! runs through acos -- see test_pix2vec_round_trip, which says why it stops at nside 2**20.
        st = sqrt(max(0.0_real64, (1.0_real64 - z) * (1.0_real64 + z)))
        vec(1) = st * cos(phi)
        vec(2) = st * sin(phi)
        vec(3) = z
    end procedure hpx_pix2vec_ring_i64

    module procedure hpx_pix2vec_ring_i32
        call hpx_pix2vec_ring_i64(int(nside, int64), int(ipix, int64), vec)
    end procedure hpx_pix2vec_ring_i32

    module procedure hpx_pix2vec_nest_i64
        integer(int64) :: ipring

        call hpx_nest2ring_i64(nside, ipix, ipring)
        call hpx_pix2vec_ring_i64(nside, ipring, vec)
    end procedure hpx_pix2vec_nest_i64

    module procedure hpx_pix2vec_nest_i32
        call hpx_pix2vec_nest_i64(int(nside, int64), int(ipix, int64), vec)
    end procedure hpx_pix2vec_nest_i32

    ! ---- Message helpers ----
    !
    ! The only impure procedures in this file: both perform an internal write, and both are reached
    ! only from an abort path in parquet_healpix_query.

    module procedure hpx_itoa
        character(len=32) :: buf

        write (buf, '(i0)') value
        text = trim(buf)
    end procedure hpx_itoa

    module procedure hpx_rtoa
        character(len=32) :: buf

        write (buf, '(g0.8)') value
        text = trim(adjustl(buf))
    end procedure hpx_rtoa

end submodule parquet_healpix_core
