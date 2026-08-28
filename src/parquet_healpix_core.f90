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
        real(real64) :: cx, cy, cz, dot, cross

        cx = vec1(2) * vec2(3) - vec1(3) * vec2(2)
        cy = vec1(3) * vec2(1) - vec1(1) * vec2(3)
        cz = vec1(1) * vec2(2) - vec1(2) * vec2(1)
        dot = vec1(1) * vec2(1) + vec1(2) * vec2(2) + vec1(3) * vec2(3)
        cross = sqrt(cx * cx + cy * cy + cz * cz)
        ! ATAN2(0, 0) IS PROHIBITED, not merely awkward: F2018 16.9.16 requires X to be nonzero
        ! when Y is zero, so the (0, 0) case is non-conforming however reasonable "it is just
        ! zero" sounds. gfortran, ifx and flang return 0 and raise nothing; nagfor returns NaN and
        ! raises IEEE_INVALID, which under its default -ieee=stop TERMINATES the process. Only a
        ! zero-length input can reach it -- with both vectors nonzero, a zero cross product means
        ! parallel or antiparallel, and then the dot product is +-|v1||v2| and cannot also be zero.
        if (cross == 0.0_real64 .and. dot == 0.0_real64) then
            dist = 0.0_real64
            return
        end if
        ! atan2 rather than acos(dot): the cross product carries the small angles and the dot
        ! product the large ones, so this form keeps about an ulp across the whole range where
        ! acos(dot) loses half its digits near 0 and near pi. It is also scale-invariant, so the
        ! inputs need not be normalised, and with the guard above it raises nothing on any finite
        ! input.
        dist = atan2(cross, dot)
    end procedure pf_angdist

    module procedure pf_angdist_deg
        real(real64) :: dl, sdl, cdl, sd1, cd1, sd2, cd2, y1, y2, x

        ! THE RA DIFFERENCE IS FORMED IN DEGREES AND FOLDED INTO [-180, 180] BEFORE IT IS SCALED,
        ! and both halves of that earn their place. Subtracting in degrees keeps the difference of
        ! two nearby right ascensions exact (Sterbenz), where converting each to radians first
        ! rounds before the cancellation. Folding then keeps a small separation that straddles the
        ! phi = 0 seam small, instead of handing sine and cosine an argument near 2*pi whose own
        ! rounding is the size of the answer. Measured against a 60-digit evaluation of the same
        ! formula, the fold takes 359.5 deg against 0.25 deg from 1.8e-14 to 1.1e-16 degrees, and
        ! makes a 2**-12 degree separation across the seam exact. It also admits a right ascension
        ! outside [0, 360) at no cost, which a caller carrying an accumulated hour angle will have.
        !
        ! `anint` rather than a subtract-until-in-range loop: such a loop never terminates on an
        ! infinite argument, since Inf - 360 is Inf. This form yields NaN there and returns.
        dl = ra1 - ra2
        ! **A QUIET NaN MUST PROPAGATE QUIETLY, and `anint` is the one step that breaks that.**
        ! IEEE says an operation on a quiet NaN yields a quiet NaN and raises nothing, and every
        ! other step below obeys it -- but rounding a NaN to an integer is an invalid operation,
        ! and nagfor duly raises `IEEE_INVALID` on `anint(NaN)` (gfortran, ifx and flang do not).
        ! That matters because nagfor UNMASKS the traps by default, so a caller who passed a NaN
        ! right ascension would have their process TERMINATED by a procedure documented as total.
        ! Returning the NaN before the fold is the whole fix, and it costs one quiet predicate.
        !
        ! **An INFINITE right ascension still raises, and must.** `sin(Inf)` and `Inf - Inf` are
        ! invalid operations on any conforming processor, so no arrangement of this arithmetic can
        ! make an infinite angle quiet -- and it should not: unlike a NaN, which is already the
        ! "no answer" value being carried through, an infinity is a real number the caller asked
        ! to take the sine of. The line is between propagating an existing NaN and creating one.
        !
        ! **`dl /= dl` rather than `ieee_is_nan(dl)`, deliberately**, and it is the same knowing
        ! departure from this project's default that `src/parquet_argsort_engine.f90` makes at
        ! length in its own header -- read that one for the measurements. In short: on ifx and
        ! nagfor `ieee_is_nan` is a CALL into the Fortran runtime rather than an instruction
        ! (measured at +2.479 ns per test on the sort's comparator under ifx, +1.58 ns here under
        ! nagfor), while gfortran inlines it to a native compare and never sees the cost. `x /= x`
        ! is free on all of them. This procedure is `pure elemental` and is meant to be broadcast
        ! over whole catalogues, so a per-call runtime call is exactly the wrong thing to put in
        ! front of it. The two spellings are equivalent for this guard's purpose: a NaN is the only
        ! value not equal to itself, and BOTH are quiet on a quiet NaN -- verified, and load-bearing,
        ! because the entire point of the guard is to raise no flag. The cost is one
        ! `-Wcompare-reals` warning under gfortran's `--profile debug`, which is accepted here
        ! exactly as it is in the sort engine.
        if (dl /= dl) then
            dist = dl
            return
        end if
        dl = (dl - 360.0_real64 * anint(dl / 360.0_real64)) * hpx_deg2rad
        ! **A POSITION IS EXACTLY ZERO DEGREES FROM ITSELF, and that has to be asserted here
        ! rather than left to the arithmetic.** Without this the answer is a few times 1e-15
        ! degrees, and whether it is that or bit-zero depends on the compiler: `y2` below is
        ! `cd1*sd2 - sd1*cd2*cdl`, whose two products are equal for coincident positions and
        ! therefore cancel exactly -- UNLESS the compiler contracts the subtraction into an FMA,
        ! which computes the first product exactly and the second rounded, leaving the second's
        ! rounding error behind. Measured: gfortran leaves it at zero, nagfor returns
        ! 1.22e-15 degrees for (12.5, 34.5) against itself.
        !
        ! It is worth a branch because the failure is silent and directional: a caller excluding
        ! self-matches from a cross-match with `dist > 0` keeps every one of them. Two comparisons
        ! against six transcendentals is not measurable, and nothing else about the result moves --
        ! this returns the value the formula is trying to compute, not a different one.
        !
        ! Both ways two positions can coincide are covered. Equal declinations with no surviving
        ! RA difference is the ordinary one; equal declinations at a POLE is the other, where any
        ! two right ascensions name the same point. The pole case is not reachable by the formula
        ! at all, because `cos(90 * hpx_deg2rad)` is 6.1e-17 rather than zero -- an input
        ! representation limit that no formulation can remove, which is exactly why it is stated
        ! as a rule instead. A separation of NaN falls through, so totality is preserved.
        if (dec1 == dec2 .and. (dl == 0.0_real64 .or. abs(dec1) == 90.0_real64)) then
            dist = 0.0_real64
            return
        end if
        sdl = sin(dl)
        cdl = cos(dl)
        sd1 = sin(dec1 * hpx_deg2rad)
        cd1 = cos(dec1 * hpx_deg2rad)
        sd2 = sin(dec2 * hpx_deg2rad)
        cd2 = cos(dec2 * hpx_deg2rad)
        ! Vincenty: atan2(|v1 x v2|, v1.v2) written in the frame where only the RA difference
        ! survives. It is the same quantity `pf_angdist` computes from vectors, and 31% cheaper,
        ! because the rotation removes one of the four sine/cosine pairs.
        !
        ! **`acos` of the dot product must not be reintroduced here**, and neither must the
        ! haversine. Measured on inputs chosen to be exactly representable, so that what is left is
        ! the formula's own error: `acos` is 2.0e-07 degrees wrong a millionth of a degree from the
        ! pole and 3.9e-08 wrong at dec 89.5, because it loses half its digits wherever the dot
        ! product approaches 1; the haversine is accurate there but 9.5e-07 degrees wrong just
        ! inside antipodal, where this form is exact. This form holds 7.8e-15 degrees or better
        ! everywhere, and that worst case is the pole, where it is `cos(dec)` losing relative
        ! precision rather than anything the formula can help.
        y1 = cd2 * sdl
        y2 = cd1 * sd2 - sd1 * cd2 * cdl
        x = sd1 * sd2 + cd1 * cd2 * cdl
        ! ATAN2(0, 0) cannot arise, so this needs no guard where `pf_angdist` above does: there a
        ! caller can pass a zero-length vector, whereas here `sqrt(y1^2 + y2^2)` and `x` are the
        ! sine and cosine of one real angle and cannot both round to zero. `sqrt` of the sum of
        ! squares rather than `hypot`: both components are bounded by 1 so nothing overflows, and
        ! the underflow that squaring costs is reached only below about 1e-154 radians, which no
        ! difference of two degree-valued doubles can express.
        dist = atan2(sqrt(y1 * y1 + y2 * y2), x) * hpx_rad2deg
    end procedure pf_angdist_deg

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

    module procedure hpx_ringij2nest_run
        integer(int64) :: i, face, t, ix, iy, kshift, d, tsum, jp_l, jm_l, ifp, ifm
        integer(int64) :: nside2, k, ipn, jpm, jmm, four_n, mx, my
        integer :: region
        logical :: use32

        if (count <= 0_int64) return
        use32 = present(out32)
        nside2 = nside * nside
        four_n = 4_int64 * nside

        ! ---- The first position, decoded exactly as `hpx_ringij2nest` decodes it ----
        !
        ! Everything after this is a step, so this is the only place the two forms can disagree,
        ! and it is deliberately the same arithmetic rather than a simplification of it.
        if (jr < nside) then
            region = 1
            i = jr
            face = jstart / i
            t = modulo(jstart, i)
            ix = nside - i + t
            iy = nside - 1_int64 - t
        else if (jr > 3_int64 * nside) then
            region = 2
            i = four_n - jr
            face = 8_int64 + jstart / i
            t = modulo(jstart, i)
            ix = t
            iy = i - 1_int64 - t
        else
            region = 3
            kshift = iand(jr - nside, 1_int64)
            d = jr - 2_int64 * nside
            tsum = 2_int64 * jstart + nside - kshift - 1_int64
            if (iand(tsum, 1_int64) /= iand(d, 1_int64)) tsum = tsum + 1_int64
            jp_l = (tsum + d) / 2_int64
            jm_l = (tsum - d) / 2_int64
            ! The phi = 0 seam, as in the scalar form. Applying it here and not again on later
            ! positions is not a difference: it adds `4*nside` to BOTH indices, which raises both
            ! face numbers by four and so changes neither their comparison nor their low two bits,
            ! and leaves both remainders alone. It matters only where an index is negative, where
            ! Fortran's truncation toward zero would otherwise give the wrong face -- and once
            ! stepped past that, the two agree again.
            if (jp_l < 0_int64 .or. jm_l < 0_int64) then
                jp_l = jp_l + four_n
                jm_l = jm_l + four_n
            end if
            ifp = jp_l / nside
            ifm = jm_l / nside
            jpm = jp_l - ifp * nside
            jmm = jm_l - ifm * nside
            ix = jmm
            iy = nside - 1_int64 - jpm
            face = belt_face(ifp, ifm)
        end if

        mx = hpx_spread_bits(ix)
        my = ishft(hpx_spread_bits(iy), 1)

        do k = 0_int64, count - 1_int64
            ipn = face * nside2 + ior(mx, my)
            if (use32) then
                out32(offset + k + 1_int64) = int(ipn, int32)
            else
                out64(offset + k + 1_int64) = ipn
            end if
            ! The advance is skipped on the last position, so the state is never stepped past the
            ! end of the run -- which is what keeps a face counter from running off its own range
            ! at the end of a ring.
            if (k == count - 1_int64) exit
            select case (region)
            case (1)
                ! North cap. Within a face the position runs diagonally: `x` up, `y` down. At the
                ! quadrant boundary it restarts at that face's own corner.
                t = t + 1_int64
                if (t == i) then
                    t = 0_int64
                    face = face + 1_int64
                    mx = hpx_spread_bits(nside - i)
                    my = ishft(hpx_spread_bits(nside - 1_int64), 1)
                else
                    mx = iand(ior(mx, hpx_m1_odd) + 1_int64, hpx_m1)
                    my = iand(my - 2_int64, hpx_m1_odd)
                end if
            case (2)
                ! South cap: the same diagonal, from the opposite corner.
                t = t + 1_int64
                if (t == i) then
                    t = 0_int64
                    face = face + 1_int64
                    mx = 0_int64
                    my = ishft(hpx_spread_bits(i - 1_int64), 1)
                else
                    mx = iand(ior(mx, hpx_m1_odd) + 1_int64, hpx_m1)
                    my = iand(my - 2_int64, hpx_m1_odd)
                end if
            case default
                ! Belt. Both diagonal indices advance by one per position -- the parity fixup above
                ! is the same for `jstart` and `jstart+1`, so their sum rises by exactly two -- and
                ! each carries into its own face counter on a multiple of `nside`. Tracking the
                ! remainders directly is what removes the two integer divisions per pixel.
                jmm = jmm + 1_int64
                if (jmm == nside) then
                    jmm = 0_int64
                    ifm = ifm + 1_int64
                    mx = 0_int64
                else
                    mx = iand(ior(mx, hpx_m1_odd) + 1_int64, hpx_m1)
                end if
                jpm = jpm + 1_int64
                if (jpm == nside) then
                    jpm = 0_int64
                    ifp = ifp + 1_int64
                    my = ishft(hpx_spread_bits(nside - 1_int64), 1)
                else
                    my = iand(my - 2_int64, hpx_m1_odd)
                end if
                face = belt_face(ifp, ifm)
            end select
        end do

    contains

        !> Which of the twelve faces a belt position lies on, from its two diagonal face indices.
        pure function belt_face(a, b) result(f)
            integer(int64), intent(in) :: a !! face index of the `jp` diagonal.
            integer(int64), intent(in) :: b !! face index of the `jm` diagonal.
            integer(int64) :: f !! the face, 0 .. 11.

            if (a == b) then
                f = iand(a, 3_int64) + 4_int64
            else if (a < b) then
                f = iand(a, 3_int64)
            else
                f = iand(b, 3_int64) + 8_int64
            end if
        end function belt_face

    end procedure hpx_ringij2nest_run

    module procedure hpx_max_pixrad
        real(real64) :: zc, sc, zv, sv, dz, ds, sh, chord2, rn, omzv

        ! The most elongated pixels are those of the first equatorial ring, whose centres sit at
        ! z = 2/3; the farthest corner of one is its northern corner, on the quadrant meridian
        ! pi/(4*nside) away in longitude, at the z of the last cap ring.
        !
        ! EVERY SMALL QUANTITY IS FORMED DIRECTLY RATHER THAN AS A DIFFERENCE OF TWO LARGE ONES,
        ! and that is what this procedure is really about. The two vectors converge as nside
        ! grows, so an implementation that builds them and then measures the angle between them
        ! subtracts z-components that agree to ~1e-9 -- catastrophic cancellation, leaving about
        ! eight significant digits. Measured against a 60-digit evaluation, the build-then-measure
        ! form is 4.3e-10 relatively wrong at nside = 2**24, 5.5e-09 at 2**26 and 2.0e-08 at
        ! 2**28; the form below holds ~1e-16 across the whole range. (An `acos` of the dot product
        ! is worse again and must not be reintroduced: 5.6e-05 at nside = 2**20 and EXACTLY ZERO
        ! at 2**29. Zero is the dangerous one -- pf_query_disc's inclusive mode enlarges its radius
        ! by this quantity, so a zero makes inclusive = .true. silently identical to
        ! inclusive = .false. at high resolution, with nothing failing unless a test asserts the
        ! enlargement itself.)
        !
        ! It also makes the answer COMPILER-INDEPENDENT. With eight digits of headroom, whether a
        ! given toolchain contracts `1 - (n-1)**2/(3n**2)` into an FMA decided the eighth
        ! significant digit, so the reference comparison at nside = 2**29 was really asserting
        ! that this compiler rounds like the one that generated the reference table.
        rn = real(nside, real64)
        zc = hpx_twothird
        sc = sqrt((1.0_real64 - zc) * (1.0_real64 + zc))
        ! dz = zv - zc, as the exact rational (2n-1)/(3n^2) rather than as a subtraction.
        dz = (2.0_real64 * rn - 1.0_real64) / (3.0_real64 * rn * rn)
        zv = zc + dz
        ! omzv = 1 - zv, likewise as the exact rational (n-1)^2/(3n^2) rather than as a
        ! subtraction. This one is not merely about precision: at nside = 1 the corner IS the
        ! pole, so zv is exactly 1 and 1 - zv must be exactly 0 -- but written as a subtraction
        ! that outcome depends on `zc + dz` having rounded to 1.0, and ifx's default
        ! -fp-model=fast reassociates it into (1 - zc) - dz, which is 2**-54 instead. That fed a
        ! spurious sv = 1.05e-08 into `ds` below and moved the answer by 8.9e-09 relative, on ifx
        ! only. Formed directly, (rn - 1) is exact and the result is exactly zero on every
        ! compiler. 1 + zv is 2 - omzv, which cannot cancel (omzv <= 1/3).
        omzv = (rn - 1.0_real64) * (rn - 1.0_real64) / (3.0_real64 * rn * rn)
        sv = sqrt(max(0.0_real64, omzv * (2.0_real64 - omzv)))
        ! ds = sv - sc, likewise: (sv^2 - sc^2)/(sv + sc) = -dz*(zv + zc)/(sv + sc), which is a
        ! quotient of well-separated quantities. sv + sc is never zero (sc = sqrt(5)/3 > 0).
        ds = -dz * (zv + zc) / (sv + sc)
        ! Chord, then 2*asin(chord/2). The half-angle sine keeps the longitude term accurate at
        ! large nside where dphi itself underflows toward zero.
        sh = sin(0.5_real64 * hpx_pi / (4.0_real64 * rn))
        chord2 = ds * ds + 4.0_real64 * sc * sv * sh * sh + dz * dz
        r = 2.0_real64 * asin(min(1.0_real64, sqrt(chord2) * 0.5_real64))
    end procedure hpx_max_pixrad

    module procedure hpx_max_pixrad_i64
        r = hpx_max_pixrad(nside)
    end procedure hpx_max_pixrad_i64

    module procedure hpx_max_pixrad_i32
        r = hpx_max_pixrad(int(nside, int64))
    end procedure hpx_max_pixrad_i32

    ! ---- Position -> pixel ----

    module procedure hpx_ang2pix_ring_i64
        ! Split into a `z`/`phi` worker so that `pf_vec2pix_ring` can reach it without computing
        ! `theta = acos(z)` only for this line to take its cosine again. Every operation below the
        ! cosine is unchanged and in its original order, which is what keeps the result
        ! bit-identical to what the frozen vectors were generated against.
        ipix = hpx_zphi2pix_ring(nside, cos(theta), phi)
    end procedure hpx_ang2pix_ring_i64

    module procedure hpx_zphi2pix_ring
        real(real64) :: za, tt, temp1, temp2, tp, tmp, rn
        integer(int64) :: jp, jm, ir, kshift, ip

        rn = real(nside, real64)
        za = abs(z)
        ! `phi` in [0, 2*pi) -- every well-formed input -- already lands in [0, 4), so the
        ! reduction is a no-op there and a compare is enough to skip it. That is worth doing rather
        ! than leaving to `modulo` because gfortran compiles real `modulo` to the x87 `fprem` loop,
        ! measured at 28% of this procedure and at 71% of `pf_query_disc`'s ring walk where the same
        ! intrinsic appears. One conditional step then covers any `phi` within a revolution of the
        ! interval, and `modulo` still backs it up beyond that, so the result is unchanged for every
        ! input rather than for the ones expected here. Both steps are exact: `tt - 4` is exact by
        ! Sterbenz's lemma for `tt` in [4, 8), and `tt + 4` is the single rounding `modulo` performs.
        tt = phi / hpx_halfpi
        if (tt >= 4.0_real64) then
            tt = tt - 4.0_real64
            if (tt >= 4.0_real64) tt = modulo(tt, 4.0_real64)
        else if (tt < 0.0_real64) then
            tt = tt + 4.0_real64
            if (tt < 0.0_real64) tt = modulo(tt, 4.0_real64)
        end if
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
            ! `nside` is a power of two, so `4*nside` is one and the remainder is a mask. This
            ! is exact for a negative `ip` too: `iand(x, 2**k - 1)` is `modulo(x, 2**k)` in two's
            ! complement, which matters because `ip` can be -1 at the phi = 0 seam.
            ip = iand(ip, 4_int64 * nside - 1_int64)
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
    end procedure hpx_zphi2pix_ring

    module procedure hpx_ang2pix_ring_i32
        integer(int64) :: p

        call hpx_ang2pix_ring_i64(int(nside, int64), theta, phi, p)
        ipix = int(p, int32)
    end procedure hpx_ang2pix_ring_i32

    ! ---- Direction to pixel ----
    !
    ! These reach the same `z`/`phi` workers `pf_ang2pix_*` does, one step further in: a caller
    ! holding a unit vector already has `z`, so routing through `pf_vec2ang` would compute an
    ! inverse tangent for this file to undo with a cosine, and lose a rounding at each end.

    module procedure hpx_vec2pix_ring_i64
        real(real64) :: x, y, z

        call hpx_vec_unit(vec, x, y, z)
        ipix = hpx_zphi2pix_ring(nside, z, hpx_xy2phi(x, y))
    end procedure hpx_vec2pix_ring_i64

    module procedure hpx_vec2pix_ring_i32
        integer(int64) :: p

        call hpx_vec2pix_ring_i64(int(nside, int64), vec, p)
        ipix = int(p, int32)
    end procedure hpx_vec2pix_ring_i32

    module procedure hpx_vec2pix_nest_i64
        real(real64) :: x, y, z

        call hpx_vec_unit(vec, x, y, z)
        ipix = hpx_zphi2pix_nest(nside, z, hpx_xy2phi(x, y))
    end procedure hpx_vec2pix_nest_i64

    module procedure hpx_vec2pix_nest_i32
        integer(int64) :: p

        call hpx_vec2pix_nest_i64(int(nside, int64), vec, p)
        ipix = int(p, int32)
    end procedure hpx_vec2pix_nest_i32

    ! ---- Nested resolution change ----

    module procedure hpx_ud_pix_nest_i64
        ! The guard is not about caller mistakes so much as about the SHIFT: a shift whose
        ! magnitude reaches the integer's bit size is not defined by the standard and is a
        ! diagnosable condition under -fcheck=all, so a procedure that shifted whatever it was
        ! given could abort in a checked build on input a released build merely gets wrong.
        ! Bounding both orders to 0 .. 29 makes the largest legal shift 58 bits.
        !
        ! The UPPER bound on `ipix` (< 12 * 4**order_in) is deliberately not checked: it costs a
        ! multiply on the one axis a caller may sweep, and an out-of-range input already produces
        ! a visibly out-of-range result.
        if (ipix < 0_int64 .or. order_in < 0_int64 .or. order_in > hpx_order_max .or. &
            order_out < 0_int64 .or. order_out > hpx_order_max) then
            ipix_out = -1_int64
        else if (order_out >= order_in) then
            ipix_out = ishft(ipix, int(2_int64 * (order_out - order_in)))
        else
            ipix_out = ishft(ipix, -int(2_int64 * (order_in - order_out)))
        end if
    end procedure hpx_ud_pix_nest_i64

    module procedure hpx_ud_pix_nest_i32
        integer(int64) :: p

        ! The int32 form caps both orders at 13, its own nside ceiling, rather than at 29: an
        ! order above 13 names a resolution whose pixel indices this kind cannot hold, so a
        ! result computed for it could only be an overflow waiting to happen.
        if (ipix < 0_int32 .or. order_in < 0_int32 .or. order_in > hpx_order_max_i32 .or. &
            order_out < 0_int32 .or. order_out > hpx_order_max_i32) then
            ipix_out = -1_int32
        else
            call hpx_ud_pix_nest_i64(int(ipix, int64), int(order_in, int64), &
                                     int(order_out, int64), p)
            ipix_out = int(p, int32)
        end if
    end procedure hpx_ud_pix_nest_i32

    module procedure hpx_ang2pix_nest_i64
        ! See `hpx_ang2pix_ring_i64`: the same split, for the same reason.
        ipix = hpx_zphi2pix_nest(nside, cos(theta), phi)
    end procedure hpx_ang2pix_nest_i64

    module procedure hpx_zphi2pix_nest
        real(real64) :: za, tt, temp1, temp2, tp, tmp, rn
        integer(int64) :: jp, jm, ifp, ifm, face, ix, iy, ntt

        rn = real(nside, real64)
        za = abs(z)
        ! `phi` in [0, 2*pi) -- every well-formed input -- already lands in [0, 4), so the
        ! reduction is a no-op there and a compare is enough to skip it. That is worth doing rather
        ! than leaving to `modulo` because gfortran compiles real `modulo` to the x87 `fprem` loop,
        ! measured at 28% of this procedure and at 71% of `pf_query_disc`'s ring walk where the same
        ! intrinsic appears. One conditional step then covers any `phi` within a revolution of the
        ! interval, and `modulo` still backs it up beyond that, so the result is unchanged for every
        ! input rather than for the ones expected here. Both steps are exact: `tt - 4` is exact by
        ! Sterbenz's lemma for `tt` in [4, 8), and `tt + 4` is the single rounding `modulo` performs.
        tt = phi / hpx_halfpi
        if (tt >= 4.0_real64) then
            tt = tt - 4.0_real64
            if (tt >= 4.0_real64) tt = modulo(tt, 4.0_real64)
        else if (tt < 0.0_real64) then
            tt = tt + 4.0_real64
            if (tt < 0.0_real64) tt = modulo(tt, 4.0_real64)
        end if
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
            ! A mask again, for the reason `hpx_zphi2pix_ring` gives: `nside` is a power of two.
            ix = iand(jm, nside - 1_int64)
            iy = nside - 1_int64 - iand(jp, nside - 1_int64)
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
    end procedure hpx_zphi2pix_nest

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
