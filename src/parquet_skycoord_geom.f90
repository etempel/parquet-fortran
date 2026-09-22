!> `parquet_skycoord`'s frame-free RA/Dec geometry: the separation of two positions, the offset by a
!! separation at a position angle, the position angle itself, the proper motion, the public unit
!! vector of a position, and the tangent-plane projection about a field centre.
!!
!! None of them takes a frame, because each is invariant under the reflection that separates the
!! two declination conventions in live use: a separation is an angle between two directions, and an
!! offset and a position angle flip their east together. Each procedure's contract is on its
!! interface in `src/parquet_skycoord.f90`; what the comments here add is why the arithmetic is
!! arranged as it is. The offset and the position angle use the RA/Dec helpers of
!! `parquet_skycoord_rotate`, and with them its two rules: a declination of exactly +/-90 is the
!! pole, and a quiet NaN is handed back before anything can raise a flag on it.
submodule (parquet_skycoord) parquet_skycoord_geom
    use parquet_utils, only: pf_to_str
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    implicit none

contains

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
        ! **A QUIET NaN MUST PROPAGATE QUIETLY, and TWO of the steps below break that.**
        ! IEEE says an operation on a quiet NaN yields a quiet NaN and raises nothing, and most of
        ! what follows obeys it -- but two steps do not, on different compilers, so the whole guard
        ! is hoisted here and every NaN argument is returned before either can be reached. The three
        ! quiet predicates cost less than they read: an unordered compare sets its parity flag when
        ! EITHER operand is a NaN, so ifx emits two `ucomisd` and two not-taken branches for the
        ! whole guard -- it tests `dl` against `dec1` in one instruction.
        !
        ! **`anint(NaN)` raises under nagfor.** Rounding a NaN to an integer is an invalid
        ! operation, and nagfor duly raises `IEEE_INVALID` on it (gfortran, ifx and flang do not).
        ! That matters because nagfor UNMASKS the traps by default, so a caller who passed a NaN
        ! right ascension would have their process TERMINATED by a procedure documented as total.
        ! Only `ra1`/`ra2` reach the fold, and `dl` is NaN whenever either of them is.
        !
        ! **`sin(NaN)` raises under ifx, once the compiler VECTORISES it.** ifx pairs the two
        ! `sin(dec * skc_deg2rad)` calls below into one `__svml_sin2` call, and that routine is not
        ! quiet on a NaN element: it raises `IEEE_INVALID` inside its own argument reduction (at a
        ! `vmulpd`, located by single-stepping the released object under gdb). The scalar `sin` on
        ! the same compiler is quiet, which is why this is invisible in a hand-written copy of this
        ! formula and appears only in the separately compiled procedure -- and it is invisible on
        ! gfortran and nagfor entirely. A declination NaN therefore has to be caught here too,
        ! rather than left to propagate through the transcendentals as the IEEE model promises.
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
        if (dl /= dl .or. dec1 /= dec1 .or. dec2 /= dec2) then
            ! Hand back whichever argument is the NaN rather than composing one: an arithmetic
            ! combination such as `dl + dec1 + dec2` reaches `Inf + (-Inf)` on a mixed
            ! infinite/NaN input and raises the very flag this guard exists to avoid.
            dist = dl
            if (dec1 /= dec1) dist = dec1
            if (dec2 /= dec2) dist = dec2
            return
        end if
        dl = (dl - 360.0_real64 * anint(dl / 360.0_real64)) * skc_deg2rad
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
        ! at all, because `cos(90 * skc_deg2rad)` is 6.1e-17 rather than zero -- an input
        ! representation limit that no formulation can remove, which is exactly why it is stated
        ! as a rule instead. A separation of NaN falls through, so totality is preserved.
        if (dec1 == dec2 .and. (dl == 0.0_real64 .or. abs(dec1) == 90.0_real64)) then
            dist = 0.0_real64
            return
        end if
        sdl = sin(dl)
        cdl = cos(dl)
        sd1 = sin(dec1 * skc_deg2rad)
        cd1 = cos(dec1 * skc_deg2rad)
        sd2 = sin(dec2 * skc_deg2rad)
        cd2 = cos(dec2 * skc_deg2rad)
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
        ! ATAN2(0, 0) cannot arise, so this needs no guard where `pf_angdist` does: there a caller
        ! can pass a zero-length vector, whereas here `sqrt(y1^2 + y2^2)` and `x` are the sine and
        ! cosine of one real angle and cannot both round to zero. `sqrt` of the sum of squares
        ! rather than `hypot`: both components are bounded by 1 so nothing overflows, and the
        ! underflow that squaring costs is reached only below about 1e-154 radians, which no
        ! difference of two degree-valued doubles can express.
        dist = atan2(sqrt(y1 * y1 + y2 * y2), x) * skc_rad2deg
    end procedure pf_angdist_deg

    module procedure pf_offset_radec
        real(real64) :: sd0, cd0, a0, sa, ca, p, sp, cp, s, ss, cs, c(3), north(3), east(3), v(3)
        character(len=:), allocatable :: t_dec, t_sep

        ! A NaN argument gives NaN results, handed back itself before the ordered comparisons below
        ! could raise on it: a null in a catalogue column is a routine event, not a caller mistake.
        if (ra0 /= ra0 .or. dec0 /= dec0 .or. pa_deg /= pa_deg .or. sep_deg /= sep_deg) then
            ra = ra0
            if (dec0 /= dec0) ra = dec0
            if (pa_deg /= pa_deg) ra = pa_deg
            if (sep_deg /= sep_deg) ra = sep_deg
            dec = ra
            return
        end if
        ! The two refusals that name nothing: a centre beyond a pole mirrors the local north and east
        ! and gives a plausible wrong point, and a negative separation has no reading. An infinite
        ! `ra0`, `pa_deg` or `sep_deg` passes and meets `sin` below, which raises as it must.
        if (.not. (dec0 >= -90.0_real64 .and. dec0 <= 90.0_real64 .and. sep_deg >= 0.0_real64)) then
            call pf_to_str(dec0, t_dec, fmt='(es14.7)')
            call pf_to_str(sep_deg, t_sep, fmt='(es14.7)')
            error stop "pf_offset_radec: dec0 must be in [-90, 90] and sep_deg at least 0 (got dec0 = " // &
                trim(adjustl(t_dec)) // ", sep_deg = " // trim(adjustl(t_sep)) // ")"
        end if
        call skc_dec_sin_cos(dec0, sd0, cd0)
        a0 = ra0 * skc_deg2rad
        sa = sin(a0)
        ca = cos(a0)
        p = pa_deg * skc_deg2rad
        sp = sin(p)
        cp = cos(p)
        s = sep_deg * skc_deg2rad
        ss = sin(s)
        cs = cos(s)
        ! The centre and its local north and east, built from `ra0` even at a pole, where `north`
        ! still points along the meridian `ra0` names -- which is what makes astropy's pole
        ! convention come out of the same expression rather than a special case.
        c = [cd0 * ca, cd0 * sa, sd0]
        north = [-(sd0 * ca), -(sd0 * sa), cd0]
        east = [-sa, ca, 0.0_real64]
        v = cs * c + ss * (cp * north + sp * east)
        call skc_unit_radec(v, ra, dec)
    end procedure pf_offset_radec

    module procedure pf_apply_pm
        real(real64) :: u, w, s, f, sd, cd, a0, sa, ca, c(3), north(3), east(3), v(3)
        character(len=:), allocatable :: t_dec

        ! A NaN argument gives NaN results, handed back itself before any comparison could raise on
        ! it: a Gaia source without a five-parameter solution has null `pmra` and `pmdec`, so a NaN is
        ! this procedure's routine input rather than a caller's mistake.
        if (ra /= ra .or. dec /= dec .or. pm_ra /= pm_ra .or. pm_dec /= pm_dec .or. dt_years /= dt_years) then
            ra_out = ra
            if (dec /= dec) ra_out = dec
            if (pm_ra /= pm_ra) ra_out = pm_ra
            if (pm_dec /= pm_dec) ra_out = pm_dec
            if (dt_years /= dt_years) ra_out = dt_years
            dec_out = ra_out
            return
        end if
        ! The centre pf_offset_radec refuses, refused here in this procedure's own name, so the
        ! message names what the caller called.
        if (.not. (dec >= -90.0_real64 .and. dec <= 90.0_real64)) then
            call pf_to_str(dec, t_dec, fmt='(es14.7)')
            error stop "pf_apply_pm: dec must be in [-90, 90] (got " // trim(adjustl(t_dec)) // ")"
        end if
        ! `pm_ra` is already the rate along the local east, `cos(dec)` included, so the two rates ARE
        ! the step's components on the tangent plane and no position angle is formed. That is the
        ! same great-circle step `pf_offset_radec` takes, written in the components the caller
        ! brought: `cos(s)*c + (sin(s)/s)*(w*north + u*east)`, whose length is `s` and whose
        ! direction is the one the components name. Three special cases disappear with the angle --
        ! `atan2(0, 0)` for a motionless source, which is prohibited and which nagfor answers with a
        ! NaN and IEEE_INVALID; the half turn a negative interval needed, the signs now riding on
        ! the components; and the separation `pf_offset_radec` refuses below zero. 3.6e6
        ! milliarcseconds make a degree.
        u = pm_ra * dt_years / 3.6e6_real64 * skc_deg2rad
        w = pm_dec * dt_years / 3.6e6_real64 * skc_deg2rad
        s = hypot(u, w)
        if (s == 0.0_real64) then
            f = 1.0_real64        ! the limit of sin(s)/s: no motion gives the position back exactly
        else
            f = sin(s) / s
        end if
        ! The centre and its local north and east, built from `ra` even at a pole, where `north`
        ! still points along the meridian `ra` names -- `pf_offset_radec`'s pole convention, reached
        ! here by the same construction rather than by a second rule.
        call skc_dec_sin_cos(dec, sd, cd)
        a0 = ra * skc_deg2rad
        sa = sin(a0)
        ca = cos(a0)
        c = [cd * ca, cd * sa, sd]
        north = [-(sd * ca), -(sd * sa), cd]
        east = [-sa, ca, 0.0_real64]
        v = cos(s) * c + f * (w * north + u * east)
        call skc_unit_radec(v, ra_out, dec_out)
    end procedure pf_apply_pm

    module procedure pf_position_angle_deg
        real(real64) :: dl, sdl, cdl, sd1, cd1, sd2, cd2, y, x

        dl = ra2 - ra1
        if (dl /= dl .or. dec1 /= dec1 .or. dec2 /= dec2) then
            pa = dl
            if (dec1 /= dec1) pa = dec1
            if (dec2 /= dec2) pa = dec2
            return
        end if
        ! Folded in degrees before scaling, as `pf_angdist_deg` does, so a small difference that
        ! straddles `ra = 0` stays small.
        dl = dl - 360.0_real64 * anint(dl / 360.0_real64)
        ! A coincident pair has no position angle, and the arithmetic alone would give 0 or 180 by
        ! the sign of an FMA residue; two positions at one pole coincide whatever their right
        ! ascensions.
        if (dec1 == dec2 .and. (dl == 0.0_real64 .or. abs(dec1) == 90.0_real64)) then
            pa = 0.0_real64
            return
        end if
        call skc_dec_sin_cos(dec1, sd1, cd1)
        call skc_dec_sin_cos(dec2, sd2, cd2)
        dl = dl * skc_deg2rad
        sdl = sin(dl)
        cdl = cos(dl)
        y = sdl * cd2
        x = cd1 * sd2 - sd1 * cd2 * cdl
        if (y == 0.0_real64 .and. x == 0.0_real64) then
            pa = 0.0_real64
        else
            pa = atan2(y, x) * skc_rad2deg
            if (pa < 0.0_real64) pa = pa + 360.0_real64
            if (pa >= 360.0_real64) pa = 0.0_real64
        end if
    end procedure pf_position_angle_deg

    ! ---- Positions as vectors ----

    module procedure pf_radec2unit
        ! The NaN screen the private kernel does not carry -- its callers screen before they reach
        ! it -- so that this public pair behaves as `parquet_sphere`'s does. The NaN argument itself
        ! is handed back: composing one (`lon + lat`) would reach `Inf - Inf` on a mixed
        ! infinite/NaN input and raise the flag the screen exists to avoid.
        if (lon /= lon) then
            v = lon
            return
        end if
        if (lat /= lat) then
            v = lat
            return
        end if
        call skc_radec_unit(lon, lat, v)
    end procedure pf_radec2unit

    module procedure pf_unit2radec
        real(real64) :: scale, w(3)
        integer :: k

        do k = 1, 3
            if (v(k) /= v(k)) then
                lon = v(k)
                lat = v(k)
                return
            end if
        end do
        ! Scaled by the largest component BEFORE anything is squared, `pf_vec2radec`'s rule and
        ! what makes the two bit-identical: each quotient is then in `[-1, 1]`, so the square root
        ! below neither overflows nor loses a tiny vector. The private kernel does not do this --
        ! every rotation hands it a unit vector by construction -- and putting it there would cost
        ! a `max` and a division in the hot path for nothing.
        scale = max(abs(v(1)), abs(v(2)), abs(v(3)))
        if (scale <= 0.0_real64) then
            lon = 0.0_real64
            lat = 0.0_real64
            return
        end if
        if (scale > huge(scale)) then
            ! Dividing by an infinite scale would make `Inf/Inf` a NaN with `IEEE_INVALID`; the
            ! direction of a vector with infinite components is that of those components alone.
            do k = 1, 3
                if (abs(v(k)) > huge(scale)) then
                    w(k) = sign(1.0_real64, v(k))
                else
                    w(k) = 0.0_real64
                end if
            end do
        else
            w = v / scale
        end if
        call skc_unit_radec(w, lon, lat)
    end procedure pf_unit2radec

    ! ---- The tangent plane ----

    module procedure pf_radec2tan
        real(real64) :: p, sp, cp, denom, xi, eta, c(3), north(3), east(3), w(3)
        character(len=:), allocatable :: t_dec

        p = 0.0_real64
        if (present(pa_deg)) p = pa_deg
        ! A NaN argument gives NaN results, handed back itself before the ordered comparisons below
        ! could raise on it.
        if (ra /= ra .or. dec /= dec .or. ra0 /= ra0 .or. dec0 /= dec0 .or. p /= p) then
            x = ra
            if (dec /= dec) x = dec
            if (ra0 /= ra0) x = ra0
            if (dec0 /= dec0) x = dec0
            if (p /= p) x = p
            y = x
            return
        end if
        ! The centre `pf_offset_radec` refuses, refused here for the same reason: a tangent point
        ! beyond a pole mirrors the local north and east and gives a plausible wrong chart.
        if (.not. (dec0 >= -90.0_real64 .and. dec0 <= 90.0_real64)) then
            call pf_to_str(dec0, t_dec, fmt='(es14.7)')
            error stop "pf_radec2tan: dec0 must be in [-90, 90] (got " // trim(adjustl(t_dec)) // ")"
        end if
        call skc_tangent_frame(ra0, dec0, c, north, east)
        call skc_radec_unit(ra, dec, w)
        ! The gnomonic projection: the point where the ray through `w` meets the plane tangent at
        ! `c`. `denom` is `cos` of the separation, so it is the far hemisphere that has no image --
        ! at exactly 90 degrees the ray is parallel to the plane. The NaN is made, not computed: a
        ! division by a zero `denom` would raise, and this procedure is total in its flags.
        denom = c(1) * w(1) + c(2) * w(2) + c(3) * w(3)
        if (.not. (denom > 0.0_real64)) then
            x = ieee_value(0.0_real64, ieee_quiet_nan)
            y = x
            return
        end if
        xi = (east(1) * w(1) + east(2) * w(2) + east(3) * w(3)) / denom
        eta = (north(1) * w(1) + north(2) * w(2) + north(3) * w(3)) / denom
        ! The axes turned so `+y` lies along position angle `p`, and radians to degrees. Written
        ! out rather than through a rotation matrix: two products and a subtraction each.
        sp = sin(p * skc_deg2rad)
        cp = cos(p * skc_deg2rad)
        x = (xi * cp - eta * sp) * skc_rad2deg
        y = (xi * sp + eta * cp) * skc_rad2deg
    end procedure pf_radec2tan

    module procedure pf_tan2radec
        real(real64) :: p, sp, cp, xi, eta, c(3), north(3), east(3), v(3)
        character(len=:), allocatable :: t_dec

        p = 0.0_real64
        if (present(pa_deg)) p = pa_deg
        if (x /= x .or. y /= y .or. ra0 /= ra0 .or. dec0 /= dec0 .or. p /= p) then
            ra = x
            if (y /= y) ra = y
            if (ra0 /= ra0) ra = ra0
            if (dec0 /= dec0) ra = dec0
            if (p /= p) ra = p
            dec = ra
            return
        end if
        if (.not. (dec0 >= -90.0_real64 .and. dec0 <= 90.0_real64)) then
            call pf_to_str(dec0, t_dec, fmt='(es14.7)')
            error stop "pf_tan2radec: dec0 must be in [-90, 90] (got " // trim(adjustl(t_dec)) // ")"
        end if
        call skc_tangent_frame(ra0, dec0, c, north, east)
        ! The axes turned back, then degrees to radians.
        sp = sin(p * skc_deg2rad)
        cp = cos(p * skc_deg2rad)
        xi = (x * cp + y * sp) * skc_deg2rad
        eta = (-(x * sp) + y * cp) * skc_deg2rad
        ! The inverse is the DIRECTION of the point on the tangent plane, so there is no `asin` and
        ! no special case at the centre: `skc_unit_radec` takes a vector of any length.
        v = c + xi * east + eta * north
        call skc_unit_radec(v, ra, dec)
    end procedure pf_tan2radec

    !> The unit vector of a tangent point and its local north and east.
    !!
    !! Built from `ra0` even at a pole, where `north` still points along the meridian `ra0` names
    !! -- `pf_offset_radec`'s pole convention, reached by construction rather than by a rule.
    !! `pf_offset_radec` and `pf_apply_pm` build the same three vectors inline and keep doing so:
    !! they are the elemental hot path, and gfortran cannot inline across a module procedure under
    !! the `-fPIC` fpm passes. Keep the four in step.
    pure subroutine skc_tangent_frame(ra0, dec0, c, north, east)
        real(real64), intent(in) :: ra0 !! the tangent point's longitude, degrees; not a NaN.
        real(real64), intent(in) :: dec0 !! the tangent point's latitude, degrees, in `[-90, 90]`.
        real(real64), intent(out) :: c(3) !! the tangent point.
        real(real64), intent(out) :: north(3) !! the local north.
        real(real64), intent(out) :: east(3) !! the local east.
        real(real64) :: sd0, cd0, a0, sa, ca

        call skc_dec_sin_cos(dec0, sd0, cd0)
        a0 = ra0 * skc_deg2rad
        sa = sin(a0)
        ca = cos(a0)
        c = [cd0 * ca, cd0 * sa, sd0]
        north = [-(sd0 * ca), -(sd0 * sa), cd0]
        east = [-sa, ca, 0.0_real64]
    end subroutine skc_tangent_frame

end submodule parquet_skycoord_geom
