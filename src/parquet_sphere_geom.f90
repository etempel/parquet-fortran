!> `parquet_sphere`'s deterministic geometry -- the RA/Dec conversions and the Fibonacci grid --
!! and the helpers the three submodules share.
!!
!! **Two rules run through all of it.** A declination of exactly +/-90 is the pole, whatever the
!! right ascension says (`sky_dec_sin_cos`): `cos(90 * pi/180)` is `6.1e-17`, a representation limit
!! no formulation removes, and `parquet_random`'s own RA/Dec twin makes the same rule, so the two
!! agree at a pole instead of differing by that residue. And a quiet NaN argument of a TOTAL
!! procedure is handed back before any comparison or transcendental can raise a flag on it -- the
!! rule `pf_angdist_deg` states at length in `src/parquet_skycoord_geom.f90` -- with `x /= x` rather
!! than `ieee_is_nan`, since these are per-element procedures.
submodule (parquet_sphere) parquet_sphere_geom
    implicit none

contains

    ! ---- Helpers shared by the three submodules (implemented first: nagfor wants a separate module
    ! ---- procedure above every call to it in the same submodule) ----

    module procedure sky_real_text
        if (x /= x) then
            t = "NaN"
        else if (x > huge(x)) then
            t = "Infinity"
        else if (x < -huge(x)) then
            t = "-Infinity"
        else if (abs(x) >= 1.0e100_real64 .or. (abs(x) > 0.0_real64 .and. abs(x) < 1.0e-99_real64)) then
            write (t, '(es15.7e3)') x
            t = adjustl(t)
        else
            write (t, '(es14.7)') x
            t = adjustl(t)
        end if
    end procedure sky_real_text

    module procedure sky_int_text
        write (t, '(i0)') n
        t = adjustl(t)
    end procedure sky_int_text

    module procedure sky_frame_sign
        sgn = 1.0_real64
        if (.not. present(frame)) return
        if (frame == PF_HP_DEC_SOUTH) then
            sgn = -1.0_real64
        else if (frame /= PF_HP_DEC_NORTH) then
            error stop who // ": frame must be PF_HP_DEC_NORTH (0) or PF_HP_DEC_SOUTH (1), got " // &
                trim(sky_int_text(int(frame, int64)))
        end if
    end procedure sky_frame_sign

    module procedure sky_dec_sin_cos
        real(real64) :: d

        if (dec == 90.0_real64) then
            sd = 1.0_real64
            cd = 0.0_real64
        else if (dec == -90.0_real64) then
            sd = -1.0_real64
            cd = 0.0_real64
        else
            d = dec * PF_RAD_PER_DEG
            sd = sin(d)
            cd = cos(d)
        end if
    end procedure sky_dec_sin_cos

    module procedure sky_radec_unit
        real(real64) :: sd, cd, a

        call sky_dec_sin_cos(dec, sd, cd)
        if (cd == 0.0_real64) then
            ! The pole exactly, with no `0 * cos(a)` to leave a signed zero behind.
            v = [0.0_real64, 0.0_real64, sd]
        else
            ! `parquet_random`'s private twin (`sph_centre_radec`) forms these same products in this
            ! same order, and `test_radec_agrees_with_stage_one` holds the two together.
            a = ra * PF_RAD_PER_DEG
            v = [cd * cos(a), cd * sin(a), sd]
        end if
    end procedure sky_radec_unit

    module procedure sky_unit_radec
        ! `atan2(0, 0)` is prohibited, and nagfor answers it with a NaN and `IEEE_INVALID`, so a pole's
        ! right ascension is 0 by rule. The `ra >= 360` fold is not redundant: a right ascension a
        ! hair below 0 comes back as exactly 360 once 360 is added.
        if (v(1) == 0.0_real64 .and. v(2) == 0.0_real64) then
            ra = 0.0_real64
        else
            ra = atan2(v(2), v(1)) * PF_DEG_PER_RAD
            if (ra < 0.0_real64) ra = ra + 360.0_real64
            if (ra >= 360.0_real64) ra = 0.0_real64
        end if
        ! `hypot` is scaled against an overflow that a unit vector cannot reach and an underflow it
        ! can: within about 1e-162 radians of a pole the squares go subnormal. Above the guard the
        ! plain square root is the same value to a rounding and costs less than half as much; below
        ! it `hypot` still answers, so no direction gains an `IEEE_UNDERFLOW` it did not raise
        ! before. `v` is a unit vector by contract, so neither component can be NaN here.
        if (abs(v(1)) >= sky_hypot_safe .or. abs(v(2)) >= sky_hypot_safe) then
            dec = atan2(v(3), sqrt(v(1) * v(1) + v(2) * v(2))) * PF_DEG_PER_RAD
        else
            dec = atan2(v(3), hypot(v(1), v(2))) * PF_DEG_PER_RAD
        end if
    end procedure sky_unit_radec

    module procedure sky_draw
        d = 1_int64
        if (present(draw)) d = draw
        if (d < 1_int64) d = 1_int64
    end procedure sky_draw

    module procedure sky_take_block
        integer(int64) :: p, r

        ! `pf_random_stream`'s cursor, block alignment and advance are private to `parquet_random`,
        ! so they are reproduced here through its public `%position`, `%rewind` and `%address`: align
        ! to the next multiple of four words, take that block, and leave the cursor after it --
        ! exactly what its own sphere producers do, which `test_polygon_tiers_agree` holds us to.
        !
        ! The bound is checked once, before any arithmetic: aligning adds at most 3 words and the new
        ! 1-based position is 5 past the aligned word, so nothing below can pass `huge(int64)`. It is
        ! up to four words stricter than the stream's own guard, at the end of 2**63 words.
        p = rng%position() - 1_int64
        if (p > huge(p) - 8_int64) then
            error stop who // ": the stream is exhausted -- it addresses at most 2**63 words"
        end if
        r = modulo(p, 4_int64)
        if (r /= 0_int64) p = p + (4_int64 - r)
        d = p / 4_int64 + 1_int64
        call rng%rewind(p + 5_int64)
        call rng%address(seed, stream)
    end procedure sky_take_block

    module procedure sky_fill_start
        d = sky_draw(draw)
        ! Compared as `d > huge - (n - 1)`, which cannot overflow for any `n >= 1`, rather than by
        ! forming the last draw, which can.
        if (d > huge(d) - (n - 1_int64)) then
            error stop who // ": draw + n - 1 must not exceed huge(int64) (got " // trim(sky_int_text(d)) // &
                " + " // trim(sky_int_text(n)) // " - 1)"
        end if
    end procedure sky_fill_start

    ! ---- The conversions ----

    module procedure pf_radec2vec
        real(real64) :: sgn

        sgn = sky_frame_sign("pf_radec2vec", frame)
        ! Hand back the NaN argument itself: composing one (`ra + dec`) would reach `Inf - Inf` on a
        ! mixed infinite/NaN input and raise the flag this screen exists to avoid.
        if (ra /= ra) then
            vec = ra
            return
        end if
        if (dec /= dec) then
            vec = dec
            return
        end if
        vec = sky_radec_unit(ra, dec)
        if (sgn < 0.0_real64) vec(3) = -vec(3)
    end procedure pf_radec2vec

    module procedure pf_vec2radec
        real(real64) :: sgn, scale, w(3)
        integer :: k

        sgn = sky_frame_sign("pf_vec2radec", frame)
        do k = 1, 3
            if (vec(k) /= vec(k)) then
                ra = vec(k)
                dec = vec(k)
                return
            end if
        end do
        ! Scale by the largest component BEFORE anything is squared, `hpx_vec_unit`'s rule: each
        ! quotient is then in [-1, 1], so `hypot` below neither overflows nor loses a tiny vector.
        scale = max(abs(vec(1)), abs(vec(2)), abs(vec(3)))
        if (scale <= 0.0_real64) then
            ra = 0.0_real64
            dec = 0.0_real64
            return
        end if
        if (scale > huge(scale)) then
            ! Dividing by an infinite scale would make `Inf/Inf` a NaN with `IEEE_INVALID`; the
            ! direction of a vector with infinite components is that of those components alone.
            do k = 1, 3
                if (abs(vec(k)) > huge(scale)) then
                    w(k) = sign(1.0_real64, vec(k))
                else
                    w(k) = 0.0_real64
                end if
            end do
        else
            w = vec / scale
        end if
        call sky_unit_radec(w, ra, dec)
        if (sgn < 0.0_real64) dec = -dec
    end procedure pf_vec2radec

    ! ---- The Fibonacci grid ----

    !> Point `t - 1/2` of an `rn`-point Fibonacci grid: its right ascension in degrees, the sine of its
    !! latitude and the cosine.
    !!
    !! `z = 1 - 2*t/n` is formed as `(n - 2*t)/n`, whose numerator is an exact integer, and the cosine
    !! as `2*sqrt(t*(n - t))/n` rather than `sqrt(1 - z**2)`, which would cancel near both poles.
    pure subroutine sky_fibonacci_point(t, rn, ra, z, s)
        real(real64), intent(in) :: t !! `k + 1/2` for point `k`, counting from 0.
        real(real64), intent(in) :: rn !! the number of points.
        real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`.
        real(real64), intent(out) :: z !! the sine of the latitude.
        real(real64), intent(out) :: s !! the cosine of the latitude.

        ra = pf_wrap_deg(t * sky_golden_step_deg)
        z = (rn - (t + t)) / rn
        s = 2.0_real64 * sqrt(t * (rn - t)) / rn
    end subroutine sky_fibonacci_point

    module procedure sky_fibonacci_i64
        real(real64) :: sgn, rn, t, ra, z, s, lon
        integer(int64) :: k

        sgn = sky_frame_sign("pf_fibonacci_grid", frame)
        if (n < 1_int64 .or. size(vec, 1, kind=int64) /= 3_int64 .or. size(vec, 2, kind=int64) /= n) then
            error stop "pf_fibonacci_grid: n must be at least 1 and vec shaped (3, n) (got n = " // &
                trim(sky_int_text(n)) // ", " // trim(sky_int_text(size(vec, 1, kind=int64))) // " x " // &
                trim(sky_int_text(size(vec, 2, kind=int64))) // ")"
        end if
        rn = real(n, real64)
        do k = 1_int64, n
            t = real(k, real64) - 0.5_real64
            call sky_fibonacci_point(t, rn, ra, z, s)
            lon = ra * PF_RAD_PER_DEG
            vec(1, k) = s * cos(lon)
            vec(2, k) = s * sin(lon)
            vec(3, k) = sgn * z
        end do
    end procedure sky_fibonacci_i64

    module procedure sky_fibonacci_i32
        call sky_fibonacci_i64(int(n, int64), vec, frame)
    end procedure sky_fibonacci_i32

    module procedure sky_fibonacci_radec_i64
        real(real64) :: rn, t, z, s
        integer(int64) :: k

        if (n < 1_int64 .or. size(ra, kind=int64) /= n .or. size(dec, kind=int64) /= n) then
            error stop "pf_fibonacci_grid_radec: n must be at least 1 and ra and dec sized n (got n = " // &
                trim(sky_int_text(n)) // ", " // trim(sky_int_text(size(ra, kind=int64))) // " and " // &
                trim(sky_int_text(size(dec, kind=int64))) // ")"
        end if
        rn = real(n, real64)
        do k = 1_int64, n
            t = real(k, real64) - 0.5_real64
            call sky_fibonacci_point(t, rn, ra(k), z, s)
            dec(k) = atan2(z, s) * PF_DEG_PER_RAD
        end do
    end procedure sky_fibonacci_radec_i64

    module procedure sky_fibonacci_radec_i32
        call sky_fibonacci_radec_i64(int(n, int64), ra, dec)
    end procedure sky_fibonacci_radec_i32

end submodule parquet_sphere_geom
