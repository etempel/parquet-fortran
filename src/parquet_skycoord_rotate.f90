!> `parquet_skycoord`'s rotations: the kernel every conversion shares, the ten named procedures,
!! `pf_sky_convert`, the selector tokens, `pf_zhel2zcmb`, and the RA/Dec helpers the geometry
!! submodule shares.
!!
!! **One kernel, one matrix per call.** `skc_rotate` is handed a compile-time matrix from the
!! module's table and does the whole conversion -- the input's unit vector, one matrix product, and
!! back -- so a named procedure is one line and `pf_sky_convert` is a dispatch onto them; the
!! rotation object's `%apply`, in `parquet_skycoord_object`, hands it the matrix `%init` took from
!! the same table. Nothing here computes a sine or cosine of a table angle at run time.
!!
!! **Two rules run through all of it**, the same two `parquet_sphere`'s RA/Dec layer follows. A
!! latitude of exactly +/-90 is the pole, whatever the longitude says (`skc_dec_sin_cos`):
!! `cos(90 * pi/180)` is `6.1e-17`, a representation limit no formulation removes, so without the
!! rule a pole would convert to a direction that depends on the longitude naming it. And a quiet
!! NaN argument is handed back before any comparison or transcendental can raise a flag on it --
!! the rule `pf_angdist_deg` states at length in `src/parquet_skycoord_geom.f90` -- with `x /= x`
!! rather than `ieee_is_nan`, since these are per-element procedures.
submodule (parquet_skycoord) parquet_skycoord_rotate
    use parquet_utils, only: pf_to_lower, pf_to_str
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    implicit none

    ! ---- pf_zhel2zcmb's constants ----
    !
    ! The module's only physical constants, kept beside the one procedure that uses them. The dipole
    ! is Planck 2018 results I (Aghanim et al. 2020, A&A 641, A1), its apex Galactic as every dipole
    ! is published; tools/generate_skycoord_reference.py --self-test reads the four literals back, so
    ! never edit one without the generator.

    !> The speed of light, km/s: the SI definition, in the unit a dipole's speed is published in.
    real(real64), parameter :: skc_c_kms = 299792.458_real64
    !> The CMB dipole apex's Galactic longitude, degrees (Planck 2018).
    real(real64), parameter :: skc_cmb_apex_lon = 264.021_real64
    !> The CMB dipole apex's Galactic latitude, degrees (Planck 2018).
    real(real64), parameter :: skc_cmb_apex_lat = 48.253_real64
    !> The Sun's speed toward the apex, km/s (Planck 2018).
    real(real64), parameter :: skc_cmb_apex_v = 369.82_real64
    !> The default apex as a Galactic unit vector, built at compile time.
    real(real64), parameter :: skc_cmb_apex_gal(3) = [ &
        cos(skc_cmb_apex_lat * skc_deg2rad) * cos(skc_cmb_apex_lon * skc_deg2rad), &
        cos(skc_cmb_apex_lat * skc_deg2rad) * sin(skc_cmb_apex_lon * skc_deg2rad), &
        sin(skc_cmb_apex_lat * skc_deg2rad)]

contains

    ! ---- Helpers shared with parquet_skycoord_geom (implemented first: nagfor wants a separate
    ! ---- module procedure above every call to it in the same submodule) ----

    module procedure skc_dec_sin_cos
        real(real64) :: d

        if (lat == 90.0_real64) then
            sl = 1.0_real64
            cl = 0.0_real64
        else if (lat == -90.0_real64) then
            sl = -1.0_real64
            cl = 0.0_real64
        else
            d = lat * skc_deg2rad
            sl = sin(d)
            cl = cos(d)
        end if
    end procedure skc_dec_sin_cos

    module procedure skc_radec_unit
        real(real64) :: sl, cl, a

        call skc_dec_sin_cos(lat, sl, cl)
        if (cl == 0.0_real64) then
            ! The pole exactly, with no `0 * cos(a)` to leave a signed zero behind.
            v(1) = 0.0_real64
            v(2) = 0.0_real64
            v(3) = sl
        else
            a = lon * skc_deg2rad
            v(1) = cl * cos(a)
            v(2) = cl * sin(a)
            v(3) = sl
        end if
    end procedure skc_radec_unit

    module procedure skc_unit_radec
        ! `atan2(0, 0)` is prohibited, and nagfor answers it with a NaN and `IEEE_INVALID`, so a pole's
        ! longitude is 0 by rule. The `lon >= 360` fold is not redundant: a longitude a hair below 0
        ! comes back as exactly 360 once 360 is added.
        if (v(1) == 0.0_real64 .and. v(2) == 0.0_real64) then
            lon = 0.0_real64
        else
            lon = atan2(v(2), v(1)) * skc_rad2deg
            if (lon < 0.0_real64) lon = lon + 360.0_real64
            if (lon >= 360.0_real64) lon = 0.0_real64
        end if
        ! Latitude from `atan2(z, hypot(x, y))`, never `asin(z)`, which loses half its digits near a
        ! pole: a pole converted from its own position comes back `8.5e-7` degrees short through
        ! `asin`. `hypot` is scaled against an underflow a unit vector can reach within about 1e-162
        ! radians of a pole; above the guard the plain square root is the same value to a rounding
        ! and costs less than half as much.
        if (abs(v(1)) >= skc_hypot_safe .or. abs(v(2)) >= skc_hypot_safe) then
            lat = atan2(v(3), sqrt(v(1) * v(1) + v(2) * v(2))) * skc_rad2deg
        else
            lat = atan2(v(3), hypot(v(1), v(2))) * skc_rad2deg
        end if
    end procedure skc_unit_radec

    ! ---- The kernel, and the selector test (shared with parquet_skycoord_object) ----

    module procedure skc_rotate
        real(real64) :: v(3), w(3)

        ! A NaN coordinate is handed back itself, in both outputs: composing one (`lon + lat`) would
        ! reach `Inf - Inf` on a mixed infinite/NaN input and raise the flag the screen exists to avoid.
        if (lon /= lon) then
            lon_out = lon
            lat_out = lon
            return
        end if
        if (lat /= lat) then
            lon_out = lat
            lat_out = lat
            return
        end if
        call skc_radec_unit(lon, lat, v)
        w(1) = m(1, 1) * v(1) + m(1, 2) * v(2) + m(1, 3) * v(3)
        w(2) = m(2, 1) * v(1) + m(2, 2) * v(2) + m(2, 3) * v(3)
        w(3) = m(3, 1) * v(1) + m(3, 2) * v(2) + m(3, 3) * v(3)
        call skc_unit_radec(w, lon_out, lat_out)
    end procedure skc_rotate

    module procedure skc_is_system
        ok = system == PF_COORD_ICRS .or. system == PF_COORD_GALACTIC .or. system == PF_COORD_ECLIPTIC .or. &
            system == PF_COORD_SUPERGALACTIC .or. system == PF_COORD_FK5
    end procedure skc_is_system

    ! ---- The named rotations ----

    module procedure pf_icrs2gal
        call skc_rotate(skc_m_icrs2gal, ra, dec, l, b)
    end procedure pf_icrs2gal

    module procedure pf_gal2icrs
        call skc_rotate(skc_m_gal2icrs, l, b, ra, dec)
    end procedure pf_gal2icrs

    module procedure pf_icrs2ecl
        call skc_rotate(skc_m_icrs2ecl, ra, dec, elon, elat)
    end procedure pf_icrs2ecl

    module procedure pf_ecl2icrs
        call skc_rotate(skc_m_ecl2icrs, elon, elat, ra, dec)
    end procedure pf_ecl2icrs

    module procedure pf_gal2sgal
        call skc_rotate(skc_m_gal2sgal, l, b, sgl, sgb)
    end procedure pf_gal2sgal

    module procedure pf_sgal2gal
        call skc_rotate(skc_m_sgal2gal, sgl, sgb, l, b)
    end procedure pf_sgal2gal

    module procedure pf_icrs2sgal
        call skc_rotate(skc_m_icrs2sgal, ra, dec, sgl, sgb)
    end procedure pf_icrs2sgal

    module procedure pf_sgal2icrs
        call skc_rotate(skc_m_sgal2icrs, sgl, sgb, ra, dec)
    end procedure pf_sgal2icrs

    module procedure pf_icrs2fk5
        call skc_rotate(skc_m_icrs2fk5, ra, dec, ra_fk5, dec_fk5)
    end procedure pf_icrs2fk5

    module procedure pf_fk52icrs
        call skc_rotate(skc_m_fk52icrs, ra_fk5, dec_fk5, ra, dec)
    end procedure pf_fk52icrs

    ! ---- The data-driven form ----

    module procedure pf_sky_convert
        character(len=:), allocatable :: tf, tt

        if (.not. (skc_is_system(from) .and. skc_is_system(to))) then
            call pf_to_str(from, tf)
            call pf_to_str(to, tt)
            error stop "pf_sky_convert: from and to must each be PF_COORD_ICRS (1), PF_COORD_GALACTIC (2), " // &
                "PF_COORD_ECLIPTIC (3), PF_COORD_SUPERGALACTIC (4) or PF_COORD_FK5 (5) (got from = " // tf // &
                ", to = " // tt // ")"
        end if
        if (from == to) then
            ! The identity is a copy, before any arithmetic: a longitude outside `[0, 360)` and a
            ! signed zero come back as given, which a trip through the kernel would change.
            lon_out = lon_in
            lat_out = lat_in
            return
        end if
        ! A pair with a named procedure CALLS it, so the two answer alike by construction rather than
        ! by keeping two bodies in step; the ten pairs without one take their own matrix.
        ! parquet_skycoord_object's `%init` chooses the same matrices: keep the two tables in step.
        select case (from)
        case (PF_COORD_ICRS)
            select case (to)
            case (PF_COORD_GALACTIC)
                call pf_icrs2gal(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_ECLIPTIC)
                call pf_icrs2ecl(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_SUPERGALACTIC)
                call pf_icrs2sgal(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_FK5)
                call pf_icrs2fk5(lon_in, lat_in, lon_out, lat_out)
            end select
        case (PF_COORD_GALACTIC)
            select case (to)
            case (PF_COORD_ICRS)
                call pf_gal2icrs(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_ECLIPTIC)
                call skc_rotate(skc_m_gal2ecl, lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_SUPERGALACTIC)
                call pf_gal2sgal(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_FK5)
                call skc_rotate(skc_m_gal2fk5, lon_in, lat_in, lon_out, lat_out)
            end select
        case (PF_COORD_ECLIPTIC)
            select case (to)
            case (PF_COORD_ICRS)
                call pf_ecl2icrs(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_GALACTIC)
                call skc_rotate(skc_m_ecl2gal, lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_SUPERGALACTIC)
                call skc_rotate(skc_m_ecl2sgal, lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_FK5)
                call skc_rotate(skc_m_ecl2fk5, lon_in, lat_in, lon_out, lat_out)
            end select
        case (PF_COORD_SUPERGALACTIC)
            select case (to)
            case (PF_COORD_ICRS)
                call pf_sgal2icrs(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_GALACTIC)
                call pf_sgal2gal(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_ECLIPTIC)
                call skc_rotate(skc_m_sgal2ecl, lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_FK5)
                call skc_rotate(skc_m_sgal2fk5, lon_in, lat_in, lon_out, lat_out)
            end select
        case (PF_COORD_FK5)
            select case (to)
            case (PF_COORD_ICRS)
                call pf_fk52icrs(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_GALACTIC)
                call skc_rotate(skc_m_fk52gal, lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_ECLIPTIC)
                call skc_rotate(skc_m_fk52ecl, lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_SUPERGALACTIC)
                call skc_rotate(skc_m_fk52sgal, lon_in, lat_in, lon_out, lat_out)
            end select
        end select
    end procedure pf_sky_convert

    ! ---- The CMB rest frame ----

    module procedure pf_zhel2zcmb
        real(real64) :: dd, ed, bad
        logical :: ok

        call skc_cmb_boost("pf_zhel2zcmb", lon, lat, z_hel, dd, ed, bad, ok, system, apex_lon, apex_lat, apex_v)
        if (.not. ok) then
            z_cmb = bad
            return
        end if
        ! `(1 + z_hel) / D - 1` written `z_hel + (1 + z_hel) (1 - D) / D`, so nothing near 1 is
        ! subtracted from 1 and a small redshift keeps its digits -- `ed` is that `1 - D`, formed
        ! without the cancellation the written-out difference would carry. Keeping `1 + z_hel` as a
        ! factor also keeps the one exact value the boost has: a `z_hel` of -1 is a zero ratio of
        ! wavelengths, which is zero in every frame, and here the factor is exactly zero.
        z_cmb = z_hel + (1.0_real64 + z_hel) * (ed / dd)
    end procedure pf_zhel2zcmb

    module procedure pf_zcmb2zhel
        real(real64) :: dd, ed, bad
        logical :: ok

        call skc_cmb_boost("pf_zcmb2zhel", lon, lat, z_cmb, dd, ed, bad, ok, system, apex_lon, apex_lat, apex_v)
        if (.not. ok) then
            z_hel = bad
            return
        end if
        ! `(1 + z_cmb) D - 1`, the same factor the other way, written `z_cmb - (1 + z_cmb)(1 - D)`
        ! for the same reason. The factor enters as `ed` alone, so `dd` is not read here.
        z_hel = z_cmb - (1.0_real64 + z_cmb) * ed
    end procedure pf_zcmb2zhel

    !> The Doppler factor `D = g (1 - beta cos(theta))` of a position against the dipole, and
    !! `1 - D` formed without cancellation: what both redshift boosts are made of.
    !!
    !! **`theta` is the angle between the apex and the position as the caller gives it**, which is
    !! the OBSERVED direction, and `D` is the factor exact for that direction:
    !! `1 + z_cmb = (1 + z_hel) / D`. `1 - D` is `1 - g + g beta cth`, formed as
    !! `g beta cth - g**2 beta**2 / (g + 1)` so that nothing near 1 is subtracted from 1 and a
    !! small speed keeps its digits. `ok` is false where there is nothing to compute -- a NaN
    !! argument, an infinite redshift, or a speed at or beyond light's -- and `bad` is then the
    !! value the caller returns: the NaN argument itself, never one composed from several, since
    !! `Inf + (-Inf)` on a mixed infinite/NaN input would raise the flag the screen exists to
    !! avoid. **Stops the program** on a `system` that is not one of the five, naming the procedure
    !! the caller called.
    pure subroutine skc_cmb_boost(who, lon, lat, z, dd, ed, bad, ok, system, apex_lon, apex_lat, apex_v)
        character(len=*), intent(in) :: who !! the procedure the caller called, for the message.
        real(real64), intent(in) :: lon !! the position's longitude in `system`, degrees; any value.
        real(real64), intent(in) :: lat !! the position's latitude in `system`, degrees.
        real(real64), intent(in) :: z !! the caller's redshift: screened here, never part of the factor.
        real(real64), intent(out) :: dd !! the Doppler factor `D`.
        real(real64), intent(out) :: ed !! `1 - D`.
        real(real64), intent(out) :: bad !! what the caller returns when `ok` is false; 0 otherwise.
        logical, intent(out) :: ok !! whether `dd` and `ed` were computed.
        integer, intent(in), optional :: system !! the system of `(lon, lat)`, a `PF_COORD_*` selector; default ICRS.
        real(real64), intent(in), optional :: apex_lon !! the apex's Galactic longitude, degrees.
        real(real64), intent(in), optional :: apex_lat !! the apex's Galactic latitude, degrees.
        real(real64), intent(in), optional :: apex_v !! the Sun's speed toward the apex, km/s.
        real(real64) :: v(3), w(3), a(3), alon, alat, av, beta, g, cth
        integer :: sys
        character(len=:), allocatable :: t

        ok = .false.
        bad = 0.0_real64
        dd = 1.0_real64
        ed = 0.0_real64
        sys = PF_COORD_ICRS
        if (present(system)) sys = system
        if (.not. skc_is_system(sys)) then
            call pf_to_str(sys, t)
            error stop who // ": system must be PF_COORD_ICRS (1), PF_COORD_GALACTIC (2), PF_COORD_ECLIPTIC (3), " // &
                "PF_COORD_SUPERGALACTIC (4) or PF_COORD_FK5 (5) (got " // t // ")"
        end if
        alon = skc_cmb_apex_lon
        if (present(apex_lon)) alon = apex_lon
        alat = skc_cmb_apex_lat
        if (present(apex_lat)) alat = apex_lat
        av = skc_cmb_apex_v
        if (present(apex_v)) av = apex_v
        ! A NaN is handed back itself, before any comparison or transcendental can raise a flag on it.
        if (lon /= lon) then
            bad = lon
            return
        end if
        if (lat /= lat) then
            bad = lat
            return
        end if
        if (z /= z) then
            bad = z
            return
        end if
        if (alon /= alon) then
            bad = alon
            return
        end if
        if (alat /= alat) then
            bad = alat
            return
        end if
        if (av /= av) then
            bad = av
            return
        end if
        ! The boost is a finite positive factor, so an infinite redshift is itself; and a speed at or
        ! beyond light's has no Lorentz factor. The speeds are compared, not their quotient with 1:
        ! ifx's default model divides by a constant as a multiplication by its rounded reciprocal,
        ! which put `c / c` one ulp below 1.
        if (abs(z) > huge(z)) then
            bad = z
            return
        end if
        if (abs(av) >= skc_c_kms) then
            bad = ieee_value(0.0_real64, ieee_quiet_nan)
            return
        end if
        beta = av / skc_c_kms
        ! The cosine of the angle to the apex: the position's unit vector taken into Galactic and
        ! dotted with the apex's.
        call skc_radec_unit(lon, lat, v)
        select case (sys)
        case (PF_COORD_ICRS)
            call skc_apply(skc_m_icrs2gal, v, w)
        case (PF_COORD_ECLIPTIC)
            call skc_apply(skc_m_ecl2gal, v, w)
        case (PF_COORD_SUPERGALACTIC)
            call skc_apply(skc_m_sgal2gal, v, w)
        case (PF_COORD_FK5)
            call skc_apply(skc_m_fk52gal, v, w)
        case default
            w = v
        end select
        if (present(apex_lon) .or. present(apex_lat)) then
            call skc_radec_unit(alon, alat, a)
        else
            a = skc_cmb_apex_gal
        end if
        cth = w(1) * a(1) + w(2) * a(2) + w(3) * a(3)
        ! `g` from the speeds, whose `(c - |v|)(c + |v|)` is positive for every speed below light's,
        ! where `1 - beta**2` of a quotient rounded up could reach 0. `D` is bounded away from zero
        ! for every one of them: `g (1 - beta) = sqrt((1 - beta) / (1 + beta))`.
        g = skc_c_kms / sqrt((skc_c_kms - abs(av)) * (skc_c_kms + abs(av)))
        dd = g * (1.0_real64 - beta * cth)
        ed = g * beta * cth - g * g * beta * beta / (g + 1.0_real64)
        ok = .true.
    end subroutine skc_cmb_boost

    !> `w = m v`, one of the module's compile-time matrices applied to a unit vector.
    pure subroutine skc_apply(m, v, w)
        real(real64), intent(in) :: m(3, 3) !! the rotation.
        real(real64), intent(in) :: v(3) !! the vector.
        real(real64), intent(out) :: w(3) !! the rotated vector.

        w(1) = m(1, 1) * v(1) + m(1, 2) * v(2) + m(1, 3) * v(3)
        w(2) = m(2, 1) * v(1) + m(2, 2) * v(2) + m(2, 3) * v(3)
        w(3) = m(3, 1) * v(1) + m(3, 2) * v(2) + m(3, 3) * v(3)
    end subroutine skc_apply

    ! ---- The selector tokens ----

    module procedure pf_coord_system_name
        character(len=:), allocatable :: t

        select case (system)
        case (PF_COORD_UNKNOWN)
            name = "unknown"
        case (PF_COORD_ICRS)
            name = "icrs"
        case (PF_COORD_GALACTIC)
            name = "galactic"
        case (PF_COORD_ECLIPTIC)
            name = "ecliptic"
        case (PF_COORD_SUPERGALACTIC)
            name = "supergalactic"
        case (PF_COORD_FK5)
            name = "fk5"
        case default
            call pf_to_str(system, t)
            error stop "pf_coord_system_name: system must be a PF_COORD_* selector (got " // t // ")"
        end select
    end procedure pf_coord_system_name

    module procedure pf_coord_system_from_name
        character(len=:), allocatable :: token

        call pf_to_lower(trim(adjustl(name)), token)
        select case (token)
        case ("icrs")
            system = PF_COORD_ICRS
        case ("galactic")
            system = PF_COORD_GALACTIC
        case ("ecliptic", "barycentricmeanecliptic")
            system = PF_COORD_ECLIPTIC
        case ("supergalactic")
            system = PF_COORD_SUPERGALACTIC
        case ("fk5")
            system = PF_COORD_FK5
        case default
            system = PF_COORD_UNKNOWN
        end select
    end procedure pf_coord_system_from_name

end submodule parquet_skycoord_rotate
