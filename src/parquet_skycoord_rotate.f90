!> `parquet_skycoord`'s rotations: the kernel every conversion shares, the eight named procedures,
!! `pf_sky_convert`, the selector tokens, and the RA/Dec helpers the geometry submodule shares.
!!
!! **One kernel, one matrix per call.** `skc_rotate` is handed a compile-time matrix from the
!! module's table and does the whole conversion -- the input's unit vector, one matrix product, and
!! back -- so a named procedure is one line and `pf_sky_convert` is a dispatch onto them. Nothing
!! here computes a sine or cosine of a table angle at run time.
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
    implicit none

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

    ! ---- The kernel ----

    !> Rotates one position by `m`: its unit vector, one matrix product, and back.
    !!
    !! A NaN coordinate is handed back itself, in both outputs, before anything touches it: composing
    !! one (`lon + lat`) would reach `Inf - Inf` on a mixed infinite/NaN input and raise the flag the
    !! screen exists to avoid.
    pure subroutine skc_rotate(m, lon, lat, lon_out, lat_out)
        real(real64), intent(in) :: m(3, 3) !! the rotation, one of the module's compile-time matrices.
        real(real64), intent(in) :: lon !! longitude, degrees; any value.
        real(real64), intent(in) :: lat !! latitude, degrees.
        real(real64), intent(out) :: lon_out !! the rotated longitude, degrees, in `[0, 360)`.
        real(real64), intent(out) :: lat_out !! the rotated latitude, degrees, in `[-90, 90]`.
        real(real64) :: v(3), w(3)

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
    end subroutine skc_rotate

    !> Whether `system` is one of the four coordinate systems (`PF_COORD_UNKNOWN` is not).
    pure function skc_is_system(system) result(ok)
        integer, intent(in) :: system !! the caller's selector.
        logical :: ok !! true for `PF_COORD_ICRS` through `PF_COORD_SUPERGALACTIC`.

        ok = system == PF_COORD_ICRS .or. system == PF_COORD_GALACTIC .or. system == PF_COORD_ECLIPTIC .or. &
            system == PF_COORD_SUPERGALACTIC
    end function skc_is_system

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

    ! ---- The data-driven form ----

    module procedure pf_sky_convert
        character(len=:), allocatable :: tf, tt

        if (.not. (skc_is_system(from) .and. skc_is_system(to))) then
            call pf_to_str(from, tf)
            call pf_to_str(to, tt)
            error stop "pf_sky_convert: from and to must each be PF_COORD_ICRS (1), PF_COORD_GALACTIC (2), " // &
                "PF_COORD_ECLIPTIC (3) or PF_COORD_SUPERGALACTIC (4) (got from = " // tf // ", to = " // tt // ")"
        end if
        if (from == to) then
            ! The identity is a copy, before any arithmetic: a longitude outside `[0, 360)` and a
            ! signed zero come back as given, which a trip through the kernel would change.
            lon_out = lon_in
            lat_out = lat_in
            return
        end if
        ! A pair with a named procedure CALLS it, so the two answer alike by construction rather than
        ! by keeping two bodies in step; the four pairs without one take their own matrix.
        select case (from)
        case (PF_COORD_ICRS)
            select case (to)
            case (PF_COORD_GALACTIC)
                call pf_icrs2gal(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_ECLIPTIC)
                call pf_icrs2ecl(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_SUPERGALACTIC)
                call pf_icrs2sgal(lon_in, lat_in, lon_out, lat_out)
            end select
        case (PF_COORD_GALACTIC)
            select case (to)
            case (PF_COORD_ICRS)
                call pf_gal2icrs(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_ECLIPTIC)
                call skc_rotate(skc_m_gal2ecl, lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_SUPERGALACTIC)
                call pf_gal2sgal(lon_in, lat_in, lon_out, lat_out)
            end select
        case (PF_COORD_ECLIPTIC)
            select case (to)
            case (PF_COORD_ICRS)
                call pf_ecl2icrs(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_GALACTIC)
                call skc_rotate(skc_m_ecl2gal, lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_SUPERGALACTIC)
                call skc_rotate(skc_m_ecl2sgal, lon_in, lat_in, lon_out, lat_out)
            end select
        case (PF_COORD_SUPERGALACTIC)
            select case (to)
            case (PF_COORD_ICRS)
                call pf_sgal2icrs(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_GALACTIC)
                call pf_sgal2gal(lon_in, lat_in, lon_out, lat_out)
            case (PF_COORD_ECLIPTIC)
                call skc_rotate(skc_m_sgal2ecl, lon_in, lat_in, lon_out, lat_out)
            end select
        end select
    end procedure pf_sky_convert

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
        case default
            system = PF_COORD_UNKNOWN
        end select
    end procedure pf_coord_system_from_name

end submodule parquet_skycoord_rotate
