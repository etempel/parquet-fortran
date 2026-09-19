!> `parquet_skycoord`'s rotation object: `pf_sky_rotation`'s `%init`, `%apply` and `%is_init`.
!!
!! `%init` does once what `pf_sky_convert` does per position -- read the two selectors and pick the
!! pair's matrix -- and `%apply` hands that matrix to the kernel every conversion shares,
!! `skc_rotate` in `parquet_skycoord_rotate`, so the object and the free procedures rotate by one
!! routine and the same compile-time matrices. The choice of matrix is written out here a second
!! time, beside `pf_sky_convert`'s: keep the two tables in step
!! (`test_rotation_object_matches_the_free_procedure` compares every ordered pair).
submodule (parquet_skycoord) parquet_skycoord_object
    use parquet_utils, only: pf_to_str
    implicit none

contains

    module procedure skc_rotation_init
        character(len=:), allocatable :: tf, tt

        if (.not. (skc_is_system(from) .and. skc_is_system(to))) then
            call pf_to_str(from, tf)
            call pf_to_str(to, tt)
            error stop "pf_sky_rotation%init: from and to must each be PF_COORD_ICRS (1), PF_COORD_GALACTIC (2), " // &
                "PF_COORD_ECLIPTIC (3), PF_COORD_SUPERGALACTIC (4) or PF_COORD_FK5 (5) (got from = " // tf // &
                ", to = " // tt // ")"
        end if
        ! Every component on every path: `this` is `intent(inout)`, and a previous rotation must not
        ! survive into this one.
        this%set = .true.
        this%identity = from == to
        this%m = skc_m_identity
        ! The matrix `pf_sky_convert` rotates by for the pair: the named procedure's where there is
        ! one, the pair's own where there is not.
        select case (from)
        case (PF_COORD_ICRS)
            select case (to)
            case (PF_COORD_GALACTIC)
                this%m = skc_m_icrs2gal
            case (PF_COORD_ECLIPTIC)
                this%m = skc_m_icrs2ecl
            case (PF_COORD_SUPERGALACTIC)
                this%m = skc_m_icrs2sgal
            case (PF_COORD_FK5)
                this%m = skc_m_icrs2fk5
            end select
        case (PF_COORD_GALACTIC)
            select case (to)
            case (PF_COORD_ICRS)
                this%m = skc_m_gal2icrs
            case (PF_COORD_ECLIPTIC)
                this%m = skc_m_gal2ecl
            case (PF_COORD_SUPERGALACTIC)
                this%m = skc_m_gal2sgal
            case (PF_COORD_FK5)
                this%m = skc_m_gal2fk5
            end select
        case (PF_COORD_ECLIPTIC)
            select case (to)
            case (PF_COORD_ICRS)
                this%m = skc_m_ecl2icrs
            case (PF_COORD_GALACTIC)
                this%m = skc_m_ecl2gal
            case (PF_COORD_SUPERGALACTIC)
                this%m = skc_m_ecl2sgal
            case (PF_COORD_FK5)
                this%m = skc_m_ecl2fk5
            end select
        case (PF_COORD_SUPERGALACTIC)
            select case (to)
            case (PF_COORD_ICRS)
                this%m = skc_m_sgal2icrs
            case (PF_COORD_GALACTIC)
                this%m = skc_m_sgal2gal
            case (PF_COORD_ECLIPTIC)
                this%m = skc_m_sgal2ecl
            case (PF_COORD_FK5)
                this%m = skc_m_sgal2fk5
            end select
        case (PF_COORD_FK5)
            select case (to)
            case (PF_COORD_ICRS)
                this%m = skc_m_fk52icrs
            case (PF_COORD_GALACTIC)
                this%m = skc_m_fk52gal
            case (PF_COORD_ECLIPTIC)
                this%m = skc_m_fk52ecl
            case (PF_COORD_SUPERGALACTIC)
                this%m = skc_m_fk52sgal
            end select
        end select
    end procedure skc_rotation_init

    module procedure skc_rotation_apply
        ! A caller mistake with no reading, not a coordinate: the totality rule does not reach it.
        if (.not. this%set) error stop "pf_sky_rotation%apply: %init has not run"
        if (this%identity) then
            ! A copy, before any arithmetic, as `pf_sky_convert` answers the same pair: a longitude
            ! outside `[0, 360)` and a signed zero come back as given.
            lon_out = lon_in
            lat_out = lat_in
            return
        end if
        call skc_rotate(this%m, lon_in, lat_in, lon_out, lat_out)
    end procedure skc_rotation_apply

    module procedure skc_rotation_is_init
        yes = this%set
    end procedure skc_rotation_is_init

end submodule parquet_skycoord_object
