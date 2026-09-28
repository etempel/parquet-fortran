!> Celestial coordinate systems -- ICRS, Galactic, ecliptic, supergalactic and FK5 J2000 -- the
!! library's frame-free RA/Dec geometry and proper motion, sky angles as sexagesimal text, and the
!! CMB rest frame of a redshift.
!!
!! Four families, all on the sky, all in degrees and all over `real64`:
!!
!! * **Rotations between coordinate systems.** Ten named procedures -- `pf_icrs2gal`,
!!   `pf_gal2icrs`, `pf_icrs2ecl`, `pf_ecl2icrs`, `pf_gal2sgal`, `pf_sgal2gal`, `pf_icrs2sgal`,
!!   `pf_sgal2icrs`, `pf_icrs2fk5`, `pf_fk52icrs` -- and `pf_sky_convert` for systems named at run
!!   time by `PF_COORD_*` selectors, with `pf_coord_system_name` and `pf_coord_system_from_name`
!!   between a selector and its token, and `pf_sky_rotation`, a rotation between two such systems
!!   prepared once and applied to any number of positions.
!! * **Frame-free RA/Dec geometry**: `pf_angdist_deg`, the separation of two positions;
!!   `pf_offset_radec`, the position a separation away at a position angle;
!!   `pf_position_angle_deg`, its inverse; `pf_apply_pm`, a position moved by its proper motion;
!!   `pf_radec2unit` and `pf_unit2radec` between a position and its unit vector; and
!!   `pf_radec2tan` and `pf_tan2radec`, the tangent-plane (gnomonic) projection about a field
!!   centre and its inverse.
!! * **Sexagesimal angles and text**: `pf_deg2hms`, `pf_deg2dms`, `pf_hms2deg` and `pf_dms2deg`
!!   split an angle into its fields and join them; `pf_ra2str`, `pf_dec2str` and `pf_radec2str`
!!   write positions as text, and `pf_str2ra`, `pf_str2dec` and `pf_str2radec` read it back.
!! * **The CMB rest frame**: `pf_zhel2zcmb`, a heliocentric redshift boosted into the rest frame of
!!   the cosmic microwave background, with Planck 2018's dipole by default, and `pf_zcmb2zhel`,
!!   the same boost the other way.
!!
!! Every procedure is `pure`. Every conversion and every text reader is `elemental`, so one call
!! converts whole columns; the three text writers are not, their text being an allocatable argument.
!!
!! **Every rotation is built from the three angles that define it**, `Rz(180 - lon0)
!! Ry(90 - pole_lat) Rz(pole_lon)`: the target system's north pole in the system it is built from,
!! and the target longitude of that system's north pole. So each matrix is orthonormal by
!! construction and its inverse is its transpose, never a second matrix. Every matrix, every
!! transpose and every product of two is a compile-time `parameter`; no procedure builds one per
!! call. The Galactic, ecliptic and FK5 rows are referred to ICRS and so carry the ICRS frame bias
!! inside them -- FK5 J2000 is that bias and nothing else -- which is what makes the answers agree
!! with astropy's `SkyCoord` rather than miss it by about 20 milliarcseconds; the supergalactic
!! system is defined in Galactic coordinates and is built there. `tools/generate_skycoord_reference.py`
!! derives the table at 60 digits, and its `--self-test` holds every literal below to the double
!! nearest the derived value.
!!
!! **Total, not validating.** A NaN argument gives NaN results and raises no IEEE flag -- screened
!! with `x /= x` before any comparison or transcendental, since these are per-element procedures,
!! and handed back itself rather than composed -- an infinite one raises `IEEE_INVALID`, as the
!! sine of an infinite angle must, and a latitude outside `[-90, 90]` is read as the direction it
!! names. A latitude of exactly +/-90 is the pole whatever the longitude says, and a pole's
!! longitude is reported as 0. What aborts is a caller mistake with no sensible reading: a
!! selector that is not one, a `pf_sky_rotation` applied before `%init`, a text writer's `sep` or
!! `precision` outside its set, and `pf_offset_radec`'s centre beyond a pole or negative
!! separation, the centre `pf_apply_pm` refuses too. Text is user data, not a caller's mistake: a
!! reader that cannot read it says so through its `ok` flag, and that is where the readers differ
!! from everything else here -- they are the one validating layer, refusing a declination past a
!! pole and an hour past a turn as well as a field of 60.
!!
!! **An output argument must not be an input argument.** `call pf_icrs2gal(ra, dec, ra, dec)` to
!! convert a column in place is undefined behaviour: an `intent(out)` dummy aliased with an
!! `intent(in)` one (F2018 15.5.2.13). It happens to answer correctly today and is not promised to.
!!
!! **This is not the HEALPix declination frame.** `PF_HP_DEC_NORTH` and `PF_HP_DEC_SOUTH` in
!! `parquet_healpix` name a sign convention for the third component of a unit vector; a coordinate
!! system is a rotation of the sphere. Nothing here takes a frame: a position is `(lon, lat)` as
!! astronomers write it, and a declination held in the mirrored convention is negated before it is
!! converted, because a rotation does not commute with the mirror.
!!
!! **`real64` only**, like every sky procedure in the library; a `real32` caller converts at the
!! call.
!!
!! **Arrow-free, settings-free and silent.** It reaches `parquet_utils` and `parquet_constants` only
!! (`check_parquet_skycoord_stays_arrow_free`), reads no knob and prints nothing, so it re-exports
!! no setting. It has no module variable, and every procedure is `pure`: anything here may be
!! called from any number of threads at once, and a `pf_sky_rotation`, read-only once `%init` has
!! run, serves a whole team. The only physical constants are the speed of light, `PF_C_KMS`, and
!! the default dipole, which is private to `pf_zhel2zcmb`, in `src/parquet_skycoord_rotate.f90`.
module parquet_skycoord
    use, intrinsic :: iso_fortran_env, only: real64
    use parquet_constants, only: PF_PI, PF_RAD_PER_DEG, PF_DEG_PER_RAD
    implicit none
    private

    ! ---- Coordinate systems ----
    public :: PF_COORD_UNKNOWN, PF_COORD_ICRS, PF_COORD_GALACTIC, PF_COORD_ECLIPTIC, PF_COORD_SUPERGALACTIC
    public :: PF_COORD_FK5
    public :: pf_coord_system_name, pf_coord_system_from_name
    ! ---- Rotations ----
    public :: pf_icrs2gal, pf_gal2icrs
    public :: pf_icrs2ecl, pf_ecl2icrs
    public :: pf_gal2sgal, pf_sgal2gal
    public :: pf_icrs2sgal, pf_sgal2icrs
    public :: pf_icrs2fk5, pf_fk52icrs
    public :: pf_sky_convert
    public :: pf_sky_rotation
    ! ---- Frame-free RA/Dec geometry ----
    public :: pf_angdist_deg, pf_offset_radec, pf_position_angle_deg
    public :: pf_apply_pm
    public :: pf_radec2unit, pf_unit2radec
    public :: pf_radec2tan, pf_tan2radec
    ! ---- Sexagesimal angles and text ----
    public :: pf_deg2hms, pf_deg2dms, pf_hms2deg, pf_dms2deg
    public :: pf_ra2str, pf_dec2str, pf_radec2str
    public :: pf_str2ra, pf_str2dec, pf_str2radec
    ! ---- The CMB rest frame ----
    public :: pf_zhel2zcmb, pf_zcmb2zhel

    ! ---- Selectors ----

    !> No coordinate system: what `pf_coord_system_from_name` answers for a token it does not know.
    !!
    !! A sentinel, not a system -- `pf_sky_convert` refuses it -- and it is 0, so an integer left at
    !! zero reads as unknown. `pf_coord_system_name` spells it `"unknown"`.
    integer, parameter :: PF_COORD_UNKNOWN = 0
    !> The International Celestial Reference System, the hub every rotation here is referred to.
    integer, parameter :: PF_COORD_ICRS = 1
    !> Galactic `(l, b)`, astropy's `Galactic`: its FK5 J2000 definition carried through the ICRS
    !! frame bias of USNO Circular 179.
    integer, parameter :: PF_COORD_GALACTIC = 2
    !> Ecliptic longitude and latitude, astropy's `BarycentricMeanEcliptic` at equinox J2000: the
    !! IAU 2006 mean ecliptic and equinox, the frame bias included.
    integer, parameter :: PF_COORD_ECLIPTIC = 3
    !> Supergalactic `(SGL, SGB)`, astropy's `Supergalactic`: de Vaucouleurs' pole at Galactic
    !! `(47.37, 6.32)`, with `SGL = 90` at the north Galactic pole.
    integer, parameter :: PF_COORD_SUPERGALACTIC = 4
    !> FK5 J2000 `(ra, dec)`, astropy's `FK5` at its default equinox: ICRS rotated by the frame bias
    !! of USNO Circular 179, at most 32 milliarcseconds. **FK5 J2000 only**: no other equinox is provided.
    integer, parameter :: PF_COORD_FK5 = 5

    ! ---- Numerical constants ----

    !> The component magnitude above which `x*x + y*y` cannot go subnormal, so `hypot` is not needed.
    !!
    !! The squares underflow below about `1.5e-154`; this sits four decades clear of it.
    real(real64), parameter :: skc_hypot_safe = 1.0e-150_real64

    ! ---- The angle table ----
    !
    ! One row per system: `pole_lon`, `pole_lat` -- its north pole in the system it is built from --
    ! and `lon0`, its own longitude of that system's north pole, all in degrees. Derived at 60 digits
    ! by tools/generate_skycoord_reference.py, whose --self-test holds each literal to the double
    ! nearest the derived value: never edit one by hand.

    !> Galactic from ICRS: the north Galactic pole's right ascension -- astropy's FK5 J2000
    !! `192.8594812065348` carried through the frame bias, which moves it from the eighth digit on.
    real(real64), parameter :: skc_gal_pole_lon = 192.859477894776054418_real64
    !> Galactic from ICRS: the north Galactic pole's declination, astropy's `27.12825118085622`
    !! through the bias.
    real(real64), parameter :: skc_gal_pole_lat = 27.1282524149679992594_real64
    !> Galactic from ICRS: the Galactic longitude of the ICRS north pole, astropy's
    !! `122.9319185680026` through the bias.
    real(real64), parameter :: skc_gal_lon0 = 122.931925255416626620_real64
    !> Ecliptic from ICRS: `270 + gamb`, with `gamb = -0.052928"` the IAU 2006 frame-bias angle.
    real(real64), parameter :: skc_ecl_pole_lon = 269.999985297777777778_real64
    !> Ecliptic from ICRS: `90 - phib`, with `phib = 84381.412819"` the bias-precession angle at J2000.
    real(real64), parameter :: skc_ecl_pole_lat = 66.5607186613888888889_real64
    !> Ecliptic from ICRS: `90 + psib`, with `psib = -0.041775"` the third bias angle.
    real(real64), parameter :: skc_ecl_lon0 = 89.9999883958333333333_real64
    !> Supergalactic from GALACTIC: de Vaucouleurs' north supergalactic pole's Galactic longitude.
    real(real64), parameter :: skc_sgal_pole_lon = 47.37_real64
    !> Supergalactic from Galactic: that pole's Galactic latitude.
    real(real64), parameter :: skc_sgal_pole_lat = 6.32_real64
    !> Supergalactic from Galactic: the supergalactic longitude of the north Galactic pole.
    real(real64), parameter :: skc_sgal_lon0 = 90.0_real64
    !> FK5 J2000 from ICRS: the FK5 pole's right ascension. The row is USNO Circular 179's frame bias
    !! and nothing else, derived at 60 digits from its three angles and never recovered from a
    !! rounded matrix, which would leave it eight digits: the pole sits 21.88 mas from the ICRS pole.
    real(real64), parameter :: skc_fk5_pole_lon = 294.573968875799802777_real64
    !> FK5 J2000 from ICRS: the FK5 pole's declination, 21.88 mas short of 90 -- the bias, not rounding.
    real(real64), parameter :: skc_fk5_pole_lat = 89.9999939216788786441_real64
    !> FK5 J2000 from ICRS: the FK5 right ascension of the ICRS pole.
    real(real64), parameter :: skc_fk5_lon0 = 114.573975236911035825_real64

    ! ---- The matrices, built at compile time ----
    !
    ! `M = Rz(a) Ry(b) Rz(c)` with `a = 180 - lon0`, `b = 90 - pole_lat` and `c = pole_lon`, each `R`
    ! a rotation of the AXES (astropy's `rotation_matrix`), written out element by element because
    ! a constant expression cannot call a procedure. `M(i, j)` takes component `j` of a unit vector
    ! in the source system to component `i` of the target's. The kernel is handed one of these and
    ! never `transpose(M)` at a call: that is an array expression at an explicit-shape dummy.

    !> The sines and cosines of the Galactic row's `a`, `b` and `c`.
    real(real64), parameter :: gal_ca = cos((180.0_real64 - skc_gal_lon0) * PF_RAD_PER_DEG), &
                               gal_sa = sin((180.0_real64 - skc_gal_lon0) * PF_RAD_PER_DEG), &
                               gal_cb = cos((90.0_real64 - skc_gal_pole_lat) * PF_RAD_PER_DEG), &
                               gal_sb = sin((90.0_real64 - skc_gal_pole_lat) * PF_RAD_PER_DEG), &
                               gal_cc = cos(skc_gal_pole_lon * PF_RAD_PER_DEG), &
                               gal_sc = sin(skc_gal_pole_lon * PF_RAD_PER_DEG)
    !> The sines and cosines of the ecliptic row's `a`, `b` and `c`.
    real(real64), parameter :: ecl_ca = cos((180.0_real64 - skc_ecl_lon0) * PF_RAD_PER_DEG), &
                               ecl_sa = sin((180.0_real64 - skc_ecl_lon0) * PF_RAD_PER_DEG), &
                               ecl_cb = cos((90.0_real64 - skc_ecl_pole_lat) * PF_RAD_PER_DEG), &
                               ecl_sb = sin((90.0_real64 - skc_ecl_pole_lat) * PF_RAD_PER_DEG), &
                               ecl_cc = cos(skc_ecl_pole_lon * PF_RAD_PER_DEG), &
                               ecl_sc = sin(skc_ecl_pole_lon * PF_RAD_PER_DEG)
    !> The sines and cosines of the supergalactic row's `a`, `b` and `c`.
    real(real64), parameter :: sgal_ca = cos((180.0_real64 - skc_sgal_lon0) * PF_RAD_PER_DEG), &
                               sgal_sa = sin((180.0_real64 - skc_sgal_lon0) * PF_RAD_PER_DEG), &
                               sgal_cb = cos((90.0_real64 - skc_sgal_pole_lat) * PF_RAD_PER_DEG), &
                               sgal_sb = sin((90.0_real64 - skc_sgal_pole_lat) * PF_RAD_PER_DEG), &
                               sgal_cc = cos(skc_sgal_pole_lon * PF_RAD_PER_DEG), &
                               sgal_sc = sin(skc_sgal_pole_lon * PF_RAD_PER_DEG)
    !> The sines and cosines of the FK5 row's `a`, `b` and `c`.
    real(real64), parameter :: fk5_ca = cos((180.0_real64 - skc_fk5_lon0) * PF_RAD_PER_DEG), &
                               fk5_sa = sin((180.0_real64 - skc_fk5_lon0) * PF_RAD_PER_DEG), &
                               fk5_cb = cos((90.0_real64 - skc_fk5_pole_lat) * PF_RAD_PER_DEG), &
                               fk5_sb = sin((90.0_real64 - skc_fk5_pole_lat) * PF_RAD_PER_DEG), &
                               fk5_cc = cos(skc_fk5_pole_lon * PF_RAD_PER_DEG), &
                               fk5_sc = sin(skc_fk5_pole_lon * PF_RAD_PER_DEG)

    !> ICRS to Galactic.
    real(real64), parameter :: skc_m_icrs2gal(3, 3) = reshape([ &
        gal_ca * gal_cb * gal_cc - gal_sa * gal_sc, -gal_sa * gal_cb * gal_cc - gal_ca * gal_sc, gal_sb * gal_cc, &
        gal_ca * gal_cb * gal_sc + gal_sa * gal_cc, -gal_sa * gal_cb * gal_sc + gal_ca * gal_cc, gal_sb * gal_sc, &
        -gal_ca * gal_sb, gal_sa * gal_sb, gal_cb], [3, 3])
    !> ICRS to ecliptic.
    real(real64), parameter :: skc_m_icrs2ecl(3, 3) = reshape([ &
        ecl_ca * ecl_cb * ecl_cc - ecl_sa * ecl_sc, -ecl_sa * ecl_cb * ecl_cc - ecl_ca * ecl_sc, ecl_sb * ecl_cc, &
        ecl_ca * ecl_cb * ecl_sc + ecl_sa * ecl_cc, -ecl_sa * ecl_cb * ecl_sc + ecl_ca * ecl_cc, ecl_sb * ecl_sc, &
        -ecl_ca * ecl_sb, ecl_sa * ecl_sb, ecl_cb], [3, 3])
    !> Galactic to supergalactic.
    real(real64), parameter :: skc_m_gal2sgal(3, 3) = reshape([ &
        sgal_ca * sgal_cb * sgal_cc - sgal_sa * sgal_sc, -sgal_sa * sgal_cb * sgal_cc - sgal_ca * sgal_sc, sgal_sb * sgal_cc, &
        sgal_ca * sgal_cb * sgal_sc + sgal_sa * sgal_cc, -sgal_sa * sgal_cb * sgal_sc + sgal_ca * sgal_cc, sgal_sb * sgal_sc, &
        -sgal_ca * sgal_sb, sgal_sa * sgal_sb, sgal_cb], [3, 3])
    !> ICRS to FK5 J2000.
    real(real64), parameter :: skc_m_icrs2fk5(3, 3) = reshape([ &
        fk5_ca * fk5_cb * fk5_cc - fk5_sa * fk5_sc, -fk5_sa * fk5_cb * fk5_cc - fk5_ca * fk5_sc, fk5_sb * fk5_cc, &
        fk5_ca * fk5_cb * fk5_sc + fk5_sa * fk5_cc, -fk5_sa * fk5_cb * fk5_sc + fk5_ca * fk5_cc, fk5_sb * fk5_sc, &
        -fk5_ca * fk5_sb, fk5_sa * fk5_sb, fk5_cb], [3, 3])
    !> Galactic to ICRS, the transpose.
    real(real64), parameter :: skc_m_gal2icrs(3, 3) = transpose(skc_m_icrs2gal)
    !> Ecliptic to ICRS, the transpose.
    real(real64), parameter :: skc_m_ecl2icrs(3, 3) = transpose(skc_m_icrs2ecl)
    !> Supergalactic to Galactic, the transpose.
    real(real64), parameter :: skc_m_sgal2gal(3, 3) = transpose(skc_m_gal2sgal)
    !> ICRS to supergalactic: one matrix, so a conversion is one rotation rather than two.
    real(real64), parameter :: skc_m_icrs2sgal(3, 3) = matmul(skc_m_gal2sgal, skc_m_icrs2gal)
    !> Supergalactic to ICRS, the transpose.
    real(real64), parameter :: skc_m_sgal2icrs(3, 3) = transpose(skc_m_icrs2sgal)
    !> Galactic to ecliptic, for `pf_sky_convert`'s pair with no named procedure.
    real(real64), parameter :: skc_m_gal2ecl(3, 3) = matmul(skc_m_icrs2ecl, skc_m_gal2icrs)
    !> Ecliptic to Galactic, the transpose.
    real(real64), parameter :: skc_m_ecl2gal(3, 3) = transpose(skc_m_gal2ecl)
    !> Ecliptic to supergalactic, for `pf_sky_convert`'s pair with no named procedure.
    real(real64), parameter :: skc_m_ecl2sgal(3, 3) = matmul(skc_m_icrs2sgal, skc_m_ecl2icrs)
    !> Supergalactic to ecliptic, the transpose.
    real(real64), parameter :: skc_m_sgal2ecl(3, 3) = transpose(skc_m_ecl2sgal)
    !> FK5 J2000 to ICRS, the transpose.
    real(real64), parameter :: skc_m_fk52icrs(3, 3) = transpose(skc_m_icrs2fk5)
    !> FK5 J2000 to Galactic, for `pf_sky_convert`'s pair with no named procedure: astropy's own
    !! FK5-referred Galactic rotation, reached through ICRS at compile time.
    real(real64), parameter :: skc_m_fk52gal(3, 3) = matmul(skc_m_icrs2gal, skc_m_fk52icrs)
    !> Galactic to FK5 J2000, the transpose.
    real(real64), parameter :: skc_m_gal2fk5(3, 3) = transpose(skc_m_fk52gal)
    !> FK5 J2000 to ecliptic, for `pf_sky_convert`'s pair with no named procedure.
    real(real64), parameter :: skc_m_fk52ecl(3, 3) = matmul(skc_m_icrs2ecl, skc_m_fk52icrs)
    !> Ecliptic to FK5 J2000, the transpose.
    real(real64), parameter :: skc_m_ecl2fk5(3, 3) = transpose(skc_m_fk52ecl)
    !> FK5 J2000 to supergalactic, for `pf_sky_convert`'s pair with no named procedure.
    real(real64), parameter :: skc_m_fk52sgal(3, 3) = matmul(skc_m_icrs2sgal, skc_m_fk52icrs)
    !> Supergalactic to FK5 J2000, the transpose.
    real(real64), parameter :: skc_m_sgal2fk5(3, 3) = transpose(skc_m_fk52sgal)
    !> The identity, which a `pf_sky_rotation` from a system to itself holds.
    real(real64), parameter :: skc_m_identity(3, 3) = reshape([1.0_real64, 0.0_real64, 0.0_real64, &
        0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [3, 3])

    ! ---- The rotation object ----

    !> A rotation between two coordinate systems, prepared once and applied to any number of
    !! positions: `pf_sky_convert` for a loop over a column, with the two selectors read once.
    !!
    !! ```fortran
    !! type(pf_sky_rotation) :: rot
    !! call rot%init(PF_COORD_ICRS, PF_COORD_GALACTIC)
    !! call rot%apply(ra, dec, l, b)            ! elemental over whole columns
    !! ```
    !!
    !! `%init(from, to)` resolves the selectors and takes the pair's compile-time matrix, the one
    !! `pf_sky_convert` rotates by, so `%apply` answers what `pf_sky_convert(lon, lat, from, to, ...)`
    !! answers, to within a few ulp: the two are separate call sites of one kernel. From a system to
    !! itself `%apply` returns its input by copy, as `pf_sky_convert` does. `%init` may run again and
    !! drops the rotation held; **`%apply` before any `%init` stops the program**, as does a selector
    !! that is not one of the five systems. Every binding is `pure`, and the object is read-only once
    !! prepared, so one rotation built before a parallel region serves the whole team.
    type :: pf_sky_rotation
        private
        !> The rotation: component `j` of a unit vector in the system `from` to component `i` in `to`.
        real(real64) :: m(3, 3) = 0.0_real64
        !> Whether `from == to`, which `%apply` answers by copy.
        logical :: identity = .false.
        !> Whether `%init` has run.
        logical :: set = .false.
    contains
        procedure, non_overridable :: init => skc_rotation_init        !! Prepares the rotation; may run again.
        procedure, non_overridable :: apply => skc_rotation_apply      !! Rotates positions; `pure elemental`.
        procedure, non_overridable :: is_init => skc_rotation_is_init  !! Whether `%init` has run.
    end type pf_sky_rotation

    ! ---- Interfaces: rotations and the selector tokens ----
    !
    ! Implemented in submodule parquet_skycoord_rotate. Every rotation shares one contract, stated
    ! on each: total in its coordinates, a latitude of exactly +/-90 the pole whatever the longitude
    ! says, the output longitude in `[0, 360)`, and a pole's longitude 0.

    interface
        !> ICRS `(ra, dec)` to Galactic `(l, b)`, in degrees, as astropy's `SkyCoord.galactic`.
        !!
        !! `pure elemental`, so one call converts whole columns. **Total**: a NaN argument gives NaN
        !! results without raising a flag, and a `dec` outside `[-90, 90]` is read as the direction
        !! it names. A `dec` of exactly +/-90 is the pole whatever `ra` says, and a result at a pole
        !! has `l = 0`.
        pure elemental module subroutine pf_icrs2gal(ra, dec, l, b)
            real(real64), intent(in) :: ra !! right ascension, ICRS, degrees; any value.
            real(real64), intent(in) :: dec !! declination, ICRS, degrees.
            real(real64), intent(out) :: l !! Galactic longitude, degrees, in `[0, 360)`.
            real(real64), intent(out) :: b !! Galactic latitude, degrees, in `[-90, 90]`.
        end subroutine pf_icrs2gal

        !> Galactic `(l, b)` to ICRS `(ra, dec)`, in degrees; the inverse of `pf_icrs2gal`.
        !!
        !! `pure elemental` and total, with `pf_icrs2gal`'s rules: NaN in gives NaN out without a
        !! flag, a `b` outside `[-90, 90]` names a direction, `b = +/-90` is the pole whatever `l`
        !! says, and a result at a pole has `ra = 0`.
        pure elemental module subroutine pf_gal2icrs(l, b, ra, dec)
            real(real64), intent(in) :: l !! Galactic longitude, degrees; any value.
            real(real64), intent(in) :: b !! Galactic latitude, degrees.
            real(real64), intent(out) :: ra !! right ascension, ICRS, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! declination, ICRS, degrees, in `[-90, 90]`.
        end subroutine pf_gal2icrs

        !> ICRS `(ra, dec)` to ecliptic `(elon, elat)`, in degrees, as astropy's
        !! `BarycentricMeanEcliptic` at equinox J2000 -- the IAU 2006 mean ecliptic, frame bias
        !! included.
        !!
        !! `pure elemental` and total, with `pf_icrs2gal`'s rules: NaN in gives NaN out without a
        !! flag, a `dec` outside `[-90, 90]` names a direction, `dec = +/-90` is the pole whatever
        !! `ra` says, and a result at a pole has `elon = 0`.
        pure elemental module subroutine pf_icrs2ecl(ra, dec, elon, elat)
            real(real64), intent(in) :: ra !! right ascension, ICRS, degrees; any value.
            real(real64), intent(in) :: dec !! declination, ICRS, degrees.
            real(real64), intent(out) :: elon !! ecliptic longitude, degrees, in `[0, 360)`.
            real(real64), intent(out) :: elat !! ecliptic latitude, degrees, in `[-90, 90]`.
        end subroutine pf_icrs2ecl

        !> Ecliptic `(elon, elat)` to ICRS `(ra, dec)`, in degrees; the inverse of `pf_icrs2ecl`.
        !!
        !! `pure elemental` and total, with `pf_icrs2gal`'s rules: NaN in gives NaN out without a
        !! flag, an `elat` outside `[-90, 90]` names a direction, `elat = +/-90` is the pole
        !! whatever `elon` says, and a result at a pole has `ra = 0`.
        pure elemental module subroutine pf_ecl2icrs(elon, elat, ra, dec)
            real(real64), intent(in) :: elon !! ecliptic longitude, degrees; any value.
            real(real64), intent(in) :: elat !! ecliptic latitude, degrees.
            real(real64), intent(out) :: ra !! right ascension, ICRS, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! declination, ICRS, degrees, in `[-90, 90]`.
        end subroutine pf_ecl2icrs

        !> Galactic `(l, b)` to supergalactic `(sgl, sgb)`, in degrees, as astropy's
        !! `Supergalactic`. The system is defined in Galactic coordinates, so this is its own
        !! rotation, not a trip through ICRS.
        !!
        !! `pure elemental` and total, with `pf_icrs2gal`'s rules: NaN in gives NaN out without a
        !! flag, a `b` outside `[-90, 90]` names a direction, `b = +/-90` is the pole whatever `l`
        !! says, and a result at a pole has `sgl = 0`.
        pure elemental module subroutine pf_gal2sgal(l, b, sgl, sgb)
            real(real64), intent(in) :: l !! Galactic longitude, degrees; any value.
            real(real64), intent(in) :: b !! Galactic latitude, degrees.
            real(real64), intent(out) :: sgl !! supergalactic longitude, degrees, in `[0, 360)`.
            real(real64), intent(out) :: sgb !! supergalactic latitude, degrees, in `[-90, 90]`.
        end subroutine pf_gal2sgal

        !> Supergalactic `(sgl, sgb)` to Galactic `(l, b)`, in degrees; the inverse of
        !! `pf_gal2sgal`.
        !!
        !! `pure elemental` and total, with `pf_icrs2gal`'s rules: NaN in gives NaN out without a
        !! flag, an `sgb` outside `[-90, 90]` names a direction, `sgb = +/-90` is the pole whatever
        !! `sgl` says, and a result at a pole has `l = 0`.
        pure elemental module subroutine pf_sgal2gal(sgl, sgb, l, b)
            real(real64), intent(in) :: sgl !! supergalactic longitude, degrees; any value.
            real(real64), intent(in) :: sgb !! supergalactic latitude, degrees.
            real(real64), intent(out) :: l !! Galactic longitude, degrees, in `[0, 360)`.
            real(real64), intent(out) :: b !! Galactic latitude, degrees, in `[-90, 90]`.
        end subroutine pf_sgal2gal

        !> ICRS `(ra, dec)` to supergalactic `(sgl, sgb)`, in degrees: the Galactic rotation and the
        !! supergalactic one composed into one matrix at compile time, so a conversion rounds once.
        !!
        !! `pure elemental` and total, with `pf_icrs2gal`'s rules: NaN in gives NaN out without a
        !! flag, a `dec` outside `[-90, 90]` names a direction, `dec = +/-90` is the pole whatever
        !! `ra` says, and a result at a pole has `sgl = 0`.
        pure elemental module subroutine pf_icrs2sgal(ra, dec, sgl, sgb)
            real(real64), intent(in) :: ra !! right ascension, ICRS, degrees; any value.
            real(real64), intent(in) :: dec !! declination, ICRS, degrees.
            real(real64), intent(out) :: sgl !! supergalactic longitude, degrees, in `[0, 360)`.
            real(real64), intent(out) :: sgb !! supergalactic latitude, degrees, in `[-90, 90]`.
        end subroutine pf_icrs2sgal

        !> Supergalactic `(sgl, sgb)` to ICRS `(ra, dec)`, in degrees; the inverse of
        !! `pf_icrs2sgal`.
        !!
        !! `pure elemental` and total, with `pf_icrs2gal`'s rules: NaN in gives NaN out without a
        !! flag, an `sgb` outside `[-90, 90]` names a direction, `sgb = +/-90` is the pole whatever
        !! `sgl` says, and a result at a pole has `ra = 0`.
        pure elemental module subroutine pf_sgal2icrs(sgl, sgb, ra, dec)
            real(real64), intent(in) :: sgl !! supergalactic longitude, degrees; any value.
            real(real64), intent(in) :: sgb !! supergalactic latitude, degrees.
            real(real64), intent(out) :: ra !! right ascension, ICRS, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! declination, ICRS, degrees, in `[-90, 90]`.
        end subroutine pf_sgal2icrs

        !> ICRS `(ra, dec)` to FK5 J2000 `(ra_fk5, dec_fk5)`, in degrees, as astropy's `FK5` at its
        !! default equinox: the frame bias of USNO Circular 179 and nothing else, at most 32
        !! milliarcseconds on the sky. **FK5 here is FK5 J2000**: no other equinox is provided.
        !!
        !! `pure elemental` and total, with `pf_icrs2gal`'s rules: NaN in gives NaN out without a
        !! flag, a `dec` outside `[-90, 90]` names a direction, `dec = +/-90` is the pole whatever
        !! `ra` says, and a result at a pole has `ra_fk5 = 0`.
        pure elemental module subroutine pf_icrs2fk5(ra, dec, ra_fk5, dec_fk5)
            real(real64), intent(in) :: ra !! right ascension, ICRS, degrees; any value.
            real(real64), intent(in) :: dec !! declination, ICRS, degrees.
            real(real64), intent(out) :: ra_fk5 !! right ascension, FK5 J2000, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec_fk5 !! declination, FK5 J2000, degrees, in `[-90, 90]`.
        end subroutine pf_icrs2fk5

        !> FK5 J2000 `(ra_fk5, dec_fk5)` to ICRS `(ra, dec)`, in degrees; the inverse of `pf_icrs2fk5`.
        !!
        !! `pure elemental` and total, with `pf_icrs2gal`'s rules: NaN in gives NaN out without a
        !! flag, a `dec_fk5` outside `[-90, 90]` names a direction, `dec_fk5 = +/-90` is the pole
        !! whatever `ra_fk5` says, and a result at a pole has `ra = 0`.
        pure elemental module subroutine pf_fk52icrs(ra_fk5, dec_fk5, ra, dec)
            real(real64), intent(in) :: ra_fk5 !! right ascension, FK5 J2000, degrees; any value.
            real(real64), intent(in) :: dec_fk5 !! declination, FK5 J2000, degrees.
            real(real64), intent(out) :: ra !! right ascension, ICRS, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! declination, ICRS, degrees, in `[-90, 90]`.
        end subroutine pf_fk52icrs

        !> A position converted between two systems named at run time by `PF_COORD_*` selectors,
        !! in degrees.
        !!
        !! For a pair with a named procedure it calls that procedure, so it answers what the
        !! procedure answers to within a few ulp -- the two are call sites of one kernel, and a
        !! compiler may inline it at one and not the other; for a pair without one -- Galactic and
        !! ecliptic, ecliptic and
        !! supergalactic, and FK5 and any system but ICRS, either way round -- it applies that pair's
        !! own compile-time matrix, never two rotations through angles. **`from == to` is the
        !! identity: the input comes back by copy, before any arithmetic**, so a longitude of `-10`
        !! stays `-10` and a `-0.0` keeps its sign; the `[0, 360)` promise on `lon_out` does not apply
        !! to it. Total in the coordinates, with the named procedures' rules. **A selector that is not
        !! one of the five systems aborts**, `PF_COORD_UNKNOWN` included and `from == to` included.
        !! `pure elemental`; `pf_sky_rotation` is the same conversion with the selectors read once.
        pure elemental module subroutine pf_sky_convert(lon_in, lat_in, from, to, lon_out, lat_out)
            real(real64), intent(in) :: lon_in !! longitude in the system `from`, degrees; any value.
            real(real64), intent(in) :: lat_in !! latitude in the system `from`, degrees.
            integer, intent(in) :: from !! the input's system: `PF_COORD_ICRS`, `_GALACTIC`, `_ECLIPTIC`, `_SUPERGALACTIC`, `_FK5`.
            integer, intent(in) :: to !! the output's system, from the same five.
            real(real64), intent(out) :: lon_out !! longitude in the system `to`, degrees, in `[0, 360)` unless `from == to`.
            real(real64), intent(out) :: lat_out !! latitude in the system `to`, degrees.
        end subroutine pf_sky_convert

        !> The token naming a coordinate system, lowercase: `"icrs"`, `"galactic"`, `"ecliptic"`,
        !! `"supergalactic"` or `"fk5"`, and `"unknown"` for `PF_COORD_UNKNOWN`, so the sentinel
        !! round-trips through text.
        !!
        !! The inverse of `pf_coord_system_from_name`. Any integer that is neither a system nor the
        !! sentinel aborts, as `pf_sky_convert` does: it is a caller mistake with no reading.
        pure module subroutine pf_coord_system_name(system, name)
            integer, intent(in) :: system !! a `PF_COORD_*` selector, or `PF_COORD_UNKNOWN`.
            character(len=:), allocatable, intent(out) :: name !! the token.
        end subroutine pf_coord_system_name

        !> The selector a token names, case-insensitively and ignoring blanks around it:
        !! `pf_coord_system_from_name("Galactic")` is `PF_COORD_GALACTIC`.
        !!
        !! **A token it does not know answers `PF_COORD_UNKNOWN` rather than aborting**: the text is
        !! user data, read out of a configuration file or a column's metadata, and a caller
        !! validating it reports the bad token in its own words. astropy's frame names are
        !! understood too -- `icrs`, `galactic`, `supergalactic` and `fk5` are the same words, and
        !! `barycentricmeanecliptic` names the ecliptic.
        pure module function pf_coord_system_from_name(name) result(system)
            character(len=*), intent(in) :: name !! the token.
            integer :: system !! the selector it names, or `PF_COORD_UNKNOWN`.
        end function pf_coord_system_from_name
    end interface

    ! ---- Interfaces: frame-free RA/Dec geometry ----
    !
    ! Implemented in submodule parquet_skycoord_geom.

    interface
        !> Angular separation of two sky positions given in degrees, in degrees.
        !!
        !! **It needs no frame and takes none.** The two declination conventions in live
        !! downstream use -- `theta = pi/2 - dec` and the mirrored `theta = pi/2 + dec` -- send the
        !! same number to opposite hemispheres, but they differ by a reflection in `z`, and a
        !! reflection preserves the angle between two directions, so this procedure returns the
        !! same answer under either.
        !!
        !! `pure elemental`, so it broadcasts over whole arrays of coordinates -- which
        !! `parquet_healpix`'s `pf_angdist` cannot do, its `vec(3)` dummies being arrays already.
        !! It is also **31% cheaper than converting to vectors and calling `pf_angdist`** (47.0 ns
        !! against 68.0 on machine B), because working in the frame where only the RA difference
        !! survives removes one of the four sine/cosine pairs.
        !!
        !! **A position is EXACTLY zero degrees from itself**, including when the two right
        !! ascensions differ by whole turns and when both positions sit at a pole with unrelated
        !! right ascensions. That is a guarantee rather than an arithmetic accident: the formula
        !! alone gives a few times 1e-15 degrees there on a compiler that contracts a
        !! multiply-subtract into an FMA, and a caller excluding self-matches with `dist > 0`
        !! would then keep every one of them.
        !!
        !! **Total, like every other elemental here: it validates nothing and never aborts.** A
        !! NaN argument gives a NaN result rather than an error, and `dec` outside [-90, 90] is
        !! read as the direction that declination names rather than refused.
        !!
        !! **A NaN argument also raises no IEEE flag**, so a caller running with the exceptions
        !! unmasked -- which is nagfor's default -- can carry a NaN through this procedure without
        !! being terminated by it. An INFINITE argument is different and does raise `IEEE_INVALID`,
        !! because taking the sine of an infinite angle is an invalid operation on any conforming
        !! processor rather than anything this formula chooses; the distinction is between
        !! propagating a NaN that already exists and creating one.
        pure elemental module function pf_angdist_deg(ra1, dec1, ra2, dec2) result(dist)
            real(real64), intent(in) :: ra1 !! right ascension of the first position, degrees; any value.
            real(real64), intent(in) :: dec1 !! declination of the first position, degrees, in [-90, 90].
            real(real64), intent(in) :: ra2 !! right ascension of the second position, degrees; any value.
            real(real64), intent(in) :: dec2 !! declination of the second position, degrees, in [-90, 90].
            real(real64) :: dist !! the angle between them, degrees, in [0, 180].
        end function pf_angdist_deg

        !> The position `sep_deg` away from `(ra0, dec0)` at position angle `pa_deg`, all in degrees.
        !!
        !! The position angle is measured from north through east, astropy's
        !! `directional_offset_by`: with `c` the centre, `north` and `east` the local unit vectors,
        !! the point is `cos(sep)*c + sin(sep)*(cos(pa)*north + sin(pa)*east)`. **At a pole the
        !! local frame follows the given `ra0`**, so an offset from the north pole at position angle
        !! `pa` lands at right ascension `ra0 + 180 - pa`. A `sep_deg` above 180 continues along the
        !! great circle. `ra` in `[0, 360)` and `dec` in `[-90, 90]`; a result at a pole has `ra = 0`.
        !! Frame-free. **A `dec0` outside `[-90, 90]` and a negative `sep_deg` stop the program**,
        !! because a centre beyond a pole mirrors the local north and east and gives a plausible
        !! wrong point, and a negative separation has no reading; **a NaN argument gives NaN
        !! results**, raising no flag, and an infinite `ra0`, `pa_deg` or `sep_deg` gives NaN results
        !! and raises `IEEE_INVALID`, as the sine of an infinite angle must. `pure elemental`.
        pure elemental module subroutine pf_offset_radec(ra0, dec0, pa_deg, sep_deg, ra, dec)
            real(real64), intent(in) :: ra0 !! the centre's right ascension, degrees; any value.
            real(real64), intent(in) :: dec0 !! the centre's declination, degrees, in `[-90, 90]`.
            real(real64), intent(in) :: pa_deg !! position angle, degrees, north through east; any value.
            real(real64), intent(in) :: sep_deg !! separation, degrees; at least 0.
            real(real64), intent(out) :: ra !! the offset position's right ascension, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! the offset position's declination, degrees, in `[-90, 90]`.
        end subroutine pf_offset_radec

        !> The position angle of `(ra2, dec2)` seen from `(ra1, dec1)`, degrees, north through east.
        !!
        !! `atan2(sin(dra)*cos(dec2), cos(dec1)*sin(dec2) - sin(dec1)*cos(dec2)*cos(dra))`, astropy's
        !! `position_angle`, in `[0, 360)`. The inverse of `pf_offset_radec` for a separation strictly
        !! between 0 and 180. **0 by rule for a coincident pair**, including two positions at one pole
        !! with different right ascensions. A declination of +/-90 is the pole exactly. **Total**: no
        !! validation, and a NaN argument gives a NaN without raising a flag. Frame-free.
        !!
        !! **The angle of a very close pair carries few digits**, and that is the formula rather
        !! than this implementation: both terms of the second argument cancel as the separation
        !! falls, so about one digit is lost per decade below a degree. A pair a milliarcsecond
        !! apart carries a handful of digits of position angle and a pair a microarcsecond apart
        !! almost none. astropy's `position_angle` behaves the same way; the SEPARATION of the same
        !! pair is unaffected, `pf_angdist_deg` keeping its accuracy all the way down.
        pure elemental module function pf_position_angle_deg(ra1, dec1, ra2, dec2) result(pa)
            real(real64), intent(in) :: ra1 !! right ascension of the reference position, degrees.
            real(real64), intent(in) :: dec1 !! declination of the reference position, degrees.
            real(real64), intent(in) :: ra2 !! right ascension of the other position, degrees.
            real(real64), intent(in) :: dec2 !! declination of the other position, degrees.
            real(real64) :: pa !! the position angle, degrees, in `[0, 360)`.
        end function pf_position_angle_deg

        !> A position moved by its proper motion over `dt_years`, in degrees; `pm_ra` is the rate in
        !! right ascension times `cos(dec)` -- Gaia's `pmra`, astropy's `pm_ra_cosdec` -- in mas/yr.
        !!
        !! The two rates are the step's components on the tangent plane -- `pm_ra` is already the
        !! rate along the local east -- and the position moves `hypot(pm_ra, pm_dec) * |dt_years|`
        !! milliarcseconds along the great circle leaving it in that direction, the circle
        !! `pf_offset_radec` would move it along at the same position angle; a negative `dt_years`
        !! moves it back the other way. **A step
        !! along a great circle, not rigorous space motion**: no parallax, radial velocity or light
        !! time enters it, and the motion is the one at the starting position. At a pole it is read in
        !! the local frame of the `ra` given, as `pf_offset_radec` reads a position angle. No motion
        !! or no time gives the position back to rounding, its right ascension wrapped into
        !! `[0, 360)`. **A NaN argument gives NaN results** without raising a flag -- the state of a
        !! source with no proper motion in a catalogue column -- and an infinite `ra`, `pm_ra`,
        !! `pm_dec` or `dt_years` gives NaN results and raises `IEEE_INVALID`; **a `dec` outside
        !! `[-90, 90]` stops the program**, as `pf_offset_radec`'s centre does. `pure elemental`.
        pure elemental module subroutine pf_apply_pm(ra, dec, pm_ra, pm_dec, dt_years, ra_out, dec_out)
            real(real64), intent(in) :: ra !! right ascension, degrees; any value.
            real(real64), intent(in) :: dec !! declination, degrees, in `[-90, 90]`.
            real(real64), intent(in) :: pm_ra !! proper motion in right ascension TIMES `cos(dec)`, mas/yr: Gaia's `pmra`.
            real(real64), intent(in) :: pm_dec !! proper motion in declination, mas/yr: Gaia's `pmdec`.
            real(real64), intent(in) :: dt_years !! the interval, in the years of the proper motion; negative moves back.
            real(real64), intent(out) :: ra_out !! right ascension after the motion, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec_out !! declination after the motion, degrees, in `[-90, 90]`.
        end subroutine pf_apply_pm

        !> A sky position in degrees as a unit vector, `(cos(lat)cos(lon), cos(lat)sin(lon), sin(lat))`.
        !!
        !! The same vector `parquet_sphere`'s `pf_radec2vec` gives under `PF_HP_DEC_NORTH`, bit for
        !! bit, and here without compiling the sphere tier for it. **No frame argument**: this
        !! module is frame-free and its rotations use the north convention, so a latitude held in
        !! the mirrored convention is negated before it is converted, as the module header says.
        !! **A latitude of exactly +/-90 is the pole `(0, 0, +/-1)`**, whatever `lon` says. Total:
        !! `lon` may take any value, a `lat` outside `[-90, 90]` is read as the direction it names,
        !! a NaN argument gives NaN components without raising a flag, and an infinite one raises
        !! `IEEE_INVALID`. Not `elemental` -- `v` is an array -- so one call is one position.
        pure module subroutine pf_radec2unit(lon, lat, v)
            real(real64), intent(in) :: lon !! longitude, degrees; any value.
            real(real64), intent(in) :: lat !! latitude, degrees.
            real(real64), intent(out) :: v(3) !! the unit vector of that direction.
        end subroutine pf_radec2unit

        !> A direction as a sky position in degrees: `pf_radec2unit` inverted.
        !!
        !! `parquet_sphere`'s `pf_vec2radec` under `PF_HP_DEC_NORTH`, bit for bit. **`v` need not
        !! have unit length**: it is scaled by its largest component first, so `[1e-300, 0,
        !! 1e-300]` is a direction. The latitude is `atan2(z, hypot(x, y))`, never `asin`, which
        !! loses half its digits near a pole. **A pole's longitude is 0 by rule**, and the zero
        !! vector answers `(0, 0)`. Total: a NaN component gives NaN outputs without raising a flag,
        !! and an infinite component is read as the direction of the infinite components alone.
        pure module subroutine pf_unit2radec(v, lon, lat)
            real(real64), intent(in) :: v(3) !! a direction; any length.
            real(real64), intent(out) :: lon !! longitude, degrees, in `[0, 360)`.
            real(real64), intent(out) :: lat !! latitude, degrees, in `[-90, 90]`.
        end subroutine pf_unit2radec

        !> A position as standard coordinates on the plane tangent at `(ra0, dec0)`, in degrees:
        !! the gnomonic projection, FITS's `TAN`.
        !!
        !! **`x` points east and `y` north**, and a point `sep` degrees from the centre at position
        !! angle `pa` lands at radius `tan(sep)` in radians, written in degrees -- so a great circle
        !! is a straight line and the scale is exact at the centre. `pa_deg` **rotates the axes**:
        !! with it given, `+y` points along position angle `pa_deg`, north through east as
        !! `pf_offset_radec` measures it, and `+x` 90 degrees east of that; the default 0 leaves
        !! `+y` north. A point at position angle `pa_deg` therefore lands on `+y` exactly.
        !!
        !! **A position in the far hemisphere has no image**: `x` and `y` are NaN, raising no flag,
        !! so a caller screens with `x /= x`. The test is the sign of `cos` of the separation, so
        !! the boundary is exactly 90 degrees to the precision the two unit vectors carry, and a
        !! position AT 90 degrees falls either side of it by a rounding; just inside, the image is
        !! correct and enormous -- 3.3e9 degrees at a millionth of a degree from the boundary. That
        !! is the projection's own singularity rather than a choice, and a field of view is better
        !! cut with `pf_angdist_deg`, which is exact there.
        !!
        !! Total otherwise, with the module's rules: a NaN argument
        !! gives NaN results and raises nothing, an infinite one raises `IEEE_INVALID`, `ra` and
        !! `ra0` may take any value, and at a pole the local frame follows the `ra0` given. **A
        !! `dec0` outside `[-90, 90]` stops the program**, as `pf_offset_radec`'s centre does and
        !! for the same reason: a centre beyond a pole mirrors the local north and east and gives a
        !! plausible wrong chart. A `dec` outside `[-90, 90]` is read as the direction it names.
        !! `pure elemental`, so a whole target list projects about one centre in a single call.
        pure elemental module subroutine pf_radec2tan(ra, dec, ra0, dec0, x, y, pa_deg)
            real(real64), intent(in) :: ra !! the position's right ascension, degrees; any value.
            real(real64), intent(in) :: dec !! the position's declination, degrees.
            real(real64), intent(in) :: ra0 !! the tangent point's right ascension, degrees; any value.
            real(real64), intent(in) :: dec0 !! the tangent point's declination, degrees, in `[-90, 90]`.
            real(real64), intent(out) :: x !! the standard coordinate east, degrees; NaN past 90 degrees.
            real(real64), intent(out) :: y !! the standard coordinate north, degrees; NaN past 90 degrees.
            real(real64), intent(in), optional :: pa_deg !! the position angle of the `+y` axis, degrees; default 0.
        end subroutine pf_radec2tan

        !> Standard coordinates on the plane tangent at `(ra0, dec0)` back to a sky position, in
        !! degrees: `pf_radec2tan` inverted.
        !!
        !! `pa_deg` means what it means there, and every `(x, y)` has an image, the plane covering
        !! the hemisphere around the tangent point. `ra` comes back in `[0, 360)` and `dec` in
        !! `[-90, 90]`, a pole's `ra` being 0. Total, with `pf_radec2tan`'s rules: a NaN argument
        !! gives NaN results and raises no flag, and **a `dec0` outside `[-90, 90]` stops the
        !! program**. `pure elemental`.
        pure elemental module subroutine pf_tan2radec(x, y, ra0, dec0, ra, dec, pa_deg)
            real(real64), intent(in) :: x !! the standard coordinate east, degrees.
            real(real64), intent(in) :: y !! the standard coordinate north, degrees.
            real(real64), intent(in) :: ra0 !! the tangent point's right ascension, degrees; any value.
            real(real64), intent(in) :: dec0 !! the tangent point's declination, degrees, in `[-90, 90]`.
            real(real64), intent(out) :: ra !! the position's right ascension, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! the position's declination, degrees, in `[-90, 90]`.
            real(real64), intent(in), optional :: pa_deg !! the position angle of the `+y` axis, degrees; default 0.
        end subroutine pf_tan2radec
    end interface

    ! ---- Interfaces: the rotation object ----
    !
    ! Implemented in submodule parquet_skycoord_object, on the rotation kernel of
    ! parquet_skycoord_rotate.

    interface
        !> `pf_sky_rotation%init`: prepares the rotation from the system `from` to the system `to`,
        !! both `PF_COORD_*` selectors.
        !!
        !! It takes the pair's compile-time matrix, the one `pf_sky_convert` rotates by -- the named
        !! procedure's where the pair has one -- or, from a system to itself, the identity, which
        !! `%apply` answers by copy. It may run again on the same object, and drops the rotation held.
        !! **A selector that is not one of the five systems stops the program**, `PF_COORD_UNKNOWN`
        !! included. `pure`; `intent(inout)` because a `pure` procedure may not take a polymorphic
        !! `intent(out)` dummy, and every component is assigned on every path.
        pure module subroutine skc_rotation_init(this, from, to)
            class(pf_sky_rotation), intent(inout) :: this !! the rotation; any previous one is dropped.
            integer, intent(in) :: from !! the input's system: `PF_COORD_ICRS`, `_GALACTIC`, `_ECLIPTIC`, `_SUPERGALACTIC`, `_FK5`.
            integer, intent(in) :: to !! the output's system, from the same five.
        end subroutine skc_rotation_init

        !> `pf_sky_rotation%apply`: a position rotated from the system `%init` named first into the
        !! one it named second, in degrees -- `pf_sky_convert`'s answer to within a few ulp.
        !!
        !! `pure elemental`, so one call rotates whole columns, with the named procedures' rules:
        !! total in the coordinates -- NaN in gives NaN out without a flag, a latitude outside
        !! `[-90, 90]` names a direction, a latitude of exactly +/-90 is the pole whatever the
        !! longitude says -- `lon_out` in `[0, 360)` and a pole's longitude 0; from a system to
        !! itself, the input back by copy. **Before any `%init` it stops the program.**
        pure elemental module subroutine skc_rotation_apply(this, lon_in, lat_in, lon_out, lat_out)
            class(pf_sky_rotation), intent(in) :: this !! a prepared rotation.
            real(real64), intent(in) :: lon_in !! longitude in the system `from`, degrees; any value.
            real(real64), intent(in) :: lat_in !! latitude in the system `from`, degrees.
            real(real64), intent(out) :: lon_out !! longitude in the system `to`, degrees, in `[0, 360)` unless `from == to`.
            real(real64), intent(out) :: lat_out !! latitude in the system `to`, degrees.
        end subroutine skc_rotation_apply

        !> `pf_sky_rotation%is_init`: whether `%init` has run on this rotation.
        pure elemental module function skc_rotation_is_init(this) result(yes)
            class(pf_sky_rotation), intent(in) :: this !! the rotation.
            logical :: yes !! `.true.` once `%init` has run.
        end function skc_rotation_is_init
    end interface

    ! ---- Interfaces: the CMB rest frame ----
    !
    ! Implemented in submodule parquet_skycoord_rotate, beside the rotation kernel whose unit vectors
    ! it shares.

    interface
        !> A heliocentric redshift in the rest frame of the cosmic microwave background:
        !! `1 + z_cmb = (1 + z_hel) / (gamma * (1 - (v/c) * cos(theta)))`.
        !!
        !! **`theta` is the angle between the dipole apex and the position AS OBSERVED** -- the
        !! direction a catalogue holds -- and the factor above is the one exact for that direction;
        !! `gamma * (1 + (v/c) * cos(theta))` is the same boost written for the apex angle measured
        !! in the CMB frame instead, and the two differ by up to `1.5e-6` in `1 + z`. `v` is the
        !! Sun's speed toward the apex and `gamma` the Lorentz factor of the whole of `v`, so
        !! looking toward the apex the CMB-frame redshift is the larger. **The apex is always
        !! Galactic**, as every dipole is published, whatever `system` names for the position.
        !! `pf_zcmb2zhel` is the inverse. The three dipole arguments default
        !! independently to Planck 2018 results I (Aghanim et al. 2020, A&A 641, A1): the apex at
        !! Galactic `(264.021, 48.253)` and `v = 369.82` km/s, the speed of light being 299792.458
        !! km/s.
        !!
        !! `pure elemental` and total: a NaN argument gives a NaN without raising a flag, a `z_hel` at
        !! or below -1 is computed as the formula says, an infinite `z_hel` comes back itself, and an
        !! `apex_v` of the speed of light or more, which has no reading, gives a NaN without raising
        !! a flag. **A `system` that is not one of the five stops the program**, as in
        !! `pf_sky_convert`.
        pure elemental module function pf_zhel2zcmb(lon, lat, z_hel, system, apex_lon, apex_lat, apex_v) &
                result(z_cmb)
            real(real64), intent(in) :: lon !! the position's longitude in `system`, degrees; any value.
            real(real64), intent(in) :: lat !! the position's latitude in `system`, degrees.
            real(real64), intent(in) :: z_hel !! the heliocentric redshift.
            integer, intent(in), optional :: system !! the system of `(lon, lat)`, a `PF_COORD_*` selector; default ICRS.
            real(real64), intent(in), optional :: apex_lon !! the apex's Galactic longitude, degrees; default 264.021.
            real(real64), intent(in), optional :: apex_lat !! the apex's Galactic latitude, degrees; default 48.253.
            real(real64), intent(in), optional :: apex_v !! the Sun's speed toward the apex, km/s; default 369.82.
            real(real64) :: z_cmb !! the redshift in the CMB rest frame.
        end function pf_zhel2zcmb

        !> A CMB-frame redshift back in the heliocentric frame: `pf_zhel2zcmb` inverted,
        !! `1 + z_hel = (1 + z_cmb) * gamma * (1 - (v/c) * cos(theta))`.
        !!
        !! Every argument means what it means there, `theta` included -- the angle between the apex
        !! and the position as observed -- so the same `(lon, lat)`, `system` and dipole carry a
        !! redshift back to what a spectrograph measured. The two are inverses to rounding: a
        !! redshift through both comes back itself. `pure elemental` and total, with
        !! `pf_zhel2zcmb`'s rules: a NaN argument gives a NaN without raising a flag, an infinite
        !! `z_cmb` comes back itself, an `apex_v` of the speed of light or more gives a NaN, and a
        !! `system` that is not one of the five stops the program.
        pure elemental module function pf_zcmb2zhel(lon, lat, z_cmb, system, apex_lon, apex_lat, apex_v) &
                result(z_hel)
            real(real64), intent(in) :: lon !! the position's longitude in `system`, degrees; any value.
            real(real64), intent(in) :: lat !! the position's latitude in `system`, degrees.
            real(real64), intent(in) :: z_cmb !! the redshift in the CMB rest frame.
            integer, intent(in), optional :: system !! the system of `(lon, lat)`, a `PF_COORD_*` selector; default ICRS.
            real(real64), intent(in), optional :: apex_lon !! the apex's Galactic longitude, degrees; default 264.021.
            real(real64), intent(in), optional :: apex_lat !! the apex's Galactic latitude, degrees; default 48.253.
            real(real64), intent(in), optional :: apex_v !! the Sun's speed toward the apex, km/s; default 369.82.
            real(real64) :: z_hel !! the heliocentric redshift.
        end function pf_zcmb2zhel
    end interface

    ! ---- Interfaces: sexagesimal angles and text ----
    !
    ! Implemented in submodule parquet_skycoord_text. The writers take a separator and a precision
    ! with the same meaning in all three -- `precision` is a declination's arcsecond decimals, and a
    ! right ascension's seconds of time carry one more, a second of time being 15 arcseconds -- so
    ! two columns written separately read like the pair written together.

    interface
        !> A right ascension in degrees as hours, minutes and seconds of time.
        !!
        !! The angle is wrapped into `[0, 360)` first, so `h` is in `[0, 23]`, `m` in `[0, 59]` and `s`
        !! in `[0, 60)`. Nothing is rounded, and the split is exact to a rounding of the seconds:
        !! the whole seconds are taken from one double product, so an angle whose exact product
        !! sits within that rounding of a whole second can split as the next second with a zero
        !! fraction rather than the previous one with a fraction of `0.99999999999997`. Both rejoin
        !! to the same angle through `pf_hms2deg`. `s` carries the angle's whole precision.
        !! `pure elemental`
        !! and total: **a NaN argument gives `h = m = 0` and `s` NaN** without raising a flag, and an
        !! infinite one gives the same and raises `IEEE_INVALID`, as its wrap must.
        pure elemental module subroutine pf_deg2hms(deg, h, m, s)
            real(real64), intent(in) :: deg !! right ascension, degrees; any value.
            integer, intent(out) :: h !! hours, in `[0, 23]`.
            integer, intent(out) :: m !! minutes, in `[0, 59]`.
            real(real64), intent(out) :: s !! seconds of time, in `[0, 60)`.
        end subroutine pf_deg2hms

        !> A declination in degrees as a sign, degrees, arcminutes and arcseconds.
        !!
        !! **The sign is its own argument**, because a declination between -1 and 0 has `d = 0`, and a
        !! sign folded into `d` would be lost: `-0.5` is `sgn = -1, d = 0, m = 30, s = 0`. `sgn` is -1
        !! for a negative `deg` and +1 otherwise, for `0` and `-0.0` alike. The magnitude splits as
        !! given, so a declination beyond 90 has `d` above 90. Nothing is rounded. `pure elemental`
        !! and total: a NaN argument gives `sgn = 1`, `d = m = 0` and `s` NaN without raising a flag;
        !! an infinite one gives zero `d` and `m` and a NaN `s` and raises `IEEE_INVALID`; and a
        !! magnitude of `huge(d)` degrees or more, whose degrees no default `integer` holds, gives
        !! zero `d` and `m` and a NaN `s` without raising a flag.
        pure elemental module subroutine pf_deg2dms(deg, sgn, d, m, s)
            real(real64), intent(in) :: deg !! declination, degrees.
            integer, intent(out) :: sgn !! -1 for a negative `deg`, +1 otherwise.
            integer, intent(out) :: d !! whole degrees of the magnitude.
            integer, intent(out) :: m !! arcminutes, in `[0, 59]`.
            real(real64), intent(out) :: s !! arcseconds, in `[0, 60)`.
        end subroutine pf_deg2dms

        !> Hours, minutes and seconds of time as degrees, `15*h + m/4 + s/240`: the inverse of
        !! `pf_deg2hms`.
        !!
        !! `pure elemental`. Nothing is validated or wrapped: `h = 24` gives 360, and a negative
        !! field subtracts.
        pure elemental module function pf_hms2deg(h, m, s) result(deg)
            integer, intent(in) :: h !! hours.
            integer, intent(in) :: m !! minutes.
            real(real64), intent(in) :: s !! seconds of time.
            real(real64) :: deg !! the angle, degrees.
        end function pf_hms2deg

        !> A sign, degrees, arcminutes and arcseconds as degrees, `d + m/60 + s/3600` negated when
        !! `sgn` is negative: the inverse of `pf_deg2dms`.
        !!
        !! `pure elemental`. Nothing is validated: the three fields are used as given, and a `sgn` of
        !! 0 or more means positive.
        pure elemental module function pf_dms2deg(sgn, d, m, s) result(deg)
            integer, intent(in) :: sgn !! negative for a negative angle.
            integer, intent(in) :: d !! degrees.
            integer, intent(in) :: m !! arcminutes.
            real(real64), intent(in) :: s !! arcseconds.
            real(real64) :: deg !! the angle, degrees.
        end function pf_dms2deg

        !> A right ascension in degrees as sexagesimal text: `"10:21:30.550"`.
        !!
        !! Wrapped into `[0, 360)` and written `hh:mm:ss.sss`, every field below ten zero-padded, the
        !! seconds rounded to `precision + 1` decimals -- **one more than `precision`**, the
        !! declination's decimals at the same resolution, since a second of time is 15 arcseconds.
        !! Seconds that round to 60 carry into the minutes and on into the hours, and 24 hours wrap
        !! to `00`. `sep` goes between the fields: `":"` or `" "`, or `"hms"` for the lettered form
        !! `10h21m30.550s`. **A NaN `ra` is the text `nan`** and raises no flag; an infinite one is
        !! `nan` too and raises `IEEE_INVALID`, as its wrap must. **A `sep` or `precision` outside
        !! those sets stops the program.** `pure`.
        pure module subroutine pf_ra2str(ra, text, sep, precision)
            real(real64), intent(in) :: ra !! right ascension, degrees; any value.
            character(len=:), allocatable, intent(out) :: text !! the text.
            character(len=*), intent(in), optional :: sep !! `":"` (the default), `" "` or `"hms"`.
            integer, intent(in), optional :: precision !! in `[0, 9]`, default 2; the seconds take one more decimal.
        end subroutine pf_ra2str

        !> A declination in degrees as sexagesimal text: `"+41:16:09.00"`.
        !!
        !! Written `+dd:mm:ss.ss` with its sign always, every field below ten zero-padded, the
        !! arcseconds rounded to `precision` decimals; seconds that round to 60 carry into the
        !! minutes and on into the degrees. The sign is `pf_deg2dms`'s, so `-0.0` is `+`, and a
        !! declination beyond 90 is written as given. `sep` goes between the fields: `":"` or `" "`,
        !! or `"hms"` for the lettered form `+41d16m09.00s`. **A NaN `dec` is the text `nan`** and
        !! raises no flag; an infinite one is `nan` too and raises `IEEE_INVALID`, and a magnitude
        !! of `huge(1)` degrees or more is `nan` without raising one. **A `sep` or `precision`
        !! outside those sets stops the program.** `pure`.
        pure module subroutine pf_dec2str(dec, text, sep, precision)
            real(real64), intent(in) :: dec !! declination, degrees.
            character(len=:), allocatable, intent(out) :: text !! the text.
            character(len=*), intent(in), optional :: sep !! `":"` (the default), `" "` or `"hms"`.
            integer, intent(in), optional :: precision !! the arcseconds' decimals, in `[0, 9]`, default 2.
        end subroutine pf_dec2str

        !> A position as one text, `pf_ra2str`'s and `pf_dec2str`'s joined by a blank:
        !! `"10:21:30.550 +41:16:09.00"`.
        !!
        !! `sep` and `precision` mean what they mean for the two, and a NaN coordinate is `nan` in
        !! its own half. **A `sep` or `precision` outside their sets stops the program.** `pure`.
        pure module subroutine pf_radec2str(ra, dec, text, sep, precision)
            real(real64), intent(in) :: ra !! right ascension, degrees; any value.
            real(real64), intent(in) :: dec !! declination, degrees.
            character(len=:), allocatable, intent(out) :: text !! the text.
            character(len=*), intent(in), optional :: sep !! `":"` (the default), `" "` or `"hms"`.
            integer, intent(in), optional :: precision !! the declination's arcsecond decimals, in `[0, 9]`, default 2.
        end subroutine pf_radec2str

        !> A right ascension read from sexagesimal text, in degrees.
        !!
        !! **Exactly three fields**, separated by colons (`10:21:30.55`), by blanks (`10 21 30.55`)
        !! or by the letters `h`, `m` and a closing `s` (`10h21m30.55s`, a blank allowed after each
        !! letter, either case): hours of one or two digits, minutes of one or two digits below 60,
        !! seconds of one or two digits below 60 with any number of decimals. No sign; blanks around
        !! the whole are ignored. Anything else -- a bare decimal number or two fields included --
        !! sets `ok` to `.false.` and never stops the program: the text is user data. **`ra` is not
        !! assigned when `ok` is `.false.`**, and must not be read then.
        !!
        !! **The hours are at most 24, and 24 only as the exact turn**: nothing is wrapped, so
        !! `24:00:00` reads as 360, while `24:00:00.001`, `25:00:00` and `99:00:00` are refused.
        !! `pure elemental`, so a whole column of text reads in one call.
        pure elemental module subroutine pf_str2ra(text, ra, ok)
            character(len=*), intent(in) :: text !! the text.
            real(real64), intent(out) :: ra !! the right ascension, degrees; assigned only when `ok`.
            logical, intent(out) :: ok !! whether the text was a right ascension.
        end subroutine pf_str2ra

        !> A declination read from sexagesimal text, in degrees.
        !!
        !! `pf_str2ra`'s three fields and three separators, with `d` in place of `h`, one to three
        !! digits of degrees, and **an optional leading `+` or `-`** applying to the whole angle, so
        !! `-00:30:00` is -0.5. Anything else sets `ok` to `.false.`, and **`dec` is not assigned
        !! then**.
        !!
        !! **The degrees are at most 90, and 90 only as the pole exactly**: `+90:00:00` and
        !! `-90:00:00` read, which is what `pf_dec2str` writes for a pole, while `+90:00:00.01` and
        !! `+91:00:00` are refused. A writer takes a declination beyond a pole and writes it as
        !! given; a reader does not take it back. `pure elemental`.
        pure elemental module subroutine pf_str2dec(text, dec, ok)
            character(len=*), intent(in) :: text !! the text.
            real(real64), intent(out) :: dec !! the declination, degrees; assigned only when `ok`.
            logical, intent(out) :: ok !! whether the text was a declination.
        end subroutine pf_str2dec

        !> A position read from one text: a right ascension as `pf_str2ra` reads it, then blanks, one
        !! comma or both, then a declination as `pf_str2dec` reads it.
        !!
        !! `"10:21:30.55 +41:16:09.0"`, `"10 21 30.55 +41 16 09.0"` and `"10h21m30.55s, +41d16m09s"`
        !! all read. Anything else -- one angle alone, two commas, trailing text, or either angle
        !! past its bound -- sets `ok` to `.false.`, and **neither output is assigned then**.
        !! `pure elemental`.
        pure elemental module subroutine pf_str2radec(text, ra, dec, ok)
            character(len=*), intent(in) :: text !! the text.
            real(real64), intent(out) :: ra !! the right ascension, degrees; assigned only when `ok`.
            real(real64), intent(out) :: dec !! the declination, degrees; assigned only when `ok`.
            logical, intent(out) :: ok !! whether the text was a position.
        end subroutine pf_str2radec
    end interface

    ! ---- Interfaces: helpers shared by the submodules ----
    !
    ! Implemented in submodule parquet_skycoord_rotate; private. The RA/Dec helpers carry the same
    ! two rules as `parquet_sphere`'s own, which that module keeps for its samplers: keep the two in
    ! step. The kernel and the selector test are here for the rotation object's submodule.

    interface
        !> Rotates one position by `m`: its unit vector, one matrix product, and back. A NaN
        !! coordinate is handed back itself, in both outputs, before anything touches it.
        pure module subroutine skc_rotate(m, lon, lat, lon_out, lat_out)
            real(real64), intent(in) :: m(3, 3) !! the rotation: a compile-time matrix, or a prepared object's.
            real(real64), intent(in) :: lon !! longitude, degrees; any value.
            real(real64), intent(in) :: lat !! latitude, degrees.
            real(real64), intent(out) :: lon_out !! the rotated longitude, degrees, in `[0, 360)`.
            real(real64), intent(out) :: lat_out !! the rotated latitude, degrees, in `[-90, 90]`.
        end subroutine skc_rotate

        !> Whether `system` is one of the five coordinate systems (`PF_COORD_UNKNOWN` is not).
        pure module function skc_is_system(system) result(ok)
            integer, intent(in) :: system !! the caller's selector.
            logical :: ok !! true for `PF_COORD_ICRS` through `PF_COORD_FK5`.
        end function skc_is_system

        !> The sine and cosine of a latitude in degrees, exactly `(+/-1, 0)` at `+/-90`.
        pure module subroutine skc_dec_sin_cos(lat, sl, cl)
            real(real64), intent(in) :: lat !! a latitude, degrees; not NaN.
            real(real64), intent(out) :: sl !! its sine.
            real(real64), intent(out) :: cl !! its cosine.
        end subroutine skc_dec_sin_cos

        !> A position in degrees as a unit vector, the pole exact. No NaN screen.
        pure module subroutine skc_radec_unit(lon, lat, v)
            real(real64), intent(in) :: lon !! longitude, degrees.
            real(real64), intent(in) :: lat !! latitude, degrees.
            real(real64), intent(out) :: v(3) !! the unit vector.
        end subroutine skc_radec_unit

        !> A nonzero, finite vector as `(lon, lat)` in degrees, the pole's `lon` 0.
        pure module subroutine skc_unit_radec(v, lon, lat)
            real(real64), intent(in) :: v(3) !! a direction; nonzero, finite, of moderate length.
            real(real64), intent(out) :: lon !! longitude, degrees, in `[0, 360)`.
            real(real64), intent(out) :: lat !! latitude, degrees, in `[-90, 90]`.
        end subroutine skc_unit_radec
    end interface

end module parquet_skycoord ! GCOVR_EXCL_LINE
