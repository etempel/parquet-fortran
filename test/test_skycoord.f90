!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Tests for `parquet_skycoord`: the coordinate-system rotations, the selector tokens, and the
!> frame-free RA/Dec geometry -- separations, offsets and position angles.
!>
!> Three layers:
!>
!>  1. **Golden rows** from `tools/generate_skycoord_reference.py`'s 60-digit model of each
!>     system's DEFINITION, asserted on the sky to a stated tolerance and never bit for bit: the
!>     library builds its matrices from rounded angles and computes with libm.
!>  2. **Identities the contract states**: orthonormality, round trips, the pole rules, the
!>     identity conversion as a copy, `pf_sky_convert` answering exactly what the named procedure
!>     it calls answers, and totality -- NaN in, NaN out, no flag raised.
!>  3. **The geometry that moved in with the module** -- `pf_angdist_deg`'s 60-digit table and its
!>     symmetries, and the offset and the position angle as inverses -- unchanged but for
!>     `pf_offset_radec`'s NaN rule.
!>
!> Everything is in memory and no test writes process-global state, so the suite runs in parallel.
module test_skycoord

    ! Narrow imports, never `use parquet`: `check_test_runner_partition` keeps this suite in the
    ! runner that reaches no `bind(C)` call. `parquet_healpix` supplies the vector separation that
    ! `pf_angdist_deg` is held equal to, and `parquet_random` the offset test's random positions.
    use parquet_skycoord
    use parquet_healpix, only: pf_angdist, pf_ang2vec
    use parquet_random, only: pf_random_at, pf_random_radec_at
    use test_skycoord_vectors
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan, ieee_get_flag, ieee_set_flag, &
        ieee_support_flag, ieee_invalid, ieee_divide_by_zero, ieee_overflow
    use iso_fortran_env, only: int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none
    private
    public :: collect_tests_skycoord

    !> The seed the offset test's random positions are drawn under.
    integer(int64), parameter :: SKY_SEED = 20260917_int64
    !> pi.
    real(real64), parameter :: PI = 3.14159265358979323846264338327950288_real64
    !> Radians per degree.
    real(real64), parameter :: DEG = PI / 180.0_real64
    !> A rotation against the 60-digit model, degrees on the sky. The library's own error is a few
    !! times 1e-14 -- the table's angles are rounded to doubles, then the matrices and libm round
    !! again -- and the cheapest wrong answer, the frame bias dropped, misses by 6e-6.
    real(real64), parameter :: ROT_TOL = 1.0e-12_real64
    !> The four systems, in selector order.
    integer, parameter :: SYSTEMS(4) = [PF_COORD_ICRS, PF_COORD_GALACTIC, PF_COORD_ECLIPTIC, PF_COORD_SUPERGALACTIC]

contains

    !> The suite's tests.
    subroutine collect_tests_skycoord(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the collected tests
        testsuite = [ &
            new_unittest("the selectors are the ones the golden rows were generated for", &
                         test_selectors_match_the_model), &
            new_unittest("ICRS and Galactic, both ways, against the 60-digit model", &
                         test_icrs_galactic_golden), &
            new_unittest("ICRS and ecliptic, both ways, against the 60-digit model, frame bias included", &
                         test_ecliptic_golden), &
            new_unittest("supergalactic against Galactic and ICRS, both ways, against the 60-digit model", &
                         test_supergalactic_golden), &
            new_unittest("pf_sky_convert's pairs with no named procedure against the 60-digit model", &
                         test_convert_unnamed_pairs_golden), &
            new_unittest("every rotation takes the three axes to a right-handed orthonormal set", &
                         test_matrices_are_orthonormal), &
            new_unittest("every rotation round-trips over a dense grid, poles included", &
                         test_round_trips), &
            new_unittest("a pole converts alike from any longitude, and a system's own pole comes back at 90", &
                         test_poles_are_exact), &
            new_unittest("a NaN coordinate gives NaN results and raises no flag, in every form", &
                         test_nan_propagates_quietly), &
            new_unittest("a latitude beyond 90 is read as the direction it names", &
                         test_out_of_range_dec_is_a_direction), &
            new_unittest("no finite edge case raises invalid, divide-by-zero or overflow", &
                         test_no_ieee_exception_on_finite_input), &
            new_unittest("a selector's token round-trips, case-insensitively, and an unknown token is the sentinel", &
                         test_system_tokens_round_trip), &
            new_unittest("pf_sky_convert from a system to itself returns its input by copy", &
                         test_convert_identity), &
            new_unittest("pf_sky_convert answers exactly what the named procedure it calls answers", &
                         test_convert_dispatches_to_named), &
            new_unittest("golden rows: pf_offset_radec and pf_position_angle_deg against the 60-digit model", &
                         test_offset_golden), &
            new_unittest("the position angle inverts the offset, at the poles and the cardinal points too", &
                         test_offset_and_position_angle), &
            new_unittest("pf_offset_radec gives NaN results for a NaN argument and raises no flag", &
                         test_offset_nan_propagates_quietly), &
            new_unittest("angdist_deg reproduces a 60-digit evaluation on every edge case", &
                         test_angdist_deg_reference), &
            new_unittest("angdist_deg agrees with angdist, and keeps its four symmetries", &
                         test_angdist_deg_agrees), &
            new_unittest("angdist_deg is total: NaN in, NaN out", test_angdist_deg_total) &
            ]
    end subroutine collect_tests_skycoord

    ! ================================================================================
    ! Golden rows
    ! ================================================================================

    !> The selectors are the values the generator emitted its rows for, so a renumbering fails here
    !! and not as a table of plausible wrong rotations.
    subroutine test_selectors_match_the_model(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion

        call check(error, PF_COORD_UNKNOWN == sc_unknown .and. PF_COORD_ICRS == sc_icrs .and. &
            PF_COORD_GALACTIC == sc_galactic .and. PF_COORD_ECLIPTIC == sc_ecliptic .and. &
            PF_COORD_SUPERGALACTIC == sc_supergalactic, &
            "the PF_COORD_* selectors are not the ones test/test_skycoord_vectors.f90 was generated for")
    end subroutine test_selectors_match_the_model

    !> Every golden row between ICRS and Galactic, both ways, through `pf_icrs2gal` and `pf_gal2icrs`.
    subroutine test_icrs_galactic_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        call golden_pair(error, PF_COORD_ICRS, PF_COORD_GALACTIC, .false.)
    end subroutine test_icrs_galactic_golden

    !> Every golden row between ICRS and the ecliptic, both ways. The rows carry the frame bias, so
    !! a naive single rotation by the IAU 2006 obliquity misses them by about 21 milliarcseconds.
    subroutine test_ecliptic_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        call golden_pair(error, PF_COORD_ICRS, PF_COORD_ECLIPTIC, .false.)
    end subroutine test_ecliptic_golden

    !> Every golden row between supergalactic and Galactic, and between supergalactic and ICRS, both
    !! ways: a wrong pole or a wrong longitude zero point misses by degrees.
    subroutine test_supergalactic_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        call golden_pair(error, PF_COORD_GALACTIC, PF_COORD_SUPERGALACTIC, .false.)
        if (allocated(error)) return
        call golden_pair(error, PF_COORD_ICRS, PF_COORD_SUPERGALACTIC, .false.)
    end subroutine test_supergalactic_golden

    !> The four ordered pairs `pf_sky_convert` serves with a pair matrix of its own -- Galactic and
    !! ecliptic, ecliptic and supergalactic -- against the model, which composes the two definitions
    !! at 60 digits rather than in doubles.
    subroutine test_convert_unnamed_pairs_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        call golden_pair(error, PF_COORD_GALACTIC, PF_COORD_ECLIPTIC, .true.)
        if (allocated(error)) return
        call golden_pair(error, PF_COORD_ECLIPTIC, PF_COORD_SUPERGALACTIC, .true.)
    end subroutine test_convert_unnamed_pairs_golden

    !> Every golden row between systems `a` and `b`, both ways, through the named procedure or through
    !! `pf_sky_convert`. Each longitude must be in `[0, 360)`; the worst gap on the sky must be within
    !! `ROT_TOL`; and at least one value must differ from its 60-digit reference in some bit, which a
    !! table read back out of a Fortran run never would.
    subroutine golden_pair(error, a, b, via_convert)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, intent(in) :: a                              !! one system
        integer, intent(in) :: b                              !! the other
        logical, intent(in) :: via_convert                    !! call `pf_sky_convert`, not the named procedure
        real(real64) :: lon, lat, got_lon, got_lat, want_lon, want_lat, worst
        integer :: k, nrow
        logical :: differs, found, in_range
        character(len=200) :: msg

        worst = 0.0_real64
        nrow = 0
        differs = .false.
        in_range = .true.
        do k = 1, n_srot
            if (.not. ((srot_from(k) == a .and. srot_to(k) == b) .or. (srot_from(k) == b .and. srot_to(k) == a))) cycle
            lon = transfer(srot_in_bits(2 * k - 1), 0.0_real64)
            lat = transfer(srot_in_bits(2 * k), 0.0_real64)
            if (via_convert) then
                call pf_sky_convert(lon, lat, srot_from(k), srot_to(k), got_lon, got_lat)
            else
                call named_rotation(srot_from(k), srot_to(k), lon, lat, got_lon, got_lat, found)
                call check(error, found, "the golden rows name a pair with no named procedure")
                if (allocated(error)) return
            end if
            want_lon = transfer(srot_out_bits(2 * k - 1), 0.0_real64)
            want_lat = transfer(srot_out_bits(2 * k), 0.0_real64)
            write (msg, '(a,i0,a,i0,a,2f12.6,a,2es24.16)') "system ", srot_from(k), " -> ", srot_to(k), " of", &
                lon, lat, " gives", got_lon, got_lat
            call check(error, sky_gap(got_lon, got_lat, want_lon, want_lat) <= ROT_TOL, trim(msg))
            if (allocated(error)) return
            worst = max(worst, sky_gap(got_lon, got_lat, want_lon, want_lat))
            in_range = in_range .and. got_lon >= 0.0_real64 .and. got_lon < 360.0_real64 .and. &
                abs(got_lat) <= 90.0_real64
            differs = differs .or. got_lon /= want_lon .or. got_lat /= want_lat
            nrow = nrow + 1
        end do
        ! Each unordered pair is two of the twelve ordered ones, and every ordered pair converts the
        ! same positions, so a pair with fewer rows lost some.
        write (msg, '(a,i0,a,i0,a,i0)') "systems ", a, " and ", b, ": golden rows found ", nrow
        call check(error, nrow > 0 .and. nrow == n_srot / 6, trim(msg))
        if (allocated(error)) return
        call check(error, in_range, "a rotation's longitude left [0, 360) or its latitude [-90, 90]")
        if (allocated(error)) return
        write (msg, '(a,i0,a,i0,a,es10.3,a)') "systems ", a, " and ", b, ": the worst row misses the model by ", worst, &
            " degrees on the sky"
        call check(error, worst <= ROT_TOL, trim(msg))
        if (allocated(error)) return
        call check(error, differs, &
            "not one rotated value differs from its 60-digit reference by even an ulp, which is what a table " // &
            "generated FROM the implementation would look like rather than one generated for it")
    end subroutine golden_pair

    ! ================================================================================
    ! The identities the contract states
    ! ================================================================================

    !> Each rotation takes the three axes to three unit vectors that are mutually perpendicular and
    !! right-handed: `M^T M = I` and `det M = +1`, read through the public procedures, where a
    !! mistyped matrix element would keep every answer plausible and break these at once.
    subroutine test_matrices_are_orthonormal(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: c1(3), c2(3), c3(3), lo, la, worst, hand
        integer :: i, j
        character(len=120) :: msg

        worst = 0.0_real64
        hand = 0.0_real64
        do i = 1, 4
            do j = 1, 4
                if (i == j) cycle
                call pf_sky_convert(0.0_real64, 0.0_real64, SYSTEMS(i), SYSTEMS(j), lo, la)
                c1 = vec_of(lo, la)
                call pf_sky_convert(90.0_real64, 0.0_real64, SYSTEMS(i), SYSTEMS(j), lo, la)
                c2 = vec_of(lo, la)
                call pf_sky_convert(0.0_real64, 90.0_real64, SYSTEMS(i), SYSTEMS(j), lo, la)
                c3 = vec_of(lo, la)
                worst = max(worst, abs(dot_product(c1, c1) - 1.0_real64), abs(dot_product(c2, c2) - 1.0_real64), &
                            abs(dot_product(c3, c3) - 1.0_real64), abs(dot_product(c1, c2)), abs(dot_product(c1, c3)), &
                            abs(dot_product(c2, c3)))
                hand = max(hand, maxval(abs(cross3(c1, c2) - c3)))
            end do
        end do
        write (msg, '(a,es10.3,a,es10.3)') "M^T M misses the identity by ", worst, "; x cross y misses z by ", hand
        call check(error, worst <= 1.0e-14_real64 .and. hand <= 1.0e-14_real64, trim(msg))
    end subroutine test_matrices_are_orthonormal

    !> There and back again, for every ordered pair, over a grid from pole to pole that includes a
    !! longitude below 0 and one past a turn. An inverse that is not the transpose misses by degrees.
    subroutine test_round_trips(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: lon, lat, a, b, lon2, lat2, worst
        integer :: i, j, p, q, ncase
        character(len=120) :: msg

        worst = 0.0_real64
        ncase = 0
        do i = 1, 4
            do j = 1, 4
                if (i == j) cycle
                do p = -2, 49
                    lon = 7.5_real64 * real(p, real64)
                    do q = 0, 24
                        lat = -90.0_real64 + 7.5_real64 * real(q, real64)
                        call pf_sky_convert(lon, lat, SYSTEMS(i), SYSTEMS(j), a, b)
                        call pf_sky_convert(a, b, SYSTEMS(j), SYSTEMS(i), lon2, lat2)
                        worst = max(worst, sky_gap(lon2, lat2, lon, lat))
                        ncase = ncase + 1
                    end do
                end do
            end do
        end do
        call check(error, ncase == 12 * 52 * 25, "the round-trip grid did not run in full")
        if (allocated(error)) return
        write (msg, '(a,es10.3,a)') "a round trip misses its start by ", worst, " degrees on the sky"
        call check(error, worst <= ROT_TOL, trim(msg))
    end subroutine test_round_trips

    !> The pole rules, both ways round.
    !!
    !! **In**: a latitude of exactly +/-90 is the pole whatever the longitude says, so every
    !! longitude naming a pole converts to the same bits -- the same compiled procedure fed the same
    !! unit vector, which is why this one comparison is exact. Without the rule `cos(90 * pi/180)`
    !! is `6.1e-17` and the answer would drift with the longitude.
    !!
    !! **Out**: a system's own pole, taken into another system and brought back, comes back at a
    !! latitude of 90 to within rounding. Taking the latitude as `asin(z)` would bring it back
    !! `8.5e-7` degrees short, which is what `atan2(z, hypot(x, y))` exists to prevent.
    subroutine test_poles_are_exact(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64), parameter :: LONS(6) = [0.0_real64, 33.3_real64, 123.4_real64, 359.9_real64, -45.0_real64, &
                                              725.0_real64]
        real(real64) :: a0, b0, a, b, pl, pb, worst
        integer :: i, j, k, s
        logical :: same

        same = .true.
        worst = 0.0_real64
        do i = 1, 4
            do j = 1, 4
                if (i == j) cycle
                do s = -1, 1, 2
                    call pf_sky_convert(LONS(1), real(s, real64) * 90.0_real64, SYSTEMS(i), SYSTEMS(j), a0, b0)
                    do k = 2, size(LONS)
                        call pf_sky_convert(LONS(k), real(s, real64) * 90.0_real64, SYSTEMS(i), SYSTEMS(j), a, b)
                        same = same .and. transfer(a, 0_int64) == transfer(a0, 0_int64) .and. &
                            transfer(b, 0_int64) == transfer(b0, 0_int64)
                    end do
                    ! System i's pole, expressed in j, is (a0, b0); taken back into i it is the pole.
                    call pf_sky_convert(a0, b0, SYSTEMS(j), SYSTEMS(i), pl, pb)
                    worst = max(worst, abs(pb - real(s, real64) * 90.0_real64))
                end do
            end do
        end do
        call check(error, same, "a pole named by two longitudes converted to two different positions")
        if (allocated(error)) return
        call check(error, worst <= 1.0e-12_real64, "a system's pole came back off latitude +/-90 -- asin, not atan2?")
    end subroutine test_poles_are_exact

    !> A NaN coordinate comes back as NaN from every rotation, for every pair and through every form,
    !! and raises no `IEEE_INVALID` doing so -- read around the calls in this test's own body, since a
    !! flag read inside a helper reads quiet under nagfor. The identity returns its input by copy, so
    !! there the other coordinate comes back unchanged.
    subroutine test_nan_propagates_quietly(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: nan, a, b, ra(4), dec(4), l(4), bb(4)
        integer :: i, j
        logical :: saved, raised, can, ok

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        can = ieee_support_flag(ieee_invalid, nan)
        saved = .false.
        raised = .false.
        if (can) then
            call ieee_get_flag(ieee_invalid, saved)
            call ieee_set_flag(ieee_invalid, .false.)
        end if
        ok = .true.
        do i = 1, 4
            do j = 1, 4
                call pf_sky_convert(nan, 10.0_real64, SYSTEMS(i), SYSTEMS(j), a, b)
                if (i == j) then
                    ok = ok .and. a /= a .and. b == 10.0_real64
                else
                    ok = ok .and. a /= a .and. b /= b
                end if
                call pf_sky_convert(10.0_real64, nan, SYSTEMS(i), SYSTEMS(j), a, b)
                if (i == j) then
                    ok = ok .and. a == 10.0_real64 .and. b /= b
                else
                    ok = ok .and. a /= a .and. b /= b
                end if
            end do
        end do
        call pf_icrs2gal(nan, 0.0_real64, a, b)
        ok = ok .and. a /= a .and. b /= b
        call pf_gal2icrs(0.0_real64, nan, a, b)
        ok = ok .and. a /= a .and. b /= b
        call pf_icrs2ecl(nan, 0.0_real64, a, b)
        ok = ok .and. a /= a .and. b /= b
        call pf_ecl2icrs(0.0_real64, nan, a, b)
        ok = ok .and. a /= a .and. b /= b
        call pf_gal2sgal(nan, 0.0_real64, a, b)
        ok = ok .and. a /= a .and. b /= b
        call pf_sgal2gal(0.0_real64, nan, a, b)
        ok = ok .and. a /= a .and. b /= b
        call pf_icrs2sgal(nan, 0.0_real64, a, b)
        ok = ok .and. a /= a .and. b /= b
        call pf_sgal2icrs(0.0_real64, nan, a, b)
        ok = ok .and. a /= a .and. b /= b
        ! Over a column, a NaN touches its own row only.
        ra = [10.0_real64, nan, 30.0_real64, 40.0_real64]
        dec = [0.0_real64, 0.0_real64, nan, 20.0_real64]
        call pf_icrs2gal(ra, dec, l, bb)
        ok = ok .and. l(1) == l(1) .and. l(2) /= l(2) .and. l(3) /= l(3) .and. l(4) == l(4) .and. &
            bb(1) == bb(1) .and. bb(2) /= bb(2) .and. bb(3) /= bb(3) .and. bb(4) == bb(4)
        if (can) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, saved .or. raised)
        end if
        call check(error, .not. raised, "a NaN coordinate raised IEEE_INVALID in a rotation")
        if (allocated(error)) return
        call check(error, ok, "a NaN coordinate did not give NaN results, or touched another row")
    end subroutine test_nan_propagates_quietly

    !> A latitude beyond 90 is the direction it names -- `(lon, 100)` is `(lon + 180, 80)` -- in every
    !! rotation, where a validating implementation would abort instead.
    subroutine test_out_of_range_dec_is_a_direction(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: a1, b1, a2, b2, worst
        integer :: i, j

        worst = 0.0_real64
        do i = 1, 4
            do j = 1, 4
                if (i == j) cycle
                call pf_sky_convert(10.0_real64, 100.0_real64, SYSTEMS(i), SYSTEMS(j), a1, b1)
                call pf_sky_convert(190.0_real64, 80.0_real64, SYSTEMS(i), SYSTEMS(j), a2, b2)
                worst = max(worst, sky_gap(a1, b1, a2, b2))
                call pf_sky_convert(10.0_real64, -100.0_real64, SYSTEMS(i), SYSTEMS(j), a1, b1)
                call pf_sky_convert(190.0_real64, -80.0_real64, SYSTEMS(i), SYSTEMS(j), a2, b2)
                worst = max(worst, sky_gap(a1, b1, a2, b2))
            end do
        end do
        call check(error, worst <= ROT_TOL, "a latitude beyond +/-90 was not read as the direction it names")
    end subroutine test_out_of_range_dec_is_a_direction

    !> The shapes that would raise a floating-point exception if anything did -- a pole named by any
    !! longitude, the seam, a latitude beyond 90, coincident and antipodal pairs, an offset from a
    !! pole, a separation of 0, 180 and past it -- raise none, through every procedure here. The flags
    !! are read around the calls in this test's own body; under nagfor's `-ieee=stop` a raise would
    !! end the runner instead, which fails it just as surely.
    subroutine test_no_ieee_exception_on_finite_input(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64), parameter :: LONS(5) = [0.0_real64, 123.5_real64, 359.999_real64, -725.5_real64, 1085.25_real64]
        real(real64), parameter :: LATS(7) = [90.0_real64, -90.0_real64, 0.0_real64, 89.9_real64, -89.9_real64, &
                                              100.0_real64, -270.0_real64]
        real(real64) :: sink, a, b
        logical :: can(3), saved(3), raised(3)
        integer :: i, j, p, q

        can = [ieee_support_flag(ieee_invalid, 0.0_real64), ieee_support_flag(ieee_divide_by_zero, 0.0_real64), &
               ieee_support_flag(ieee_overflow, 0.0_real64)]
        saved = .false.
        raised = .false.
        if (can(1)) call ieee_get_flag(ieee_invalid, saved(1))
        if (can(2)) call ieee_get_flag(ieee_divide_by_zero, saved(2))
        if (can(3)) call ieee_get_flag(ieee_overflow, saved(3))
        if (can(1)) call ieee_set_flag(ieee_invalid, .false.)
        if (can(2)) call ieee_set_flag(ieee_divide_by_zero, .false.)
        if (can(3)) call ieee_set_flag(ieee_overflow, .false.)
        sink = 0.0_real64
        do i = 1, 4
            do j = 1, 4
                do p = 1, size(LONS)
                    do q = 1, size(LATS)
                        call pf_sky_convert(LONS(p), LATS(q), SYSTEMS(i), SYSTEMS(j), a, b)
                        sink = sink + a + b
                    end do
                end do
            end do
        end do
        ! pf_angdist_deg over the shapes tools/check_healpix_fptrap.sh held it to while it was
        ! parquet_healpix's: the seam, the poles, coincident and antipodal positions, and right
        ! ascensions turns outside [0, 360).
        sink = sink + pf_angdist_deg(359.999_real64, 0.0_real64, 0.001_real64, 0.0_real64)
        sink = sink + pf_angdist_deg(0.0_real64, 90.0_real64, 123.5_real64, -90.0_real64)
        sink = sink + pf_angdist_deg(45.0_real64, 45.0_real64, 45.0_real64, 45.0_real64)
        sink = sink + pf_angdist_deg(0.0_real64, 0.0_real64, 180.0_real64, 0.0_real64)
        sink = sink + pf_angdist_deg(-725.5_real64, 89.9_real64, 1085.25_real64, -89.9_real64)
        do p = 1, size(LONS)
            call pf_offset_radec(LONS(p), 90.0_real64, 30.0_real64, 250.0_real64, a, b)
            sink = sink + a + b
            call pf_offset_radec(LONS(p), -90.0_real64, 30.0_real64, 0.0_real64, a, b)
            sink = sink + a + b
            call pf_offset_radec(LONS(p), 0.0_real64, 400.0_real64, 180.0_real64, a, b)
            sink = sink + a + b
            sink = sink + pf_position_angle_deg(LONS(p), 90.0_real64, 250.0_real64, 90.0_real64)
            sink = sink + pf_position_angle_deg(LONS(p), 10.0_real64, LONS(p) + 720.0_real64, 10.0_real64)
            sink = sink + pf_position_angle_deg(LONS(p), -20.0_real64, LONS(p) + 180.0_real64, 20.0_real64)
        end do
        if (can(1)) call ieee_get_flag(ieee_invalid, raised(1))
        if (can(2)) call ieee_get_flag(ieee_divide_by_zero, raised(2))
        if (can(3)) call ieee_get_flag(ieee_overflow, raised(3))
        if (can(1)) call ieee_set_flag(ieee_invalid, saved(1) .or. raised(1))
        if (can(2)) call ieee_set_flag(ieee_divide_by_zero, saved(2) .or. raised(2))
        if (can(3)) call ieee_set_flag(ieee_overflow, saved(3) .or. raised(3))
        call check(error, sink == sink, "a finite edge case gave a NaN")
        if (allocated(error)) return
        call check(error, .not. raised(1), "a finite edge case raised IEEE_INVALID")
        if (allocated(error)) return
        call check(error, .not. raised(2), "a finite edge case raised IEEE_DIVIDE_BY_ZERO")
        if (allocated(error)) return
        call check(error, .not. raised(3), "a finite edge case raised IEEE_OVERFLOW")
    end subroutine test_no_ieee_exception_on_finite_input

    !> Every selector's token names it back, `PF_COORD_UNKNOWN` included; the lookup ignores case and
    !! surrounding blanks and knows astropy's frame names; and an unknown token is the sentinel, not
    !! an abort -- the text is user data.
    subroutine test_system_tokens_round_trip(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        character(len=:), allocatable :: name
        integer :: k
        integer, parameter :: ALL5(5) = [PF_COORD_UNKNOWN, PF_COORD_ICRS, PF_COORD_GALACTIC, PF_COORD_ECLIPTIC, &
                                         PF_COORD_SUPERGALACTIC]

        do k = 1, size(ALL5)
            call pf_coord_system_name(ALL5(k), name)
            call check(error, pf_coord_system_from_name(name) == ALL5(k), &
                "the token '" // name // "' does not name its selector back")
            if (allocated(error)) return
        end do
        call pf_coord_system_name(PF_COORD_GALACTIC, name)
        call check(error, name == "galactic", "PF_COORD_GALACTIC's token is '" // name // "', not 'galactic'")
        if (allocated(error)) return
        call pf_coord_system_name(PF_COORD_UNKNOWN, name)
        call check(error, name == "unknown", "PF_COORD_UNKNOWN's token is '" // name // "', not 'unknown'")
        if (allocated(error)) return
        call check(error, pf_coord_system_from_name("Galactic") == PF_COORD_GALACTIC .and. &
            pf_coord_system_from_name("  SUPERGALACTIC  ") == PF_COORD_SUPERGALACTIC .and. &
            pf_coord_system_from_name("ICRS") == PF_COORD_ICRS, "a token's case or surrounding blanks mattered")
        if (allocated(error)) return
        call check(error, pf_coord_system_from_name("BarycentricMeanEcliptic") == PF_COORD_ECLIPTIC .and. &
            pf_coord_system_from_name("ecliptic") == PF_COORD_ECLIPTIC, "astropy's ecliptic frame name was not understood")
        if (allocated(error)) return
        call check(error, pf_coord_system_from_name("fk4") == PF_COORD_UNKNOWN .and. &
            pf_coord_system_from_name("") == PF_COORD_UNKNOWN .and. &
            pf_coord_system_from_name("gal") == PF_COORD_UNKNOWN .and. &
            pf_coord_system_from_name("galactic2") == PF_COORD_UNKNOWN .and. &
            pf_coord_system_from_name("ga lactic") == PF_COORD_UNKNOWN, "an unknown token named a system")
    end subroutine test_system_tokens_round_trip

    !> From a system to itself the input comes back by copy, bit for bit: a longitude below 0 stays
    !! below 0, a latitude beyond 90 stays beyond it, a negative zero keeps its sign and a NaN its
    !! payload. The kernel would have wrapped, folded, and lost the sign.
    subroutine test_convert_identity(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: zero, negz, nan, a, b
        integer :: i
        logical :: ok

        ! A negative zero built at run time, and its sign bit asserted before anything relies on it.
        zero = 0.0_real64
        negz = -zero
        call check(error, transfer(negz, 0_int64) < 0_int64, "the fixture could not build a negative zero")
        if (allocated(error)) return
        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        ok = .true.
        do i = 1, 4
            call pf_sky_convert(-10.0_real64, 100.0_real64, SYSTEMS(i), SYSTEMS(i), a, b)
            ok = ok .and. transfer(a, 0_int64) == transfer(-10.0_real64, 0_int64) .and. &
                transfer(b, 0_int64) == transfer(100.0_real64, 0_int64)
            call pf_sky_convert(725.5_real64, negz, SYSTEMS(i), SYSTEMS(i), a, b)
            ok = ok .and. transfer(a, 0_int64) == transfer(725.5_real64, 0_int64) .and. &
                transfer(b, 0_int64) == transfer(negz, 0_int64)
            call pf_sky_convert(nan, 30.0_real64, SYSTEMS(i), SYSTEMS(i), a, b)
            ok = ok .and. transfer(a, 0_int64) == transfer(nan, 0_int64) .and. b == 30.0_real64
        end do
        call check(error, ok, "a conversion from a system to itself changed its input")
    end subroutine test_convert_identity

    !> For every pair with a named procedure, `pf_sky_convert` answers what that procedure answers, to
    !! the bit, over a grid of positions: it calls the procedure rather than keeping a second body in
    !! step with it, and this is what would notice the second body.
    subroutine test_convert_dispatches_to_named(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: lon, lat, a1, b1, a2, b2
        integer :: i, j, p, q, npair
        logical :: found, same

        same = .true.
        npair = 0
        do i = 1, 4
            do j = 1, 4
                if (i == j) cycle
                call named_rotation(SYSTEMS(i), SYSTEMS(j), 0.0_real64, 0.0_real64, a1, b1, found)
                if (.not. found) cycle
                npair = npair + 1
                do p = -1, 23
                    lon = 17.3_real64 * real(p, real64)
                    do q = 0, 16
                        lat = -90.0_real64 + 11.25_real64 * real(q, real64)
                        call named_rotation(SYSTEMS(i), SYSTEMS(j), lon, lat, a1, b1, found)
                        call pf_sky_convert(lon, lat, SYSTEMS(i), SYSTEMS(j), a2, b2)
                        same = same .and. transfer(a1, 0_int64) == transfer(a2, 0_int64) .and. &
                            transfer(b1, 0_int64) == transfer(b2, 0_int64)
                    end do
                end do
            end do
        end do
        call check(error, npair == 8, "the eight named rotations were not all found")
        if (allocated(error)) return
        call check(error, same, "pf_sky_convert differs from the named procedure for its pair")
    end subroutine test_convert_dispatches_to_named

    ! ================================================================================
    ! Offsets and position angles
    ! ================================================================================

    !> Every offset and position-angle row of `test/test_skycoord_vectors.f90`: offsets to 1e-11
    !! degrees on the sky, position angles to each row's own tolerance, which grows as the separation
    !! nears 0 or 180.
    subroutine test_offset_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: ra, dec, got, want, worst
        integer :: k
        logical :: differs
        character(len=160) :: msg

        ! Vacuity guard, as the rotation rows have: a table read back out of a Fortran run would
        ! agree with it to the bit everywhere.
        differs = .false.
        worst = 0.0_real64
        do k = 1, n_soff
            call pf_offset_radec(transfer(soff_in_bits(4 * k - 3), 0.0_real64), transfer(soff_in_bits(4 * k - 2), 0.0_real64), &
                                 transfer(soff_in_bits(4 * k - 1), 0.0_real64), transfer(soff_in_bits(4 * k), 0.0_real64), &
                                 ra, dec)
            worst = max(worst, sky_gap(ra, dec, transfer(soff_out_bits(2 * k - 1), 0.0_real64), &
                                       transfer(soff_out_bits(2 * k), 0.0_real64)))
            differs = differs .or. ra /= transfer(soff_out_bits(2 * k - 1), 0.0_real64) .or. &
                dec /= transfer(soff_out_bits(2 * k), 0.0_real64)
        end do
        write (msg, '(a,es10.3,a)') "pf_offset_radec misses the model by ", worst, " degrees on the sky (at most 1e-11)"
        call check(error, worst <= 1.0e-11_real64, trim(msg))
        if (allocated(error)) return

        do k = 1, n_spa
            got = pf_position_angle_deg(transfer(spa_in_bits(4 * k - 3), 0.0_real64), &
                                        transfer(spa_in_bits(4 * k - 2), 0.0_real64), &
                                        transfer(spa_in_bits(4 * k - 1), 0.0_real64), transfer(spa_in_bits(4 * k), 0.0_real64))
            want = transfer(spa_out_bits(k), 0.0_real64)
            write (msg, '(a,i0,a,es24.16,a,es24.16)') "position-angle row ", k, ": ", got, " against ", want
            call check(error, turn_gap(got, want) <= transfer(spa_tol_bits(k), 0.0_real64), trim(msg))
            differs = differs .or. got /= want
            if (allocated(error)) return
        end do
        call check(error, n_soff > 0 .and. n_spa > 0, "the offset and position-angle tables lost their rows")
        if (allocated(error)) return
        call check(error, differs, &
            "not one offset or position angle differs from its 60-digit reference by even an ulp, which is what " // &
            "a table generated FROM the implementation would look like rather than one generated for it")
    end subroutine test_offset_golden

    !> `pf_position_angle_deg(p0, pf_offset_radec(p0, pa, sep)) == pa` and the separation is `sep`, over
    !! random positions; then the poles, separations 0 and 180, the four cardinal directions, the
    !! coincidence rule, and a quiet NaN.
    subroutine test_offset_and_position_angle(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NPAIR = 10000_int64
        real(real64) :: ra0, dec0, pa, sep, ra, dec, worst_pa, worst_sep, nan, got
        integer(int64) :: k
        logical :: had_invalid, raised
        character(len=120) :: msg

        worst_pa = 0.0_real64
        worst_sep = 0.0_real64
        do k = 1_int64, NPAIR
            call pf_random_radec_at(SKY_SEED, k, ra0, dec0)
            pa = 360.0_real64 * pf_random_at(SKY_SEED, k, 3_int64)
            sep = 0.01_real64 + 179.98_real64 * pf_random_at(SKY_SEED, k, 4_int64)
            call pf_offset_radec(ra0, dec0, pa, sep, ra, dec)
            worst_pa = max(worst_pa, turn_gap(pf_position_angle_deg(ra0, dec0, ra, dec), pa) * sin(sep * DEG))
            worst_sep = max(worst_sep, abs(pf_angdist_deg(ra0, dec0, ra, dec) - sep))
        end do
        write (msg, '(a,es10.3,a,es10.3)') "the offset and the position angle are not inverses: angle ", worst_pa, &
            ", separation ", worst_sep
        call check(error, worst_pa <= 1.0e-9_real64 .and. worst_sep <= 1.0e-9_real64, trim(msg))
        if (allocated(error)) return

        ! The poles: the local frame follows ra0.
        call pf_offset_radec(10.0_real64, 90.0_real64, 30.0_real64, 5.0_real64, ra, dec)
        call check(error, sky_gap(ra, dec, 160.0_real64, 85.0_real64) <= 1.0e-12_real64, &
            "an offset from the north pole at position angle pa does not land at ra0 + 180 - pa")
        if (allocated(error)) return
        call pf_offset_radec(10.0_real64, -90.0_real64, 30.0_real64, 5.0_real64, ra, dec)
        call check(error, sky_gap(ra, dec, 40.0_real64, -85.0_real64) <= 1.0e-12_real64, &
            "an offset from the south pole at position angle pa does not land at ra0 + pa")
        if (allocated(error)) return
        ! Separations 0 and 180, and past 180 along the same great circle.
        call pf_offset_radec(725.0_real64, -33.0_real64, 77.0_real64, 0.0_real64, ra, dec)
        call check(error, sky_gap(ra, dec, 5.0_real64, -33.0_real64) <= 1.0e-12_real64, "a zero offset moves the position")
        if (allocated(error)) return
        call pf_offset_radec(10.0_real64, 20.0_real64, 77.0_real64, 180.0_real64, ra, dec)
        call check(error, sky_gap(ra, dec, 190.0_real64, -20.0_real64) <= 1.0e-12_real64, &
            "an offset of 180 degrees is not the antipode")
        if (allocated(error)) return
        call pf_offset_radec(10.0_real64, 90.0_real64, 90.0_real64, 250.0_real64, ra, dec)
        call check(error, sky_gap(ra, dec, 280.0_real64, -20.0_real64) <= 1.0e-12_real64, &
            "an offset of 250 degrees from the pole does not continue past the south pole")
        if (allocated(error)) return
        ! The cardinal directions on the equator.
        call pf_offset_radec(10.0_real64, 0.0_real64, 0.0_real64, 5.0_real64, ra, dec)
        call check(error, sky_gap(ra, dec, 10.0_real64, 5.0_real64) <= 1.0e-12_real64, "position angle 0 is not north")
        if (allocated(error)) return
        call pf_offset_radec(10.0_real64, 0.0_real64, 90.0_real64, 5.0_real64, ra, dec)
        call check(error, sky_gap(ra, dec, 15.0_real64, 0.0_real64) <= 1.0e-12_real64, "position angle 90 is not east")
        if (allocated(error)) return
        call pf_offset_radec(10.0_real64, 0.0_real64, 180.0_real64, 5.0_real64, ra, dec)
        call check(error, sky_gap(ra, dec, 10.0_real64, -5.0_real64) <= 1.0e-12_real64, "position angle 180 is not south")
        if (allocated(error)) return
        call pf_offset_radec(10.0_real64, 0.0_real64, 270.0_real64, 5.0_real64, ra, dec)
        call check(error, sky_gap(ra, dec, 5.0_real64, 0.0_real64) <= 1.0e-12_real64, "position angle 270 is not west")
        if (allocated(error)) return

        call check(error, pf_position_angle_deg(33.0_real64, 12.0_real64, 393.0_real64, 12.0_real64) == 0.0_real64 .and. &
            pf_position_angle_deg(10.0_real64, -90.0_real64, 250.0_real64, -90.0_real64) == 0.0_real64, &
            "a coincident pair -- one position written two turns apart, or two labels of one pole -- has an angle")
        if (allocated(error)) return
        got = pf_position_angle_deg(0.0_real64, 0.0_real64, 0.0_real64, -1.0_real64)
        call check(error, abs(got - 180.0_real64) <= 1.0e-12_real64, "due south is not position angle 180")
        if (allocated(error)) return

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        if (ieee_support_flag(ieee_invalid)) then
            call ieee_get_flag(ieee_invalid, had_invalid)
            call ieee_set_flag(ieee_invalid, .false.)
        end if
        got = pf_position_angle_deg(nan, 0.0_real64, 1.0_real64, 1.0_real64)
        if (ieee_support_flag(ieee_invalid)) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, had_invalid .or. raised)
            call check(error, .not. raised, "a NaN argument raised IEEE_INVALID in pf_position_angle_deg")
            if (allocated(error)) return
        end if
        call check(error, got /= got, "a NaN argument did not give a NaN position angle")
    end subroutine test_offset_and_position_angle

    !> A NaN in any of `pf_offset_radec`'s four arguments gives NaN results and raises no
    !! `IEEE_INVALID`: a null in a catalogue column is a routine event, not a caller mistake. Its two
    !! refusals -- a centre beyond a pole, a negative separation -- still abort, out of process
    !! (`sphere_offset_dec_out_of_range` and `sphere_offset_negative_separation`).
    subroutine test_offset_nan_propagates_quietly(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: nan, ra, dec, ra0(3), dec0(3), ra_o(3), dec_o(3)
        logical :: saved, raised, can, ok

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        can = ieee_support_flag(ieee_invalid, nan)
        saved = .false.
        raised = .false.
        if (can) then
            call ieee_get_flag(ieee_invalid, saved)
            call ieee_set_flag(ieee_invalid, .false.)
        end if
        call pf_offset_radec(nan, 20.0_real64, 30.0_real64, 1.0_real64, ra, dec)
        ok = ra /= ra .and. dec /= dec
        call pf_offset_radec(10.0_real64, nan, 30.0_real64, 1.0_real64, ra, dec)
        ok = ok .and. ra /= ra .and. dec /= dec
        call pf_offset_radec(10.0_real64, 20.0_real64, nan, 1.0_real64, ra, dec)
        ok = ok .and. ra /= ra .and. dec /= dec
        call pf_offset_radec(10.0_real64, 20.0_real64, 30.0_real64, nan, ra, dec)
        ok = ok .and. ra /= ra .and. dec /= dec
        ! Over a column, a NaN touches its own row only.
        ra0 = [10.0_real64, nan, 10.0_real64]
        dec0 = [20.0_real64, 20.0_real64, 20.0_real64]
        call pf_offset_radec(ra0, dec0, 90.0_real64, 5.0_real64, ra_o, dec_o)
        ok = ok .and. ra_o(1) == ra_o(1) .and. ra_o(2) /= ra_o(2) .and. ra_o(3) == ra_o(3) .and. &
            dec_o(2) /= dec_o(2) .and. dec_o(1) == dec_o(1)
        if (can) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, saved .or. raised)
        end if
        call check(error, .not. raised, "a NaN argument raised IEEE_INVALID in pf_offset_radec")
        if (allocated(error)) return
        call check(error, ok, "a NaN argument to pf_offset_radec did not give NaN results, or touched another row")
    end subroutine test_offset_nan_propagates_quietly

    ! ================================================================================
    ! Angular separation
    ! ================================================================================

    !> `pf_angdist_deg` against a 60-DIGIT evaluation of its own defining formula, case by case.
    !>
    !> **Every input below is exactly representable in binary64**, deliberately, so that what this
    !> measures is the formula's error and not the inputs'. That distinction is not pedantry: a
    !> separation of 1e-09 degrees written as the difference of two numbers near 360 carries about
    !> 1e-05 relative input error before any formula runs, so a test built from such inputs
    !> measures binary64 rather than the library and would pass against a much worse
    !> implementation.
    !>
    !> The cases are the ones that discriminate between the candidate formulas, and the bound is
    !> set so that the rejected ones fail it: `acos` of the dot product misses by 2.0e-07 degrees
    !> near the pole and the haversine by 9.5e-07 just inside antipodal, against the 1e-12 asserted
    !> here and the 7.8e-15 actually achieved. Reproduce the table with `mpmath` at 60 digits from
    !> `atan2(|v1 x v2|, v1.v2)`.
    subroutine test_angdist_deg_reference(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: nq = 15
        real(real64), parameter :: qa1(nq) = [ &
            359.5_real64, &
            360.0_real64, &
            720.25_real64, &
            -0.25_real64, &
            0.0_real64, &
            0.0_real64, &
            17.0_real64, &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            45.25_real64, &
            0.0_real64, &
            123.25_real64, &
            0.0_real64]
        real(real64), parameter :: qd1(nq) = [ &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            89.9999990463256835938_real64, &
            90.0_real64, &
            90.0_real64, &
            0.0_real64, &
            89.5_real64, &
            0.0_real64, &
            0.0_real64, &
            -45.25_real64, &
            -70.0_real64, &
            -31.5_real64, &
            90.0_real64]
        real(real64), parameter :: qa2(nq) = [ &
            0.25_real64, &
            0.000244140625_real64, &
            0.25_real64, &
            0.25_real64, &
            180.0_real64, &
            123.5_real64, &
            230.0_real64, &
            9.09494701772928237915e-13_real64, &
            0.000244140625_real64, &
            179.999999046325683594_real64, &
            180.0_real64, &
            45.25_real64, &
            0.5_real64, &
            124.75_real64, &
            0.0_real64]
        real(real64), parameter :: qd2(nq) = [ &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            0.0_real64, &
            89.9999990463256835938_real64, &
            45.0_real64, &
            90.0_real64, &
            0.0_real64, &
            89.5_real64, &
            0.0_real64, &
            0.0_real64, &
            -45.25_real64, &
            -70.0_real64, &
            -30.25_real64, &
            -90.0_real64]
        real(real64), parameter :: want(nq) = [ &
            0.75_real64, &
            0.000244140625_real64, &
            2.6172567683030617969e-59_real64, &
            0.5_real64, &
            0.0000019073486328125_real64, &
            45.0_real64, &
            6.27369360166581369792e-60_real64, &
            9.09494701772928237915e-13_real64, &
            0.00000213050183065608730972_real64, &
            179.999999046325683594_real64, &
            180.0_real64, &
            0.0_real64, &
            0.171009592506928063872_real64, &
            1.79438664828566646748_real64, &
            180.0_real64]
        integer :: k, nbad
        real(real64) :: got, worst
        character(len=160) :: detail

        nbad = 0
        worst = 0.0_real64
        detail = ""
        do k = 1, nq
            got = pf_angdist_deg(qa1(k), qd1(k), qa2(k), qd2(k))
            worst = max(worst, abs(got - want(k)))
            if (abs(got - want(k)) > 1.0e-12_real64) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,es22.15,a,es22.15)') &
                    "case ", k, " want=", want(k), " got=", got
            end if
        end do
        call check(error, nbad, 0, "angdist_deg disagreed with the 60-digit values: " // trim(detail))
        if (allocated(error)) return
        ! A vacuity guard on the table itself: an empty or accidentally-zeroed table would pass
        ! every comparison above.
        call check(error, nq >= 15, "the angdist_deg reference table lost its cases")
        if (allocated(error)) return
        ! Set the bound negative to have the run report the largest disagreement it found.
        call check(error, worst <= 1.0e-12_real64, "angdist_deg worst absolute error exceeded 1e-12 deg")
    end subroutine test_angdist_deg_reference

    !> `pf_angdist_deg` equals `pf_angdist` of the same two directions, and holds four symmetries.
    !>
    !> The agreement is the load-bearing half: the two procedures share no arithmetic beyond
    !> `atan2`, one working from unit vectors and the other in the frame where only the right
    !> ascension difference survives, so a defect in either would have to be reproduced exactly by
    !> the other to hide here.
    !>
    !> The four symmetries are each a property the doc-comment claims, and three of them are
    !> asserted EXACTLY rather than to a tolerance, because each is an exact identity of the
    !> arithmetic rather than an approximation:
    !>
    !> * swapping the two positions negates `dl` and `y2` and touches nothing else;
    !> * **reflecting both declinations** negates `sd1`, `sd2` and hence `y2`, and leaves `x`
    !>   alone -- which is why this module can offer a free RA/Dec entry point here and nowhere
    !>   else, the two live declination conventions differing by exactly this reflection;
    !> * adding whole turns to either right ascension is removed by the fold;
    !> * the elemental form broadcasts to the same values the scalar form gives.
    subroutine test_angdist_deg_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first fault.
        integer :: i, j, ncase
        real(real64) :: a1, d1, a2, d2, got, v1(3), v2(3), rad
        real(real64) :: acol(7), dcol(7), bulk(7)
        real(real64), parameter :: r2d = 180.0_real64 / pi

        ncase = 0
        do i = 0, 12
            a1 = 30.0_real64 * real(i, real64)
            d1 = -90.0_real64 + 15.0_real64 * real(i, real64)
            do j = 0, 12
                a2 = 27.5_real64 * real(j, real64)
                d2 = 90.0_real64 - 14.0_real64 * real(j, real64)
                call pf_ang2vec((90.0_real64 - d1) * pi / 180.0_real64, a1 * pi / 180.0_real64, v1)
                call pf_ang2vec((90.0_real64 - d2) * pi / 180.0_real64, a2 * pi / 180.0_real64, v2)
                call pf_angdist(v1, v2, rad)
                got = pf_angdist_deg(a1, d1, a2, d2)
                ncase = ncase + 1
                ! 1e-11 degrees is 4e-8 arcsec, and about four orders above the 2e-15 the two
                ! forms actually differ by; it is loose because the vector route reaches the same
                ! angle through four sine/cosine pairs where this reaches it through three, not
                ! because either is in doubt.
                call check(error, got, rad * r2d, "angdist_deg disagreed with angdist", &
                           thr=1.0e-11_real64)
                if (allocated(error)) return
                ! Swapping the two positions is an exact identity in real arithmetic and a
                ! few-ulp one in binary64, so this is asserted to a tolerance rather than to the
                ! bit. `y1` is `cos(dec2)*sin(dl)`, so the swap gives it the OTHER declination's
                ! cosine; the two expressions are different roundings of one quantity, and only
                ! the reflection and fold identities below survive exactly.
                call check(error, pf_angdist_deg(a2, d2, a1, d1), got, &
                           "angdist_deg was not symmetric in its two positions", thr=1.0e-12_real64)
                if (allocated(error)) return
                ! Reflecting both declinations: the mirrored convention, and the reason this
                ! procedure needs no frame argument.
                call check(error, pf_angdist_deg(a1, -d1, a2, -d2), got, &
                           "reflecting both declinations changed the separation", thr=0.0_real64)
                if (allocated(error)) return
                ! Whole turns of right ascension, in both directions and on both arguments.
                call check(error, pf_angdist_deg(a1 + 360.0_real64, d1, a2, d2), got, &
                           "adding a turn to the first RA changed the separation", thr=0.0_real64)
                if (allocated(error)) return
                call check(error, pf_angdist_deg(a1, d1, a2 - 720.0_real64, d2), got, &
                           "removing two turns from the second RA changed the separation", &
                           thr=0.0_real64)
                if (allocated(error)) return
            end do
        end do
        ! Without this the loops could be skipped entirely and every assertion above would go
        ! unexercised while the test still passed.
        call check(error, ncase, 169, "the angdist_deg sweep did not run its whole grid")
        if (allocated(error)) return

        ! Elemental: one call over arrays must give exactly what seven scalar calls give.
        acol = [0.0_real64, 45.0_real64, 90.0_real64, 180.0_real64, 270.0_real64, &
                359.5_real64, 123.25_real64]
        dcol = [0.0_real64, 30.0_real64, -30.0_real64, 89.0_real64, -89.0_real64, &
                0.5_real64, -31.5_real64]
        bulk = pf_angdist_deg(acol, dcol, 10.0_real64, -20.0_real64)
        do i = 1, 7
            call check(error, bulk(i), pf_angdist_deg(acol(i), dcol(i), 10.0_real64, -20.0_real64), &
                       "the elemental form did not match the scalar form", thr=0.0_real64)
            if (allocated(error)) return
        end do
        ! **Exact zero for a coincident pair is a documented guarantee, not a tolerance.** The
        ! formula alone does not deliver it wherever the compiler contracts `cd1*sd2 - sd1*cd2*cdl`
        ! into an FMA -- the two products are then rounded differently and the second's rounding
        ! error survives the cancellation -- so a guard in the procedure is what makes it true.
        ! nagfor returned 1.22e-15 degrees for the first pair below before that guard existed,
        ! while gfortran returned zero, which is precisely why this is asserted rather than assumed.
        call check(error, pf_angdist_deg(12.5_real64, 34.5_real64, 12.5_real64, 34.5_real64), &
                   0.0_real64, "a position was not exactly zero degrees from itself", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_angdist_deg(359.5_real64, -89.9_real64, 359.5_real64, -89.9_real64), &
                   0.0_real64, "a position near the south pole was not exactly zero from itself", &
                   thr=0.0_real64)
        if (allocated(error)) return
        ! Whole turns apart in RA is the same position, and the fold has to make it exactly zero
        ! rather than merely small.
        call check(error, pf_angdist_deg(10.0_real64, 20.0_real64, 370.0_real64, 20.0_real64), &
                   0.0_real64, "a whole turn of RA was not exactly zero degrees away", &
                   thr=0.0_real64)
        if (allocated(error)) return
        ! **At a POLE any two right ascensions name the same point**, and this one the formula
        ! cannot reach even in principle: `cos(90 * pi/180)` is 6.1e-17 rather than zero, so the
        ! arithmetic answers 6.2e-15 degrees on every compiler. It is a rule, and this asserts it.
        call check(error, pf_angdist_deg(0.0_real64, 90.0_real64, 123.0_real64, 90.0_real64), &
                   0.0_real64, "two positions at the north pole were not exactly coincident", &
                   thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_angdist_deg(0.0_real64, -90.0_real64, 123.0_real64, -90.0_real64), &
                   0.0_real64, "two positions at the south pole were not exactly coincident", &
                   thr=0.0_real64)
        if (allocated(error)) return
        ! The negative control for the guard: it must not swallow a real separation. A pair one
        ! ulp apart in declination, and a pair at the same declination but genuinely apart in RA.
        call check(error, pf_angdist_deg(30.0_real64, 45.0_real64, 30.0_real64, &
                                         nearest(45.0_real64, 1.0_real64)) > 0.0_real64, &
                   "the coincidence guard swallowed a one-ulp declination difference")
        if (allocated(error)) return
        call check(error, pf_angdist_deg(0.0_real64, 45.0_real64, 1.0e-9_real64, 45.0_real64) &
                   > 0.0_real64, "the coincidence guard swallowed a nanodegree RA difference")
        if (allocated(error)) return
        call check(error, pf_angdist_deg(0.0_real64, 89.0_real64, 180.0_real64, 89.0_real64) &
                   > 1.0_real64, "the coincidence guard fired just short of the pole")
    end subroutine test_angdist_deg_agrees

    !> `pf_angdist_deg` is total: it validates nothing, aborts on nothing, and propagates NaN.
    !>
    !> This is the module's rule for every elemental entry point, and it is asserted rather than
    !> assumed because the procedure it replaces downstream aborted on a NaN result instead. A
    !> caller who wants that check keeps it at their own boundary, where it runs once per array
    !> rather than once per element.
    subroutine test_angdist_deg_total(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first fault.
        real(real64) :: nan, got
        logical :: saved, raised, can_test

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        ! **The flag is saved and restored around the whole test**, because a NaN fixture is
        ! exactly the sort that raises one by accident and leaves a "Floating invalid operation
        ! occurred" line at STOP that names no test -- and clearing it is what turns the nuisance
        ! into the assertion below. `saved .or. raised` on the way out so a flag raised elsewhere
        ! in the run is neither hidden nor blamed on this call.
        can_test = ieee_support_flag(ieee_invalid, 0.0_real64)
        saved = .false.
        if (can_test) then
            call ieee_get_flag(ieee_invalid, saved)
            call ieee_set_flag(ieee_invalid, .false.)
        end if
        got = pf_angdist_deg(nan, 0.0_real64, 10.0_real64, 20.0_real64)
        ! **A quiet NaN must propagate quietly.** `anint(NaN)` -- which the RA fold reaches -- is
        ! an invalid operation and nagfor raises on it, so without the procedure's own guard this
        ! terminates any caller running with the traps unmasked, which is nagfor's default. Only
        ! a compiler that raises can see this, so nothing else in the fleet covers it.
        raised = .false.
        if (can_test) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, .false.)
        end if
        call check(error, .not. raised, &
                   "a NaN right ascension raised IEEE_INVALID, which terminates a caller whose " // &
                   "traps are unmasked")
        if (allocated(error)) then
            if (can_test) call ieee_set_flag(ieee_invalid, saved)
            return
        end if
        call check(error, ieee_is_nan(got), "a NaN right ascension did not give a NaN separation")
        if (allocated(error)) then
            if (can_test) call ieee_set_flag(ieee_invalid, saved)
            return
        end if
        ! **A NaN DECLINATION is a separate case from a NaN right ascension, and it is the one
        ! ifx fails.** The RA guard alone lets a declination NaN reach `sin`, which ifx vectorises
        ! into `__svml_sin2` -- not quiet on a NaN element. Both declinations are checked because
        ! that pairing is exactly what the vector call covers, so a guard that caught only the
        ! first would still raise on the second.
        got = pf_angdist_deg(0.0_real64, nan, 10.0_real64, 20.0_real64)
        raised = .false.
        if (can_test) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, .false.)
        end if
        call check(error, .not. raised, "a NaN declination raised IEEE_INVALID")
        if (allocated(error)) then
            if (can_test) call ieee_set_flag(ieee_invalid, saved)
            return
        end if
        call check(error, ieee_is_nan(got), "a NaN declination did not give a NaN separation")
        if (allocated(error)) then
            if (can_test) call ieee_set_flag(ieee_invalid, saved)
            return
        end if
        got = pf_angdist_deg(0.0_real64, 45.0_real64, 10.0_real64, nan)
        if (can_test) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, saved)
        end if
        call check(error, .not. raised, "a NaN second declination raised IEEE_INVALID")
        if (allocated(error)) return
        call check(error, ieee_is_nan(got), &
                   "a NaN second declination did not give a NaN separation")
        if (allocated(error)) return
        ! A declination outside [-90, 90] is read as the direction it names rather than refused:
        ! dec = 100 is the same direction as dec = 80 at the opposite right ascension.
        call check(error, pf_angdist_deg(0.0_real64, 100.0_real64, 180.0_real64, 80.0_real64), &
                   0.0_real64, "an out-of-range declination was not read as the direction it names", &
                   thr=1.0e-13_real64)
    end subroutine test_angdist_deg_total

    ! ================================================================================
    ! Helpers
    ! ================================================================================

    !> The named procedure for the pair `from -> to`, if the pair has one.
    subroutine named_rotation(from, to, lon, lat, lon_out, lat_out, found)
        integer, intent(in) :: from                 !! the input's system
        integer, intent(in) :: to                   !! the output's system
        real(real64), intent(in) :: lon             !! longitude, degrees
        real(real64), intent(in) :: lat             !! latitude, degrees
        real(real64), intent(out) :: lon_out        !! the converted longitude, degrees
        real(real64), intent(out) :: lat_out        !! the converted latitude, degrees
        logical, intent(out) :: found               !! whether the pair has a named procedure

        found = .true.
        if (from == PF_COORD_ICRS .and. to == PF_COORD_GALACTIC) then
            call pf_icrs2gal(lon, lat, lon_out, lat_out)
        else if (from == PF_COORD_GALACTIC .and. to == PF_COORD_ICRS) then
            call pf_gal2icrs(lon, lat, lon_out, lat_out)
        else if (from == PF_COORD_ICRS .and. to == PF_COORD_ECLIPTIC) then
            call pf_icrs2ecl(lon, lat, lon_out, lat_out)
        else if (from == PF_COORD_ECLIPTIC .and. to == PF_COORD_ICRS) then
            call pf_ecl2icrs(lon, lat, lon_out, lat_out)
        else if (from == PF_COORD_GALACTIC .and. to == PF_COORD_SUPERGALACTIC) then
            call pf_gal2sgal(lon, lat, lon_out, lat_out)
        else if (from == PF_COORD_SUPERGALACTIC .and. to == PF_COORD_GALACTIC) then
            call pf_sgal2gal(lon, lat, lon_out, lat_out)
        else if (from == PF_COORD_ICRS .and. to == PF_COORD_SUPERGALACTIC) then
            call pf_icrs2sgal(lon, lat, lon_out, lat_out)
        else if (from == PF_COORD_SUPERGALACTIC .and. to == PF_COORD_ICRS) then
            call pf_sgal2icrs(lon, lat, lon_out, lat_out)
        else
            found = .false.
            lon_out = 0.0_real64
            lat_out = 0.0_real64
        end if
    end subroutine named_rotation

    !> A position in degrees as a unit vector, computed here rather than by the library under test.
    pure function vec_of(lon, lat) result(v)
        real(real64), intent(in) :: lon             !! longitude, degrees
        real(real64), intent(in) :: lat             !! latitude, degrees
        real(real64) :: v(3)                        !! the unit vector
        v = [cos(lat * DEG) * cos(lon * DEG), cos(lat * DEG) * sin(lon * DEG), sin(lat * DEG)]
    end function vec_of

    !> The cross product of two 3-vectors.
    pure function cross3(a, b) result(c)
        real(real64), intent(in) :: a(3)            !! the first vector
        real(real64), intent(in) :: b(3)            !! the second vector
        real(real64) :: c(3)                        !! `a x b`
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
    end function cross3

    !> The gap between two angles in degrees, the short way round the turn.
    pure function turn_gap(a, b) result(g)
        real(real64), intent(in) :: a               !! one angle, degrees
        real(real64), intent(in) :: b               !! the other, degrees
        real(real64) :: g                           !! the gap, in `[0, 180]`
        g = modulo(abs(a - b), 360.0_real64)
        g = min(g, 360.0_real64 - g)
    end function turn_gap

    !> The on-sky gap between two positions in degrees: the latitude gap, or the longitude gap
    !! scaled by `cos(lat)`, whichever is larger.
    pure function sky_gap(ra, dec, ra_ref, dec_ref) result(g)
        real(real64), intent(in) :: ra              !! longitude under test, degrees
        real(real64), intent(in) :: dec             !! latitude under test, degrees
        real(real64), intent(in) :: ra_ref          !! the reference longitude, degrees
        real(real64), intent(in) :: dec_ref         !! the reference latitude, degrees
        real(real64) :: g                           !! the gap, degrees
        g = max(abs(dec - dec_ref), turn_gap(ra, ra_ref) * cos(dec_ref * DEG))
    end function sky_gap

end module test_skycoord
