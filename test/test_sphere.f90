!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Tests for `parquet_sphere`: sky polygons, points in HEALPix pixels and masks, and the RA/Dec
!> geometry -- conversions and the Fibonacci grid. The offset and the position angle are
!> `parquet_skycoord`'s, and so are their tests (`test/test_skycoord.f90`).
!>
!> Three layers, as for `parquet_random`'s own sphere family:
!>
!>  1. **Golden rows** from `tools/generate_sphere_reference.py`'s 60-digit model: containment answers
!>     and candidate counts exactly, everything the library computes with libm to a stated tolerance.
!>  2. **Cross-form agreement**: coordinate, stream and fill forms, both integer kinds, `do concurrent`,
!>     the free conversions against `pf_healpix_grid`'s and against `parquet_random`'s own twin.
!>  3. **Distributional gates, each with a negative control** the same gate must reject.
!>
!> Everything is in memory and no test writes process-global state -- the candidate counts come from
!> the `pure` debug entry points, not from a counter -- so the suite runs in parallel.
module test_sphere

    ! Narrow imports, never `use parquet`: `check_test_runner_partition` keeps this suite in the
    ! runner that reaches no `bind(C)` call.
    use parquet_sphere
    use parquet_random, only: pf_random_at, pf_random_key, pf_random_int_at, pf_random_direction_at, &
        pf_random_radec_at, pf_random_disc_at, pf_random_disc_radec_at, pf_random_pair_spare_at, &
        pf_random_disc_cap
    use parquet_healpix, only: pf_vec2pix_nest, pf_ring2nest, pf_angdist
    ! The sky tier's own pair, which this one's must equal bit for bit under PF_HP_DEC_NORTH.
    use parquet_skycoord, only: pf_radec2unit, pf_unit2radec
    use test_sphere_vectors
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_negative_inf, &
        ieee_get_flag, ieee_set_flag, ieee_support_flag, ieee_invalid
    use iso_fortran_env, only: int32, int64, real32, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none
    private
    public :: collect_tests_sphere

    !> The seed every test draws under.
    integer(int64), parameter :: SKY_SEED = 20260917_int64
    !> pi.
    real(real64), parameter :: PI = 3.14159265358979323846264338327950288_real64
    !> Radians per degree.
    real(real64), parameter :: DEG = PI / 180.0_real64
    !> A unit-vector component against the 60-digit model: 32 ulp of 1, absolute.
    real(real64), parameter :: VEC_TOL = 32.0_real64 * epsilon(1.0_real64)
    !> Two forms computing one value through one body agree to a few ulp, not to the bit: a compiler
    !! may inline one call site and not the other and round them differently.
    real(real64), parameter :: SAME = 4.0_real64 * epsilon(1.0_real64)
    !> The `+z` axis. Constant vectors reach explicit-shape dummies as named parameters, never as
    !! array constructors, which ifx copies through a temporary and reports on every call.
    real(real64), parameter :: ZAXIS(3) = [0.0_real64, 0.0_real64, 1.0_real64]
    !> A direction at declination 45, as a vector of length about `1.4e-300`.
    real(real64), parameter :: TINY45(3) = [1.0e-300_real64, 0.0_real64, 1.0e-300_real64]
    !> The south pole, at length 5.
    real(real64), parameter :: SOUTH5(3) = [0.0_real64, 0.0_real64, -5.0_real64]
    !> The zero vector.
    real(real64), parameter :: ZERO3(3) = [0.0_real64, 0.0_real64, 0.0_real64]

contains

    !> The suite's tests.
    subroutine collect_tests_sphere(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the collected tests
        testsuite = [ &
            new_unittest("golden rows: identifier, areas, containment, grid points and draws", &
                         test_sphere_golden), &
            new_unittest("pf_radec2vec and pf_vec2radec round-trip, name their frame, and agree with the grid's", &
                         test_radec2vec_round_trip), &
            new_unittest("parquet_skycoord's pf_radec2unit and pf_unit2radec are this pair under PF_HP_DEC_NORTH", &
                         test_skycoord_vector_pair_is_this_one), &
            new_unittest("pf_vec2radec reads parquet_random's directions and discs as its own RA/Dec twin does", &
                         test_radec_agrees_with_stage_one), &
            new_unittest("a prepared disc cap draws what the scalar form draws", &
                         test_disc_cap_matches_the_scalar_form), &
            new_unittest("a polygon's accessors before, after and between %init and %clear", &
                         test_polygon_init_and_accessors), &
            new_unittest("chart containment wraps RA into the polygon's range and refuses what is not a position", &
                         test_polygon_chart_contains), &
            new_unittest("great-circle containment excludes the antipode of every inside point", &
                         test_polygon_gc_contains), &
            new_unittest("polygon areas match closed forms and decompositions, orientation-free", &
                         test_polygon_area), &
            new_unittest("a crossing polygon is measured by its even-odd interior under both edge rules", &
                         test_polygon_self_intersecting), &
            new_unittest("strict= accepts a polygon that is not a short-way band, under both edge rules", &
                         test_polygon_strict_accepts), &
            new_unittest("polygon draws are uniform per solid angle and inside, with a chart-uniform control", &
                         test_polygon_random_is_uniform_and_inside), &
            new_unittest("a polygon draw takes 1/%acceptance candidates, and a whole-sky box takes one", &
                         test_polygon_acceptance_identity), &
            new_unittest("polygon stream, coordinate and fill forms are one grid, one block per point", &
                         test_polygon_tiers_agree), &
            new_unittest("region families are independent of each other and of parquet_random's, with a control", &
                         test_region_families_independent), &
            new_unittest("%contains and %random_at are pure: do concurrent gives the serial values", &
                         test_polygon_purity), &
            new_unittest("pixel draws stay in their pixel and are uniform over its children, with a control", &
                         test_pixel_random_in_pixel), &
            new_unittest("mask draws are uniform over the list, a duplicate counting twice, with a control", &
                         test_mask_uniform_over_pixels), &
            new_unittest("pixel and mask stream, coordinate and fill forms are one grid", &
                         test_pixel_mask_tiers_agree), &
            new_unittest("every int32 and int64 specific pair agrees, and a draw below 1 is draw 1", &
                         test_region_kinds_agree), &
            new_unittest("the Fibonacci grid is unit, spaced as astropy's, and its two forms are one grid", &
                         test_fibonacci_grid), &
            new_unittest("a mask too long for the spare bits falls back and still spans its list", &
                         test_mask_choice_fallback) &
            ]
    end subroutine collect_tests_sphere

    ! ================================================================================
    ! Golden rows
    ! ================================================================================

    !> Every golden row of `test/test_sphere_vectors.f90`.
    !!
    !! Exact: the identifier, every containment answer, every candidate count and every mask choice.
    !! To a tolerance: areas and acceptances to 1e-12 relative; the Fibonacci points to 1e-12 degrees
    !! in declination and to each row's tolerance in right ascension, which grows with the unwrapped
    !! longitude; a polygon draw to `POLY_TOL` degrees on the sky; a pixel or mask draw's vector to
    !! 32 ulp.
    subroutine test_sphere_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        !> A polygon draw on the sky: `asin` near a pole and the box's own rounding move a chart
        !! candidate by up to about `1e-14/cos(dec)` radians, and the cap rows sit within a degree of it.
        real(real64), parameter :: POLY_TOL = 1.0e-11_real64
        type(pf_sky_polygon), allocatable :: polys(:)
        type(pf_healpix_grid) :: grid
        real(real64) :: ra, dec, want_ra, want_dec, v(3), want, worst, tol, u1, u2
        logical :: differs, spare_ok
        real(real64), allocatable :: fra(:), fdec(:)
        integer(int64) :: ncand, ipix, choice, dd
        integer(int32), allocatable :: list(:)
        integer :: k, p, f, l, m
        character(len=160) :: msg

        ! Vacuity guard, accumulated over every tolerance-pinned group below and asserted at the
        ! end: a table generated FROM the implementation would agree with it to the bit everywhere.
        differs = .false.

        call check(error, pf_sky_region_algorithm == ssph_algorithm, &
            "pf_sky_region_algorithm is not the identifier the golden rows were generated for")
        if (allocated(error)) return

        allocate(polys(n_spoly))
        do p = 1, n_spoly
            f = spoly_first(p)
            l = f + spoly_count(p) - 1
            call polys(p)%init(transfer(spoly_ra_bits(f:l), [0.0_real64]), transfer(spoly_dec_bits(f:l), [0.0_real64]), &
                               spoly_rule(p))
            write (msg, '(a,i0,a,l1,a,l1)') "polygon ", p, ": %is_simple ", polys(p)%is_simple(), &
                " against ", spoly_simple(p)
            call check(error, polys(p)%is_simple() .eqv. spoly_simple(p), trim(msg))
            if (allocated(error)) return
            ! A self-intersecting polygon's area is measured on a lattice, not integrated, so its
            ! row carries a tolerance of its own: the lattice's discretisation error, not an ulp.
            tol = transfer(spoly_area_tol_bits(p), 0.0_real64)
            want = transfer(spoly_area_bits(p), 0.0_real64)
            write (msg, '(a,i0,a,es24.16,a,es24.16)') "polygon ", p, ": %area ", polys(p)%area(), " against ", want
            call check(error, abs(polys(p)%area() - want) <= tol * want, trim(msg))
            differs = differs .or. polys(p)%area() /= want
            if (allocated(error)) return
            want = transfer(spoly_acceptance_bits(p), 0.0_real64)
            write (msg, '(a,i0,a,es24.16,a,es24.16)') "polygon ", p, ": %acceptance ", polys(p)%acceptance(), &
                " against ", want
            call check(error, abs(polys(p)%acceptance() - want) <= tol * want, trim(msg))
            differs = differs .or. polys(p)%acceptance() /= want
            if (allocated(error)) return
        end do

        do k = 1, n_scont
            ra = transfer(scont_ra_bits(k), 0.0_real64)
            dec = transfer(scont_dec_bits(k), 0.0_real64)
            write (msg, '(a,i0,a,i0,a,2f14.8,a,l1)') "containment row ", k, " (polygon ", scont_poly(k), ", ", ra, dec, &
                ") should read ", scont_inside(k)
            call check(error, polys(scont_poly(k))%contains(ra, dec) .eqv. scont_inside(k), trim(msg))
            if (allocated(error)) return
        end do

        do k = 1, n_sfib
            allocate(fra(sfib_n(k)), fdec(sfib_n(k)))
            call pf_fibonacci_grid_radec(sfib_n(k), fra, fdec)
            want_ra = transfer(sfib_ra_bits(k), 0.0_real64)
            want_dec = transfer(sfib_dec_bits(k), 0.0_real64)
            write (msg, '(a,i0,a,i0,a,2es24.16)') "Fibonacci point ", sfib_k(k), " of ", sfib_n(k), ": ", &
                fra(sfib_k(k)), fdec(sfib_k(k))
            call check(error, abs(fdec(sfib_k(k)) - want_dec) <= 1.0e-12_real64 .and. &
                turn_gap(fra(sfib_k(k)), want_ra) <= transfer(sfib_tol_bits(k), 0.0_real64), trim(msg))
            differs = differs .or. fra(sfib_k(k)) /= want_ra .or. fdec(sfib_k(k)) /= want_dec
            deallocate(fra, fdec)
            if (allocated(error)) return
        end do

        worst = 0.0_real64
        do k = 1, n_spd
            call parquet_debug_sphere_polygon_draw(polys(spd_poly(k)), spd_seed(k), spd_stream(k), spd_draw(k), ra, dec, ncand)
            write (msg, '(a,i0,a,i0,a,i0,a)') "polygon draw row ", k, " took ", ncand, " candidates where the model took ", &
                spd_ncand(k), ": a label, a key derivation or the candidate order moved"
            call check(error, ncand == spd_ncand(k), trim(msg))
            if (allocated(error)) return
            worst = max(worst, sky_gap(ra, dec, transfer(spd_ra_bits(k), 0.0_real64), transfer(spd_dec_bits(k), 0.0_real64)))
            differs = differs .or. ra /= transfer(spd_ra_bits(k), 0.0_real64) .or. &
                dec /= transfer(spd_dec_bits(k), 0.0_real64)
        end do
        write (msg, '(a,es10.3,a)') "a polygon draw misses the model by ", worst, " degrees on the sky"
        call check(error, worst <= POLY_TOL, trim(msg))
        if (allocated(error)) return

        worst = 0.0_real64
        do k = 1, n_spx
            call grid%init(spx_nside(k), spx_scheme(k))
            call parquet_debug_sphere_pixel_draw(grid, spx_seed(k), spx_stream(k), spx_ipix(k), spx_draw(k), v, ncand)
            write (msg, '(a,i0,a,i0,a,i0)') "pixel draw row ", k, " took ", ncand, " candidates where the model took ", &
                spx_ncand(k)
            call check(error, ncand == spx_ncand(k), trim(msg))
            if (allocated(error)) return
            worst = max(worst, maxval(abs(v - transfer(spx_v_bits(3 * k - 2:3 * k), [0.0_real64]))))
            differs = differs .or. any(v /= transfer(spx_v_bits(3 * k - 2:3 * k), [0.0_real64]))
        end do
        write (msg, '(a,es10.3)') "a pixel draw misses the model by ", worst
        call check(error, worst <= VEC_TOL, trim(msg))
        if (allocated(error)) return

        worst = 0.0_real64
        do k = 1, n_smd
            m = smd_mask(k)
            call grid%init(smask_nside(m), smask_scheme(m))
            list = smask_pixels(smask_first(m):smask_first(m) + smask_count(m) - 1)
            ! The choice is recomputed the way the library makes it, not merely read back: the
            ! point's own block spares 22 bits and the choice comes out of those, so ONE enciphering
            ! carries both. `smd_draw` includes 0, which every entry point clamps to 1, so the key
            ! is derived from the clamped value here too.
            dd = max(smd_draw(k), 1_int64)
            call pf_random_pair_spare_at(pf_random_key(pf_random_key(smd_seed(k), ssph_label_mask_point), dd), &
                                         smd_stream(k), 1_int64, int(size(list), int64), u1, u2, choice, spare_ok)
            if (.not. spare_ok) then
                choice = pf_random_int_at(pf_random_key(smd_seed(k), ssph_label_mask_choice), smd_stream(k), &
                                          1_int64, int(size(list), int64), dd)
            end if
            call check(error, choice == smd_choice(k), &
                "the mask's choice of an entry is not the model's: its label, its draw or its packing moved")
            if (allocated(error)) return
            v = pf_random_mask_at(grid, smd_seed(k), smd_stream(k), list, smd_draw(k))
            call grid%vec2pix(v, ipix)
            call check(error, ipix == int(list(choice), int64), "a mask draw is not in the entry the model chose")
            if (allocated(error)) return
            worst = max(worst, maxval(abs(v - transfer(smd_v_bits(3 * k - 2:3 * k), [0.0_real64]))))
            differs = differs .or. any(v /= transfer(smd_v_bits(3 * k - 2:3 * k), [0.0_real64]))
        end do
        write (msg, '(a,es10.3)') "a mask draw misses the model by ", worst
        call check(error, worst <= VEC_TOL, trim(msg))
        if (allocated(error)) return

        ! Vacuity guard. Every group above is pinned to a tolerance, so a table READ BACK out of a
        ! Fortran run would pass each of them while agreeing to the bit everywhere -- which is what
        ! a table derived for the implementation, in 60-digit decimal, never does.
        call check(error, differs, &
            "not one golden value differs from its 60-digit reference by even an ulp, which is what a table " // &
            "generated FROM the implementation would look like rather than one generated for it")
    end subroutine test_sphere_golden

    ! ================================================================================
    ! Conversions
    ! ================================================================================

    !> **`parquet_skycoord`'s `pf_radec2unit`/`pf_unit2radec` are this module's pair under
    !! `PF_HP_DEC_NORTH`, bit for bit**, which is the promise that lets a caller needing vectors
    !! stay in the sky tier instead of compiling this one. The two are separate bodies in separate
    !! modules; nothing but this test keeps them from drifting apart, and a difference of an ulp in
    !! the last bit is exactly the kind nobody can explain later. Both directions, over the poles,
    !! a signed zero, the seam and a dense pseudorandom sweep.
    subroutine test_skycoord_vector_pair_is_this_one(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64), parameter :: HARD_RA(6) = [0.0_real64, 123.4_real64, 359.999_real64, 0.0_real64, &
                                                 180.0_real64, 45.0_real64]
        real(real64), parameter :: HARD_DEC(6) = [90.0_real64, -90.0_real64, 0.0_real64, -0.0_real64, &
                                                  89.9999999_real64, -12.5_real64]
        real(real64) :: ra, dec, v(3), w(3), a1, b1, a2, b2
        integer :: k, n_same
        character(len=140) :: msg

        n_same = 0
        do k = 1, 20000
            if (k <= size(HARD_RA)) then
                ra = HARD_RA(k)
                dec = HARD_DEC(k)
            else
                ra = 360.0_real64 * pf_random_at(SKY_SEED + 7_int64, k, 1_int64)
                dec = 180.0_real64 * pf_random_at(SKY_SEED + 7_int64, k, 2_int64) - 90.0_real64
            end if
            call pf_radec2unit(ra, dec, v)
            call pf_radec2vec(ra, dec, w, PF_HP_DEC_NORTH)
            call pf_unit2radec(w, a1, b1)
            call pf_vec2radec(w, a2, b2, PF_HP_DEC_NORTH)
            if (all(v == w) .and. a1 == a2 .and. b1 == b2) n_same = n_same + 1
        end do
        write (msg, '(a,i0,a)') "parquet_skycoord's vector pair agrees with this module's on only ", n_same, &
            " of 20000 positions, not all of them"
        call check(error, n_same == 20000, trim(msg))
        if (allocated(error)) return
        ! The mirrored frame is the documented negation and nothing else, so the two conventions
        ! cannot be confused for one another.
        call pf_radec2unit(33.0_real64, 44.0_real64, v)
        call pf_radec2vec(33.0_real64, 44.0_real64, w, PF_HP_DEC_SOUTH)
        call check(error, v(1) == w(1) .and. v(2) == w(2) .and. v(3) == -w(3), &
            "PF_HP_DEC_SOUTH differs from the sky tier's vector by something other than the sign of z")
    end subroutine test_skycoord_vector_pair_is_this_one

    !> `pf_radec2vec` and `pf_vec2radec`: a round trip, the frame, the grid's own layer, scale, the
    !! poles, the zero vector, infinities, and a quiet NaN.
    subroutine test_radec2vec_round_trip(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64), parameter :: RAS(6) = [0.0_real64, 17.25_real64, 180.0_real64, 359.999_real64, -30.0_real64, &
                                              400.0_real64]
        real(real64), parameter :: DECS(7) = [-90.0_real64, -60.0_real64, -1.0e-10_real64, 0.0_real64, 45.0_real64, &
                                               89.999999_real64, 90.0_real64]
        type(pf_healpix_grid) :: north, south
        real(real64) :: v(3), w(3), c3(3), ra, dec, ra2, dec2, nan, pinf, ninf, worst_vec, worst_deg
        integer :: a, b
        logical :: had_invalid, raised

        call north%init(4_int64, PF_HP_RING)
        call south%init(4_int64, PF_HP_RING, frame=PF_HP_DEC_SOUTH)
        worst_vec = 0.0_real64
        worst_deg = 0.0_real64
        do a = 1, size(RAS)
            do b = 1, size(DECS)
                call pf_radec2vec(RAS(a), DECS(b), v)
                call check(error, abs(norm2(v) - 1.0_real64) <= SAME, "pf_radec2vec did not return a unit vector")
                if (allocated(error)) return
                call pf_vec2radec(v, ra, dec)
                call check(error, ra >= 0.0_real64 .and. ra < 360.0_real64 .and. abs(dec - DECS(b)) <= 1.0e-12_real64, &
                    "pf_vec2radec(pf_radec2vec(ra, dec)) does not return the declination, or its RA leaves [0, 360)")
                if (allocated(error)) return
                if (abs(DECS(b)) == 90.0_real64) then
                    call check(error, v(1) == 0.0_real64 .and. v(2) == 0.0_real64 .and. ra == 0.0_real64, &
                        "a declination of +/-90 is not the pole (0, 0, +/-1) exactly, with right ascension 0")
                else
                    call check(error, turn_gap(ra, modulo(RAS(a), 360.0_real64)) * cos(DECS(b) * DEG) <= 1.0e-12_real64, &
                        "pf_vec2radec(pf_radec2vec(ra, dec)) does not return the right ascension")
                end if
                if (allocated(error)) return

                ! The SOUTH frame is the reflection in z, both ways.
                call pf_radec2vec(RAS(a), DECS(b), w, frame=PF_HP_DEC_SOUTH)
                call check(error, all(abs(w - [v(1), v(2), -v(3)]) <= SAME), &
                    "pf_radec2vec in PF_HP_DEC_SOUTH is not the NORTH vector reflected in z")
                if (allocated(error)) return
                call pf_vec2radec(w, ra2, dec2, frame=PF_HP_DEC_SOUTH)
                call check(error, abs(dec2 - dec) <= SAME * 90.0_real64 .and. abs(ra2 - ra) <= SAME * 360.0_real64, &
                    "pf_vec2radec in PF_HP_DEC_SOUTH does not invert pf_radec2vec in PF_HP_DEC_SOUTH")
                if (allocated(error)) return
                if (DECS(b) == 45.0_real64) then
                    call check(error, abs(w(3) - v(3)) > 1.0_real64, &
                        "control: the two frames give the same vector at dec 45, so the frame argument is ignored")
                    if (allocated(error)) return
                end if

                ! Against pf_healpix_grid's own RA/Dec layer, in both frames. The grid forms its vector as
                ! `pf_ang2vec(pi/2 -/+ dec, ra)`, so the two differ by rounding only -- by 6.1e-17 at a pole,
                ! where this module is exact by rule.
                call north%radec2vec(RAS(a), DECS(b), w)
                worst_vec = max(worst_vec, maxval(abs(w - v)))
                call south%radec2vec(RAS(a), DECS(b), w)
                call pf_radec2vec(RAS(a), DECS(b), v, frame=PF_HP_DEC_SOUTH)
                worst_vec = max(worst_vec, maxval(abs(w - v)))
                call south%vec2radec(v, ra2, dec2)
                call pf_vec2radec(v, ra, dec, frame=PF_HP_DEC_SOUTH)
                worst_deg = max(worst_deg, sky_gap(ra, dec, ra2, dec2))
            end do
        end do
        call check(error, worst_vec <= 4.0_real64 * epsilon(1.0_real64), &
            "pf_radec2vec disagrees with pf_healpix_grid%radec2vec by more than 4 ulp")
        if (allocated(error)) return
        call check(error, worst_deg <= 1.0e-12_real64, "pf_vec2radec disagrees with pf_healpix_grid%vec2radec")
        if (allocated(error)) return

        ! Scale: a direction is a direction at any length.
        call pf_vec2radec(TINY45, ra, dec)
        call check(error, ra == 0.0_real64 .and. abs(dec - 45.0_real64) <= 1.0e-12_real64, &
            "[1e-300, 0, 1e-300] is not the direction at declination 45")
        if (allocated(error)) return
        call pf_radec2vec(123.0_real64, -33.0_real64, v)
        w = 1.0e300_real64 * v
        call pf_vec2radec(w, ra, dec)
        w = 1.0e-300_real64 * v
        call pf_vec2radec(w, ra2, dec2)
        call check(error, sky_gap(ra, dec, 123.0_real64, -33.0_real64) <= 1.0e-12_real64 .and. &
            sky_gap(ra2, dec2, 123.0_real64, -33.0_real64) <= 1.0e-12_real64, "pf_vec2radec depends on the vector's length")
        if (allocated(error)) return
        call pf_vec2radec(SOUTH5, ra, dec)
        call check(error, ra == 0.0_real64 .and. dec == -90.0_real64, "the south pole is not (0, -90) exactly")
        if (allocated(error)) return
        call pf_vec2radec(ZERO3, ra, dec)
        call check(error, ra == 0.0_real64 .and. dec == 0.0_real64, "the zero vector does not answer (0, 0)")
        if (allocated(error)) return

        ! Infinities are the direction of the infinite components, with no flag raised.
        pinf = ieee_value(0.0_real64, ieee_positive_inf)
        ninf = ieee_value(0.0_real64, ieee_negative_inf)
        w = [pinf, 1.0_real64, 0.0_real64]
        call pf_vec2radec(w, ra, dec)
        call check(error, ra == 0.0_real64 .and. dec == 0.0_real64, "(Inf, 1, 0) is not the direction of +x")
        if (allocated(error)) return
        w = [ninf, 0.0_real64, pinf]
        call pf_vec2radec(w, ra, dec)
        call check(error, abs(ra - 180.0_real64) <= 1.0e-12_real64 .and. abs(dec - 45.0_real64) <= 1.0e-12_real64, &
            "(-Inf, 0, Inf) is not the direction (180, 45)")
        if (allocated(error)) return

        ! A NaN argument answers NaN and raises nothing: these are total, like pf_angdist_deg.
        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        if (ieee_support_flag(ieee_invalid)) then
            call ieee_get_flag(ieee_invalid, had_invalid)
            call ieee_set_flag(ieee_invalid, .false.)
        end if
        c3 = [1.0_real64, nan, 0.0_real64]
        call pf_radec2vec(nan, 10.0_real64, v)
        call pf_radec2vec(10.0_real64, nan, w)
        call pf_vec2radec(c3, ra, dec)
        if (ieee_support_flag(ieee_invalid)) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, had_invalid .or. raised)
            call check(error, .not. raised, "a NaN argument raised IEEE_INVALID in pf_radec2vec or pf_vec2radec")
            if (allocated(error)) return
        end if
        call check(error, all(v /= v) .and. all(w /= w) .and. ra /= ra .and. dec /= dec, &
            "a NaN argument did not give NaN results")
    end subroutine test_radec2vec_round_trip

    !> `pf_vec2radec` of `parquet_random`'s directions and discs is `parquet_random`'s own RA/Dec form.
    !!
    !! `parquet_random` keeps a private RA/Dec twin because it can import nothing; this is what holds
    !! the two together -- the reading of a vector, and, through the disc form about an RA/Dec centre,
    !! the conversion of the centre with its pole rule. A centre on a pole is included: there a
    !! conversion leaving `6.1e-17` behind would pick a different frame axis and a different point.
    subroutine test_radec_agrees_with_stage_one(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 10000_int64
        real(real64), parameter :: RA0(5) = [150.0_real64, 359.9_real64, 10.0_real64, 283.0_real64, 42.0_real64]
        real(real64), parameter :: DEC0(5) = [-89.0_real64, 10.0_real64, 90.0_real64, 61.0_real64, -90.0_real64]
        real(real64) :: v(3), c(3), ra, dec, ra2, dec2, worst
        integer(int64) :: k
        integer :: j
        character(len=120) :: msg

        worst = 0.0_real64
        do k = 1_int64, NDRAW
            v = pf_random_direction_at(SKY_SEED, k)
            call pf_vec2radec(v, ra, dec)
            call pf_random_radec_at(SKY_SEED, k, ra2, dec2)
            worst = max(worst, sky_gap(ra, dec, ra2, dec2))
            j = int(modulo(k, 5_int64)) + 1
            call pf_radec2vec(RA0(j), DEC0(j), c)
            v = pf_random_disc_at(SKY_SEED, k, c, 2.5_real64 * DEG, 3_int64)
            call pf_vec2radec(v, ra, dec)
            call pf_random_disc_radec_at(SKY_SEED, k, RA0(j), DEC0(j), 2.5_real64, ra2, dec2, 3_int64)
            worst = max(worst, sky_gap(ra, dec, ra2, dec2))
        end do
        write (msg, '(a,es10.3,a)') "pf_vec2radec and parquet_random's RA/Dec forms differ by ", worst, &
            " degrees (at most 1e-9)"
        call check(error, worst <= 1.0e-9_real64, trim(msg))
    end subroutine test_radec_agrees_with_stage_one

    !> `pf_random_disc_cap` draws what the scalar form draws, and says whether it is prepared.
    !!
    !! The cap exists so a loop over one disc validates and builds its frame ONCE instead of per
    !! draw, so what has to hold is that moving that work out of the loop changes no draw: `%at`
    !! is held to `pf_random_disc_at` BIT FOR BIT, both for a full disc and for a ring with an
    !! inner radius. A frame built differently -- a different `e1`, a different normalisation --
    !! answers a different point on the same circle and nothing but this equality would see it.
    !!
    !! `%is_set` is asserted in both directions around `%prepare`, and it is the binding with no
    !! other caller anywhere: `pf_random_disc_at` reaches `%prepare` and `%at` internally, so
    !! those two are exercised by every disc draw in this file, and the accessor beside them is
    !! reached by nothing. `%prepare` may be re-run, so the last block prepares a second disc over
    !! the first and checks the draws follow the new one.
    subroutine test_disc_cap_matches_the_scalar_form(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_random_disc_cap) :: cap
        real(real64), parameter :: RA0 = 150.0_real64, DEC0 = -89.0_real64
        real(real64), parameter :: RA1 = 10.0_real64, DEC1 = 90.0_real64
        real(real64) :: c(3), c2(3), v(3), w(3)
        integer(int64) :: k
        integer :: bad

        call pf_radec2vec(RA0, DEC0, c)
        call check(error, .not. cap%is_set(), "an untouched cap must report itself unprepared")
        if (allocated(error)) return

        call cap%prepare(c, 2.5_real64 * DEG)
        call check(error, cap%is_set(), "and a prepared one must report itself prepared")
        if (allocated(error)) return
        bad = 0
        do k = 1_int64, 200_int64
            v = cap%at(SKY_SEED, k, 3_int64)
            w = pf_random_disc_at(SKY_SEED, k, c, 2.5_real64 * DEG, 3_int64)
            if (any(v /= w)) bad = bad + 1
        end do
        call check(error, bad == 0, "a prepared cap must draw exactly what the scalar form draws")
        if (allocated(error)) return

        ! A RING, which is the case the inner radius reaches and a full disc does not.
        call cap%prepare(c, 2.5_real64 * DEG, r_inner = 1.0_real64 * DEG)
        bad = 0
        do k = 1_int64, 200_int64
            v = cap%at(SKY_SEED, k, 3_int64)
            w = pf_random_disc_at(SKY_SEED, k, c, 2.5_real64 * DEG, 3_int64, r_inner = 1.0_real64 * DEG)
            if (any(v /= w)) bad = bad + 1
        end do
        call check(error, bad == 0, "and a prepared ring must draw what the scalar ring draws")
        if (allocated(error)) return

        ! `%prepare` is re-runnable: a second disc replaces the first, and the draws follow it.
        call pf_radec2vec(RA1, DEC1, c2)
        call cap%prepare(c2, 2.5_real64 * DEG)
        v = cap%at(SKY_SEED, 1_int64, 3_int64)
        w = pf_random_disc_at(SKY_SEED, 1_int64, c2, 2.5_real64 * DEG, 3_int64)
        call check(error, all(v == w), "re-preparing a cap must move it to the new disc")
        if (allocated(error)) return
        call check(error, any(v /= pf_random_disc_at(SKY_SEED, 1_int64, c, 2.5_real64 * DEG, 3_int64)), &
                   "and away from the old one, or the re-prepare did nothing")

    end subroutine test_disc_cap_matches_the_scalar_form

    ! ================================================================================
    ! The polygon
    ! ================================================================================

    !> A polygon's accessors: an unbuilt one, a built one of each rule, `%clear` and a second `%init`,
    !! and an acceptance recomputed here from the bounding region's own formula.
    subroutine test_polygon_init_and_accessors(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_sky_polygon) :: poly, copy
        real(real64) :: ra_lo, ra_hi, dec_lo, dec_hi, box, c(3), s(3), u(3), radius, cap
        real(real64), parameter :: GRA(3) = [20.0_real64, 100.0_real64, 50.0_real64]
        real(real64), parameter :: GDEC(3) = [5.0_real64, 10.0_real64, 80.0_real64]
        integer :: k

        call check(error, .not. poly%is_set() .and. poly%size() == 0_int64 .and. poly%edges() == -1, &
            "an unbuilt polygon does not report unset, 0 vertices and edge rule -1")
        if (allocated(error)) return

        call poly%init([350.0_real64, 370.0_real64, 370.0_real64, 350.0_real64], &
                       [-10.0_real64, -10.0_real64, 10.0_real64, 30.0_real64])
        call check(error, poly%is_set() .and. poly%size() == 4_int64 .and. poly%edges() == PF_EDGE_RADEC, &
            "a built chart polygon does not report set, 4 vertices and PF_EDGE_RADEC")
        if (allocated(error)) return
        call poly%bounds(ra_lo, ra_hi, dec_lo, dec_hi)
        call check(error, ra_lo == 350.0_real64 .and. ra_hi == 370.0_real64 .and. dec_lo == -10.0_real64 .and. &
            dec_hi == 30.0_real64, "%bounds is not the vertex box with right ascension as written")
        if (allocated(error)) return
        box = 20.0_real64 * DEG * 2.0_real64 * cos(10.0_real64 * DEG) * sin(20.0_real64 * DEG)
        call check(error, abs(poly%acceptance() - poly%area() / box) <= 1.0e-12_real64, &
            "a chart polygon's %acceptance is not its area over its RA/Dec box")
        if (allocated(error)) return
        call check(error, abs(poly%area_deg2() - poly%area() / DEG**2) <= 1.0e-12_real64 * poly%area_deg2(), &
            "%area_deg2 is not %area in square degrees")
        if (allocated(error)) return

        copy = poly
        call check(error, copy%contains(0.0_real64, 0.0_real64) .and. copy%size() == 4_int64, &
            "an assigned copy of a polygon does not answer as the original")
        if (allocated(error)) return

        call poly%clear()
        call check(error, .not. poly%is_set() .and. poly%size() == 0_int64 .and. poly%edges() == -1, &
            "%clear does not return the polygon to its unbuilt state")
        if (allocated(error)) return
        call poly%init(GRA, GDEC, PF_EDGE_GREAT_CIRCLE)
        call check(error, poly%is_set() .and. poly%size() == 3_int64 .and. poly%edges() == PF_EDGE_GREAT_CIRCLE, &
            "a polygon built again after %clear does not report its new vertices and rule")
        if (allocated(error)) return
        ! The cap: about the normalised vertex sum, through the farthest vertex.
        s = 0.0_real64
        do k = 1, 3
            s = s + vec_of(GRA(k), GDEC(k))
        end do
        c = s / norm2(s)
        radius = 0.0_real64
        do k = 1, 3
            u = vec_of(GRA(k), GDEC(k))
            radius = max(radius, atan2(norm2([u(2) * c(3) - u(3) * c(2), u(3) * c(1) - u(1) * c(3), &
                                              u(1) * c(2) - u(2) * c(1)]), dot_product(u, c)))
        end do
        cap = 4.0_real64 * PI * sin(0.5_real64 * radius)**2
        call check(error, abs(poly%acceptance() - poly%area() / cap) <= 1.0e-12_real64, &
            "a great-circle polygon's %acceptance is not its area over the cap through its farthest vertex")
        if (allocated(error)) return
        call check(error, copy%edges() == PF_EDGE_RADEC .and. copy%contains(5.0_real64, 0.0_real64), &
            "clearing and rebuilding the original changed its copy")
    end subroutine test_polygon_init_and_accessors

    !> Chart containment: the RA wrap into the polygon's own range, the polar cap, box edges, and
    !! arguments that name no position.
    subroutine test_polygon_chart_contains(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_sky_polygon) :: wrapped, short, cap
        real(real64) :: nan, inf

        call wrapped%init([350.0_real64, 370.0_real64, 370.0_real64, 350.0_real64], &
                          [-10.0_real64, -10.0_real64, 10.0_real64, 10.0_real64])
        call check(error, wrapped%contains(5.0_real64, 0.0_real64) .and. wrapped%contains(355.0_real64, 0.0_real64) .and. &
            .not. wrapped%contains(20.0_real64, 0.0_real64) .and. .not. wrapped%contains(340.0_real64, 0.0_real64), &
            "a polygon written 350..370 does not hold RA 5 and 355 and exclude 20 and 340")
        if (allocated(error)) return
        call check(error, wrapped%contains(-5.0_real64, 1.0_real64) .and. wrapped%contains(725.0_real64, -1.0_real64) .and. &
            wrapped%contains(365.0_real64, 9.0_real64), "a right ascension written outside [0, 360) is not wrapped")
        if (allocated(error)) return
        ! Control: the same RA range written the short way round is the complementary band.
        call short%init([350.0_real64, 10.0_real64, 10.0_real64, 350.0_real64], &
                        [-10.0_real64, -10.0_real64, 10.0_real64, 10.0_real64])
        call check(error, .not. short%contains(5.0_real64, 0.0_real64) .and. short%contains(180.0_real64, 0.0_real64), &
            "control: a polygon written 350, 10 is read as 350..370 -- vertices are not read as written")
        if (allocated(error)) return

        call cap%init([0.0_real64, 360.0_real64, 360.0_real64, 0.0_real64], [80.0_real64, 80.0_real64, 90.0_real64, 90.0_real64])
        call check(error, cap%contains(0.0_real64, 85.0_real64) .and. cap%contains(123.0_real64, 89.999_real64) .and. &
            cap%contains(359.99_real64, 80.001_real64) .and. .not. cap%contains(200.0_real64, 79.999_real64), &
            "the polar cap 0..360 x 80..90 does not hold every position above 80 and nothing below")
        if (allocated(error)) return
        call check(error, wrapped%contains(350.0_real64 + 1.0e-9_real64, 10.0_real64 - 1.0e-9_real64) .and. &
            .not. wrapped%contains(350.0_real64 - 1.0e-9_real64, 0.0_real64) .and. &
            .not. wrapped%contains(360.0_real64, 10.0_real64 + 1.0e-9_real64), &
            "a position 1e-9 degrees inside or outside the box's edges is decided wrongly")
        if (allocated(error)) return

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        inf = ieee_value(0.0_real64, ieee_positive_inf)
        call check(error, .not. wrapped%contains(nan, 0.0_real64) .and. .not. wrapped%contains(0.0_real64, nan) .and. &
            .not. wrapped%contains(inf, 0.0_real64) .and. .not. cap%contains(0.0_real64, 90.5_real64), &
            "a NaN, an infinite right ascension or a declination outside [-90, 90] reads inside")
    end subroutine test_polygon_chart_contains

    !> Great-circle containment: the antipode of an inside point is outside, an edge bulges as a great
    !! circle does, and the chart rule on the same vertices answers differently.
    subroutine test_polygon_gc_contains(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_sky_polygon) :: gc, chart
        real(real64) :: ra, dec
        integer(int64) :: k, inside

        call gc%init([10.0_real64, 60.0_real64, 50.0_real64, 0.0_real64], [0.0_real64, 5.0_real64, 40.0_real64, 30.0_real64], &
                     PF_EDGE_GREAT_CIRCLE)
        inside = 0_int64
        do k = 1_int64, 2000_int64
            call pf_random_radec_at(SKY_SEED, k, ra, dec)
            if (gc%contains(ra, dec)) then
                inside = inside + 1_int64
                call check(error, .not. gc%contains(modulo(ra + 180.0_real64, 360.0_real64), -dec), &
                    "the antipode of an inside position reads inside: the far hemisphere is not excluded")
                if (allocated(error)) return
            end if
        end do
        call check(error, inside > 50_int64, "vacuity guard: too few of the directions landed inside the quadrilateral")
        if (allocated(error)) return

        ! Two vertices at declination 40, 60 degrees apart: the great circle between them reaches 44.1
        ! degrees at its midpoint, the chart edge stays at 40.
        call gc%clear()
        call gc%init([0.0_real64, 60.0_real64, 60.0_real64, 0.0_real64], [0.0_real64, 0.0_real64, 40.0_real64, 40.0_real64], &
                     PF_EDGE_GREAT_CIRCLE)
        call chart%init([0.0_real64, 60.0_real64, 60.0_real64, 0.0_real64], [0.0_real64, 0.0_real64, 40.0_real64, 40.0_real64])
        call check(error, gc%contains(30.0_real64, 42.0_real64) .and. .not. chart%contains(30.0_real64, 42.0_real64), &
            "the great-circle edge between two positions at declination 40 does not bulge north past 42 at the midpoint")
    end subroutine test_polygon_gc_contains

    !> Areas: closed forms, a decomposition into rectangles and into fan triangles, reversed vertex
    !! order, a shift by a whole turn, and a small box beside the pole.
    subroutine test_polygon_area(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_sky_polygon) :: poly, rev, part
        real(real64), parameter :: LRA(6) = [0.0_real64, 30.0_real64, 30.0_real64, 15.0_real64, 15.0_real64, 0.0_real64]
        real(real64), parameter :: LDEC(6) = [0.0_real64, 0.0_real64, 15.0_real64, 15.0_real64, 30.0_real64, 30.0_real64]
        real(real64) :: total, want
        integer :: k

        call poly%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        want = 20.0_real64 * DEG * 2.0_real64 * sin(5.0_real64 * DEG)
        call check(error, abs(poly%area() - want) <= 1.0e-14_real64 * want, "a chart rectangle's area is not its closed form")
        if (allocated(error)) return
        call poly%clear()
        call poly%init([0.0_real64, 360.0_real64, 360.0_real64, 0.0_real64], [80.0_real64, 80.0_real64, 90.0_real64, 90.0_real64])
        want = 2.0_real64 * PI * 2.0_real64 * sin(5.0_real64 * DEG)**2
        call check(error, abs(poly%area() - want) <= 1.0e-13_real64 * want, "the polar cap's area is not 2*pi*(1 - sin 80)")
        if (allocated(error)) return
        call poly%clear()
        call poly%init([0.0_real64, 90.0_real64, 0.0_real64], [0.0_real64, 0.0_real64, 90.0_real64], PF_EDGE_GREAT_CIRCLE)
        call check(error, abs(poly%area() - 0.5_real64 * PI) <= 1.0e-14_real64, "the octant's area is not pi/2")
        if (allocated(error)) return
        call poly%clear()
        ! A small box beside the pole, where a Green's-theorem sum not taken about a reference latitude
        ! loses about four digits.
        call poly%init([40.0_real64, 41.0_real64, 41.0_real64, 40.0_real64], [89.9_real64, 89.9_real64, 89.91_real64, 89.91_real64])
        want = DEG * 2.0_real64 * cos(89.905_real64 * DEG) * sin(0.005_real64 * DEG)
        call check(error, abs(poly%area() - want) <= 1.0e-11_real64 * want, &
            "a 1 by 0.01 degree box beside the pole does not keep its area's digits")
        if (allocated(error)) return

        ! The chart L as two rectangles, and as fan triangles from its corner; the great-circle L as fan
        ! triangles from its corner, from which it is star-shaped.
        call poly%clear()
        call poly%init(LRA, LDEC)
        call part%init([0.0_real64, 30.0_real64, 30.0_real64, 0.0_real64], [0.0_real64, 0.0_real64, 15.0_real64, 15.0_real64])
        total = part%area()
        call part%clear()
        call part%init([0.0_real64, 15.0_real64, 15.0_real64, 0.0_real64], [15.0_real64, 15.0_real64, 30.0_real64, 30.0_real64])
        total = total + part%area()
        call part%clear()
        call check(error, abs(poly%area() - total) <= 1.0e-13_real64 * total, "the chart L is not the sum of its two rectangles")
        if (allocated(error)) return
        do k = 1, 2
            total = 0.0_real64
            call poly%clear()
            call poly%init(LRA, LDEC, k - 1)
            call fan_area(LRA, LDEC, k - 1, total)
            call check(error, abs(poly%area() - total) <= 1.0e-12_real64 * total, "an L is not the sum of its fan triangles")
            if (allocated(error)) return
            call rev%init(LRA(6:1:-1), LDEC(6:1:-1), k - 1)
            call check(error, abs(rev%area() - poly%area()) <= 1.0e-14_real64 * total, &
                "reversing an L's vertex order changes its area")
            call rev%clear()
            if (allocated(error)) return
        end do
        call poly%clear()
        call poly%init(LRA + 360.0_real64, LDEC)
        call rev%init(LRA, LDEC)
        call check(error, abs(poly%area() - rev%area()) <= 1.0e-13_real64 * rev%area(), &
            "a chart polygon shifted by a whole turn in right ascension changes its area")
    end subroutine test_polygon_area

    !> A SELF-INTERSECTING polygon is measured by sampling its even-odd interior, under both edge
    !! rules, because the signed sum the two exact formulas compute is not that region at all.
    !!
    !! The fixture is a bow tie: the four corners of a 20-degree square taken in crossing order, so
    !! the two diagonals meet in the middle and the interior is the two triangles left and right of
    !! that meeting point. What it must come to is HALF the simple quadrilateral on the same four
    !! corners, and that is what makes the assertion evidence: the signed sum over these vertices
    !! happens to be zero (the two lobes have opposite orientation), so a build that had not
    !! switched to the sampled measure would abort here rather than answer half.
    !!
    !! **Half, but not exactly half, and the tolerance says which parts are which.** The two lobes
    !! put their width at the middle declination, where `cos(dec)` is largest, so the true ratio is
    !! 0.5013 rather than 0.5 for this fixture -- and the great-circle rule adds the gnomonic
    !! projection's own distortion over 20 degrees. The measure itself is `2**18` points of an `R2`
    !! low-discrepancy lattice over the bounding region, which carries about 8e-5 on this fixture --
    !! far the smallest of the three. 10 per cent covers all three together and still separates
    !! "half" from the answers a broken measure gives: the whole box, nothing, or twice the interior.
    subroutine test_polygon_self_intersecting(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        !> The square's four corners in CROSSING order: the edges 1-2 and 3-4 are its diagonals.
        real(real64), parameter :: BRA(4) = [0.0_real64, 20.0_real64, 20.0_real64, 0.0_real64]
        real(real64), parameter :: BDEC(4) = [0.0_real64, 20.0_real64, 0.0_real64, 20.0_real64]
        !> The same four corners in order round the square.
        real(real64), parameter :: SRA(4) = [0.0_real64, 20.0_real64, 20.0_real64, 0.0_real64]
        real(real64), parameter :: SDEC(4) = [0.0_real64, 0.0_real64, 20.0_real64, 20.0_real64]
        type(pf_sky_polygon) :: bow, square
        real(real64) :: ratio
        integer :: rule

        do rule = 0, 1
            call bow%clear()
            call square%clear()
            call bow%init(BRA, BDEC, rule)
            call square%init(SRA, SDEC, rule)
            call check(error, bow%area() > 0.0_real64, &
                "a crossing polygon must be given the area of its even-odd interior, not the signed sum")
            if (allocated(error)) return
            ratio = bow%area() / square%area()
            call check(error, abs(ratio - 0.5_real64) <= 0.1_real64, &
                "a bow tie must cover half the square on its own four corners, edge rule " // achar(iachar("0") + rule))
            if (allocated(error)) return
        end do
        call bow%clear()
        call square%clear()
    end subroutine test_polygon_self_intersecting

    !> `strict = .true.` is a REFUSAL, and this is the side of it that must go through: a polygon
    !! that is not a suspected short-way band builds exactly as it does without the flag.
    !!
    !! The refusal itself aborts, so it lives in `sphere_polygon_strict_short_way`. What that
    !! scenario cannot show is where the test stops, and the screen has two separate ways of
    !! deciding a polygon is innocent -- both of them reached here, because either one failing open
    !! would turn `strict=` into a flag that refuses ordinary work:
    !!
    !!  * an RA extent of 180 degrees or less, which no band written the long way round can have;
    !!  * an extent above that with a vertex more than 90 degrees from BOTH ends of it, which a band
    !!    written as two clusters at the ends cannot have.
    !!
    !! Both fixtures sit next to a pole so that a wide RA extent still fits inside one hemisphere.
    subroutine test_polygon_strict_accepts(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        !> A narrow box: 10 degrees of RA, so the extent alone clears it.
        real(real64), parameter :: NRA(4) = [10.0_real64, 20.0_real64, 20.0_real64, 10.0_real64]
        real(real64), parameter :: NDEC(4) = [80.0_real64, 80.0_real64, 85.0_real64, 85.0_real64]
        !> A WIDE one: 200 degrees of RA, with the middle vertex 100 degrees from each end.
        real(real64), parameter :: WRA(4) = [0.0_real64, 100.0_real64, 200.0_real64, 100.0_real64]
        real(real64), parameter :: WDEC(4) = [86.0_real64, 84.0_real64, 86.0_real64, 88.0_real64]
        type(pf_sky_polygon) :: plain, strict
        integer :: rule

        do rule = 0, 1
            call plain%clear()
            call strict%clear()
            call plain%init(NRA, NDEC, rule)
            call strict%init(NRA, NDEC, rule, strict=.true.)
            call check(error, strict%area() == plain%area(), &
                "strict= changed a narrow polygon's area, edge rule " // achar(iachar("0") + rule))
            if (allocated(error)) return
            call plain%clear()
            call strict%clear()
            call plain%init(WRA, WDEC, rule)
            call strict%init(WRA, WDEC, rule, strict=.true.)
            call check(error, strict%area() == plain%area(), &
                "strict= refused a wide polygon whose vertices are not clustered at the ends, edge rule " // &
                achar(iachar("0") + rule))
            if (allocated(error)) return
        end do
        call plain%clear()
        call strict%clear()
    end subroutine test_polygon_strict_accepts

    !> Chart and great-circle draws: inside, right ascension in `[0, 360)`, and uniform per solid
    !! angle over cells of exactly known area, with a control drawing uniformly in the chart instead.
    subroutine test_polygon_random_is_uniform_and_inside(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 60000_int64
        ! The chart L of 20-degree squares at declinations 40 to 80, where cos(dec) varies by a factor
        ! of four, so a chart-uniform draw is far from uniform on the sky.
        real(real64), parameter :: LRA(6) = [0.0_real64, 40.0_real64, 40.0_real64, 20.0_real64, 20.0_real64, 0.0_real64]
        real(real64), parameter :: LDEC(6) = [40.0_real64, 40.0_real64, 60.0_real64, 60.0_real64, 80.0_real64, 80.0_real64]
        ! Cells: the L's three 20-degree squares, each in four declination bands of 5 degrees.
        integer, parameter :: NCELL = 12
        type(pf_sky_polygon) :: poly, gc
        integer(int64) :: counts(NCELL), control(NCELL), k, gcounts(16), gcontrol(16)
        real(real64) :: ra, dec, expect(NCELL), gexpect(16), area, u(2), v(3), c3(3), cx, cy, cz
        integer :: cell
        real(real64), parameter :: QRA(4) = [10.0_real64, 60.0_real64, 50.0_real64, 0.0_real64]
        real(real64), parameter :: QDEC(4) = [0.0_real64, 5.0_real64, 40.0_real64, 30.0_real64]

        call poly%init(LRA, LDEC)
        counts = 0_int64
        control = 0_int64
        do k = 1_int64, NDRAW
            call poly%random_at(SKY_SEED, k, ra, dec)
            call check(error, ra >= 0.0_real64 .and. ra < 360.0_real64 .and. poly%contains(ra, dec), &
                "a chart polygon's draw is outside it, or its RA outside [0, 360)")
            if (allocated(error)) return
            cell = l_cell(ra, dec)
            counts(cell) = counts(cell) + 1_int64
            ! CONTROL: one candidate uniform in the chart box, the declination uniform in degrees.
            ra = 40.0_real64 * pf_random_at(SKY_SEED + 1_int64, k, 1_int64)
            dec = 40.0_real64 + 40.0_real64 * pf_random_at(SKY_SEED + 1_int64, k, 2_int64)
            if (poly%contains(ra, dec)) then
                cell = l_cell(ra, dec)
                control(cell) = control(cell) + 1_int64
            end if
        end do
        do cell = 1, NCELL
            expect(cell) = l_cell_area(cell)
        end do
        area = sum(expect)
        call check(error, abs(area - poly%area()) <= 1.0e-12_real64 * area, "the chart L's cells do not tile it")
        if (allocated(error)) return
        call check(error, chi2_weighted(counts, expect) <= chi2_999(NCELL - 1), &
            "chart polygon draws are not uniform per solid angle over its cells at the 0.999 level")
        if (allocated(error)) return
        call check(error, chi2_weighted(control, expect) > chi2_999(NCELL - 1), &
            "the gate ACCEPTS points uniform in the chart, so it measures nothing")
        if (allocated(error)) return

        ! Great circles: sixteen cells, the four fan triangles from an inside point each split at their
        ! edges' midpoints, with areas from the triple-product formula the test writes itself.
        call gc%init(QRA, QDEC, PF_EDGE_GREAT_CIRCLE)
        call gc_cells(QRA, QDEC, gexpect)
        call check(error, abs(sum(gexpect) - gc%area()) <= 1.0e-12_real64 * gc%area(), &
            "the quadrilateral's sixteen cells do not tile it")
        if (allocated(error)) return
        gcounts = 0_int64
        gcontrol = 0_int64
        call centre_of(QRA, QDEC, cx, cy, cz)
        do k = 1_int64, NDRAW
            call gc%random_at(SKY_SEED, k, ra, dec, 2_int64)
            call check(error, ra >= 0.0_real64 .and. ra < 360.0_real64 .and. gc%contains(ra, dec), &
                "a great-circle polygon's draw is outside it, or its RA outside [0, 360)")
            if (allocated(error)) return
            v = vec_of(ra, dec)
            cell = gc_cell(QRA, QDEC, v)
            if (cell > 0) gcounts(cell) = gcounts(cell) + 1_int64
            ! CONTROL: uniform in the gnomonic plane about the vertex mean, which crowds the rim.
            u = [pf_random_at(SKY_SEED + 2_int64, k, 1_int64), pf_random_at(SKY_SEED + 2_int64, k, 2_int64)]
            c3 = [cx, cy, cz]
            v = gnomonic_point(c3, 1.4_real64 * (u(1) - 0.5_real64), 1.4_real64 * (u(2) - 0.5_real64))
            call pf_vec2radec(v, ra, dec)
            if (gc%contains(ra, dec)) then
                cell = gc_cell(QRA, QDEC, v)
                if (cell > 0) gcontrol(cell) = gcontrol(cell) + 1_int64
            end if
        end do
        call check(error, sum(gcounts) == NDRAW, "a great-circle draw fell in no cell of its own polygon")
        if (allocated(error)) return
        call check(error, chi2_weighted(gcounts, gexpect) <= chi2_999(15), &
            "great-circle polygon draws are not uniform per solid angle over sixteen cells at the 0.999 level")
        if (allocated(error)) return
        call check(error, sum(gcontrol) > 5000_int64 .and. chi2_weighted(gcontrol, gexpect) > chi2_999(15), &
            "the gate ACCEPTS points uniform in the gnomonic plane, so it measures nothing")
    end subroutine test_polygon_random_is_uniform_and_inside

    !> `parquet_debug_sphere_polygon_draw`'s candidate count: its mean is `1/%acceptance()` to four
    !! standard errors, a whole-sky box takes exactly one, and the point is `%random_at`'s.
    subroutine test_polygon_acceptance_identity(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 100000_int64
        type(pf_sky_polygon) :: poly(2), whole
        real(real64) :: ra, dec, ra2, dec2, mean, p, se
        integer(int64) :: k, ncand, total
        integer :: j
        character(len=160) :: msg

        call poly(1)%init([0.0_real64, 20.0_real64, 20.0_real64, 10.0_real64, 10.0_real64, 0.0_real64], &
                          [0.0_real64, 0.0_real64, 10.0_real64, 10.0_real64, 20.0_real64, 20.0_real64])
        call poly(2)%init([10.0_real64, 60.0_real64, 50.0_real64, 0.0_real64], [0.0_real64, 5.0_real64, 40.0_real64, 30.0_real64], &
                          PF_EDGE_GREAT_CIRCLE)
        do j = 1, 2
            total = 0_int64
            do k = 1_int64, NDRAW
                call parquet_debug_sphere_polygon_draw(poly(j), SKY_SEED, 7_int64, k, ra, dec, ncand)
                total = total + ncand
                if (k <= 200_int64) then
                    call poly(j)%random_at(SKY_SEED, 7_int64, ra2, dec2, k)
                    call check(error, abs(ra - ra2) <= SAME * 360.0_real64 .and. abs(dec - dec2) <= SAME * 90.0_real64, &
                        "the debug entry point's draw is not %random_at's")
                    if (allocated(error)) return
                end if
            end do
            mean = real(total, real64) / real(NDRAW, real64)
            p = poly(j)%acceptance()
            se = sqrt((1.0_real64 - p) / (p * p) / real(NDRAW, real64))
            write (msg, '(a,i0,a,f10.6,a,f10.6,a)') "polygon ", j, ": a draw took ", mean, " candidates on average, not ", &
                1.0_real64 / p, " = 1/%acceptance() to four standard errors"
            call check(error, abs(mean - 1.0_real64 / p) <= 4.0_real64 * se, trim(msg))
            if (allocated(error)) return
        end do

        ! CONTROL: a box covering the whole sky keeps every candidate.
        call whole%init([0.0_real64, 360.0_real64, 360.0_real64, 0.0_real64], &
                        [-90.0_real64, -90.0_real64, 90.0_real64, 90.0_real64])
        do k = 1_int64, 1000_int64
            call parquet_debug_sphere_polygon_draw(whole, SKY_SEED, 7_int64, k, ra, dec, ncand)
            call check(error, ncand == 1_int64, "a whole-sky box rejected a candidate")
            if (allocated(error)) return
        end do
    end subroutine test_polygon_acceptance_identity

    !> `%random_next` at the block it takes is `%random_at` there, `%position` moves one block per point
    !! after aligning, and `%random_fill` is the scalar form, split anywhere.
    subroutine test_polygon_tiers_agree(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: NFILL = 57
        type(pf_sky_polygon) :: poly(2)
        type(pf_random_stream) :: rng
        real(real64) :: ra, dec, ra2, dec2, u, whole_ra(NFILL), whole_dec(NFILL), part_ra(NFILL), part_dec(NFILL)
        real(real32) :: small
        integer(int64) :: p0, aligned, d, k
        integer :: j, split, step

        call poly(1)%init([10.0_real64, 30.0_real64, 25.0_real64], [-5.0_real64, -5.0_real64, 20.0_real64])
        call poly(2)%init([10.0_real64, 60.0_real64, 50.0_real64, 0.0_real64], [0.0_real64, 5.0_real64, 40.0_real64, 30.0_real64], &
                          PF_EDGE_GREAT_CIRCLE)
        do j = 1, 2
            call rng%seed(SKY_SEED, 11_int64)
            do step = 1, 12
                ! Leave the cursor on a word that is not a block boundary, in every one of the four ways.
                select case (modulo(step, 4))
                case (1)
                    call rng%uniform(u)
                case (2)
                    call rng%uniform32(small)
                case (3)
                    call rng%uniform32(small)
                    call rng%uniform(u)
                case default
                end select
                p0 = rng%position() - 1_int64
                aligned = 4_int64 * ((p0 + 3_int64) / 4_int64)
                d = aligned / 4_int64 + 1_int64
                call poly(j)%random_next(rng, ra, dec)
                call poly(j)%random_at(SKY_SEED, 11_int64, ra2, dec2, d)
                call check(error, abs(ra - ra2) <= SAME * 360.0_real64 .and. abs(dec - dec2) <= SAME * 90.0_real64, &
                    "%random_next is not %random_at at the block it took")
                if (allocated(error)) return
                call check(error, rng%position() == aligned + 5_int64, &
                    "%random_next did not align to a block and take exactly one")
                if (allocated(error)) return
            end do
            call rng%seed(SKY_SEED, 3_int64)
            do k = 1_int64, 5_int64
                call poly(j)%random_next(rng, ra, dec)
            end do
            call check(error, rng%position() == 21_int64, "five draws from a fresh stream did not leave it at word 21")
            if (allocated(error)) return

            call poly(j)%random_fill(SKY_SEED, -4_int64, whole_ra, whole_dec, 9_int64)
            do k = 1_int64, NFILL
                call poly(j)%random_at(SKY_SEED, -4_int64, ra, dec, 8_int64 + k)
                call check(error, abs(whole_ra(k) - ra) <= SAME * 360.0_real64 .and. &
                    abs(whole_dec(k) - dec) <= SAME * 90.0_real64, &
                    "%random_fill element k is not %random_at at draw + k - 1")
                if (allocated(error)) return
            end do
            do split = 1, NFILL - 1, 7
                call poly(j)%random_fill(SKY_SEED, -4_int64, part_ra(1:split), part_dec(1:split), 9_int64)
                call poly(j)%random_fill(SKY_SEED, -4_int64, part_ra(split + 1:), part_dec(split + 1:), &
                                         9_int64 + int(split, int64))
                call check(error, all(part_ra == whole_ra) .and. all(part_dec == whole_dec), &
                    "a polygon fill split in two is not the whole fill, exactly")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_polygon_tiers_agree

    !> Region families are independent at one coordinate: of each other, of `parquet_random`'s disc and
    !! of `pf_random_at`, by a Pearson test on an 8 x 8 table, with a deliberately coupled control.
    subroutine test_region_families_independent(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: NSTAT = 6
        integer, parameter :: NBIN = 8
        integer(int64), parameter :: NDRAW = 40000_int64
        !> The family each statistic belongs to; a pair within one family is not tested.
        integer, parameter :: FAMILY(NSTAT) = [1, 2, 3, 3, 4, 5]
        type(pf_sky_polygon) :: rect
        type(pf_healpix_grid) :: grid
        real(real64), allocatable :: stat(:, :), coupled(:), serial(:)
        real(real64) :: ra, dec, v(3), c(3), limit, chi2
        integer(int64), parameter :: LIST(8) = [100_int64, 200_int64, 300_int64, 400_int64, 500_int64, 600_int64, &
                                                700_int64, 800_int64]
        integer(int64) :: k, ipix
        integer :: a, b

        call rect%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call grid%init(64_int64, PF_HP_NEST)
        call grid%pix2vec(1000_int64, c)
        allocate(stat(NSTAT, NDRAW), coupled(NDRAW), serial(NDRAW))
        do k = 1_int64, NDRAW
            ! A box keeps its first candidate, so its right ascension is the polygon family's first uniform.
            call rect%random_at(SKY_SEED, k, ra, dec)
            stat(1, k) = (ra - 10.0_real64) / 20.0_real64
            v = pf_random_pixel_at(grid, SKY_SEED, k, 1000_int64)
            stat(2, k) = azimuth_about(c, v)
            v = pf_random_mask_at(grid, SKY_SEED, k, LIST)
            call grid%vec2pix(v, ipix)
            stat(3, k) = (real(ipix, real64) / 100.0_real64 - 1.0_real64) / 8.0_real64
            call grid%pix2vec(ipix, c)
            stat(4, k) = azimuth_about(c, v)
            call grid%pix2vec(1000_int64, c)
            v = pf_random_disc_at(SKY_SEED, k, ZAXIS, PI)
            stat(5, k) = 0.5_real64 * (1.0_real64 - v(3))
            stat(6, k) = pf_random_at(SKY_SEED, k)
            ! CONTROL: the polygon's first uniform for draw 1, taken by hand.
            coupled(k) = pf_random_at(pf_random_key(pf_random_key(SKY_SEED, ssph_label_polygon), 1_int64), k, 1_int64)
        end do
        limit = chi2_quantile((NBIN - 1) * (NBIN - 1), 5.0_real64)
        chi2 = independence(stat(1, :), coupled, NBIN)
        call check(error, chi2 > 100.0_real64 * limit, &
            "the deliberately coupled control -- the polygon's own first uniform -- passes the independence test, " // &
            "so this test has no power")
        if (allocated(error)) return
        do a = 1, NSTAT - 1
            do b = a + 1, NSTAT
                if (FAMILY(a) == FAMILY(b)) cycle
                chi2 = independence(stat(a, :), stat(b, :), NBIN)
                call check(error, chi2 <= limit, &
                    "two region families, or one and parquet_random's disc or pf_random_at, are dependent at one " // &
                    "coordinate: they share a label (feature_risks.md Risk-2)")
                if (allocated(error)) return
            end do
        end do

        ! Lag-1 SERIAL independence, along the DRAW axis. Every pairing above varies the STREAM index
        ! at one draw, so between them they measure label separation and say nothing about consecutive
        ! draws of ONE family: a defect making the block a draw addresses a function of `d/2` would
        ! leave each draw equal to its neighbour and pass all of them. The box keeps its first
        ! candidate, so its right ascension is the polygon family's first uniform at that draw.
        do k = 1_int64, NDRAW
            call rect%random_at(SKY_SEED, 1_int64, ra, dec, k)
            serial(k) = (ra - 10.0_real64) / 20.0_real64
            ! CONTROL: exactly that defect, consecutive draws sharing one block.
            call rect%random_at(SKY_SEED, 1_int64, ra, dec, (k + 1_int64) / 2_int64)
            coupled(k) = (ra - 10.0_real64) / 20.0_real64
        end do
        chi2 = independence(coupled(1:NDRAW - 1_int64), coupled(2:NDRAW), NBIN)
        call check(error, chi2 > 100.0_real64 * limit, &
            "the lag-1 control -- consecutive draws sharing one block -- passes the serial independence test, " // &
            "so that test has no power")
        if (allocated(error)) return
        chi2 = independence(serial(1:NDRAW - 1_int64), serial(2:NDRAW), NBIN)
        call check(error, chi2 <= limit, &
            "consecutive draws of the polygon family are dependent: the block a draw addresses is not a " // &
            "one-to-one function of the draw index")
    end subroutine test_region_families_independent

    !> `%contains` and `%random_at` inside `do concurrent` give the serial values.
    subroutine test_polygon_purity(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: N = 500
        type(pf_sky_polygon) :: poly
        real(real64) :: ra(N), dec(N), cra(N), cdec(N), probe_ra(N), probe_dec(N)
        logical :: inside(N), serial
        integer :: k

        call poly%init([100.0_real64, 140.0_real64, 150.0_real64, 95.0_real64], &
                       [-40.0_real64, -30.0_real64, 10.0_real64, -5.0_real64])
        do k = 1, N
            call pf_random_radec_at(SKY_SEED, int(k, int64), probe_ra(k), probe_dec(k))
        end do
        do concurrent (k = 1:N)
            inside(k) = poly%contains(probe_ra(k), probe_dec(k))
            call poly%random_at(SKY_SEED, int(k, int64), cra(k), cdec(k), 3_int64)
        end do
        do k = 1, N
            serial = poly%contains(probe_ra(k), probe_dec(k))
            call poly%random_at(SKY_SEED, int(k, int64), ra(k), dec(k), 3_int64)
            call check(error, (inside(k) .eqv. serial) .and. abs(cra(k) - ra(k)) <= SAME * 360.0_real64 .and. &
                abs(cdec(k) - dec(k)) <= SAME * 90.0_real64, "do concurrent gave a value the serial loop does not")
            if (allocated(error)) return
        end do
    end subroutine test_polygon_purity

    ! ================================================================================
    ! Pixels and masks
    ! ================================================================================

    !> Pixel draws: every one inside its pixel, at resolutions from 1 to `2**24`, in both schemes; the
    !! candidate count near the cap's acceptance; the four children and sixteen grandchildren equally
    !! filled; the `_radec` form the vector read in the grid's frame. The controls are the first
    !! candidates, which a walk accepting everything would return, and a draw uniform in ANGLE from the
    !! centre, which crowds the grandchildren at the middle.
    subroutine test_pixel_random_in_pixel(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 16000_int64
        integer(int64), parameter :: NSIDES(4) = [1_int64, 8_int64, 1024_int64, 16777216_int64]
        integer, parameter :: SCHEMES(4) = [PF_HP_RING, PF_HP_NEST, PF_HP_RING, PF_HP_NEST]
        type(pf_healpix_grid) :: grid, south
        integer(int64) :: pix(3), ipix, jpix, nest, k, ncand, total, outside, child(4), grand(16), cgrand(16)
        real(real64) :: v(3), c(3), f1(3), f2(3), ra, dec, ra2, dec2, radius, mean, theta, phi, sh, expect
        integer :: g, q
        character(len=160) :: msg

        call polar_cap_coverage(error)
        if (allocated(error)) return
        do g = 1, size(NSIDES)
            call grid%init(NSIDES(g), SCHEMES(g))
            call south%init(NSIDES(g), SCHEMES(g), frame=PF_HP_DEC_SOUTH)
            ! A pixel next to the north pole, one on the equator, and one at a base-face corner -- except
            ! at 2**24, where the children test needs 2**26, which no longer names a pixel near a pole.
            pix = [0_int64, 6_int64 * NSIDES(g) * NSIDES(g) + 2_int64 * NSIDES(g), 4_int64 * NSIDES(g) * NSIDES(g)]
            if (NSIDES(g) == 16777216_int64) pix(1) = 5_int64 * NSIDES(g) * NSIDES(g) + 12345_int64
            radius = grid%max_pixrad()
            do q = 1, 3
                ipix = pix(q)
                nest = ipix
                if (SCHEMES(g) == PF_HP_RING) call pf_ring2nest(NSIDES(g), ipix, nest)
                call grid%pix2vec(ipix, c)
                call frame_about(c, f1, f2)
                total = 0_int64
                outside = 0_int64
                child = 0_int64
                grand = 0_int64
                cgrand = 0_int64
                do k = 1_int64, NDRAW
                    call parquet_debug_sphere_pixel_draw(grid, SKY_SEED, 5_int64, ipix, k, v, ncand)
                    total = total + ncand
                    call grid%vec2pix(v, jpix)
                    call check(error, jpix == ipix, "a pixel draw is outside its pixel")
                    if (allocated(error)) return
                    call pf_vec2pix_nest(2_int64 * NSIDES(g), v, jpix)
                    child(jpix - 4_int64 * nest + 1_int64) = child(jpix - 4_int64 * nest + 1_int64) + 1_int64
                    call pf_vec2pix_nest(4_int64 * NSIDES(g), v, jpix)
                    grand(jpix - 16_int64 * nest + 1_int64) = grand(jpix - 16_int64 * nest + 1_int64) + 1_int64
                    ! CONTROL 1: the walk's first candidate, which a walk accepting everything returns.
                    v = pf_random_disc_at(pf_random_key(pf_random_key(SKY_SEED, ssph_label_pixel), k), 5_int64, c, radius, 1_int64)
                    call grid%vec2pix(v, jpix)
                    if (jpix /= ipix) outside = outside + 1_int64
                    ! CONTROL 2: the angle from the centre uniform, not its cosine, kept inside the pixel.
                    theta = radius * pf_random_at(SKY_SEED + 3_int64, k, 1_int64)
                    phi = 2.0_real64 * PI * pf_random_at(SKY_SEED + 3_int64, k, 2_int64)
                    v = cos(theta) * c + sin(theta) * (cos(phi) * f1 + sin(phi) * f2)
                    call grid%vec2pix(v, jpix)
                    if (jpix == ipix) then
                        call pf_vec2pix_nest(4_int64 * NSIDES(g), v, jpix)
                        cgrand(jpix - 16_int64 * nest + 1_int64) = cgrand(jpix - 16_int64 * nest + 1_int64) + 1_int64
                    end if
                end do
                ! This family does not reject: it reads one block as a position inside the pixel's
                ! own square in the equal-area projection, so EVERY draw takes exactly one candidate.
                ! `expect` is what a walk over the bounding cap would have cost instead --
                ! caparea/pixarea = npix*sin(R/2)**2, derived rather than banded -- and is asserted
                ! to exceed 1 so the gate above is telling two different samplers apart rather than
                ! recording what this one happens to do.
                mean = real(total, real64) / real(NDRAW, real64)
                sh = sin(0.5_real64 * radius)
                expect = real(12_int64 * NSIDES(g) * NSIDES(g), real64) * sh * sh
                write (msg, '(a,i0,a,i0,a,f9.5,a,f9.5)') "nside ", NSIDES(g), " pixel ", ipix, ": ", mean, &
                    " candidates per draw, where a cap-rejection walk would take ", expect
                call check(error, mean == 1.0_real64 .and. expect > 1.5_real64, trim(msg))
                if (allocated(error)) return
                call check(error, outside > NDRAW / 4_int64, &
                    "control: a walk accepting its first candidate would put fewer than a quarter of its draws outside the pixel")
                if (allocated(error)) return
                write (msg, '(a,i0,a,i0,a)') "nside ", NSIDES(g), " pixel ", ipix, &
                    ": draws are not uniform over the four children or sixteen grandchildren at the 0.999 level"
                call check(error, sph_chi2(child) <= chi2_999(3) .and. sph_chi2(grand) <= chi2_999(15), trim(msg))
                if (allocated(error)) return
                call check(error, sph_chi2(cgrand) > chi2_999(15), &
                    "the grandchildren gate ACCEPTS a draw uniform in angle from the centre, so it measures nothing")
                if (allocated(error)) return

                ! The cap covers the pixel: directions uniform over a cap half as wide again about the
                ! centre, kept where `%vec2pix` names the pixel, are all within `%max_pixrad` of it. Near a
                ! pole at high resolution the centre `%pix2vec` returns is rounded by up to a few percent
                ! of the radius, which only this can show is still covered.
                do k = 1_int64, 20000_int64
                    v = pf_random_disc_at(SKY_SEED + 5_int64, k, c, 1.5_real64 * radius)
                    call grid%vec2pix(v, jpix)
                    if (jpix == ipix) then
                        call pf_angdist(c, v, theta)
                        call check(error, theta <= radius, "a direction in the pixel lies outside its bounding cap")
                        if (allocated(error)) return
                    end if
                end do

                ! The RA/Dec form: the same point, in the grid's frame -- the SOUTH frame mirrors dec.
                do k = 1_int64, 50_int64
                    v = pf_random_pixel_at(grid, SKY_SEED, 5_int64, ipix, k)
                    call grid%vec2radec(v, ra2, dec2)
                    call pf_random_pixel_radec_at(grid, SKY_SEED, 5_int64, ipix, ra, dec, k)
                    call check(error, abs(ra - ra2) <= SAME * 360.0_real64 .and. abs(dec - dec2) <= SAME * 90.0_real64, &
                        "pf_random_pixel_radec_at is not the vector form read through the grid")
                    if (allocated(error)) return
                    call pf_random_pixel_radec_at(south, SKY_SEED, 5_int64, ipix, ra2, dec2, k)
                    call check(error, abs(ra - ra2) <= SAME * 360.0_real64 .and. abs(dec + dec2) <= SAME * 90.0_real64, &
                        "pf_random_pixel_radec_at on a PF_HP_DEC_SOUTH grid does not mirror the declination")
                    if (allocated(error)) return
                end do
            end do
        end do
    end subroutine test_pixel_random_in_pixel

    !> The bounding cap covers the pixels touching a pole at `nside` `2**20` and `2**24`, in both schemes.
    !!
    !! There `%pix2vec` forms the centre's transverse length from a rounded `z`, and at `2**24` moves it by
    !! a few percent of `%max_pixrad`, while `%vec2pix`'s own boundaries move by a few percent of a pixel:
    !! the cap still covers the region `%vec2pix` calls the pixel only because a polar pixel is more
    !! compact than the equatorial one `%max_pixrad` is attained on. Directions uniform over a cap 1.6
    !! times as wide, kept where `%vec2pix` names the pixel, must all be within `%max_pixrad` of the
    !! centre, and the walk's draws must all be inside the pixel.
    subroutine polar_cap_coverage(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NSIDES(2) = [1048576_int64, 16777216_int64]
        type(pf_healpix_grid) :: grid
        integer(int64) :: pix(4), ipix, jpix, k, n, kept
        real(real64) :: c(3), v(3), radius, theta
        integer :: g, q, scheme

        do g = 1, size(NSIDES)
            n = NSIDES(g)
            do scheme = PF_HP_RING, PF_HP_NEST
                call grid%init(n, scheme)
                if (scheme == PF_HP_RING) then
                    pix = [0_int64, 3_int64, 12_int64 * n * n - 1_int64, 12_int64 * n * n - 8_int64]
                else
                    pix = [n * n - 1_int64, 4_int64 * n * n - 1_int64, 8_int64 * n * n, 11_int64 * n * n + 1_int64]
                end if
                radius = grid%max_pixrad()
                do q = 1, size(pix)
                    ipix = pix(q)
                    call grid%pix2vec(ipix, c)
                    kept = 0_int64
                    do k = 1_int64, 40000_int64
                        v = pf_random_disc_at(SKY_SEED + 6_int64, k, c, 1.6_real64 * radius)
                        call grid%vec2pix(v, jpix)
                        if (jpix /= ipix) cycle
                        kept = kept + 1_int64
                        call pf_angdist(c, v, theta)
                        call check(error, theta <= radius, &
                            "a direction a polar pixel holds lies outside its bounding cap: the walk cannot reach it")
                        if (allocated(error)) return
                    end do
                    call check(error, kept > 2000_int64, "vacuity guard: too few directions landed in the polar pixel")
                    if (allocated(error)) return
                    ! The invariant asserted here is GEOMETRIC -- every point of a pixel lies within
                    ! `%max_pixrad` of its centre -- rather than `%vec2pix(v) == ipix`. At these
                    ! resolutions the two are not the same claim: within a pixel of a pole `z` sits
                    ! closer to 1 than a double can resolve, so `%vec2pix` cannot name the pixel a
                    ! direction is in, and it misnames a drawn point about 0.6 % of the time at
                    ! nside 2**24. The draw is right and the round trip is what runs out of digits;
                    ! `test_pix2vec_round_trip` says the same of pixel CENTRES above 2**20.
                    do k = 1_int64, 2000_int64
                        v = pf_random_pixel_at(grid, SKY_SEED, 1_int64, ipix, k)
                        call pf_angdist(c, v, theta)
                        call check(error, theta <= radius, &
                            "a draw in a polar pixel is farther from the pixel centre than %max_pixrad")
                        if (allocated(error)) return
                    end do
                end do
            end do
        end do
    end subroutine polar_cap_coverage

    !> A mask longer than the 22 spare bits can span: the choice falls back, and still spans the list.
    !!
    !! **The only route to `sky_mask_choose`'s fallback branch.** A mask draw takes its choice from
    !! the 22 bits the point's two uniforms leave over, which can decide a list of at most `2**22`
    !! entries; a longer one falls back to the choice family. Lemire's rejection reaches the same
    !! branch, but only with probability `mod(2**22, n)/2**22`, which for any list small enough to
    !! build quickly is far too rare to reach it in a test.
    !!
    !! The list is `0 .. 2**22`, so membership is a range test rather than a search. **The spread
    !! assertion is the load-bearing one**: a fallback that failed to draw at all would leave the
    !! chosen entry at the bottom of the range, giving pixel 0 every time, which lands in the list
    !! and would satisfy every other check here.
    subroutine test_mask_choice_fallback(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NLIST = 4194305_int64    ! 2**22 + 1: one past what 22 bits span
        integer(int64), parameter :: NDRAW = 20000_int64
        type(pf_healpix_grid) :: grid
        integer(int32), allocatable :: list(:)
        integer(int64) :: k, ipix, lo_seen, hi_seen, distinct(16), b
        real(real64) :: v(3)
        character(len=160) :: msg

        call grid%init(1024_int64, PF_HP_NEST)
        allocate(list(NLIST))
        do k = 1_int64, NLIST
            list(k) = int(k - 1_int64, int32)
        end do
        lo_seen = NLIST
        hi_seen = -1_int64
        distinct = 0_int64
        do k = 1_int64, NDRAW
            v = pf_random_mask_at(grid, SKY_SEED, 2_int64, list, k)
            call grid%vec2pix(v, ipix)
            call check(error, ipix >= 0_int64 .and. ipix < NLIST, &
                "a mask draw over a list longer than 2**22 left the list: the fallback choice is wrong")
            if (allocated(error)) return
            lo_seen = min(lo_seen, ipix)
            hi_seen = max(hi_seen, ipix)
            b = min(15_int64, ipix * 16_int64 / NLIST)
            distinct(b + 1_int64) = distinct(b + 1_int64) + 1_int64
        end do
        write (msg, '(a,i0,a,i0)') "the fallback choice spans only [", lo_seen, ", ", hi_seen
        call check(error, lo_seen < NLIST / 100_int64 .and. hi_seen > NLIST - NLIST / 100_int64, trim(msg))
        if (allocated(error)) return
        call check(error, all(distinct > 0_int64), &
            "some sixteenth of the list was never chosen: the fallback is not uniform over it")
        if (allocated(error)) return
        call check(error, sph_chi2(distinct) <= chi2_999(15), &
            "the fallback choice is not uniform over the list at the 0.999 level")
        deallocate(list)
    end subroutine test_mask_choice_fallback

    !> A mask of 50 pixels with one listed twice: counts per pixel follow the list, the duplicate
    !! twice; every point in a listed pixel; an `int32` list is the `int64` one. The control draws over
    !! the distinct pixels, which the same gate must reject.
    subroutine test_mask_uniform_over_pixels(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 102000_int64
        type(pf_healpix_grid) :: grid
        integer(int64) :: list(51), k, ipix, j, counts(50), control(50)
        integer(int32) :: list32(51)
        real(real64) :: v(3), w(3), expect(50)

        call grid%init(16_int64, PF_HP_NEST)
        do k = 1_int64, 50_int64
            list(k) = 37_int64 * k + 11_int64
        end do
        list(51) = list(7)
        list32 = int(list, int32)
        counts = 0_int64
        control = 0_int64
        do k = 1_int64, NDRAW
            v = pf_random_mask_at(grid, SKY_SEED, 1_int64, list, k)
            call grid%vec2pix(v, ipix)
            j = findloc_i64(list(1:50), ipix)
            call check(error, j > 0_int64, "a mask draw is not in any listed pixel")
            if (allocated(error)) return
            counts(j) = counts(j) + 1_int64
            if (k <= 3000_int64) then
                w = pf_random_mask_at(grid, SKY_SEED, 1_int64, list32, k)
                call check(error, all(abs(v - w) <= SAME), "an int32 pixel list does not draw what the int64 one does")
                if (allocated(error)) return
            end if
            ! CONTROL: a choice over the 50 distinct pixels, ignoring the duplicate.
            j = pf_random_int_at(SKY_SEED + 4_int64, 1_int64, 1_int64, 50_int64, k)
            control(j) = control(j) + 1_int64
        end do
        expect = 1.0_real64
        expect(7) = 2.0_real64
        call check(error, chi2_weighted(counts, expect) <= chi2_999(49), &
            "mask draws are not uniform over the list at the 0.999 level, with the duplicate counting twice")
        if (allocated(error)) return
        call check(error, chi2_weighted(control, expect) > chi2_999(49), &
            "the gate ACCEPTS a choice that ignores the duplicate, so it measures nothing")
    end subroutine test_mask_uniform_over_pixels

    !> Pixel and mask stream forms at the block they take are the coordinate forms there; the mask fills
    !! are the scalar forms, split anywhere.
    subroutine test_pixel_mask_tiers_agree(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: NFILL = 41
        type(pf_healpix_grid) :: grid
        type(pf_random_stream) :: rng
        integer(int64), parameter :: LIST(5) = [3_int64, 700_int64, 700_int64, 1500_int64, 3071_int64]
        real(real64) :: v(3), w(3), ra, dec, ra2, dec2, u, whole(3, NFILL), part(3, NFILL), wra(NFILL), wdec(NFILL), &
            pra(NFILL), pdec(NFILL)
        integer(int64) :: p0, aligned, d, k
        integer :: step, split

        call grid%init(16_int64, PF_HP_RING, frame=PF_HP_DEC_SOUTH)
        call rng%seed(SKY_SEED, 2_int64)
        do step = 1, 8
            if (modulo(step, 2) == 1) call rng%uniform(u)
            p0 = rng%position() - 1_int64
            aligned = 4_int64 * ((p0 + 3_int64) / 4_int64)
            d = aligned / 4_int64 + 1_int64
            select case (modulo(step, 4))
            case (0)
                call pf_random_pixel_next(grid, rng, 1000_int64, v)
                w = pf_random_pixel_at(grid, SKY_SEED, 2_int64, 1000_int64, d)
            case (1)
                call pf_random_mask_next(grid, rng, LIST, v)
                w = pf_random_mask_at(grid, SKY_SEED, 2_int64, LIST, d)
            case (2)
                call pf_random_pixel_radec_next(grid, rng, 1000_int64, ra, dec)
                call pf_random_pixel_radec_at(grid, SKY_SEED, 2_int64, 1000_int64, ra2, dec2, d)
                v = [ra, dec, 0.0_real64] / 360.0_real64
                w = [ra2, dec2, 0.0_real64] / 360.0_real64
            case default
                call pf_random_mask_radec_next(grid, rng, LIST, ra, dec)
                call pf_random_mask_radec_at(grid, SKY_SEED, 2_int64, LIST, ra2, dec2, d)
                v = [ra, dec, 0.0_real64] / 360.0_real64
                w = [ra2, dec2, 0.0_real64] / 360.0_real64
            end select
            call check(error, all(abs(v - w) <= SAME), "a pixel or mask stream form is not its coordinate form at its block")
            if (allocated(error)) return
            call check(error, rng%position() == aligned + 5_int64, "a pixel or mask stream form did not take one aligned block")
            if (allocated(error)) return
        end do

        call pf_random_fill_mask(grid, SKY_SEED, 8_int64, LIST, whole, 4_int64)
        call pf_random_fill_mask_radec(grid, SKY_SEED, 8_int64, LIST, wra, wdec, 4_int64)
        do k = 1_int64, NFILL
            v = pf_random_mask_at(grid, SKY_SEED, 8_int64, LIST, 3_int64 + k)
            call pf_random_mask_radec_at(grid, SKY_SEED, 8_int64, LIST, ra, dec, 3_int64 + k)
            call check(error, all(abs(whole(:, k) - v) <= SAME) .and. abs(wra(k) - ra) <= SAME * 360.0_real64 .and. &
                abs(wdec(k) - dec) <= SAME * 90.0_real64, "a mask fill's element k is not the scalar form at draw + k - 1")
            if (allocated(error)) return
        end do
        do split = 1, NFILL - 1, 6
            call pf_random_fill_mask(grid, SKY_SEED, 8_int64, LIST, part(:, 1:split), 4_int64)
            call pf_random_fill_mask(grid, SKY_SEED, 8_int64, LIST, part(:, split + 1:), 4_int64 + int(split, int64))
            call pf_random_fill_mask_radec(grid, SKY_SEED, 8_int64, LIST, pra(1:split), pdec(1:split), 4_int64)
            call pf_random_fill_mask_radec(grid, SKY_SEED, 8_int64, LIST, pra(split + 1:), pdec(split + 1:), &
                                           4_int64 + int(split, int64))
            call check(error, all(part == whole) .and. all(pra == wra) .and. all(pdec == wdec), &
                "a mask fill split in two is not the whole fill, exactly")
            if (allocated(error)) return
        end do
        ! A fill of no columns is a defined no-op.
        call pf_random_fill_mask(grid, SKY_SEED, 8_int64, LIST, part(:, 1:0))
    end subroutine test_pixel_mask_tiers_agree

    !> Every `int32`/`int64` specific pair gives the same value, and a draw below 1 is draw 1.
    subroutine test_region_kinds_agree(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_sky_polygon) :: poly
        type(pf_healpix_grid) :: grid
        type(pf_random_stream) :: r32, r64
        integer(int64), parameter :: L64(4) = [5_int64, 17_int64, 999_int64, 2000_int64]
        integer(int32), parameter :: L32(4) = [5_int32, 17_int32, 999_int32, 2000_int32]
        real(real64) :: v(4, 3), ra(4), dec(4), f64(2), g64(2), f32(2), g32(2), m64(3, 3), m32(3, 3), fib64(3, 9), fib32(3, 9)
        real(real64) :: fra64(9), fdec64(9), fra32(9), fdec32(9), zero_ra, zero_dec, s1(3), s2(3), s3(3), s4(3)
        integer :: k

        call poly%init([10.0_real64, 30.0_real64, 25.0_real64], [-5.0_real64, -5.0_real64, 20.0_real64])
        call grid%init(16_int64, PF_HP_NEST)
        do k = -1, 3
            call poly%random_at(SKY_SEED, 6_int32, ra(1), dec(1), int(k, int64))
            call poly%random_at(SKY_SEED, 6_int64, ra(2), dec(2), int(k, int64))
            call check(error, abs(ra(1) - ra(2)) <= SAME * 360.0_real64 .and. abs(dec(1) - dec(2)) <= SAME * 90.0_real64, &
                "%random_at's int32 and int64 stream indices disagree")
            if (allocated(error)) return
            if (k == 1) then
                zero_ra = ra(2)
                zero_dec = dec(2)
            end if
            v(1, :) = pf_random_pixel_at(grid, SKY_SEED, 6_int32, 999_int32, int(k, int64))
            v(2, :) = pf_random_pixel_at(grid, SKY_SEED, 6_int32, 999_int64, int(k, int64))
            v(3, :) = pf_random_pixel_at(grid, SKY_SEED, 6_int64, 999_int32, int(k, int64))
            v(4, :) = pf_random_pixel_at(grid, SKY_SEED, 6_int64, 999_int64, int(k, int64))
            call check(error, maxval(abs(v - spread(v(4, :), 1, 4))) <= SAME, "pf_random_pixel_at's four specifics disagree")
            if (allocated(error)) return
            call pf_random_pixel_radec_at(grid, SKY_SEED, 6_int32, 999_int32, ra(1), dec(1), int(k, int64))
            call pf_random_pixel_radec_at(grid, SKY_SEED, 6_int32, 999_int64, ra(2), dec(2), int(k, int64))
            call pf_random_pixel_radec_at(grid, SKY_SEED, 6_int64, 999_int32, ra(3), dec(3), int(k, int64))
            call pf_random_pixel_radec_at(grid, SKY_SEED, 6_int64, 999_int64, ra(4), dec(4), int(k, int64))
            call check(error, maxval(abs(ra - ra(4))) <= SAME * 360.0_real64 .and. &
                maxval(abs(dec - dec(4))) <= SAME * 90.0_real64, &
                "pf_random_pixel_radec_at's four specifics disagree")
            if (allocated(error)) return
            v(1, :) = pf_random_mask_at(grid, SKY_SEED, 6_int32, L32, int(k, int64))
            v(2, :) = pf_random_mask_at(grid, SKY_SEED, 6_int32, L64, int(k, int64))
            v(3, :) = pf_random_mask_at(grid, SKY_SEED, 6_int64, L32, int(k, int64))
            v(4, :) = pf_random_mask_at(grid, SKY_SEED, 6_int64, L64, int(k, int64))
            call check(error, maxval(abs(v - spread(v(4, :), 1, 4))) <= SAME, "pf_random_mask_at's four specifics disagree")
            if (allocated(error)) return
            call pf_random_mask_radec_at(grid, SKY_SEED, 6_int32, L32, ra(1), dec(1), int(k, int64))
            call pf_random_mask_radec_at(grid, SKY_SEED, 6_int32, L64, ra(2), dec(2), int(k, int64))
            call pf_random_mask_radec_at(grid, SKY_SEED, 6_int64, L32, ra(3), dec(3), int(k, int64))
            call pf_random_mask_radec_at(grid, SKY_SEED, 6_int64, L64, ra(4), dec(4), int(k, int64))
            call check(error, maxval(abs(ra - ra(4))) <= SAME * 360.0_real64 .and. &
                maxval(abs(dec - dec(4))) <= SAME * 90.0_real64, &
                "pf_random_mask_radec_at's four specifics disagree")
            if (allocated(error)) return
        end do
        ! A draw below 1 is draw 1, which the loop above recorded.
        call poly%random_at(SKY_SEED, 6_int64, ra(1), dec(1), -7_int64)
        call check(error, abs(ra(1) - zero_ra) <= SAME * 360.0_real64 .and. abs(dec(1) - zero_dec) <= SAME * 90.0_real64, &
            "a polygon draw below 1 is not draw 1")
        if (allocated(error)) return

        call poly%random_fill(SKY_SEED, 6_int32, f32, g32, 2_int64)
        call poly%random_fill(SKY_SEED, 6_int64, f64, g64, 2_int64)
        call check(error, all(abs(f32 - f64) <= SAME * 360.0_real64) .and. all(abs(g32 - g64) <= SAME * 90.0_real64), &
            "%random_fill's int32 and int64 stream indices disagree")
        if (allocated(error)) return
        call pf_random_fill_mask(grid, SKY_SEED, 6_int32, L32, m32)
        call pf_random_fill_mask(grid, SKY_SEED, 6_int64, L64, m64)
        call check(error, all(abs(m32 - m64) <= SAME), "pf_random_fill_mask's specifics disagree")
        if (allocated(error)) return
        call pf_random_fill_mask(grid, SKY_SEED, 6_int32, L64, m32)
        call pf_random_fill_mask(grid, SKY_SEED, 6_int64, L32, m64)
        call check(error, all(abs(m32 - m64) <= SAME), "pf_random_fill_mask's mixed-kind specifics disagree")
        if (allocated(error)) return
        call pf_random_fill_mask_radec(grid, SKY_SEED, 6_int32, L32, f32, g32)
        call pf_random_fill_mask_radec(grid, SKY_SEED, 6_int64, L64, f64, g64)
        call check(error, all(abs(f32 - f64) <= SAME * 360.0_real64) .and. all(abs(g32 - g64) <= SAME * 90.0_real64), &
            "pf_random_fill_mask_radec's specifics disagree")
        if (allocated(error)) return
        call pf_random_fill_mask_radec(grid, SKY_SEED, 6_int32, L64, f32, g32)
        call pf_random_fill_mask_radec(grid, SKY_SEED, 6_int64, L32, f64, g64)
        call check(error, all(abs(f32 - f64) <= SAME * 360.0_real64) .and. all(abs(g32 - g64) <= SAME * 90.0_real64), &
            "pf_random_fill_mask_radec's mixed-kind specifics disagree")
        if (allocated(error)) return

        call r32%seed(SKY_SEED, 4_int64)
        call r64%seed(SKY_SEED, 4_int64)
        ! Contiguous locals, not rows of `v`: a row section reaching the explicit-shape `v(3)` dummy is
        ! copied through an array temporary on every call.
        call pf_random_pixel_next(grid, r32, 999_int32, s1)
        call pf_random_pixel_next(grid, r64, 999_int64, s2)
        call pf_random_mask_next(grid, r32, L32, s3)
        call pf_random_mask_next(grid, r64, L64, s4)
        call check(error, all(abs(s1 - s2) <= SAME) .and. all(abs(s3 - s4) <= SAME), &
            "the stream forms' int32 and int64 pixel kinds disagree")
        if (allocated(error)) return
        call pf_random_pixel_radec_next(grid, r32, 999_int32, ra(1), dec(1))
        call pf_random_pixel_radec_next(grid, r64, 999_int64, ra(2), dec(2))
        call pf_random_mask_radec_next(grid, r32, L32, ra(3), dec(3))
        call pf_random_mask_radec_next(grid, r64, L64, ra(4), dec(4))
        ! All four pairs, not one from each form: asserting only the pixel form's RA and the mask
        ! form's declination leaves half of what the four calls produced unpinned.
        call check(error, abs(ra(1) - ra(2)) <= SAME * 360.0_real64 .and. abs(dec(1) - dec(2)) <= SAME * 90.0_real64 .and. &
            abs(ra(3) - ra(4)) <= SAME * 360.0_real64 .and. abs(dec(3) - dec(4)) <= SAME * 90.0_real64, &
            "the RA/Dec stream forms' int32 and int64 pixel kinds disagree")
        if (allocated(error)) return

        call pf_fibonacci_grid(9_int32, fib32, frame=PF_HP_DEC_SOUTH)
        call pf_fibonacci_grid(9_int64, fib64, frame=PF_HP_DEC_SOUTH)
        call pf_fibonacci_grid_radec(9_int32, fra32, fdec32)
        call pf_fibonacci_grid_radec(9_int64, fra64, fdec64)
        call check(error, all(abs(fib32 - fib64) <= SAME) .and. all(abs(fra32 - fra64) <= SAME * 360.0_real64) .and. &
            all(abs(fdec32 - fdec64) <= SAME * 90.0_real64), "the Fibonacci grid's int32 and int64 counts disagree")
    end subroutine test_region_kinds_agree

    ! ================================================================================
    ! The Fibonacci grid
    ! ================================================================================

    !> The Fibonacci grid: unit vectors, the two forms one grid in both frames, a mean near 0, and
    !! nearest-neighbour separations within the band astropy's grid shows.
    subroutine test_fibonacci_grid(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: NS(4) = [1, 2, 7, 1000]
        real(real64), allocatable :: vec(:, :), south(:, :), ra(:), dec(:)
        real(real64) :: vra, vdec, rmin, lo, hi, best, gap, mean(3)
        integer :: g, n, k, j
        character(len=120) :: msg

        do g = 1, size(NS)
            n = NS(g)
            allocate(vec(3, n), south(3, n), ra(n), dec(n))
            call pf_fibonacci_grid(n, vec)
            call pf_fibonacci_grid(n, south, frame=PF_HP_DEC_SOUTH)
            call pf_fibonacci_grid_radec(n, ra, dec)
            do k = 1, n
                call check(error, abs(norm2(vec(:, k)) - 1.0_real64) <= SAME, "a Fibonacci vector is not of unit length")
                if (allocated(error)) return
                call pf_vec2radec(vec(:, k), vra, vdec)
                call check(error, sky_gap(vra, vdec, ra(k), dec(k)) <= 1.0e-12_real64, &
                    "the Fibonacci grid's vector form is not its RA/Dec form")
                if (allocated(error)) return
                call check(error, all(abs(south(:, k) - [vec(1, k), vec(2, k), -vec(3, k)]) <= SAME), &
                    "the Fibonacci grid in PF_HP_DEC_SOUTH is not the NORTH grid reflected in z")
                if (allocated(error)) return
            end do
            if (n == 1000) then
                mean = sum(vec, dim=2) / real(n, real64)
                call check(error, norm2(mean) <= 2.0_real64 / real(n, real64), "the 1000-point grid's mean is not near 0")
                if (allocated(error)) return
                rmin = sqrt(4.0_real64 * PI / real(n, real64))
                lo = huge(1.0_real64)
                hi = 0.0_real64
                do k = 1, n
                    best = huge(1.0_real64)
                    do j = 1, n
                        if (j == k) cycle
                        call pf_angdist(vec(:, k), vec(:, j), gap)
                        best = min(best, gap)
                    end do
                    lo = min(lo, best)
                    hi = max(hi, best)
                end do
                write (msg, '(a,f6.3,a,f6.3,a)') "the 1000-point grid's neighbour separations run from ", lo / rmin, " to ", &
                    hi / rmin, " of sqrt(4*pi/n), outside [0.85, 1.0]"
                call check(error, lo >= 0.85_real64 * rmin .and. hi <= 1.0_real64 * rmin, trim(msg))
                if (allocated(error)) return
            end if
            deallocate(vec, south, ra, dec)
        end do
    end subroutine test_fibonacci_grid

    ! ================================================================================
    ! Helpers
    ! ================================================================================

    !> A unit vector from `(ra, dec)` in degrees, written here independently of the library.
    pure function vec_of(ra, dec) result(v)
        real(real64), intent(in) :: ra              !! right ascension, degrees
        real(real64), intent(in) :: dec             !! declination, degrees
        real(real64) :: v(3)                        !! the unit vector
        v = [cos(dec * DEG) * cos(ra * DEG), cos(dec * DEG) * sin(ra * DEG), sin(dec * DEG)]
    end function vec_of

    !> The gap between two angles in degrees, the short way round the turn.
    pure function turn_gap(a, b) result(g)
        real(real64), intent(in) :: a               !! one angle, degrees
        real(real64), intent(in) :: b               !! the other, degrees
        real(real64) :: g                           !! the gap, in `[0, 180]`
        g = modulo(abs(a - b), 360.0_real64)
        g = min(g, 360.0_real64 - g)
    end function turn_gap

    !> The on-sky gap between two positions in degrees: the declination gap, or the right-ascension
    !! gap scaled by `cos(dec)`, whichever is larger.
    pure function sky_gap(ra, dec, ra_ref, dec_ref) result(g)
        real(real64), intent(in) :: ra              !! right ascension under test, degrees
        real(real64), intent(in) :: dec             !! declination under test, degrees
        real(real64), intent(in) :: ra_ref          !! the reference right ascension, degrees
        real(real64), intent(in) :: dec_ref         !! the reference declination, degrees
        real(real64) :: g                           !! the gap, degrees
        g = max(abs(dec - dec_ref), turn_gap(ra, ra_ref) * cos(dec_ref * DEG))
    end function sky_gap

    !> The index of `value` in `list`, or 0.
    pure function findloc_i64(list, value) result(j)
        integer(int64), intent(in) :: list(:)       !! the list
        integer(int64), intent(in) :: value         !! the value sought
        integer(int64) :: j                         !! its first index, or 0
        integer(int64) :: k
        j = 0_int64
        do k = 1_int64, size(list, kind=int64)
            if (list(k) == value) then
                j = k
                return
            end if
        end do
    end function findloc_i64

    !> Which of the chart L's twelve cells holds `(ra, dec)`: its three 20-degree squares, four bands each.
    pure function l_cell(ra, dec) result(cell)
        real(real64), intent(in) :: ra              !! right ascension, degrees, inside the L
        real(real64), intent(in) :: dec             !! declination, degrees, inside the L
        integer :: cell                             !! 1 .. 12
        integer :: square, band
        if (dec < 60.0_real64) then
            square = merge(1, 2, ra < 20.0_real64)
            band = min(4, int((dec - 40.0_real64) / 5.0_real64) + 1)
        else
            square = 3
            band = min(4, int((dec - 60.0_real64) / 5.0_real64) + 1)
        end if
        cell = (square - 1) * 4 + max(1, band)
    end function l_cell

    !> The exact area, steradians, of the chart L's cell `cell`.
    pure function l_cell_area(cell) result(a)
        integer, intent(in) :: cell                 !! 1 .. 12
        real(real64) :: a                           !! its area
        real(real64) :: lo
        integer :: square, band
        square = (cell - 1) / 4 + 1
        band = mod(cell - 1, 4) + 1
        lo = merge(40.0_real64, 60.0_real64, square < 3) + 5.0_real64 * real(band - 1, real64)
        a = 20.0_real64 * DEG * (sin((lo + 5.0_real64) * DEG) - sin(lo * DEG))
    end function l_cell_area

    !> The normalised vertex sum of a polygon given in degrees.
    pure subroutine centre_of(ra, dec, cx, cy, cz)
        real(real64), intent(in) :: ra(:)           !! right ascensions, degrees
        real(real64), intent(in) :: dec(:)          !! declinations, degrees
        real(real64), intent(out) :: cx             !! the centre's x
        real(real64), intent(out) :: cy             !! the centre's y
        real(real64), intent(out) :: cz             !! the centre's z
        real(real64) :: s(3)
        integer :: k
        s = 0.0_real64
        do k = 1, size(ra)
            s = s + vec_of(ra(k), dec(k))
        end do
        s = s / norm2(s)
        cx = s(1)
        cy = s(2)
        cz = s(3)
    end subroutine centre_of

    !> A right-handed frame perpendicular to a unit vector, built the test's own way.
    pure subroutine frame_about(c, f1, f2)
        real(real64), intent(in) :: c(3)            !! a unit vector
        real(real64), intent(out) :: f1(3)          !! perpendicular to `c`
        real(real64), intent(out) :: f2(3)          !! `c x f1`
        real(real64) :: a(3)
        a = merge([1.0_real64, 0.0_real64, 0.0_real64], [0.0_real64, 1.0_real64, 0.0_real64], abs(c(1)) < abs(c(2)))
        f1 = a - dot_product(a, c) * c
        f1 = f1 / norm2(f1)
        f2 = [c(2) * f1(3) - c(3) * f1(2), c(3) * f1(1) - c(1) * f1(3), c(1) * f1(2) - c(2) * f1(1)]
    end subroutine frame_about

    !> The point at gnomonic coordinates `(x, y)` about the unit vector `c`, in the test's frame.
    pure function gnomonic_point(c, x, y) result(v)
        real(real64), intent(in) :: c(3)            !! the tangent point
        real(real64), intent(in) :: x               !! the first plane coordinate
        real(real64), intent(in) :: y               !! the second
        real(real64) :: v(3)                        !! the unit vector
        real(real64) :: f1(3), f2(3)
        call frame_about(c, f1, f2)
        v = c + x * f1 + y * f2
        v = v / norm2(v)
    end function gnomonic_point

    !> The azimuth of `v` about `c` as a fraction of a turn, in `[0, 1)`.
    pure function azimuth_about(c, v) result(f)
        real(real64), intent(in) :: c(3)            !! the axis, a unit vector
        real(real64), intent(in) :: v(3)            !! the direction
        real(real64) :: f                           !! the azimuth, turns
        real(real64) :: f1(3), f2(3)
        call frame_about(c, f1, f2)
        f = modulo(atan2(dot_product(v, f2), dot_product(v, f1)) / (2.0_real64 * PI), 1.0_real64)
        if (f >= 1.0_real64) f = 0.0_real64
    end function azimuth_about

    !> The solid angle of the spherical triangle `(a, b, c)`, signed by its orientation.
    pure function triangle_area(a, b, c) result(e)
        real(real64), intent(in) :: a(3)            !! a unit vector
        real(real64), intent(in) :: b(3)            !! a unit vector
        real(real64), intent(in) :: c(3)            !! a unit vector
        real(real64) :: e                           !! the area, steradians
        real(real64) :: t
        t = a(1) * (b(2) * c(3) - b(3) * c(2)) - a(2) * (b(1) * c(3) - b(3) * c(1)) + a(3) * (b(1) * c(2) - b(2) * c(1))
        e = 2.0_real64 * atan2(t, 1.0_real64 + dot_product(a, b) + dot_product(b, c) + dot_product(c, a))
    end function triangle_area

    !> The area of a polygon as the sum of its fan triangles from its first vertex: straight chart
    !! triangles by Green's theorem, or spherical triangles.
    subroutine fan_area(ra, dec, rule, total)
        real(real64), intent(in) :: ra(:)           !! right ascensions, degrees
        real(real64), intent(in) :: dec(:)          !! declinations, degrees
        integer, intent(in) :: rule                 !! `PF_EDGE_RADEC` or `PF_EDGE_GREAT_CIRCLE`
        real(real64), intent(out) :: total          !! the summed area, steradians
        type(pf_sky_polygon) :: tri
        real(real64) :: a(3), b(3), c(3)
        integer :: k
        total = 0.0_real64
        do k = 2, size(ra) - 1
            if (rule == PF_EDGE_GREAT_CIRCLE) then
                a = vec_of(ra(1), dec(1))
                b = vec_of(ra(k), dec(k))
                c = vec_of(ra(k + 1), dec(k + 1))
                total = total + abs(triangle_area(a, b, c))
            else
                call tri%init([ra(1), ra(k), ra(k + 1)], [dec(1), dec(k), dec(k + 1)])
                total = total + tri%area()
                call tri%clear()
            end if
        end do
    end subroutine fan_area

    !> The sixteen cells of a convex great-circle quadrilateral: each fan triangle from the vertex
    !! mean, split at its edges' midpoints into four. Their areas, steradians.
    pure subroutine gc_cells(ra, dec, areas)
        real(real64), intent(in) :: ra(4)           !! right ascensions, degrees
        real(real64), intent(in) :: dec(4)          !! declinations, degrees
        real(real64), intent(out) :: areas(16)      !! the cell areas
        real(real64) :: c(3), a(3), b(3), mca(3), mab(3), mbc(3), cx, cy, cz
        integer :: k
        call centre_of(ra, dec, cx, cy, cz)
        c = [cx, cy, cz]
        do k = 1, 4
            a = vec_of(ra(k), dec(k))
            b = vec_of(ra(mod(k, 4) + 1), dec(mod(k, 4) + 1))
            mca = (c + a) / norm2(c + a)
            mab = (a + b) / norm2(a + b)
            mbc = (b + c) / norm2(b + c)
            areas(4 * k - 3) = abs(triangle_area(c, mca, mbc))
            areas(4 * k - 2) = abs(triangle_area(mca, a, mab))
            areas(4 * k - 1) = abs(triangle_area(mbc, mab, b))
            areas(4 * k) = abs(triangle_area(mca, mab, mbc))
        end do
    end subroutine gc_cells

    !> Which of `gc_cells`' sixteen cells holds the unit vector `v`, or 0.
    pure function gc_cell(ra, dec, v) result(cell)
        real(real64), intent(in) :: ra(4)           !! right ascensions, degrees
        real(real64), intent(in) :: dec(4)          !! declinations, degrees
        real(real64), intent(in) :: v(3)            !! a direction inside the quadrilateral
        integer :: cell                             !! 1 .. 16, or 0
        real(real64) :: c(3), a(3), b(3), mca(3), mab(3), mbc(3), cx, cy, cz
        integer :: k
        call centre_of(ra, dec, cx, cy, cz)
        c = [cx, cy, cz]
        cell = 0
        do k = 1, 4
            a = vec_of(ra(k), dec(k))
            b = vec_of(ra(mod(k, 4) + 1), dec(mod(k, 4) + 1))
            mca = (c + a) / norm2(c + a)
            mab = (a + b) / norm2(a + b)
            mbc = (b + c) / norm2(b + c)
            if (in_triangle(c, mca, mbc, v)) cell = 4 * k - 3
            if (in_triangle(mca, a, mab, v)) cell = 4 * k - 2
            if (in_triangle(mbc, mab, b, v)) cell = 4 * k - 1
            if (in_triangle(mca, mab, mbc, v)) cell = 4 * k
            if (cell > 0) return
        end do
    end function gc_cell

    !> Whether `v` lies inside the small spherical triangle `(a, b, c)`, either orientation.
    pure function in_triangle(a, b, c, v) result(inside)
        real(real64), intent(in) :: a(3)            !! a vertex
        real(real64), intent(in) :: b(3)            !! a vertex
        real(real64), intent(in) :: c(3)            !! a vertex
        real(real64), intent(in) :: v(3)            !! the direction
        logical :: inside                           !! whether it is inside
        real(real64) :: s1, s2, s3
        s1 = triple(a, b, v)
        s2 = triple(b, c, v)
        s3 = triple(c, a, v)
        inside = (s1 >= 0.0_real64 .and. s2 >= 0.0_real64 .and. s3 >= 0.0_real64) .or. &
                 (s1 <= 0.0_real64 .and. s2 <= 0.0_real64 .and. s3 <= 0.0_real64)
    end function in_triangle

    !> `a . (b x c)`.
    pure function triple(a, b, c) result(t)
        real(real64), intent(in) :: a(3)            !! a vector
        real(real64), intent(in) :: b(3)            !! a vector
        real(real64), intent(in) :: c(3)            !! a vector
        real(real64) :: t                           !! the triple product
        t = a(1) * (b(2) * c(3) - b(3) * c(2)) - a(2) * (b(1) * c(3) - b(3) * c(1)) + a(3) * (b(1) * c(2) - b(2) * c(1))
    end function triple

    !> The chi-square of `counts` against expectations proportional to `weights`.
    pure function chi2_weighted(counts, weights) result(c)
        integer(int64), intent(in) :: counts(:)     !! observed cell counts
        real(real64), intent(in) :: weights(:)      !! each cell's relative expectation
        real(real64) :: c                           !! the statistic
        real(real64) :: expect(size(counts))
        expect = real(sum(counts), real64) * weights / sum(weights)
        c = sum((real(counts, real64) - expect)**2 / expect)
    end function chi2_weighted

    !> The chi-square of `counts` against equal expectations.
    pure function sph_chi2(counts) result(c)
        integer(int64), intent(in) :: counts(:)     !! observed cell counts
        real(real64) :: c                           !! the statistic
        real(real64) :: expect
        expect = real(sum(counts), real64) / real(size(counts), real64)
        c = sum((real(counts, real64) - expect)**2) / expect
    end function sph_chi2

    !> Pearson's chi-square test of independence of two samples in `[0, 1)`, on an `nbin x nbin` table.
    pure function independence(a, b, nbin) result(c)
        real(real64), intent(in) :: a(:)            !! the first sample
        real(real64), intent(in) :: b(:)            !! the second, paired with the first
        integer, intent(in) :: nbin                 !! cells per axis
        real(real64) :: c                           !! the statistic, `(nbin-1)**2` degrees of freedom
        integer(int64) :: table(nbin, nbin), rows(nbin), cols(nbin)
        integer :: k, i, j
        real(real64) :: expect
        table = 0_int64
        do k = 1, size(a)
            i = max(1, min(nbin, int(real(nbin, real64) * a(k)) + 1))
            j = max(1, min(nbin, int(real(nbin, real64) * b(k)) + 1))
            table(i, j) = table(i, j) + 1_int64
        end do
        rows = sum(table, dim=2)
        cols = sum(table, dim=1)
        c = 0.0_real64
        do j = 1, nbin
            do i = 1, nbin
                expect = real(rows(i), real64) * real(cols(j), real64) / real(size(a), real64)
                if (expect > 0.0_real64) c = c + (real(table(i, j), real64) - expect)**2 / expect
            end do
        end do
    end function independence

    !> The 0.999 quantile of a chi-square with `df` degrees of freedom, by Wilson-Hilferty.
    pure function chi2_999(df) result(c)
        integer, intent(in) :: df                   !! degrees of freedom, at least 1
        real(real64) :: c                           !! the quantile
        c = chi2_quantile(df, 3.090232306167813_real64)
    end function chi2_999

    !> The upper quantile of a chi-square with `df` degrees of freedom `z` normal deviates out.
    pure function chi2_quantile(df, z) result(q)
        integer, intent(in) :: df                   !! degrees of freedom, at least 1
        real(real64), intent(in) :: z               !! the deviate
        real(real64) :: q                           !! the quantile
        real(real64) :: d, h
        d = real(max(df, 1), real64)
        h = 2.0_real64 / (9.0_real64 * d)
        q = d * (1.0_real64 - h + z * sqrt(h))**3
    end function chi2_quantile

end module test_sphere
