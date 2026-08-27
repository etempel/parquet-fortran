!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `pf_healpix_grid`: the grid object and the RA/Dec layer.
!>
!> **This suite needs no new reference vectors, and that is deliberate.** Every binding on the type
!> delegates to a free procedure that Tier A and Tier B already pin against `healpy`-derived golden
!> vectors, so the object is correct exactly when it agrees with those procedures. Generating a
!> second table would measure the same thing twice and leave two copies to keep true.
!>
!> So the central test here is a sweep asserting **exact** equality between each binding and the
!> free call it stands for, across seven resolutions, both schemes, both integer kinds and a set of
!> directions covering both poles, a face seam, the boundary between the polar caps and the
!> equatorial belt, and a general direction. Exactness is the right bar because the binding calls
!> the same procedure with the same arguments: anything but bit-identity is a defect in the
!> delegation itself.
!>
!> **The RA/Dec layer is the one place new arithmetic appears**, so it is checked three ways: the
!> mirror identity between the two conventions, agreement with the conversion written out in full
!> here rather than in the library, and a round trip. The first and second are independent -- an
!> error in both frames the same way passes the mirror identity and fails the literal.
module test_healpix_grid
    use parquet_healpix
    use iso_fortran_env, only : int32, int64, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_tests_healpix_grid

    !> pi, to the precision a `real64` literal carries.
    real(real64), parameter :: pi = 3.141592653589793238462643_real64
    !> The resolutions the sweep covers: 1 (where the schemes coincide) up to the int32 ceiling.
    integer(int64), parameter :: sweep_nside(7) = [1_int64, 2_int64, 4_int64, 16_int64, &
        256_int64, 1024_int64, 8192_int64]

contains

    !> Registers every `pf_healpix_grid` test.
    subroutine collect_tests_healpix_grid(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("every conversion binding equals the free procedure it delegates to", &
                         test_delegation_matches), &
            new_unittest("the disc bindings equal pf_query_disc in both schemes", &
                         test_disc_delegation), &
            new_unittest("the accessors report what init was given", &
                         test_accessors), &
            new_unittest("get_nside and get_npix answer in the caller's kind", &
                         test_kind_matching_accessors), &
            new_unittest("the two frames are mirror images in declination", &
                         test_frame_mirror_identity), &
            new_unittest("radec2pix equals the conversion written out in full", &
                         test_frame_literal), &
            new_unittest("radec2pix and pix2radec round trip in both frames", &
                         test_radec_round_trip), &
            new_unittest("a grid built without frame= uses PF_HP_DEC_NORTH", &
                         test_default_frame), &
            new_unittest("an unbuilt grid reports the sentinel from every total binding", &
                         test_unbuilt_sentinels), &
            new_unittest("at_nside and at_order keep the scheme and the frame", &
                         test_derived_grids), &
            new_unittest("ud_pix converts in NEST and reports -1 otherwise", &
                         test_ud_pix), &
            new_unittest("== and /= compare resolution, scheme and frame", &
                         test_comparison), &
            new_unittest("the bulk bindings equal the elemental ones at every thread count", &
                         test_bulk_matches_scalar), &
            new_unittest("a shared grid is read correctly by a whole OpenMP team", &
                         test_shared_across_threads) &
            ]
    end subroutine collect_tests_healpix_grid

    ! ---- The delegation sweep ----

    !> Every conversion binding, against the free procedure it stands for, exactly.
    !>
    !> The vacuity guard is the case count: the loops must produce 1848 comparisons, so a loop that
    !> silently stops iterating fails here rather than passing with nothing asserted.
    subroutine test_delegation_matches(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: g
        integer :: is, isch, scheme, ncase
        integer(int64) :: ns, ip64, ref64, vecref, k
        integer(int32) :: ip32, ref32
        real(real64) :: th, ph, tref, pref, v(3), vref(3), dir(3)

        ncase = 0
        do is = 1, size(sweep_nside)
            ns = sweep_nside(is)
            do isch = 1, 2
                scheme = merge(PF_HP_RING, PF_HP_NEST, isch == 1)
                call g%init(ns, scheme)
                do k = 1_int64, 6_int64
                    call sweep_direction(k, dir)
                    call pf_vec2ang(dir, th, ph)

                    ! ang2pix, both kinds
                    call g%ang2pix(th, ph, ip64)
                    if (scheme == PF_HP_NEST) then
                        call pf_ang2pix_nest(ns, th, ph, ref64)
                    else
                        call pf_ang2pix_ring(ns, th, ph, ref64)
                    end if
                    call check(error, ip64, ref64, "%ang2pix disagreed with the free procedure")
                    if (allocated(error)) return
                    call g%ang2pix(th, ph, ip32)
                    call check(error, int(ip32, int64), ref64, "%ang2pix int32 disagreed")
                    if (allocated(error)) return

                    ! vec2pix, against the free vector form -- NOT against %ang2pix of the
                    ! same direction: vec -> (theta, phi) -> z reintroduces an acos/cos pair, so
                    ! the two forms legitimately differ by a pixel near a boundary.
                    call g%vec2pix(dir, ip64)
                    if (scheme == PF_HP_NEST) then
                        call pf_vec2pix_nest(ns, dir, vecref)
                    else
                        call pf_vec2pix_ring(ns, dir, vecref)
                    end if
                    call check(error, ip64, vecref, "%vec2pix disagreed with the free procedure")
                    if (allocated(error)) return

                    ! pix2ang against the free procedure
                    call g%pix2ang(ref64, th, ph)
                    if (scheme == PF_HP_NEST) then
                        call pf_pix2ang_nest(ns, ref64, tref, pref)
                    else
                        call pf_pix2ang_ring(ns, ref64, tref, pref)
                    end if
                    call check(error, th, tref, "%pix2ang theta disagreed", thr=0.0_real64)
                    if (allocated(error)) return
                    call check(error, ph, pref, "%pix2ang phi disagreed", thr=0.0_real64)
                    if (allocated(error)) return

                    ! pix2vec against the free procedure
                    call g%pix2vec(ref64, v)
                    if (scheme == PF_HP_NEST) then
                        call pf_pix2vec_nest(ns, ref64, vref)
                    else
                        call pf_pix2vec_ring(ns, ref64, vref)
                    end if
                    call check(error, maxval(abs(v - vref)), 0.0_real64, &
                               "%pix2vec disagreed with the free procedure", thr=0.0_real64)
                    if (allocated(error)) return

                    ! and the int32 index path, which every sweep nside can address
                    ref32 = int(ref64, int32)
                    call g%pix2ang(ref32, th, ph)
                    call check(error, th, tref, "%pix2ang int32 theta disagreed", thr=0.0_real64)
                    if (allocated(error)) return
                    ncase = ncase + 1
                end do
            end do
        end do
        call check(error, ncase, 7 * 2 * 6, "the delegation sweep did not run every case")
    end subroutine test_delegation_matches

    !> The three disc bindings, against `pf_query_disc` and friends, in both schemes.
    subroutine test_disc_delegation(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: g
        integer :: isch, scheme, ncase
        integer(int64) :: got(4096), ref(4096), ngot, nref, k
        integer(int64), allocatable :: alloc_got(:)
        real(real64) :: dir(3)

        ncase = 0
        call sweep_direction(5_int64, dir)
        do isch = 1, 2
            scheme = merge(PF_HP_RING, PF_HP_NEST, isch == 1)
            call g%init(256_int64, scheme)
            call g%query_disc(dir, 0.05_real64, got, ngot)
            call pf_query_disc(256_int64, dir, 0.05_real64, ref, nref, scheme=scheme)
            call check(error, ngot, nref, "%query_disc returned a different count")
            if (allocated(error)) return
            do k = 1_int64, nref
                call check(error, got(k), ref(k), "%query_disc returned a different pixel")
                if (allocated(error)) return
            end do
            call g%query_disc_count(dir, 0.05_real64, ngot)
            call check(error, ngot, nref, "%query_disc_count disagreed with %query_disc")
            if (allocated(error)) return
            call g%query_disc_alloc(dir, 0.05_real64, alloc_got, ngot)
            call check(error, ngot, nref, "%query_disc_alloc returned a different count")
            if (allocated(error)) return
            call check(error, allocated(alloc_got), "%query_disc_alloc left listpix unallocated")
            if (allocated(error)) return
            call check(error, int(size(alloc_got), int64), nref, &
                       "%query_disc_alloc sized listpix to something other than nlist")
            if (allocated(error)) return
            do k = 1_int64, nref
                call check(error, alloc_got(k), ref(k), "%query_disc_alloc differed")
                if (allocated(error)) return
            end do
            ! The RA/Dec spelling must find the same disc, given the same centre in degrees.
            call g%query_disc_radec(0.0_real64, 0.0_real64, 2.0_real64, got, ngot)
            call pf_query_disc(256_int64, [1.0_real64, 0.0_real64, 0.0_real64], &
                               2.0_real64 * pi / 180.0_real64, ref, nref, scheme=scheme)
            call check(error, ngot, nref, "%query_disc_radec returned a different count")
            if (allocated(error)) return
            do k = 1_int64, nref
                call check(error, got(k), ref(k), "%query_disc_radec returned a different pixel")
                if (allocated(error)) return
            end do
            ncase = ncase + 1
        end do
        call check(error, ncase, 2, "the disc sweep did not run both schemes")
    end subroutine test_disc_delegation

    ! ---- State ----

    !> Every accessor reports what `%init` was given, and the derived ones agree with the free forms.
    subroutine test_accessors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: g
        integer(int64) :: npix

        call g%init(1024_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
        call check(error, g%is_set(), "%is_set was false after %init")
        if (allocated(error)) return
        call check(error, g%order(), 10, "%order was not log2(nside)")
        if (allocated(error)) return
        call g%get_npix(npix)
        call check(error, npix, 12582912_int64, "%get_npix was not 12*nside**2")
        if (allocated(error)) return
        call check(error, g%scheme(), PF_HP_NEST, "%scheme did not report what init was given")
        if (allocated(error)) return
        call check(error, g%frame(), PF_HP_DEC_SOUTH, "%frame did not report what init was given")
        if (allocated(error)) return
        call check(error, g%pixarea(), pf_nside2pixarea(1024_int64), &
                   "%pixarea disagreed with pf_nside2pixarea", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, g%resol(), pf_nside2resol(1024_int64), &
                   "%resol disagreed with pf_nside2resol", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, g%max_pixrad(), pf_max_pixrad(1024_int64), &
                   "%max_pixrad disagreed with pf_max_pixrad", thr=0.0_real64)
    end subroutine test_accessors

    !> `%get_nside`/`%get_npix` resolve on the caller's kind and agree across the two.
    !>
    !> The int32 overflow abort itself cannot be asserted in process -- `error stop` kills it -- so
    !> it is an error scenario (`healpix_grid_npix_int32_overflow`). What is checked here is the
    !> case just below the ceiling, which must NOT abort: at nside 8192 npix is 805306368, the
    !> largest a valid int32 grid can hold.
    subroutine test_kind_matching_accessors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: g
        integer(int32) :: n32, p32
        integer(int64) :: n64, p64

        call g%init(8192_int64, PF_HP_RING)
        call g%get_nside(n32)
        call g%get_nside(n64)
        call check(error, int(n32, int64), n64, "%get_nside disagreed between the two kinds")
        if (allocated(error)) return
        call check(error, n64, 8192_int64, "%get_nside was not the nside init was given")
        if (allocated(error)) return
        call g%get_npix(p32)
        call g%get_npix(p64)
        call check(error, int(p32, int64), p64, "%get_npix disagreed between the two kinds")
        if (allocated(error)) return
        call check(error, p64, 805306368_int64, "%get_npix was not 12*nside**2")
    end subroutine test_kind_matching_accessors

    ! ---- The RA/Dec layer ----

    !> The two conventions are exact mirror images: north at `dec` is south at `-dec`.
    !>
    !> This is the assertion that catches a sign error in one frame. It does NOT catch the same
    !> error made in both, which is what `test_frame_literal` is for -- the two are independent on
    !> purpose.
    subroutine test_frame_mirror_identity(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: gn, gs
        integer(int64) :: ip_n, ip_s, k
        integer :: ncase
        real(real64) :: ra, dec

        call gn%init(256_int64, PF_HP_RING, frame=PF_HP_DEC_NORTH)
        call gs%init(256_int64, PF_HP_RING, frame=PF_HP_DEC_SOUTH)
        ncase = 0
        do k = 0_int64, 17_int64
            ra = 20.0_real64 * real(k, real64)
            dec = -85.0_real64 + 10.0_real64 * real(k, real64)
            call gn%radec2pix(ra, dec, ip_n)
            call gs%radec2pix(ra, -dec, ip_s)
            call check(error, ip_n, ip_s, "the two frames were not mirror images at this position")
            if (allocated(error)) return
            ncase = ncase + 1
        end do
        call check(error, ncase, 18, "the mirror sweep did not run every position")
    end subroutine test_frame_mirror_identity

    !> `%radec2pix` equals `pf_ang2pix_*` fed the conversion written out here rather than in the library.
    subroutine test_frame_literal(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: gn, gs
        integer(int64) :: got, want, k
        integer :: ncase
        real(real64) :: ra, dec, d2r

        d2r = pi / 180.0_real64
        call gn%init(512_int64, PF_HP_NEST, frame=PF_HP_DEC_NORTH)
        call gs%init(512_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
        ncase = 0
        do k = 0_int64, 17_int64
            ra = 20.0_real64 * real(k, real64)
            dec = -85.0_real64 + 10.0_real64 * real(k, real64)
            call gn%radec2pix(ra, dec, got)
            call pf_ang2pix_nest(512_int64, (90.0_real64 - dec) * d2r, ra * d2r, want)
            call check(error, got, want, "PF_HP_DEC_NORTH was not theta = (90 - dec) in radians")
            if (allocated(error)) return
            call gs%radec2pix(ra, dec, got)
            call pf_ang2pix_nest(512_int64, (90.0_real64 + dec) * d2r, ra * d2r, want)
            call check(error, got, want, "PF_HP_DEC_SOUTH was not theta = (90 + dec) in radians")
            if (allocated(error)) return
            ncase = ncase + 1
        end do
        call check(error, ncase, 18, "the literal sweep did not run every position")
    end subroutine test_frame_literal

    !> `%pix2radec` inverts `%radec2pix`, and reports ra in `[0, 360)` and dec in `[-90, 90]`.
    subroutine test_radec_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: g
        integer :: iframe, frame, ncase
        integer(int64) :: ip, back, k, npix
        real(real64) :: ra, dec, v(3), ra2, dec2

        ncase = 0
        do iframe = 1, 2
            frame = merge(PF_HP_DEC_NORTH, PF_HP_DEC_SOUTH, iframe == 1)
            call g%init(128_int64, PF_HP_RING, frame=frame)
            call g%get_npix(npix)
            do k = 0_int64, 11_int64
                ip = modulo(k * 15749_int64 + 7_int64, npix)
                call g%pix2radec(ip, ra, dec)
                call check(error, ra >= 0.0_real64 .and. ra < 360.0_real64, &
                           "%pix2radec returned a right ascension outside [0, 360)")
                if (allocated(error)) return
                call check(error, dec >= -90.0_real64 .and. dec <= 90.0_real64, &
                           "%pix2radec returned a declination outside [-90, 90]")
                if (allocated(error)) return
                call g%radec2pix(ra, dec, back)
                call check(error, back, ip, "the RA/Dec round trip did not return the same pixel")
                if (allocated(error)) return
                ! and the vector spelling of the same pair
                call g%radec2vec(ra, dec, v)
                call g%vec2radec(v, ra2, dec2)
                call check(error, abs(ra2 - ra) < 1.0e-9_real64, &
                           "%radec2vec / %vec2radec did not round trip in right ascension")
                if (allocated(error)) return
                call check(error, abs(dec2 - dec) < 1.0e-9_real64, &
                           "%radec2vec / %vec2radec did not round trip in declination")
                if (allocated(error)) return
                ncase = ncase + 1
            end do
        end do
        call check(error, ncase, 24, "the round-trip sweep did not run every case")
    end subroutine test_radec_round_trip

    !> A grid built without `frame=` behaves exactly as one built with `PF_HP_DEC_NORTH`.
    !>
    !> The assertion that would catch the default being wired to the wrong selector. Nothing else
    !> in this suite would notice, because every other RA/Dec test names its frame.
    subroutine test_default_frame(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: gd, gn
        integer(int64) :: ip_d, ip_n

        call gd%init(64_int64, PF_HP_RING)
        call gn%init(64_int64, PF_HP_RING, frame=PF_HP_DEC_NORTH)
        call check(error, gd%frame(), PF_HP_DEC_NORTH, &
                   "a grid built without frame= did not report PF_HP_DEC_NORTH")
        if (allocated(error)) return
        call gd%radec2pix(123.5_real64, 41.25_real64, ip_d)
        call gn%radec2pix(123.5_real64, 41.25_real64, ip_n)
        call check(error, ip_d, ip_n, "the default frame did not behave as PF_HP_DEC_NORTH")
    end subroutine test_default_frame

    !> Every total binding on an unbuilt grid reports its sentinel and raises nothing.
    subroutine test_unbuilt_sentinels(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: g
        integer(int32) :: ip32
        integer(int64) :: ip64
        real(real64) :: th, ph, v(3), ra, dec

        call check(error, g%is_set(), .false., "an untouched grid reported %is_set")
        if (allocated(error)) return
        call check(error, g%order(), -1, "an untouched grid did not report order -1")
        if (allocated(error)) return
        call check(error, g%frame(), PF_HP_DEC_NORTH, &
                   "an untouched grid did not report the default frame")
        if (allocated(error)) return
        call g%ang2pix(0.5_real64, 0.5_real64, ip64)
        call check(error, ip64, -1_int64, "%ang2pix on an unbuilt grid was not -1")
        if (allocated(error)) return
        call g%ang2pix(0.5_real64, 0.5_real64, ip32)
        call check(error, int(ip32, int64), -1_int64, "%ang2pix int32 on an unbuilt grid was not -1")
        if (allocated(error)) return
        call g%radec2pix(10.0_real64, 10.0_real64, ip64)
        call check(error, ip64, -1_int64, "%radec2pix on an unbuilt grid was not -1")
        if (allocated(error)) return
        call g%pix2ang(0_int64, th, ph)
        call check(error, th, -999.0_real64, "%pix2ang theta on an unbuilt grid was not -999", &
                   thr=0.0_real64)
        if (allocated(error)) return
        call g%pix2vec(0_int64, v)
        call check(error, maxval(abs(v + 999.0_real64)), 0.0_real64, &
                   "%pix2vec on an unbuilt grid was not -999 in every component", thr=0.0_real64)
        if (allocated(error)) return
        call g%pix2radec(0_int64, ra, dec)
        call check(error, ra, -999.0_real64, "%pix2radec ra on an unbuilt grid was not -999", &
                   thr=0.0_real64)
        if (allocated(error)) return
        call check(error, g%pixarea(), -1.0_real64, "%pixarea on an unbuilt grid was not -1", &
                   thr=0.0_real64)
    end subroutine test_unbuilt_sentinels

    ! ---- Derived grids, resolution change and comparison ----

    !> `%at_nside`/`%at_order` carry the scheme and the frame, and refuse an invalid resolution.
    subroutine test_derived_grids(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: g, fine, same, bad
        integer(int64) :: npix

        call g%init(256_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
        fine = g%at_nside(1024_int64)
        call check(error, fine%is_set(), "%at_nside produced an unbuilt grid for a valid nside")
        if (allocated(error)) return
        call fine%get_npix(npix)
        call check(error, npix, 12582912_int64, "%at_nside did not change the resolution")
        if (allocated(error)) return
        call check(error, fine%scheme(), PF_HP_NEST, "%at_nside did not keep the scheme")
        if (allocated(error)) return
        call check(error, fine%frame(), PF_HP_DEC_SOUTH, "%at_nside did not keep the frame")
        if (allocated(error)) return
        same = g%at_order(10_int64)
        call check(error, same == fine, "%at_order(10) did not equal %at_nside(1024)")
        if (allocated(error)) return
        bad = g%at_nside(100_int64)
        call check(error, bad%is_set(), .false., "%at_nside accepted a non-power-of-two nside")
        if (allocated(error)) return
        call check(error, bad%order(), -1, "an unbuilt derived grid did not report order -1")
    end subroutine test_derived_grids

    !> `%ud_pix` matches `pf_ud_pix_nest`, and reports -1 on a RING grid.
    subroutine test_ud_pix(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: coarse, fine, ring
        integer(int64) :: got, want, k
        integer :: ncase

        call coarse%init(256_int64, PF_HP_NEST)
        fine = coarse%at_nside(1024_int64)
        call ring%init(256_int64, PF_HP_RING)
        ncase = 0
        do k = 0_int64, 9_int64
            call coarse%ud_pix(k * 7919_int64, 10_int64, got)
            call pf_ud_pix_nest(k * 7919_int64, 8_int64, 10_int64, want)
            call check(error, got, want, "%ud_pix to an order disagreed with pf_ud_pix_nest")
            if (allocated(error)) return
            call coarse%ud_pix(k * 7919_int64, fine, got)
            call check(error, got, want, "%ud_pix to a grid disagreed with %ud_pix to its order")
            if (allocated(error)) return
            call ring%ud_pix(k * 7919_int64, 10_int64, got)
            call check(error, got, -1_int64, "%ud_pix on a RING grid was not -1")
            if (allocated(error)) return
            ncase = ncase + 1
        end do
        call check(error, ncase, 10, "the ud_pix sweep did not run every case")
    end subroutine test_ud_pix

    !> `==` and `/=` compare all three of resolution, scheme and declination convention.
    subroutine test_comparison(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: a, b

        call a%init(256_int64, PF_HP_NEST, frame=PF_HP_DEC_NORTH)
        call b%init(256_int64, PF_HP_NEST, frame=PF_HP_DEC_NORTH)
        call check(error, a == b, "two identically built grids did not compare equal")
        if (allocated(error)) return
        call check(error, a /= b, .false., "two identically built grids compared unequal")
        if (allocated(error)) return
        call b%init(512_int64, PF_HP_NEST, frame=PF_HP_DEC_NORTH)
        call check(error, a /= b, "grids differing in nside compared equal")
        if (allocated(error)) return
        call b%init(256_int64, PF_HP_RING, frame=PF_HP_DEC_NORTH)
        call check(error, a /= b, "grids differing in scheme compared equal")
        if (allocated(error)) return
        call b%init(256_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
        call check(error, a /= b, "grids differing only in frame compared equal")
    end subroutine test_comparison

    ! ---- Bulk and concurrency ----

    !> Every bulk binding equals the elemental one, at one thread and at several.
    subroutine test_bulk_matches_scalar(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: g
        integer, parameter :: n = 1000
        real(real64) :: th(n), ph(n), ra(n), dec(n), tb(n), pb(n), rb(n), db(n), vecs(3, n)
        integer(int64) :: ipix(n), ipix_rd(n), bulk(n)
        integer :: k, it, nt, ncase

        call g%init(512_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
        do k = 1, n
            ra(k) = 360.0_real64 * real(k - 1, real64) / real(n, real64)
            dec(k) = -89.0_real64 + 178.0_real64 * real(k - 1, real64) / real(n - 1, real64)
            th(k) = (90.0_real64 + dec(k)) * pi / 180.0_real64
            ph(k) = ra(k) * pi / 180.0_real64
        end do
        call g%ang2pix(th, ph, ipix)
        ! The RA/Dec rows get their OWN reference, from the scalar %radec2pix rather than from
        ! %ang2pix of the angles computed above. The two are the same quantity but not the same
        ! expression: this test writes `(90 + dec) * pi / 180.0`, which associates as
        ! `((90 + dec) * pi) / 180`, while the library multiplies by a precomputed `pi/180`. That
        ! is a last-bit difference, and a last-bit difference in a colatitude flips a pixel
        ! whenever the direction sits near a boundary. gfortran happened not to flip one here;
        ! ifx did, on the first run. Comparing a bulk form against its own scalar form is the
        ! assertion that was meant, and is exact by construction.
        call g%radec2pix(ra, dec, ipix_rd)
        ncase = 0
        do it = 1, 2
            nt = merge(1, 4, it == 1)
            call g%ang2pix_bulk(th, ph, bulk, threads=nt)
            call check(error, count(bulk /= ipix), 0, "%ang2pix_bulk differed from %ang2pix")
            if (allocated(error)) return
            call g%radec2pix_bulk(ra, dec, bulk, threads=nt)
            call check(error, count(bulk /= ipix_rd), 0, "%radec2pix_bulk differed from %radec2pix")
            if (allocated(error)) return
            call g%pix2ang_bulk(ipix, tb, pb, threads=nt)
            call g%pix2vec_bulk(ipix, vecs, threads=nt)
            call g%vec2pix_bulk(vecs, bulk, threads=nt)
            call check(error, count(bulk /= ipix), 0, "%vec2pix_bulk did not invert %pix2vec_bulk")
            if (allocated(error)) return
            call g%pix2radec_bulk(ipix, rb, db, threads=nt)
            call g%radec2pix_bulk(rb, db, bulk, threads=nt)
            call check(error, count(bulk /= ipix), 0, &
                       "%pix2radec_bulk did not round trip through %radec2pix_bulk")
            if (allocated(error)) return
            ncase = ncase + 1
        end do
        call check(error, ncase, 2, "the bulk sweep did not run both thread counts")
    end subroutine test_bulk_matches_scalar

    !> A grid shared across an OpenMP team is read correctly by every thread.
    !>
    !> The type has no allocatable components, so this is expected to hold; the test is what keeps
    !> it true if a component is ever added.
    subroutine test_shared_across_threads(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(pf_healpix_grid) :: g
        integer, parameter :: n = 2000
        integer(int64) :: serial(n), shared_out(n), k
        real(real64) :: th, ph

        call g%init(256_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
        do k = 1_int64, int(n, int64)
            th = pi * real(k, real64) / real(n + 1, real64)
            ph = 6.0_real64 * real(mod(k, 97_int64), real64) / 97.0_real64
            call g%ang2pix(th, ph, serial(k))
        end do
        shared_out = -2_int64
        !$omp parallel do default(shared) private(k, th, ph) schedule(static)
        do k = 1_int64, int(n, int64)
            th = pi * real(k, real64) / real(n + 1, real64)
            ph = 6.0_real64 * real(mod(k, 97_int64), real64) / 97.0_real64
            call g%ang2pix(th, ph, shared_out(k))
        end do
        call check(error, count(shared_out /= serial), 0, &
                   "a grid shared across an OpenMP team gave a different answer on some element")
    end subroutine test_shared_across_threads

    ! ---- Fixture ----

    !> The `k`-th sweep direction: both poles, a seam, a cap/belt boundary and a general direction.
    subroutine sweep_direction(k, dir)
        integer(int64), intent(in) :: k !! which direction, `1 .. 6`.
        real(real64), intent(out) :: dir(3) !! a unit vector.

        select case (k)
        case (1_int64)
            dir = [0.0_real64, 0.0_real64, 1.0_real64]      ! north pole
        case (2_int64)
            dir = [0.0_real64, 0.0_real64, -1.0_real64]     ! south pole
        case (3_int64)
            dir = [1.0_real64, 0.0_real64, 0.0_real64]      ! equator, on a face seam
        case (4_int64)
            dir = [0.70710678118654752_real64, 0.70710678118654752_real64, 0.0_real64]
        case (5_int64)
            ! the |z| = 2/3 boundary between the polar caps and the equatorial belt
            dir = [0.74535599249992990_real64, 0.0_real64, 0.66666666666666663_real64]
        case default
            dir = [0.42426406871192851_real64, 0.56568542494923801_real64, 0.70710678118654752_real64]
        end select
    end subroutine sweep_direction

end module test_healpix_grid
