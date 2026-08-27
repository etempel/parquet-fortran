!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `parquet_healpix` against the committed reference vectors.
!>
!> **This is the suite that makes a from-scratch numerical module trustworthy, and it is the only
!> one here whose oracle is external.** Everything in `test_healpix.f90` compares the module
!> against itself -- round trips, an enumeration oracle, a metric oracle -- which is exactly what a
!> consistently wrong implementation would pass. The vectors in `test_healpix_vectors.f90` are
!> derived by an independent Python model of the published pixelisation and cross-checked against
!> `healpy`, so agreeing with them is a statement about the world rather than about this module.
!>
!> `tools/generate_healpix_reference.py` emits those vectors; `--check` (run in CI) proves the
!> committed file is what the generator produces, and `--verify-oracle` re-confirms every value
!> against `healpy`. Never hand-edit a value there.
!>
!> **Boundary cases are deliberately not in the vectors.** The generator perturbs every fixture and
!> rejects any whose answer moves, so nothing frozen here sits within a fraction of a pixel of a
!> pixel boundary or within an ulp of a disc's rim. Cross-implementation agreement there is not
!> promised and could not be; it is `test_healpix.f90`'s oracles, where both sides are the same
!> arithmetic, that cover those.
!>
!> Every test walks whole tables and reports the FIRST disagreement with its full coordinates,
!> rather than asserting per element: a table of a few thousand values would otherwise need a few
!> thousand `check` calls, and the coordinates are what make a failure diagnosable anyway. Each
!> also asserts the table it walked was non-empty, so a truncated regeneration fails loudly rather
!> than passing vacuously.
module test_healpix_reference
    use parquet_healpix
    use test_healpix_vectors
    use iso_fortran_env, only : int32, int64, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_tests_healpix_reference

    !> Absolute tolerance on a recorded angle, in radians.
    !>
    !> The vectors carry 17 significant digits, so a value that round-trips through the emitted
    !> literal is exact to an ulp; this bound is what remains once `acos` near a pole is allowed
    !> for. At `nside = 2**29` the first ring's `z` is within 2e-18 of 1, which a double resolves
    !> to about three digits, so `theta` there is good to ~1e-9 rather than to an ulp -- see
    !> `pf_pix2ang_ring`'s own doc-comment. Longitude carries no such loss.
    real(real64), parameter :: ang_tol = 1.0e-8_real64

    !> Absolute tolerance on a recorded separation, in radians.
    real(real64), parameter :: dist_tol = 1.0e-12_real64

    !> Modulus the recorded disc checksums are taken to.
    integer(int64), parameter :: checksum_mod = 2305843009213693952_int64

    !> Elements allocated for a disc result here.
    !>
    !> The largest recorded query is a whole-sphere disc at nside 64, which returns all 49152
    !> pixels. Sized with room to spare deliberately: a buffer that is exactly large enough would
    !> make `pf_query_disc`'s too-small-buffer abort a live hazard in a suite whose job is to test
    !> the answers, and that abort has its own out-of-process scenario.
    integer, parameter :: disc_buffer = 65536

contains

    !> Registers every test in this suite.
    subroutine collect_tests_healpix_reference(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("ang2pix reproduces the reference pixels in both schemes", &
                         test_ang2pix_matches_reference), &
            new_unittest("ang2pix agrees across the int32 and int64 kinds", &
                         test_ang2pix_kinds_agree), &
            new_unittest("pix2ang reproduces the reference centres in both schemes", &
                         test_pix2ang_matches_reference), &
            new_unittest("ring2nest and nest2ring reproduce the reference maps", &
                         test_scheme_maps_match_reference), &
            new_unittest("pix2vec agrees with pix2ang on every reference pixel", &
                         test_pix2vec_matches_pix2ang), &
            new_unittest("query_disc reproduces every recorded pixel list", &
                         test_query_disc_matches_reference_lists), &
            new_unittest("query_disc reproduces every recorded disc checksum", &
                         test_query_disc_matches_reference_checksums), &
            new_unittest("angdist reproduces the reference separations", &
                         test_angdist_matches_reference), &
            new_unittest("max_pixrad reproduces the reference pixel radii", &
                         test_max_pixrad_matches_reference) &
            ]
    end subroutine collect_tests_healpix_reference

    !> Every recorded direction lands in the recorded pixel, at every nside, in both schemes.
    subroutine test_ang2pix_matches_reference(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, i, idx, nbad
        integer(int64) :: nside, got
        character(len=256) :: detail

        nbad = 0
        detail = ""
        do k = 1, hv_n_nside
            nside = hv_nside(k)
            do i = 1, hv_n_pos
                idx = (k - 1) * hv_n_pos + i
                call pf_ang2pix_ring(nside, hv_theta(i), hv_phi(i), got)
                if (got /= hv_pix_ring(idx)) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                        "ring nside=", nside, " position=", i, " expected=", hv_pix_ring(idx), " got=", got
                end if
                call pf_ang2pix_nest(nside, hv_theta(i), hv_phi(i), got)
                if (got /= hv_pix_nest(idx)) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                        "nest nside=", nside, " position=", i, " expected=", hv_pix_nest(idx), " got=", got
                end if
            end do
        end do
        call check(error, hv_n_nside * hv_n_pos > 0, "the reference position table is empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "ang2pix disagreed with the reference vectors: " // trim(detail))
    end subroutine test_ang2pix_matches_reference

    !> The int32 specifics answer exactly what the int64 ones do, wherever the kind can hold it.
    !>
    !> The two share one worker, so this is a test of the shims rather than of the pixelisation --
    !> and the shims are where a kind conversion can silently truncate. `hv_nside_int32_max` is the
    !> largest nside whose pixel indices fit `integer(int32)` at all.
    subroutine test_ang2pix_kinds_agree(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, i, checked, nbad
        integer(int32) :: got32
        integer(int64) :: nside, got64
        character(len=256) :: detail

        nbad = 0
        checked = 0
        detail = ""
        do k = 1, hv_n_nside
            nside = hv_nside(k)
            if (nside > hv_nside_int32_max) cycle
            do i = 1, hv_n_pos
                call pf_ang2pix_ring(int(nside, int32), hv_theta(i), hv_phi(i), got32)
                call pf_ang2pix_ring(nside, hv_theta(i), hv_phi(i), got64)
                checked = checked + 1
                if (int(got32, int64) /= got64) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                        "ring nside=", nside, " position=", i, " int64=", got64, " int32=", got32
                end if
                call pf_ang2pix_nest(int(nside, int32), hv_theta(i), hv_phi(i), got32)
                call pf_ang2pix_nest(nside, hv_theta(i), hv_phi(i), got64)
                if (int(got32, int64) /= got64) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                        "nest nside=", nside, " position=", i, " int64=", got64, " int32=", got32
                end if
            end do
        end do
        call check(error, checked > 0, "no nside in the reference table fits the int32 kind")
        if (allocated(error)) return
        call check(error, nbad, 0, "the int32 and int64 specifics disagreed: " // trim(detail))
    end subroutine test_ang2pix_kinds_agree

    !> Every recorded pixel's centre is where the reference says it is, in both schemes.
    subroutine test_pix2ang_matches_reference(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, j, idx, nbad
        integer(int64) :: nside, pix
        real(real64) :: theta, phi
        character(len=256) :: detail

        nbad = 0
        detail = ""
        do k = 1, hv_n_nside
            nside = hv_nside(k)
            do j = 1, hv_n_probe
                idx = (k - 1) * hv_n_probe + j
                pix = hv_probe_pix(idx)
                call pf_pix2ang_ring(nside, pix, theta, phi)
                if (abs(theta - hv_probe_theta_ring(idx)) > ang_tol .or. &
                    abs(phi - hv_probe_phi_ring(idx)) > ang_tol) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,es16.9,a,es16.9)') &
                        "ring nside=", nside, " pixel=", pix, " dtheta=", theta - hv_probe_theta_ring(idx), &
                        " dphi=", phi - hv_probe_phi_ring(idx)
                end if
                call pf_pix2ang_nest(nside, pix, theta, phi)
                if (abs(theta - hv_probe_theta_nest(idx)) > ang_tol .or. &
                    abs(phi - hv_probe_phi_nest(idx)) > ang_tol) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,es16.9,a,es16.9)') &
                        "nest nside=", nside, " pixel=", pix, " dtheta=", theta - hv_probe_theta_nest(idx), &
                        " dphi=", phi - hv_probe_phi_nest(idx)
                end if
            end do
        end do
        call check(error, hv_n_nside * hv_n_probe > 0, "the reference pixel-probe table is empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "pix2ang disagreed with the reference vectors: " // trim(detail))
    end subroutine test_pix2ang_matches_reference

    !> The two scheme conversions reproduce the reference maps, in both directions.
    subroutine test_scheme_maps_match_reference(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, j, idx, nbad
        integer(int64) :: nside, pix, got
        character(len=256) :: detail

        nbad = 0
        detail = ""
        do k = 1, hv_n_nside
            nside = hv_nside(k)
            do j = 1, hv_n_probe
                idx = (k - 1) * hv_n_probe + j
                pix = hv_probe_pix(idx)
                call pf_ring2nest(nside, pix, got)
                if (got /= hv_probe_ring2nest(idx)) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                        "ring2nest nside=", nside, " pixel=", pix, " expected=", hv_probe_ring2nest(idx), &
                        " got=", got
                end if
                call pf_nest2ring(nside, pix, got)
                if (got /= hv_probe_nest2ring(idx)) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                        "nest2ring nside=", nside, " pixel=", pix, " expected=", hv_probe_nest2ring(idx), &
                        " got=", got
                end if
            end do
        end do
        call check(error, hv_n_nside * hv_n_probe > 0, "the reference pixel-probe table is empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "a scheme conversion disagreed with the reference: " // trim(detail))
    end subroutine test_scheme_maps_match_reference

    !> The vector form and the angle form describe the same direction, on every reference pixel.
    !>
    !> Not a restatement of the two tests above: `pf_pix2vec_*` is the primitive and `pf_pix2ang_*`
    !> is derived from it, so this asserts the derivation rather than the pixelisation. It is also
    !> the only assertion here that would notice `pf_pix2vec_*` being built through an `acos`/`cos`
    !> round trip -- which is what the disc walk's exactness depends on it NOT doing.
    subroutine test_pix2vec_matches_pix2ang(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, j, idx, nbad
        integer(int64) :: nside, pix
        real(real64) :: theta, phi, vec(3), want(3), sep
        character(len=256) :: detail

        nbad = 0
        detail = ""
        do k = 1, hv_n_nside
            nside = hv_nside(k)
            do j = 1, hv_n_probe
                idx = (k - 1) * hv_n_probe + j
                pix = hv_probe_pix(idx)
                call pf_pix2ang_ring(nside, pix, theta, phi)
                call pf_pix2vec_ring(nside, pix, vec)
                want = [sin(theta) * cos(phi), sin(theta) * sin(phi), cos(theta)]
                call pf_angdist(vec, want, sep)
                if (sep > ang_tol) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,es16.9)') &
                        "ring nside=", nside, " pixel=", pix, " separation=", sep
                end if
                call pf_pix2ang_nest(nside, pix, theta, phi)
                call pf_pix2vec_nest(nside, pix, vec)
                want = [sin(theta) * cos(phi), sin(theta) * sin(phi), cos(theta)]
                call pf_angdist(vec, want, sep)
                if (sep > ang_tol) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,es16.9)') &
                        "nest nside=", nside, " pixel=", pix, " separation=", sep
                end if
            end do
        end do
        call check(error, hv_n_nside * hv_n_probe > 0, "the reference pixel-probe table is empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "pix2vec and pix2ang described different directions: " // trim(detail))
    end subroutine test_pix2vec_matches_pix2ang

    !> Every disc recorded with a full pixel list is reproduced element for element, in order.
    !>
    !> The ordering half matters as much as the contents: `pf_query_disc` promises ascending RING
    !> indices, and comparing the two sequences elementwise is what asserts it. A set comparison
    !> would pass against a walk that emits its wrapped arc in the wrong order.
    subroutine test_query_disc_matches_reference_lists(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: q, nbad, checked
        integer(int64) :: nside, nlist, off, m
        integer(int64), allocatable :: listpix(:)
        character(len=256) :: detail

        ! Allocated rather than automatic: the largest recorded disc holds every pixel of an
        ! nside 64 pixelisation, and half a megabyte of stack inside a test-drive suite that runs
        ! its tests concurrently is not worth the two lines this costs.
        allocate (listpix(disc_buffer))

        nbad = 0
        checked = 0
        detail = ""
        do q = 1, hv_n_disc
            if (.not. hv_disc_has_list(q)) cycle
            nside = hv_disc_nside(q)
            call pf_query_disc(nside, hv_disc_vec(3 * (q - 1) + 1:3 * (q - 1) + 3), hv_disc_radius(q), &
                               listpix, nlist, scheme=PF_HP_RING, inclusive=hv_disc_inclusive(q))
            checked = checked + 1
            if (nlist /= hv_disc_count(q)) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                    "disc ", q, " at nside=", nside, ": expected ", hv_disc_count(q), " pixels, got ", nlist
                cycle
            end if
            off = hv_disc_offset(q)
            do m = 1_int64, nlist
                if (listpix(m) /= hv_disc_pixels(off + m - 1_int64)) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0,a,i0)') &
                        "disc ", q, " at nside=", nside, ", element ", m, ": expected ", &
                        hv_disc_pixels(off + m - 1_int64), " got ", listpix(m)
                    exit
                end if
            end do
        end do
        call check(error, checked > 0, "no reference disc carries a full pixel list")
        if (allocated(error)) return
        call check(error, nbad, 0, "query_disc disagreed with a recorded pixel list: " // trim(detail))
    end subroutine test_query_disc_matches_reference_lists

    !> Every recorded disc reproduces its count and both index checksums, in both schemes.
    !>
    !> **This is what pins `inclusive = .true.`'s margin, and it is the only test here that would
    !> catch that margin being wrong rather than merely bounded.** The vectors were generated with
    !> the true maximum centre-to-corner distance, so a margin that is too small returns too few
    !> pixels and a margin that is too large returns too many -- either way the count moves. A
    !> contract test asserting only that every returned centre lies within `radius + max_pixrad`
    !> cannot see a margin of zero at all.
    !>
    !> The NEST half is asserted as a checksum rather than a sequence because the NEST output order
    !> is deliberately unspecified: the sum and the sum of squares fix the multiset without
    !> promising an order this module does not offer.
    subroutine test_query_disc_matches_reference_checksums(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: q, nbad
        integer(int64) :: nside, nlist, m, s, sq
        integer(int64), allocatable :: listpix(:)
        character(len=256) :: detail

        allocate (listpix(disc_buffer))

        nbad = 0
        detail = ""
        do q = 1, hv_n_disc
            nside = hv_disc_nside(q)
            call pf_query_disc(nside, hv_disc_vec(3 * (q - 1) + 1:3 * (q - 1) + 3), hv_disc_radius(q), &
                               listpix, nlist, scheme=PF_HP_RING, inclusive=hv_disc_inclusive(q))
            if (nlist /= hv_disc_count(q)) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                    "ring disc ", q, " at nside=", nside, ": expected ", hv_disc_count(q), " pixels, got ", nlist
                cycle
            end if
            s = 0_int64
            sq = 0_int64
            do m = 1_int64, nlist
                s = modulo(s + listpix(m), checksum_mod)
                sq = modulo(sq + modulo(listpix(m) * listpix(m), checksum_mod), checksum_mod)
            end do
            if (s /= hv_disc_sum(q) .or. sq /= hv_disc_sumsq(q)) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                    "ring disc ", q, " checksum: expected sum ", hv_disc_sum(q), " got ", s
                cycle
            end if
            call pf_query_disc(nside, hv_disc_vec(3 * (q - 1) + 1:3 * (q - 1) + 3), hv_disc_radius(q), &
                               listpix, nlist, scheme=PF_HP_NEST, inclusive=hv_disc_inclusive(q))
            if (nlist /= hv_disc_count(q)) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                    "nest disc ", q, " at nside=", nside, ": expected ", hv_disc_count(q), " pixels, got ", nlist
                cycle
            end if
            s = 0_int64
            do m = 1_int64, nlist
                s = modulo(s + listpix(m), checksum_mod)
            end do
            if (s /= hv_disc_nest_sum(q)) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                    "nest disc ", q, " checksum: expected sum ", hv_disc_nest_sum(q), " got ", s
            end if
        end do
        call check(error, hv_n_disc > 0, "the reference disc table is empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "query_disc disagreed with a recorded disc checksum: " // trim(detail))
    end subroutine test_query_disc_matches_reference_checksums

    !> Every recorded vector pair separates by the recorded angle.
    subroutine test_angdist_matches_reference(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, base, nbad
        real(real64) :: got
        character(len=256) :: detail

        nbad = 0
        detail = ""
        do k = 1, hv_n_angdist
            base = 6 * (k - 1)
            call pf_angdist(hv_angdist_vec(base + 1:base + 3), hv_angdist_vec(base + 4:base + 6), got)
            if (abs(got - hv_angdist(k)) > dist_tol) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,es16.9,a,es16.9)') &
                    "pair ", k, ": expected ", hv_angdist(k), " got ", got
            end if
        end do
        call check(error, hv_n_angdist > 0, "the reference separation table is empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "angdist disagreed with the reference vectors: " // trim(detail))
    end subroutine test_angdist_matches_reference

    !> The largest centre-to-corner distance matches the reference at every nside.
    !>
    !> **This is what pins `inclusive = .true.`'s margin at the resolutions a disc fixture cannot
    !> reach**, and it is not redundant with the disc tests. A disc's rim is resolved only to a
    !> fraction `2.2e-16 / radius**2` of its own radius, so a pixel-scale disc stops being
    !> reproducible somewhere above nside 2**20 and is meaningless by 2**29, where `cos(radius)` is
    !> exactly 1. The margin itself has no such limit, and freezing it directly is what catches the
    !> defect a disc fixture up there was wanted for: computing this quantity as `acos` of a dot
    !> product instead of with `atan2` is right to a part in 1e13 at nside 64, wrong by a percent
    !> at 2**24, and returns EXACTLY ZERO at 2**29 -- which would make an inclusive query silently
    !> identical to an exact one, with every disc test still passing.
    !>
    !> The tolerance is relative because the quantity spans nine orders of magnitude across the
    !> table, from 0.84 rad at nside 1 to 2e-09 at 2**29.
    subroutine test_max_pixrad_matches_reference(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, nbad
        integer(int64) :: nside
        real(real64) :: got
        character(len=256) :: detail

        nbad = 0
        detail = ""
        do k = 1, hv_n_nside
            nside = hv_nside(k)
            got = pf_max_pixrad(nside)
            if (abs(got - hv_max_pixrad(k)) > 1.0e-9_real64 * hv_max_pixrad(k)) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,es16.9,a,es16.9)') &
                    "nside=", nside, " expected=", hv_max_pixrad(k), " got=", got
            end if
            ! The int32 specific must answer identically wherever the kind can hold the nside.
            if (nside <= hv_nside_int32_max) then
                if (pf_max_pixrad(int(nside, int32)) /= got) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0)') "the two kinds disagreed at nside=", nside
                end if
            end if
        end do
        call check(error, hv_n_nside > 0, "the reference nside table is empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "max_pixrad disagreed with the reference vectors: " // trim(detail))
    end subroutine test_max_pixrad_matches_reference

end module test_healpix_reference
