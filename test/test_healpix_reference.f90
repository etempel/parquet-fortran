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

    !> pi, for wrapping a longitude difference into `[-pi, pi]`.
    real(real64), parameter :: pi_ref = 3.141592653589793238462643_real64

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
                         test_max_pixrad_matches_reference), &
            new_unittest("max_pixrad is accurate where the reference itself is not", &
                         test_max_pixrad_high_precision), &
            new_unittest("vec2ang reproduces the reference angles", &
                         test_vec2ang_matches_reference), &
            new_unittest("the grid arithmetic reproduces the reference tables", &
                         test_grid_matches_reference), &
            new_unittest("ud_pix_nest reproduces the reference resolution changes", &
                         test_ud_matches_reference) &
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
                sq = modulo(sq + square_mod(listpix(m)), checksum_mod)
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
            ! Relative to 1e-9, OR absolute to 1e-15, whichever is looser -- and the absolute
            ! floor is about the REFERENCE, not about this library. `hv_max_pixrad` is healpy's
            ! double-precision output, and healpy measures this angle by building the two vectors
            ! and taking the angle between them; their z-components agree to ~1e-9 at the largest
            ! nside, so that subtraction leaves the reference itself with about eight significant
            ! digits. Checked against a 60-digit evaluation, the reference is 2.1e-11 relatively
            ! wrong at nside = 2**20 and 1.5e-08 wrong at 2**29, where the true value is
            ! 1.99110981231e-09 and the table says 1.99110984188e-09.
            !
            ! `hpx_max_pixrad` forms every small quantity directly instead (see its own comment)
            ! and is accurate to ~1e-16 across the whole range, so at 2**29 it now disagrees with
            ! the reference BY THE REFERENCE'S OWN ERROR. Tightening this back to a pure relative
            ! bound would therefore assert that this library reproduces healpy's rounding rather
            ! than that it computes the right answer -- which is also what made the old bound
            ! compiler-dependent, since eight digits of headroom left the eighth digit to whether
            ! a given toolchain contracted one expression into an FMA.
            !
            ! The floor is 1e-15 against an observed 3.0e-17 absolute discrepancy: a 30x margin,
            ! still tight enough to catch any error above ~5e-07 relative at the smallest radius.
            if (abs(got - hv_max_pixrad(k)) > max(1.0e-9_real64 * hv_max_pixrad(k), 1.0e-15_real64)) then
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

    !> `max_pixrad` against a 50-DIGIT evaluation of its own defining formula, not against healpy.
    !>
    !> **This test exists because the reference comparison above cannot catch a regression here.**
    !> `hv_max_pixrad` is healpy's double-precision output, and healpy builds the two vectors and
    !> measures the angle between them -- a subtraction of z-components that agree to ~1e-9 at the
    !> largest nside, so the reference carries only about eight significant digits there. The test
    !> above therefore had to gain an absolute floor to stop asserting that this library
    !> reproduces healpy's rounding; and that floor is loose enough that the cancelling form would
    !> pass it too. Something has to pin the accuracy, and it cannot be healpy.
    !>
    !> The oracle is `atan2(|c x v|, c.v)` over the same two vectors, evaluated at 50 decimal
    !> digits with mpmath, so it shares the DEFINITION with `hpx_max_pixrad` and shares none of
    !> its arithmetic. At 1e-12 relative the build-then-measure form fails at four of the seven
    !> nside values below (4.3e-10 at 2**24, 5.5e-09 at 2**26, 2.0e-08 at 2**28, 1.5e-08 at
    !> 2**29), while the shipped form holds ~1e-16 at every one -- which is the gap this pins.
    subroutine test_max_pixrad_high_precision(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, nbad
        real(real64) :: got
        character(len=256) :: detail
        !> nside values chosen so that the cancelling form fails at the last four.
        integer(int64), parameter :: ns(7) = [1_int64, 16_int64, 1048576_int64, 16777216_int64, &
                                              67108864_int64, 268435456_int64, 536870912_int64]
        !> The true angle at each, to 18 significant digits (mpmath, 50-digit working precision).
        real(real64), parameter :: truth(7) = [ &
            8.41068670567930256e-01_real64, 6.60147614325134066e-02_real64, &
            1.01944803956972594e-06_real64, 6.37155132949673289e-08_real64, &
            1.59288784590150697e-08_real64, 3.98221962320834474e-09_real64, &
            1.99110981230872048e-09_real64]

        nbad = 0
        detail = ""
        do k = 1, size(ns)
            got = pf_max_pixrad(ns(k))
            if (abs(got - truth(k)) > 1.0e-12_real64 * truth(k)) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,es22.15,a,es22.15)') &
                    "nside=", ns(k), " true=", truth(k), " got=", got
            end if
        end do
        call check(error, nbad, 0, "max_pixrad lost precision: " // trim(detail))
    end subroutine test_max_pixrad_high_precision

    !> `pf_vec2ang` against the recorded angles, including the inputs an oracle cannot answer.
    !>
    !> **Three of the recorded cases are pinned to an EXACT value rather than to `healpy`, because
    !> healpy is wrong on them** -- it forms `arccos(z / sqrt(sum(v**2)))`, which squares before
    !> scaling and then inverts a cosine. Measured on healpy 1.20.0, against a true colatitude of
    !> pi/4 for the first two: `[1e-300, 0, 1e-300]` gives NaN (its squares underflow, then it
    !> divides by zero), `[1e300, 0, 1e300]` gives pi/2 (its squares overflow to infinity, so the
    !> answer is 45 degrees wrong), and `[1e-08, 0, 1]` gives 0 where the true value is 1e-08 (the
    !> quotient rounds to exactly 1.0 and `acos` of that is 0). The generator records this and
    !> checks those three analytically; nothing here needs to know which is which, because the
    !> table it walks already holds the right answers.
    subroutine test_vec2ang_matches_reference(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, nbad
        real(real64) :: v(3), th, ph, dphi
        character(len=200) :: detail

        nbad = 0
        detail = ""
        do k = 1, hv_n_v2a
            v = hv_v2a_vec(3 * (k - 1) + 1:3 * (k - 1) + 3)
            call pf_vec2ang(v, th, ph)
            if (abs(th - hv_v2a_theta(k)) > 1.0e-14_real64 * max(1.0_real64, hv_v2a_theta(k))) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,es22.15,a,es22.15)') "theta case=", k, &
                    " expected=", hv_v2a_theta(k), " got=", th
                cycle
            end if
            dphi = abs(modulo(ph - hv_v2a_phi(k) + pi_ref, 2.0_real64 * pi_ref) - pi_ref)
            if (dphi > 1.0e-14_real64) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,es22.15,a,es22.15)') "phi case=", k, &
                    " expected=", hv_v2a_phi(k), " got=", ph
            end if
        end do
        call check(error, hv_n_v2a > 0, "the vec2ang reference table was empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "vec2ang disagreed with the reference: " // trim(detail))
    end subroutine test_vec2ang_matches_reference

    !> The grid arithmetic against the recorded tables, at every order the module accepts.
    subroutine test_grid_matches_reference(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, nbad
        integer(int64) :: nside
        character(len=200) :: detail

        nbad = 0
        detail = ""
        do k = 1, hv_n_grid
            nside = hv_grid_nside(k)
            if (pf_nside2npix(nside) /= hv_grid_npix(k)) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,i0,a,i0)') "npix nside=", nside, &
                    " expected=", hv_grid_npix(k), " got=", pf_nside2npix(nside)
            end if
            if (pf_npix2nside(hv_grid_npix(k)) /= nside) nbad = nbad + 1
            if (pf_nside2order(nside) /= hv_grid_order(k)) nbad = nbad + 1
            if (pf_order2nside(hv_grid_order(k)) /= nside) nbad = nbad + 1
            if (abs(pf_nside2pixarea(nside) - hv_grid_pixarea(k)) > &
                1.0e-14_real64 * hv_grid_pixarea(k)) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,es22.15,a,es22.15)') "pixarea nside=", &
                    nside, " expected=", hv_grid_pixarea(k), " got=", pf_nside2pixarea(nside)
            end if
            if (abs(pf_nside2resol(nside) - hv_grid_resol(k)) > &
                1.0e-14_real64 * hv_grid_resol(k)) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,es22.15,a,es22.15)') "resol nside=", &
                    nside, " expected=", hv_grid_resol(k), " got=", pf_nside2resol(nside)
            end if
        end do
        call check(error, hv_n_grid > 0, "the grid reference table was empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "the grid arithmetic disagreed with the reference: " // &
                   trim(detail))
    end subroutine test_grid_matches_reference

    !> `pf_ud_pix_nest` against the recorded conversions.
    !>
    !> Each recorded index was obtained from a real direction at `order_in`, so `--verify-oracle`
    !> can cross-check the result through `healpy`'s `ang2pix` at both resolutions -- `healpy` has
    !> no entry point of this name to compare against directly.
    subroutine test_ud_matches_reference(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, nbad
        integer(int64) :: got
        character(len=200) :: detail

        nbad = 0
        detail = ""
        do k = 1, hv_n_ud
            call pf_ud_pix_nest(hv_ud_ipix(k), hv_ud_order_in(k), hv_ud_order_out(k), got)
            if (got /= hv_ud_result(k)) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,i0,a,i0,a,i0,a,i0)') "case=", k, &
                    " ipix=", hv_ud_ipix(k), " order ", hv_ud_order_in(k), " -> ", &
                    hv_ud_order_out(k), " got=", got
            end if
        end do
        call check(error, hv_n_ud > 0, "the resolution-change reference table was empty")
        if (allocated(error)) return
        call check(error, nbad, 0, "ud_pix_nest disagreed with the reference: " // trim(detail))
    end subroutine test_ud_matches_reference

    !> `(a * a) mod checksum_mod`, computed without ever overflowing a 64-bit integer.
    !>
    !> A recorded pixel index reaches 4.1e12 here, so its square is about 1.7e25 and a plain
    !> `a * a` overflows int64. That overflow is undefined behaviour: gfortran, ifx and flang wrap
    !> silently, and `nagfor -C=intovf` -- which `fpm.toml`'s `nagdeb` profile turns on -- makes it
    !> a fatal runtime error. The wrapped answer happens to be the RIGHT one, because 2**61 divides
    !> 2**64 so reducing a wrapped product modulo 2**61 still gives the true product modulo 2**61,
    !> which is why this reproduced the reference checksums on every other compiler. It is still
    !> undefined, and CLAUDE.md's "a compiler that WRAPS may still use the overflow's undefinedness
    !> to delete a branch somewhere else" rule applies to a test exactly as it does to the library.
    !>
    !> Splitting `a` at bit 30 keeps every partial product inside int64: with `a = ah*2**30 + al`,
    !> `a*a` is `ah*ah*2**60 + 2*ah*al*2**30 + al*al`, of which the first term contributes only its
    !> lowest bit once the result is reduced modulo 2**61. Reducing `a` itself first is what bounds
    !> `ah` below 2**31, and is exact for the same reason the wrapping was: `a` and `a mod 2**61`
    !> have the same square modulo 2**61.
    pure function square_mod(a) result(res)
        integer(int64), intent(in) :: a !! a non-negative pixel index.
        integer(int64) :: res           !! `(a*a) mod checksum_mod`, in `[0, checksum_mod)`.
        integer(int64), parameter :: mask30 = 1073741823_int64
        integer(int64) :: r, hi, lo, mid

        r = iand(a, checksum_mod - 1_int64)
        lo = iand(r, mask30)
        hi = ishft(r, -30)
        mid = iand(hi * lo, mask30)
        res = iand(ishft(iand(hi * hi, 1_int64), 60) + ishft(2_int64 * mid, 30) + lo * lo, &
                   checksum_mod - 1_int64)
    end function square_mod

end module test_healpix_reference
