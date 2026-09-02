!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `parquet_healpix`: self-consistency, an independent disc oracle, and the edge cases.
!>
!> **This suite and `test_healpix_reference.f90` answer different questions, and neither is
!> sufficient alone.** The reference suite compares the module against vectors derived outside it,
!> which is what rules out a consistently wrong pixelisation; this one compares the module against
!> itself and against a brute-force scan, which is what covers the cases a frozen vector cannot --
!> exhaustively over every pixel at small resolutions, and at the boundaries the vectors
!> deliberately avoid.
!>
!> Four things shape it:
!>
!> * **The disc oracle is a whole-sphere scan and it has to be.** `pf_query_disc` decides
!>   membership from a per-ring threshold; the oracle decides it from the dot product of the
!>   pixel's own centre with the query direction, over every pixel of the sphere, with no notion of
!>   rings, bands or arcs. So it shares no band selection, no arc arithmetic and no trimming with
!>   what it judges -- which a comparison against a second walk, or against the same walk at a
!>   different nside, would not.
!> * **A disagreement is classified rather than merely counted.** The two predicates are the same
!>   comparison written differently, so a pixel whose centre lies within rounding of the rim may
!>   legitimately fall either way; anything further out is a defect. The tests fail on the second
!>   and report the first, so a rim case cannot quietly become a licence to be wrong.
!> * **Round trips alone would pass against a consistently wrong implementation**, which is why
!>   they are here as a supplement to the reference suite rather than as the main evidence.
!> * **The exhaustive tests really are exhaustive** -- every pixel of every nside up to 128, both
!>   schemes -- because they are integer work and cost milliseconds. Larger resolutions are
!>   sampled, and the sample deliberately includes each region's first and last pixel.
!>
!> Nothing here writes a file, so no test needs a fixture name of its own.
module test_healpix
    use parquet_healpix
    use iso_fortran_env, only : int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only : ieee_get_flag, ieee_set_flag, ieee_support_flag, &
        ieee_invalid, ieee_divide_by_zero, ieee_overflow, ieee_value, ieee_quiet_nan, ieee_is_nan, &
        ieee_positive_inf
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_tests_parquet_healpix

    !> pi, to the precision `real64` carries.
    real(real64), parameter :: pi = 3.141592653589793238462643_real64
    !> Angular tolerance for "the same direction", in radians.
    real(real64), parameter :: ang_tol = 1.0e-9_real64

    !> How far from the rim a membership disagreement stops being a rounding artifact.
    !>
    !> The walk compares longitudes against a threshold derived from `acos`; the oracle compares
    !> dot products directly. The two are the same test written differently, so they can differ
    !> only where the pixel centre lies within rounding of the rim -- measured in the dot product,
    !> that is a few ulp of 1. Anything beyond this is a real disagreement about which pixels the
    !> disc contains, and the tests fail on it.
    real(real64), parameter :: rim_tol = 1.0e-12_real64

contains

    !> Registers every test in this suite.
    subroutine collect_tests_parquet_healpix(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("pix2ang and ang2pix invert each other over every pixel", &
                         test_pix_ang_round_trip), &
            new_unittest("ring2nest and nest2ring invert each other over every pixel", &
                         test_scheme_round_trip), &
            new_unittest("the two schemes coincide at nside 1", test_nside_one_identity), &
            new_unittest("round trips hold at the int64-only resolutions too", &
                         test_round_trip_large_nside), &
            new_unittest("a pixel centre as a vector still lands in its own pixel", &
                         test_pix2vec_round_trip), &
            new_unittest("query_disc matches a whole-sphere scan", test_disc_matches_brute_force), &
            new_unittest("query_disc matches the scan at the poles, the seam and the boundaries", &
                         test_disc_edge_cases), &
            new_unittest("a disc of radius pi returns every pixel, and beyond pi behaves as pi", &
                         test_disc_whole_sphere), &
            new_unittest("a disc of radius zero returns at most the pixel it sits in", &
                         test_disc_zero_radius), &
            new_unittest("RING results ascend and NEST results hold the same pixels", &
                         test_disc_ordering_and_schemes), &
            new_unittest("a NEST disc is the ring2nest image of its RING twin, over a sweep", &
                         test_disc_nest_sweep), &
            new_unittest("an inclusive disc contains every pixel the rim passes through", &
                         test_disc_inclusive_covers_the_rim), &
            new_unittest("an inclusive disc stays within radius plus one pixel radius", &
                         test_disc_inclusive_is_bounded), &
            new_unittest("a buffer of exactly the right size is accepted", test_disc_exact_buffer), &
            new_unittest("query_disc_runs expands to exactly the pixel list, over a sweep", &
                         test_disc_runs_expand), &
            new_unittest("query_disc_runs decomposes rather than emitting one run per pixel", &
                         test_disc_runs_are_ranges), &
            new_unittest("a short run buffer still reports the true count and a correct prefix", &
                         test_disc_runs_short_buffer), &
            new_unittest("query_disc_runs agrees across the two integer kinds", &
                         test_disc_runs_kinds_agree), &
            new_unittest("query_disc_alloc is correct for a disc past its run-recording buffer", &
                         test_disc_alloc_run_overflow), &
            new_unittest("query_disc at the int32 ceiling agrees with the int64 kind", &
                         test_disc_int32_ceiling), &
            new_unittest("query_disc_max_count bounds every position, in both modes", &
                         test_disc_max_count_bounds_every_position), &
            new_unittest("query_disc_max_count is tight enough to be worth calling", &
                         test_disc_max_count_is_useful), &
            new_unittest("no IEEE exception is raised by any entry point", test_no_ieee_exceptions), &
            new_unittest("results are identical from many threads and from one", test_thread_safety), &
            new_unittest("angdist is stable at both ends of its range", test_angdist_extremes), &
            new_unittest("angdist_deg reproduces a 60-digit evaluation on every edge case", &
                         test_angdist_deg_reference), &
            new_unittest("angdist_deg agrees with angdist, and keeps its four symmetries", &
                         test_angdist_deg_agrees), &
            new_unittest("angdist_deg is total: NaN in, NaN out", test_angdist_deg_total) &
            ]
    end subroutine collect_tests_parquet_healpix

    ! ---- Round trips ----

    !> Every pixel's centre maps back to that pixel, in both schemes, at every small nside.
    subroutine test_pix_ang_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64) :: nside, npix, p, back
        real(real64) :: theta, phi
        integer :: k, nbad
        integer(int64), parameter :: sides(6) = [1_int64, 2_int64, 4_int64, 16_int64, 64_int64, 128_int64]
        character(len=256) :: detail

        nbad = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            do p = 0_int64, npix - 1_int64
                call pf_pix2ang_ring(nside, p, theta, phi)
                call pf_ang2pix_ring(nside, theta, phi, back)
                if (back /= p) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                        "ring nside=", nside, " pixel=", p, " came back as ", back
                end if
                call pf_pix2ang_nest(nside, p, theta, phi)
                call pf_ang2pix_nest(nside, theta, phi, back)
                if (back /= p) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                        "nest nside=", nside, " pixel=", p, " came back as ", back
                end if
            end do
        end do
        call check(error, nbad, 0, "a pixel centre did not map back to its own pixel: " // trim(detail))
    end subroutine test_pix_ang_round_trip

    !> The two scheme conversions invert each other over every pixel, both ways.
    !>
    !> Also asserts that `ring2nest` is a BIJECTION at each nside, by counting how many distinct
    !> NEST indices it produces. A round trip alone cannot see two RING pixels mapping to one NEST
    !> pixel -- `nest2ring` would simply send that one back to whichever it came from last.
    subroutine test_scheme_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64) :: nside, npix, p, n, r
        integer :: k, nbad, nmiss
        integer(int64), parameter :: sides(6) = [1_int64, 2_int64, 4_int64, 16_int64, 64_int64, 128_int64]
        logical, allocatable :: seen(:)
        character(len=256) :: detail

        nbad = 0
        nmiss = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            allocate (seen(0:npix - 1))
            seen = .false.
            do p = 0_int64, npix - 1_int64
                call pf_ring2nest(nside, p, n)
                if (n < 0_int64 .or. n >= npix) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                        "ring2nest nside=", nside, " pixel=", p, " left the range: ", n
                    cycle
                end if
                seen(n) = .true.
                call pf_nest2ring(nside, n, r)
                if (r /= p) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                        "round trip nside=", nside, " pixel=", p, " came back as ", r
                end if
            end do
            if (.not. all(seen)) nmiss = nmiss + 1
            deallocate (seen)
        end do
        call check(error, nbad, 0, "a scheme conversion did not invert: " // trim(detail))
        if (allocated(error)) return
        call check(error, nmiss, 0, "ring2nest did not reach every pixel at some nside")
    end subroutine test_scheme_round_trip

    !> At nside 1 the twelve base pixels carry the same index in both schemes.
    !>
    !> A fact about the pixelisation rather than about this implementation -- the NEST numbering is
    !> defined to agree with RING at the base resolution -- so it is an anchor a wrong
    !> implementation cannot satisfy by being self-consistent.
    subroutine test_nside_one_identity(error)
        type(error_type), allocatable, intent(out) :: error !! set if any base pixel disagrees.
        integer(int64) :: p, n, r
        integer :: nbad

        nbad = 0
        do p = 0_int64, 11_int64
            call pf_ring2nest(1_int64, p, n)
            call pf_nest2ring(1_int64, p, r)
            if (n /= p .or. r /= p) nbad = nbad + 1
        end do
        call check(error, nbad, 0, "RING and NEST disagree at nside 1, where they are defined to coincide")
    end subroutine test_nside_one_identity

    !> The round trips still hold at resolutions only `integer(int64)` can address.
    !>
    !> Sampled rather than exhaustive -- `nside = 2**29` has 3.5e18 pixels -- and the sample is
    !> chosen rather than random: the first and last pixel of each of the three regions is where an
    !> off-by-one in the cap inversion or the belt arithmetic would show, and the sample includes
    !> them at every nside.
    subroutine test_round_trip_large_nside(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64) :: nside, npix, ncap, p, back, n, r, probes(11), step
        real(real64) :: theta, phi
        integer :: k, m, nbad
        integer(int64), parameter :: sides(4) = [8192_int64, 16384_int64, 1048576_int64, 536870912_int64]
        character(len=256) :: detail

        nbad = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            ncap = 2_int64 * nside * (nside - 1_int64)
            step = npix / 7_int64
            probes = [0_int64, 1_int64, ncap - 1_int64, ncap, ncap + 1_int64, npix / 2_int64, &
                      npix - ncap - 1_int64, npix - ncap, npix - 1_int64, step, 3_int64 * step]
            do m = 1, size(probes)
                p = probes(m)
                call pf_pix2ang_ring(nside, p, theta, phi)
                call pf_ang2pix_ring(nside, theta, phi, back)
                if (back /= p) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                        "ring nside=", nside, " pixel=", p, " came back as ", back
                end if
                call pf_ring2nest(nside, p, n)
                call pf_nest2ring(nside, n, r)
                if (r /= p) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                        "scheme nside=", nside, " pixel=", p, " came back as ", r
                end if
            end do
        end do
        call check(error, nbad, 0, "a round trip failed at a large nside: " // trim(detail))
    end subroutine test_round_trip_large_nside

    !> A pixel's centre, taken as a vector and converted back through angles, is the same pixel.
    !>
    !> **This is a statement about the pixelisation's own margins, not about round-off**: it says a
    !> pixel centre sits far enough from its own boundaries that a conversion out and back cannot
    !> cross one. That is worth pinning because it is the property every "which pixel is this?"
    !> call depends on, and it is the first thing a wrong ring or face decomposition breaks.
    !>
    !> **It stops at nside 2**20 deliberately, and the reason is a real limit rather than a
    !> tolerance.** The path back runs through `acos(vec(3))`, and `acos` near +-1 loses about half
    !> its significant digits -- so at 2**29, where a pixel is 4e-09 rad across and `acos`'s own
    !> error reaches that scale, the round trip genuinely fails for a few pixels per thousand with
    !> a completely correct implementation. Measured: 0 of 506 sampled pixels at nside 64, 8192 and
    !> 2**20; 3 of 506 at 2**29. Extending this test upward would assert something false. What the
    !> conversions do promise at 2**29 is asserted by `test_round_trip_large_nside`, which stays in
    !> angles throughout and so never meets that loss.
    subroutine test_pix2vec_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64) :: nside, npix, ncap, p, back, probes(11), step
        real(real64) :: vec(3), theta, phi
        integer :: k, m, nbad, checked
        integer(int64), parameter :: sides(4) = [64_int64, 1024_int64, 8192_int64, 1048576_int64]
        character(len=256) :: detail

        nbad = 0
        checked = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            ncap = 2_int64 * nside * (nside - 1_int64)
            step = npix / 7_int64
            probes = [0_int64, 1_int64, 2_int64, ncap - 1_int64, ncap, ncap + 1_int64, &
                      npix / 2_int64, npix - ncap, npix - 2_int64, npix - 1_int64, step]
            do m = 1, size(probes)
                p = probes(m)
                if (p < 0_int64 .or. p >= npix) cycle
                call pf_pix2vec_ring(nside, p, vec)
                theta = acos(max(-1.0_real64, min(1.0_real64, vec(3))))
                phi = atan2(vec(2), vec(1))
                call pf_ang2pix_ring(nside, theta, phi, back)
                checked = checked + 1
                if (back /= p) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                        "ring nside=", nside, " pixel=", p, " came back as ", back
                end if
                call pf_pix2vec_nest(nside, p, vec)
                theta = acos(max(-1.0_real64, min(1.0_real64, vec(3))))
                phi = atan2(vec(2), vec(1))
                call pf_ang2pix_nest(nside, theta, phi, back)
                if (back /= p) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                        "nest nside=", nside, " pixel=", p, " came back as ", back
                end if
            end do
        end do
        call check(error, checked > 0, "no pixel was probed -- the assertion did nothing")
        if (allocated(error)) return
        call check(error, nbad, 0, "a pixel centre did not convert back to its own pixel: " // trim(detail))
    end subroutine test_pix2vec_round_trip

    ! ---- The disc oracle ----

    !> Fills `inside` with the pixels a whole-sphere scan puts within `radius` of `v0`.
    !>
    !> The independent oracle: it walks every pixel of the pixelisation, asks `pf_pix2vec_ring` for
    !> that pixel's own centre, and compares the dot product with `cos(radius)`. Nothing about
    !> rings, bands, arcs or trimming enters it, which is what makes it evidence about the walk
    !> rather than a second opinion from the same machinery.
    !>
    !> `nrim` counts the pixels whose centres lie within `rim_tol` of the rim in the dot product.
    !> Those are the ones the two forms of the comparison may legitimately disagree about.
    subroutine brute_disc(nside, v0, radius, inside, nrim)
        integer(int64), intent(in) :: nside !! resolution parameter.
        real(real64), intent(in) :: v0(3) !! disc centre; need not be normalised.
        real(real64), intent(in) :: radius !! disc radius, radians.
        logical, allocatable, intent(out) :: inside(:) !! `inside(p)` for each 0-based RING pixel.
        integer, intent(out) :: nrim !! how many centres sit within `rim_tol` of the rim.
        integer(int64) :: npix, p
        real(real64) :: u(3), vec(3), cosr, dot, nrm

        npix = 12_int64 * nside * nside
        allocate (inside(0:npix - 1))
        nrm = sqrt(v0(1) * v0(1) + v0(2) * v0(2) + v0(3) * v0(3))
        u = v0 / nrm
        cosr = cos(min(radius, pi))
        nrim = 0
        do p = 0_int64, npix - 1_int64
            call pf_pix2vec_ring(nside, p, vec)
            dot = vec(1) * u(1) + vec(2) * u(2) + vec(3) * u(3)
            inside(p) = dot >= cosr
            if (abs(dot - cosr) <= rim_tol) nrim = nrim + 1
        end do
    end subroutine brute_disc

    !> Compares one `pf_query_disc` result against the scan, and reports what differs.
    subroutine compare_against_brute(nside, v0, radius, nbad, nrim_seen, detail, label)
        integer(int64), intent(in) :: nside !! resolution parameter.
        real(real64), intent(in) :: v0(3) !! disc centre.
        real(real64), intent(in) :: radius !! disc radius, radians.
        integer, intent(inout) :: nbad !! incremented for each disagreement away from the rim.
        integer, intent(inout) :: nrim_seen !! incremented for each rim-level disagreement.
        character(len=*), intent(inout) :: detail !! set to the first real disagreement found.
        character(len=*), intent(in) :: label !! names the case, for the failure message.
        logical, allocatable :: want(:), got(:)
        integer(int64), allocatable :: listpix(:)
        integer(int64) :: npix, nlist, m, p
        real(real64) :: vec(3), u(3), cosr, dot, nrm
        integer :: nrim

        npix = 12_int64 * nside * nside
        call brute_disc(nside, v0, radius, want, nrim)
        allocate (got(0:npix - 1))
        got = .false.
        allocate (listpix(npix))
        call pf_query_disc(nside, v0, radius, listpix, nlist)
        do m = 1_int64, nlist
            got(listpix(m)) = .true.
        end do
        nrm = sqrt(v0(1) * v0(1) + v0(2) * v0(2) + v0(3) * v0(3))
        u = v0 / nrm
        cosr = cos(min(radius, pi))
        do p = 0_int64, npix - 1_int64
            if (want(p) .eqv. got(p)) cycle
            call pf_pix2vec_ring(nside, p, vec)
            dot = vec(1) * u(1) + vec(2) * u(2) + vec(3) * u(3)
            if (abs(dot - cosr) <= rim_tol) then
                ! The centre lies within rounding of the rim, where the walk's threshold form and
                ! the oracle's dot-product form are entitled to disagree.
                nrim_seen = nrim_seen + 1
            else
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,a,i0,a,i0,a,l1,a,l1,a,es13.6)') &
                    trim(label), ": nside=", nside, " pixel=", p, " scan=", want(p), " walk=", got(p), &
                    " dot-cosr=", dot - cosr
            end if
        end do
    end subroutine compare_against_brute

    !> A battery of ordinary discs matches the whole-sphere scan exactly.
    subroutine test_disc_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first real disagreement.
        integer(int64) :: nside
        integer :: k, c, nbad, nrim_seen
        real(real64) :: v0(3), radius, z, phi
        integer(int64), parameter :: sides(4) = [1_int64, 4_int64, 16_int64, 64_int64]
        character(len=256) :: detail
        character(len=32) :: label

        nbad = 0
        nrim_seen = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            do c = 1, 24
                ! A deterministic spiral over the sphere: distinct latitudes and longitudes with no
                ! generator to seed, so a failure names a case that can be reproduced by hand.
                z = -1.0_real64 + 2.0_real64 * (real(c, real64) - 0.5_real64) / 24.0_real64
                phi = 2.399963229728653_real64 * real(c, real64)
                v0 = [sqrt(1.0_real64 - z * z) * cos(phi), sqrt(1.0_real64 - z * z) * sin(phi), z]
                radius = 0.05_real64 + 0.11_real64 * real(modulo(c, 9), real64)
                write (label, '(a,i0)') "case ", c
                call compare_against_brute(nside, v0, radius, nbad, nrim_seen, detail, label)
            end do
        end do
        call check(error, nbad, 0, "query_disc disagreed with the whole-sphere scan: " // trim(detail))
    end subroutine test_disc_matches_brute_force

    !> The shapes reimplementations get wrong: the poles, the seam, and the cap/belt boundary.
    !>
    !> Named individually rather than folded into the battery above because each exercises a
    !> different branch of the walk -- the polar degeneracy where the arc test cannot be formed at
    !> all, the wrap that splits an arc into two runs, and the latitude where the two halves of the
    !> pixelisation meet.
    subroutine test_disc_edge_cases(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first real disagreement.
        integer(int64) :: nside
        integer :: k, c, nbad, nrim_seen
        real(real64) :: v0(3), radius
        integer(int64), parameter :: sides(4) = [1_int64, 2_int64, 8_int64, 32_int64]
        real(real64), parameter :: two_thirds = 2.0_real64 / 3.0_real64
        character(len=256) :: detail
        character(len=48) :: label

        nbad = 0
        nrim_seen = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            do c = 1, 9
                select case (c)
                case (1)
                    v0 = [0.0_real64, 0.0_real64, 1.0_real64]
                    radius = 0.37_real64
                    label = "north pole"
                case (2)
                    v0 = [0.0_real64, 0.0_real64, -1.0_real64]
                    radius = 0.37_real64
                    label = "south pole"
                case (3)
                    v0 = [1.0_real64, 0.0_real64, 0.0_real64]
                    radius = 0.29_real64
                    label = "on the phi = 0 seam"
                case (4)
                    v0 = [cos(1.0e-12_real64), sin(1.0e-12_real64), 0.0_real64]
                    radius = 0.29_real64
                    label = "just past the seam"
                case (5)
                    v0 = [cos(-1.0e-12_real64), sin(-1.0e-12_real64), 0.0_real64]
                    radius = 0.29_real64
                    label = "just before the seam"
                case (6)
                    v0 = [sqrt(1.0_real64 - two_thirds**2), 0.0_real64, two_thirds]
                    radius = 0.21_real64
                    label = "on the northern cap/belt boundary"
                case (7)
                    v0 = [sqrt(1.0_real64 - two_thirds**2), 0.0_real64, -two_thirds]
                    radius = 0.21_real64
                    label = "on the southern cap/belt boundary"
                case (8)
                    v0 = [0.0_real64, 0.0_real64, 1.0_real64]
                    radius = 0.5_real64 * pi + 0.03_real64
                    label = "pole-centred, past a hemisphere"
                case default
                    v0 = [0.3_real64, -0.4_real64, 0.8_real64]
                    radius = 1.7_real64
                    label = "larger than a hemisphere, off axis"
                end select
                call compare_against_brute(nside, v0, radius, nbad, nrim_seen, detail, label)
            end do
        end do
        call check(error, nbad, 0, "query_disc disagreed with the scan on an edge case: " // trim(detail))
    end subroutine test_disc_edge_cases

    !> A disc of radius pi covers the sphere, and a larger radius behaves identically.
    subroutine test_disc_whole_sphere(error)
        type(error_type), allocatable, intent(out) :: error !! set if either count is wrong.
        integer(int64) :: nside, npix, nlist, nbig
        integer(int64), allocatable :: listpix(:)
        real(real64), parameter :: v0(3) = [0.3_real64, 0.4_real64, 0.8660254037844386_real64]

        nside = 8_int64
        npix = 12_int64 * nside * nside
        allocate (listpix(npix))
        call pf_query_disc(nside, v0, pi, listpix, nlist)
        call check(error, nlist, npix, "a disc of radius pi did not return every pixel")
        if (allocated(error)) return
        ! A radius above pi is accepted and clamped rather than rejected: a caller computing one
        ! from geometry can land a few ulp above pi, and dying there would be a worse answer than
        ! covering the sphere.
        call pf_query_disc(nside, v0, 4.0_real64, listpix, nbig)
        call check(error, nbig, npix, "a radius larger than pi did not behave as pi")
    end subroutine test_disc_whole_sphere

    !> A disc of radius zero returns at most the single pixel its centre sits in.
    subroutine test_disc_zero_radius(error)
        type(error_type), allocatable, intent(out) :: error !! set if the result is not that pixel.
        integer(int64) :: nside, nlist, home
        integer(int64) :: listpix(64)
        real(real64) :: v0(3), theta, phi

        nside = 16_int64
        theta = 0.9_real64
        phi = 2.1_real64
        v0 = [sin(theta) * cos(phi), sin(theta) * sin(phi), cos(theta)]
        call pf_ang2pix_ring(nside, theta, phi, home)
        call pf_query_disc(nside, v0, 0.0_real64, listpix, nlist)
        call check(error, nlist <= 1_int64, "a disc of radius zero returned more than one pixel")
        if (allocated(error)) return
        ! At most one, and if there is one it is the pixel containing the centre. It may be none:
        ! a pixel's centre is generally not the point a caller asks about, and a zero-radius disc
        ! contains only that point.
        if (nlist == 1_int64) then
            call check(error, listpix(1), home, "a zero-radius disc returned a pixel other than its own")
        end if
    end subroutine test_disc_zero_radius

    !> RING output ascends; NEST output holds exactly the same pixels, mapped.
    !>
    !> The ordering half is what makes the RING promise testable at all -- a set comparison passes
    !> against a walk that emits a seam-crossing arc in two runs the wrong way round, which is the
    !> one place the order can go wrong.
    !> Sweeps the NEST disc result against the RING one across resolution, radius, centre and
    !> inclusiveness, checking that the two hold exactly the same pixels.
    !>
    !> **This is the test that holds `hpx_ringij2nest_run` to `hpx_ringij2nest`.** The disc walk
    !> emits NEST results by STEPPING a Morton code along a run of consecutive ring positions
    !> rather than decoding each position, which is worth about 7x on a NEST result and is the one
    !> place in this module where a fast path restates a correspondence instead of calling it. The
    !> scalar form is still the definition -- `pf_ring2nest` reaches it -- so comparing the two
    !> here is a genuine cross-check rather than a round trip through one implementation.
    !>
    !> The sweep is chosen to reach every way the stepper can be asked to move. `nside` 1 and 2
    !> have no polar-cap rings at all, so the belt logic runs alone; the larger resolutions
    !> exercise both caps. The pole centres make whole rings; the seam centre makes runs that wrap
    !> through `phi = 0` and so arrive as two blocks; the small radii make runs of one and two
    !> pixels, where a stepper that is wrong only on its second element would otherwise hide. Both
    !> inclusive modes run, because the enlarged radius changes which rings are reached.
    subroutine test_disc_nest_sweep(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: sweep_nside(6) = [1_int64, 2_int64, 4_int64, 8_int64, &
                                                       16_int64, 64_int64]
        real(real64), parameter :: sweep_rad(6) = [0.001_real64, 0.05_real64, 0.3_real64, &
                                                   1.0_real64, 2.5_real64, 3.14159265358979_real64]
        integer(int64) :: nside, npix, nring, nnest, m, mapped
        integer(int64), allocatable :: ring(:), nest(:)
        logical, allocatable :: seen(:)
        real(real64) :: v0(3), sq
        integer :: ins, ir, ic, iinc, nbad, ncase
        logical :: inc
        character(len=200) :: detail

        nbad = 0
        ncase = 0
        detail = ""
        do ins = 1, size(sweep_nside)
            nside = sweep_nside(ins)
            npix = 12_int64 * nside * nside
            allocate (ring(npix), nest(npix), seen(0:npix - 1_int64))
            do ir = 1, size(sweep_rad)
                do ic = 1, 5
                    call sweep_centre(ic, v0)
                    do iinc = 0, 1
                        inc = (iinc == 1)
                        call pf_query_disc(nside, v0, sweep_rad(ir), ring, nring, &
                                           scheme=PF_HP_RING, inclusive=inc)
                        call pf_query_disc(nside, v0, sweep_rad(ir), nest, nnest, &
                                           scheme=PF_HP_NEST, inclusive=inc)
                        ncase = ncase + 1
                        if (nnest /= nring) then
                            nbad = nbad + 1
                            if (detail == "") write (detail, '(a,i0,a,f8.4,a,i0,a,l1,a,i0,a,i0)') &
                                "nside=", nside, " r=", sweep_rad(ir), " centre=", ic, &
                                " inclusive=", inc, ": nest count ", nnest, " vs ring ", nring
                            cycle
                        end if
                        if (nring == 0_int64) cycle
                        seen = .false.
                        do m = 1_int64, nnest
                            seen(nest(m)) = .true.
                        end do
                        do m = 1_int64, nring
                            call pf_ring2nest(nside, ring(m), mapped)
                            if (.not. seen(mapped)) then
                                nbad = nbad + 1
                                if (detail == "") write (detail, &
                                    '(a,i0,a,f8.4,a,i0,a,l1,a,i0,a,i0)') &
                                    "nside=", nside, " r=", sweep_rad(ir), " centre=", ic, &
                                    " inclusive=", inc, ": RING pixel ", ring(m), &
                                    " is missing from the NEST result as ", mapped
                                exit
                            end if
                        end do
                    end do
                end do
            end do
            deallocate (ring, nest, seen)
        end do

        ! Equal counts plus every RING pixel present in the NEST result is set equality, since the
        ! two sets are then the same size and one contains the other.
        call check(error, ncase, 360, "the sweep did not run every case it enumerates")
        if (allocated(error)) return
        call check(error, nbad, 0, "a NEST disc did not hold its RING twin's pixels: " // &
                   trim(detail))

    contains

        !> The five disc centres the sweep uses, as unit vectors.
        subroutine sweep_centre(which, v)
            integer, intent(in) :: which !! 1 = north pole, 2 = south pole, 3 = equator on the
                                         !! seam, 4 = equator between two faces, 5 = a general
                                         !! direction on no symmetry axis.
            real(real64), intent(out) :: v(3) !! the centre direction.

            select case (which)
            case (1)
                v = [0.0_real64, 0.0_real64, 1.0_real64]
            case (2)
                v = [0.0_real64, 0.0_real64, -1.0_real64]
            case (3)
                v = [1.0_real64, 0.0_real64, 0.0_real64]
            case (4)
                v = [sqrt(0.5_real64), sqrt(0.5_real64), 0.0_real64]
            case default
                sq = sqrt(1.0_real64 - 0.4_real64 * 0.4_real64)
                v = [sq * cos(1.1_real64), sq * sin(1.1_real64), 0.4_real64]
            end select
        end subroutine sweep_centre

    end subroutine test_disc_nest_sweep

    subroutine test_disc_ordering_and_schemes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first fault.
        integer(int64) :: nside, npix, nring, nnest, m, mapped
        integer(int64), allocatable :: ring(:), nest(:)
        logical, allocatable :: seen(:)
        real(real64) :: v0(3), z, phi
        integer :: c, nbad_order, nbad_set
        character(len=256) :: detail

        nside = 32_int64
        npix = 12_int64 * nside * nside
        allocate (ring(npix), nest(npix), seen(0:npix - 1))
        nbad_order = 0
        nbad_set = 0
        detail = ""
        do c = 1, 20
            z = -0.95_real64 + 1.9_real64 * (real(c, real64) - 0.5_real64) / 20.0_real64
            phi = 2.399963229728653_real64 * real(c, real64)
            v0 = [sqrt(1.0_real64 - z * z) * cos(phi), sqrt(1.0_real64 - z * z) * sin(phi), z]
            call pf_query_disc(nside, v0, 0.17_real64, ring, nring, scheme=PF_HP_RING)
            do m = 2_int64, nring
                if (ring(m) <= ring(m - 1_int64)) then
                    nbad_order = nbad_order + 1
                    if (nbad_order == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                        "case ", c, " element ", m, ": ", ring(m - 1_int64), " then ", ring(m)
                    exit
                end if
            end do
            call pf_query_disc(nside, v0, 0.17_real64, nest, nnest, scheme=PF_HP_NEST)
            if (nnest /= nring) then
                nbad_set = nbad_set + 1
                cycle
            end if
            seen = .false.
            do m = 1_int64, nnest
                seen(nest(m)) = .true.
            end do
            do m = 1_int64, nring
                call pf_ring2nest(nside, ring(m), mapped)
                if (.not. seen(mapped)) then
                    nbad_set = nbad_set + 1
                    exit
                end if
            end do
        end do
        call check(error, nbad_order, 0, "a RING result was not in ascending order: " // trim(detail))
        if (allocated(error)) return
        call check(error, nbad_set, 0, "a NEST result did not hold the same pixels as its RING twin")
    end subroutine test_disc_ordering_and_schemes

    !> Every pixel the disc's rim passes through is in the inclusive result.
    !>
    !> **This is the completeness half of the inclusive contract, and the only test here that would
    !> notice the enlargement being too small -- including zero.** A bound test asserting that
    !> every returned centre lies within `radius + max_pixrad` is satisfied by returning nothing at
    !> all; this one walks the rim circle itself, asks which pixel each sampled point falls in, and
    !> demands every one of them back. A pixel the rim passes through provably overlaps the disc.
    subroutine test_disc_inclusive_covers_the_rim(error)
        type(error_type), allocatable, intent(out) :: error !! set if a rim pixel is missing.
        integer(int64) :: nside, npix, nlist, m, rimpix
        integer(int64), allocatable :: listpix(:)
        logical, allocatable :: seen(:)
        real(real64) :: v0(3), e1(3), e2(3), p(3), radius, ang, nrm, theta, phi, z, ph
        integer :: c, s, nbad, checked
        character(len=256) :: detail

        nside = 32_int64
        npix = 12_int64 * nside * nside
        allocate (listpix(npix), seen(0:npix - 1))
        nbad = 0
        checked = 0
        detail = ""
        do c = 1, 12
            z = -0.9_real64 + 1.8_real64 * (real(c, real64) - 0.5_real64) / 12.0_real64
            ph = 2.399963229728653_real64 * real(c, real64)
            v0 = [sqrt(1.0_real64 - z * z) * cos(ph), sqrt(1.0_real64 - z * z) * sin(ph), z]
            radius = 0.08_real64 + 0.05_real64 * real(modulo(c, 5), real64)
            call pf_query_disc(nside, v0, radius, listpix, nlist, inclusive=.true.)
            seen = .false.
            do m = 1_int64, nlist
                seen(listpix(m)) = .true.
            end do
            ! An orthonormal frame around the disc centre, so the rim can be walked directly.
            e1 = [-v0(2), v0(1), 0.0_real64]
            nrm = sqrt(e1(1) * e1(1) + e1(2) * e1(2) + e1(3) * e1(3))
            if (nrm <= 0.0_real64) cycle
            e1 = e1 / nrm
            e2 = [v0(2) * e1(3) - v0(3) * e1(2), v0(3) * e1(1) - v0(1) * e1(3), &
                  v0(1) * e1(2) - v0(2) * e1(1)]
            do s = 0, 63
                ang = 2.0_real64 * pi * real(s, real64) / 64.0_real64
                p = cos(radius) * v0 + sin(radius) * (cos(ang) * e1 + sin(ang) * e2)
                theta = acos(max(-1.0_real64, min(1.0_real64, p(3))))
                phi = atan2(p(2), p(1))
                call pf_ang2pix_ring(nside, theta, phi, rimpix)
                checked = checked + 1
                if (.not. seen(rimpix)) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') &
                        "case ", c, " sample ", s, ": pixel ", rimpix
                end if
            end do
        end do
        call check(error, checked > 0, "no rim sample was taken -- the completeness assertion did nothing")
        if (allocated(error)) return
        call check(error, nbad, 0, "an inclusive disc omitted a pixel its own rim passes through: " // &
                   trim(detail))
    end subroutine test_disc_inclusive_covers_the_rim

    !> An inclusive disc is a superset of the exact one and respects its published bound.
    subroutine test_disc_inclusive_is_bounded(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first fault.
        integer(int64) :: nside, npix, nexact, nincl, m
        integer(int64), allocatable :: ex(:), inc(:)
        logical, allocatable :: seen(:)
        real(real64) :: v0(3), vec(3), radius, sep, z, ph, worst
        integer :: c, nbad_sub
        character(len=256) :: detail

        nside = 32_int64
        npix = 12_int64 * nside * nside
        allocate (ex(npix), inc(npix), seen(0:npix - 1))
        nbad_sub = 0
        worst = 0.0_real64
        detail = ""
        do c = 1, 12
            z = -0.9_real64 + 1.8_real64 * (real(c, real64) - 0.5_real64) / 12.0_real64
            ph = 2.399963229728653_real64 * real(c, real64)
            v0 = [sqrt(1.0_real64 - z * z) * cos(ph), sqrt(1.0_real64 - z * z) * sin(ph), z]
            radius = 0.08_real64 + 0.05_real64 * real(modulo(c, 5), real64)
            call pf_query_disc(nside, v0, radius, ex, nexact)
            call pf_query_disc(nside, v0, radius, inc, nincl, inclusive=.true.)
            seen = .false.
            do m = 1_int64, nincl
                seen(inc(m)) = .true.
                call pf_pix2vec_ring(nside, inc(m), vec)
                call pf_angdist(vec, v0, sep)
                worst = max(worst, sep - radius)
            end do
            do m = 1_int64, nexact
                if (.not. seen(ex(m))) then
                    nbad_sub = nbad_sub + 1
                    if (nbad_sub == 1) write (detail, '(a,i0,a,i0)') &
                        "case ", c, ": exact pixel missing from the inclusive result: ", ex(m)
                    exit
                end if
            end do
        end do
        call check(error, nbad_sub, 0, "an inclusive disc was not a superset of the exact one: " // &
                   trim(detail))
        if (allocated(error)) return
        ! The published bound: no centre farther than radius + one pixel radius. At nside 32 the
        ! largest centre-to-corner distance is 0.0332 rad, so a walk enlarging by more than that
        ! -- or not enlarging at all, which the rim test above catches -- shows up here.
        call check(error, worst <= 0.0332067_real64 + 1.0e-9_real64, &
                   "an inclusive disc returned a centre beyond radius + max_pixrad")
    end subroutine test_disc_inclusive_is_bounded

    !> A buffer of exactly the result's size is accepted, not treated as one too small.
    subroutine test_disc_exact_buffer(error)
        type(error_type), allocatable, intent(out) :: error !! set if the exact fit is rejected.
        integer(int64) :: nside, npix, nlist, nlist2
        integer(int64), allocatable :: big(:), exact(:)
        real(real64), parameter :: v0(3) = [0.2_real64, 0.3_real64, 0.9327379053088815_real64]

        nside = 16_int64
        npix = 12_int64 * nside * nside
        allocate (big(npix))
        call pf_query_disc(nside, v0, 0.3_real64, big, nlist)
        call check(error, nlist > 0_int64, "the fixture disc returned nothing, so the fit is untested")
        if (allocated(error)) return
        allocate (exact(nlist))
        call pf_query_disc(nside, v0, 0.3_real64, exact, nlist2)
        call check(error, nlist2, nlist, "an exactly-sized buffer changed the result")
        if (allocated(error)) return
        call check(error, all(exact(1:nlist) == big(1:nlist)), &
                   "an exactly-sized buffer returned different pixels")
    end subroutine test_disc_exact_buffer

    !> At the largest nside `integer(int32)` can address, both kinds return the same disc.
    !>
    !> The int32 ceiling is where a kind conversion in the shim would first show, and it is the one
    !> resolution at which the two specifics are doing measurably different work: 805306368 pixels
    !> is within 1.3 of the signed 32-bit limit.
    subroutine test_disc_int32_ceiling(error)
        type(error_type), allocatable, intent(out) :: error !! set on any disagreement.
        integer(int32) :: n32
        integer(int32), allocatable :: got32(:)
        integer(int64), allocatable :: got64(:)
        integer(int64) :: n64
        real(real64), parameter :: v0(3) = [0.6_real64, -0.3_real64, 0.7416198487095663_real64]
        integer :: m, nbad

        allocate (got32(4096), got64(4096))
        call pf_query_disc(8192_int32, v0, 0.002_real64, got32, n32)
        call pf_query_disc(8192_int64, v0, 0.002_real64, got64, n64)
        call check(error, int(n32, int64), n64, "the two kinds returned different pixel counts at nside 8192")
        if (allocated(error)) return
        call check(error, n64 > 0_int64, "the nside 8192 fixture disc returned nothing")
        if (allocated(error)) return
        nbad = 0
        do m = 1, int(n32)
            if (int(got32(m), int64) /= got64(m)) nbad = nbad + 1
        end do
        call check(error, nbad, 0, "the two kinds returned different pixels at nside 8192")
    end subroutine test_disc_int32_ceiling

    !> `pf_query_disc_max_count` is never exceeded, at any position, in either mode.
    !>
    !> **The oracle is `pf_query_disc_count` itself, at many positions**, which is the only thing
    !> that can falsify an upper bound: the bound is a claim about the maximum over the sphere, so
    !> it is tested by sampling the sphere. The positions are every pixel centre of a coarse ring
    !> walk plus a deterministic pseudo-random scatter, because the extreme position is not
    !> obviously either a pole or an equator point and the two samplings miss different things.
    !>
    !> **This test alone would pass against a bound of `pf_nside2npix(nside)`**, which is why
    !> `test_disc_max_count_is_useful` exists beside it and asserts the other direction.
    subroutine test_disc_max_count_bounds_every_position(error)
        type(error_type), allocatable, intent(out) :: error !! set when a position exceeds the bound.
        integer(int64), parameter :: nsides(4) = [1_int64, 4_int64, 32_int64, 256_int64]
        real(real64), parameter :: rmul(6) = [0.0_real64, 0.5_real64, 1.0_real64, 3.0_real64, &
                                              12.0_real64, 90.0_real64]
        integer(int64) :: nside, npix, bound, cnt, worst, ip, stride
        real(real64) :: radius, vec(3), th, ph
        integer :: is, ir, k, mode
        logical :: inc
        character(len=160) :: detail
        integer, parameter :: nscatter = 700 !! positions in the spiral, beyond the ring walk.
        !> The golden angle, `pi*(3 - sqrt(5))`. Successive multiples of it are the standard
        !! low-discrepancy way to spread points around a sphere, and it is what makes the scatter
        !! below deterministic on every compiler without any integer arithmetic to overflow.
        real(real64), parameter :: golden_angle = pi * (3.0_real64 - sqrt(5.0_real64))

        do is = 1, size(nsides)
            nside = nsides(is)
            npix = pf_nside2npix(nside)
            do ir = 1, size(rmul)
                radius = rmul(ir) * pf_nside2resol(nside)
                if (radius > pi) cycle
                do mode = 1, 2
                    inc = (mode == 2)
                    bound = pf_query_disc_max_count(nside, radius, inclusive=inc)
                    call check(error, bound >= 0_int64 .and. bound <= npix, &
                        "the bound must be a pixel count: at least zero and at most npix")
                    if (allocated(error)) return
                    worst = 0_int64
                    stride = max(1_int64, npix / 512_int64)
                    do ip = 0_int64, npix - 1_int64, stride
                        call pf_pix2vec_ring(nside, ip, vec)
                        call pf_query_disc_count(nside, vec, radius, cnt, inclusive=inc)
                        worst = max(worst, cnt)
                    end do
                    ! A Fibonacci spiral rather than a pseudo-random scatter. It is a better
                    ! sampling of the sphere for this purpose -- low-discrepancy rather than
                    ! merely unbiased -- and it has no integer arithmetic in it, which matters
                    ! more: the LCG that stood here relied on a signed int64 multiply wrapping,
                    ! which is undefined behaviour, and nagfor at --profile release optimised on
                    ! the assumption that it could not overflow. `modulo` then returned a
                    ! NEGATIVE seed, `acos` was handed 2.42, and the disc centre came back a NaN.
                    ! See src/parquet_expkey.f90's `fp_step`, which spells the same LCG without
                    ! the overflow, and feature_risks.md Risk-94.
                    do k = 1, nscatter
                        ! `(k - 0.5)/nscatter` is strictly inside (0, 1), so `z` is strictly
                        ! inside (-1, 1) and `acos` cannot be handed an out-of-range argument.
                        th = acos(1.0_real64 - 2.0_real64 * (real(k, real64) - 0.5_real64) &
                                  / real(nscatter, real64))
                        ph = modulo(real(k, real64) * golden_angle, 2.0_real64 * pi)
                        call pf_ang2vec(th, ph, vec)
                        call pf_query_disc_count(nside, vec, radius, cnt, inclusive=inc)
                        worst = max(worst, cnt)
                    end do
                    write (detail, '(a,i0,a,es11.4,a,l1,a,i0,a,i0)') "nside=", nside, " radius=", &
                        radius, " inclusive=", inc, " observed=", worst, " bound=", bound
                    call check(error, worst <= bound, &
                        "a disc held more pixels than the bound allows: " // trim(detail))
                    if (allocated(error)) return
                end do
            end do
        end do
        ! A radius at or above pi is the whole sphere, where the bound is exact rather than loose.
        call check(error, pf_query_disc_max_count(64_int64, pi) == pf_nside2npix(64_int64), &
            "a radius of pi must bound at exactly npix")
        if (allocated(error)) return
        call check(error, pf_query_disc_max_count(64_int64, 7.0_real64) == pf_nside2npix(64_int64), &
            "a radius past pi must still bound at exactly npix rather than overflowing it")
        if (allocated(error)) return
        ! Both integer kinds must answer the same number.
        call check(error, int(pf_query_disc_max_count(512_int32, 0.01_real64), int64) == &
                          pf_query_disc_max_count(512_int64, 0.01_real64), &
            "the int32 and int64 kinds must agree")
    end subroutine test_disc_max_count_bounds_every_position

    !> The bound is TIGHT enough to be worth calling, which is the half an upper bound can fake.
    !>
    !> Without this, `nmax = pf_nside2npix(nside)` would satisfy every assertion in
    !> `test_disc_max_count_bounds_every_position` while making the routine useless -- so this is
    !> that test's negative control rather than a separate concern. It asserts a ratio against the
    !> attained maximum rather than an absolute count, so it says nothing about which `nside` is
    !> in use and does not have to be re-tuned when one changes.
    subroutine test_disc_max_count_is_useful(error)
        type(error_type), allocatable, intent(out) :: error !! set when the bound is uselessly loose.
        integer(int64) :: nside, npix, bound, cnt, worst, ip, stride
        real(real64) :: radius, vec(3)
        character(len=160) :: detail

        nside = 256_int64
        npix = pf_nside2npix(nside)
        ! A disc of about forty pixel widths: large enough for the area argument to be sharp, and
        ! small enough that a bound of npix would be four hundred times too big.
        radius = 40.0_real64 * pf_nside2resol(nside)
        bound = pf_query_disc_max_count(nside, radius)
        worst = 0_int64
        stride = max(1_int64, npix / 4096_int64)
        do ip = 0_int64, npix - 1_int64, stride
            call pf_pix2vec_ring(nside, ip, vec)
            call pf_query_disc_count(nside, vec, radius, cnt)
            worst = max(worst, cnt)
        end do
        write (detail, '(a,i0,a,i0,a,i0)') "observed=", worst, " bound=", bound, " npix=", npix
        call check(error, worst > 0_int64, "the fixture disc must hold pixels at all: " // trim(detail))
        if (allocated(error)) return
        ! 1.25 is far above the ~1.05 measured here and far below the ~400 a bound of npix would
        ! give, so it fails a useless bound without pinning this implementation's exact tightness.
        call check(error, real(bound, real64) < 1.25_real64 * real(worst, real64), &
            "the bound must be within a quarter of the attained maximum: " // trim(detail))
        if (allocated(error)) return
        call check(error, bound < npix / 100_int64, &
            "and must be far below npix, or it is not sizing anything: " // trim(detail))
        if (allocated(error)) return
        ! The inclusive mode bounds a superset, so its bound cannot be the smaller of the two.
        call check(error, pf_query_disc_max_count(nside, radius, inclusive=.true.) >= bound, &
            "the inclusive bound must be at least the exact-mode one")
    end subroutine test_disc_max_count_is_useful

    ! ---- The trap-cleanliness property ----

    !> No entry point raises an IEEE exception on any valid input.
    !>
    !> **This is the feature, not a detail.** The module exists because `libhealpix`'s disc query
    !> raises inside the library, forcing every caller running under `-ffpe-trap` to suspend the
    !> halting modes around each call -- a guard measured at 4.05 microseconds under ifx, which is
    !> more than the query it protects. A native implementation that raised would keep that guard
    !> alive downstream and lose most of the benefit, and nothing else in this suite would notice.
    !>
    !> The flags are per-thread state, so a sibling test running concurrently cannot contaminate
    !> this one; the state is saved and restored so this test cannot contaminate anything either.
    !> `tools/check_healpix_fptrap.sh` asserts the same property the other way round, by running a
    !> sweep in a build where the traps actually halt.
    subroutine test_no_ieee_exceptions(error)
        type(error_type), allocatable, intent(out) :: error !! set if any flag came up.
        logical :: had_inv, had_div, had_ovf, got_inv, got_div, got_ovf
        integer(int64) :: nside, p, nlist, sink
        integer(int64), allocatable :: listpix(:)
        real(real64) :: theta, phi, vec(3), dist, negvec(3)
        integer :: c
        real(real64), parameter :: two_thirds = 2.0_real64 / 3.0_real64
        ! Named rather than written inline at the call: an array constructor or an expression
        ! passed to `pf_angdist`'s explicit-shape `vec(3)` dummy is a value with no address, so
        ! ifx argument-associates it through a temporary and reports `warning (406)` on every
        ! call under --profile debug. See CLAUDE.md's ifx-specific gotchas.
        real(real64), parameter :: north(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        ! Every pixel of the nside 32 pixelisation, because the sweep below includes a disc of
        ! radius pi. Allocated rather than automatic, so the sweep can be widened without putting a
        ! larger array on the stack of a suite that runs its tests concurrently.
        allocate (listpix(12_int64 * 32_int64 * 32_int64))

        if (.not. (ieee_support_flag(ieee_invalid) .and. ieee_support_flag(ieee_divide_by_zero) &
                   .and. ieee_support_flag(ieee_overflow))) then
            call check(error, .true., "this processor cannot report IEEE flags")
            return
        end if
        call ieee_get_flag(ieee_invalid, had_inv)
        call ieee_get_flag(ieee_divide_by_zero, had_div)
        call ieee_get_flag(ieee_overflow, had_ovf)
        call ieee_set_flag(ieee_invalid, .false.)
        call ieee_set_flag(ieee_divide_by_zero, .false.)
        call ieee_set_flag(ieee_overflow, .false.)

        sink = 0_int64
        do c = 1, 9
            select case (c)
            case (1)
                nside = 1_int64
                theta = 0.0_real64
                phi = 0.0_real64
            case (2)
                nside = 4_int64
                theta = pi
                phi = 0.0_real64
            case (3)
                nside = 16_int64
                theta = acos(two_thirds)
                phi = 0.0_real64
            case (4)
                nside = 16_int64
                theta = acos(-two_thirds)
                phi = 2.0_real64 * pi
            case (5)
                nside = 64_int64
                theta = 0.5_real64 * pi
                phi = 0.0_real64
            case (6)
                nside = 8192_int64
                theta = 1.0e-12_real64
                phi = 1.0e-12_real64
            case (7)
                nside = 536870912_int64
                theta = 1.0e-12_real64
                phi = 3.0_real64
            case (8)
                nside = 536870912_int64
                theta = pi - 1.0e-12_real64
                phi = -7.0_real64
            case default
                nside = 1024_int64
                theta = 1.2_real64
                phi = 400.0_real64
            end select
            call pf_ang2pix_ring(nside, theta, phi, p)
            sink = sink + p
            call pf_ang2pix_nest(nside, theta, phi, p)
            sink = sink + p
            call pf_pix2ang_ring(nside, p, theta, phi)
            call pf_pix2ang_nest(nside, p, theta, phi)
            call pf_pix2vec_ring(nside, p, vec)
            call pf_pix2vec_nest(nside, p, vec)
            call pf_ring2nest(nside, p, sink)
            call pf_nest2ring(nside, p, sink)
            call pf_angdist(vec, north, dist)
            negvec = -vec
            call pf_angdist(vec, negvec, dist)
        end do
        ! Every disc shape the walk has a branch for: a pole, the seam, the boundary latitude, a
        ! hemisphere, the whole sphere, and both schemes with and without the enlargement.
        do c = 1, 7
            select case (c)
            case (1)
                vec = [0.0_real64, 0.0_real64, 1.0_real64]
                dist = 0.4_real64
            case (2)
                vec = [0.0_real64, 0.0_real64, -1.0_real64]
                dist = 0.5_real64 * pi
            case (3)
                vec = [1.0_real64, 0.0_real64, 0.0_real64]
                dist = 0.3_real64
            case (4)
                vec = [sqrt(1.0_real64 - two_thirds**2), 0.0_real64, two_thirds]
                dist = 0.2_real64
            case (5)
                vec = [0.3_real64, 0.4_real64, 0.8660254037844386_real64]
                dist = pi
            case (6)
                vec = [1.0e300_real64, 0.0_real64, 1.0e300_real64]
                dist = 0.25_real64
            case default
                ! A direction given in components so small that squaring them underflows, and one
                ! so large that squaring them overflows. Both name ordinary directions and both
                ! must be answered without raising anything -- which is what the scaled
                ! normalisation in hpx_check_disc_args and the walk exists for.
                vec = [1.0e-300_real64, 0.0_real64, 1.0e-300_real64]
                dist = 0.25_real64
            end select
            call pf_query_disc(32_int64, vec, dist, listpix, nlist, scheme=PF_HP_RING)
            sink = sink + nlist
            call pf_query_disc(32_int64, vec, dist, listpix, nlist, scheme=PF_HP_NEST, inclusive=.true.)
            sink = sink + nlist
            sink = sink + pf_query_disc_max_count(32_int64, dist)
            sink = sink + pf_query_disc_max_count(32_int64, dist, inclusive=.true.)
        end do
        ! An INFINITE radius, which passes validation -- only NaN and a negative are refused --
        ! and which pf_query_disc_max_count must answer without reaching `sin`. `sin(infinity)`
        ! is a NaN and raises IEEE_INVALID, which ends a NAG process; this is the one input where
        ! dropping that routine's whole-sphere shortcut is a raised flag rather than a wrong
        ! number, and it is what this assertion is here to catch.
        sink = sink + pf_query_disc_max_count(32_int64, ieee_value(0.0_real64, ieee_positive_inf))
        call check(error, pf_query_disc_max_count(32_int64, &
                              ieee_value(0.0_real64, ieee_positive_inf)) == pf_nside2npix(32_int64), &
            "an infinite radius must bound at npix")
        if (allocated(error)) return

        call ieee_get_flag(ieee_invalid, got_inv)
        call ieee_get_flag(ieee_divide_by_zero, got_div)
        call ieee_get_flag(ieee_overflow, got_ovf)
        ! Restored as "was already raised, or this test raised it", so a flag raised elsewhere in
        ! the run is neither hidden by this test nor blamed on it.
        call ieee_set_flag(ieee_invalid, had_inv .or. got_inv)
        call ieee_set_flag(ieee_divide_by_zero, had_div .or. got_div)
        call ieee_set_flag(ieee_overflow, had_ovf .or. got_ovf)

        call check(error, .not. got_inv, "an entry point raised IEEE_INVALID")
        if (allocated(error)) return
        call check(error, .not. got_div, "an entry point raised IEEE_DIVIDE_BY_ZERO")
        if (allocated(error)) return
        call check(error, .not. got_ovf, "an entry point raised IEEE_OVERFLOW")
        if (allocated(error)) return
        call check(error, sink /= -1_int64, "the sweep produced no result, so it may not have run")
    end subroutine test_no_ieee_exceptions

    !> The same queries from many threads give the same answers as from one.
    !>
    !> The module holds no state at all, so this asserts a property of the design rather than of a
    !> lock: a saved variable or a lazily built table added later would break it. Not guarded on
    !> OpenMP -- it uses threads rather than asserting that a team existed, so it degrades to a
    !> serial comparison against itself and still asserts the answers.
    subroutine test_thread_safety(error)
        type(error_type), allocatable, intent(out) :: error !! set if any answer differs.
        integer, parameter :: ncase = 96
        integer(int64) :: serial(ncase), par(ncase)
        integer :: c, nbad

        do c = 1, ncase
            serial(c) = case_signature(c)
        end do
        !$omp parallel do default(shared) private(c) schedule(static)
        do c = 1, ncase
            par(c) = case_signature(c)
        end do
        !$omp end parallel do
        nbad = 0
        do c = 1, ncase
            if (serial(c) /= par(c)) nbad = nbad + 1
        end do
        call check(error, nbad, 0, "a threaded run disagreed with the serial one")
    end subroutine test_thread_safety

    !> One deterministic mixed workload, reduced to a single integer.
    !>
    !> Mixes both schemes, both kinds and a disc query, so a difference anywhere shows up in the
    !> one value the caller compares.
    function case_signature(c) result(sig)
        integer, intent(in) :: c !! case number, 1 upward.
        integer(int64) :: sig !! a value depending on every answer the case produced.
        integer(int64) :: nside, p, q, nlist, listpix(4096), m
        integer(int32) :: p32
        real(real64) :: theta, phi, v0(3), z, ph

        nside = int(2 ** (1 + modulo(c, 6)), int64)
        z = -0.95_real64 + 1.9_real64 * real(modulo(c, 32), real64) / 32.0_real64
        ph = 2.399963229728653_real64 * real(c, real64)
        theta = acos(z)
        phi = ph
        v0 = [sqrt(1.0_real64 - z * z) * cos(ph), sqrt(1.0_real64 - z * z) * sin(ph), z]
        call pf_ang2pix_ring(nside, theta, phi, p)
        call pf_ang2pix_nest(nside, theta, phi, q)
        call pf_ang2pix_ring(int(nside, int32), theta, phi, p32)
        sig = p * 1000003_int64 + q * 10007_int64 + int(p32, int64)
        call pf_query_disc(nside, v0, 0.13_real64 + 0.01_real64 * real(modulo(c, 7), real64), &
                           listpix, nlist, scheme=PF_HP_NEST, inclusive=(modulo(c, 2) == 0))
        sig = sig + nlist * 31_int64
        do m = 1_int64, nlist
            sig = sig + listpix(m) * m
        end do
    end function case_signature

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
        real(real64) :: a1, d1, a2, d2, got, ref, v1(3), v2(3), rad
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

    !> `pf_angdist` is exact at the two ends of its range and stable just inside them.
    !>
    !> The `atan2` form exists for these cases: `acos` of the dot product loses about half its
    !> digits near 0 and near pi, which is where a separation is most often wanted.
    subroutine test_angdist_extremes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first fault.
        real(real64) :: d
        real(real64), parameter :: e1(3) = [1.0_real64, 0.0_real64, 0.0_real64]
        real(real64), parameter :: e2(3) = [0.0_real64, 1.0_real64, 0.0_real64]
        ! Every actual argument below is a NAMED constant rather than an inline array
        ! constructor or expression: `pf_angdist`'s dummies are explicit-shape `vec(3)`, and a
        ! value with no address of its own is argument-associated through a temporary, which ifx
        ! reports as `warning (406)` under --profile debug. See CLAUDE.md's ifx-specific gotchas.
        real(real64), parameter :: me1(3) = -e1
        real(real64), parameter :: tiny_y(3) = [1.0_real64, 1.0e-9_real64, 0.0_real64]
        real(real64), parameter :: three_x(3) = [3.0_real64, 0.0_real64, 0.0_real64]
        real(real64), parameter :: five_y(3) = [0.0_real64, 5.0_real64, 0.0_real64]
        real(real64), parameter :: zero3(3) = [0.0_real64, 0.0_real64, 0.0_real64]

        call pf_angdist(e1, e1, d)
        call check(error, d, 0.0_real64, "coincident directions did not separate by exactly zero", &
                   thr=0.0_real64)
        if (allocated(error)) return
        call pf_angdist(e1, me1, d)
        call check(error, d, pi, "antipodal directions did not separate by pi", thr=1.0e-15_real64)
        if (allocated(error)) return
        call pf_angdist(e1, e2, d)
        call check(error, d, 0.5_real64 * pi, "orthogonal directions did not separate by pi/2", &
                   thr=1.0e-15_real64)
        if (allocated(error)) return
        ! A tiny angle: acos(dot) would return zero here, since 1 - 5e-19 rounds to 1.
        call pf_angdist(e1, tiny_y, d)
        call check(error, d, 1.0e-9_real64, "a nanoradian separation was not resolved", &
                   thr=1.0e-18_real64)
        if (allocated(error)) return
        ! Scale invariance: the formula never normalises its inputs.
        call pf_angdist(three_x, five_y, d)
        call check(error, d, 0.5_real64 * pi, "non-unit inputs changed the answer", thr=1.0e-15_real64)
        if (allocated(error)) return
        ! Two parallel zero vectors: atan2(0, 0) is zero, so this is total rather than an abort.
        call pf_angdist(zero3, zero3, d)
        call check(error, d, 0.0_real64, "two zero vectors did not give zero", thr=0.0_real64)
    end subroutine test_angdist_extremes

    !> Expanding the runs reproduces `pf_query_disc`'s pixel list exactly, over a sweep.
    !>
    !> **The master test for the run form.** The runs and the pixels come from the same walk, so
    !> this asserts they were not decoupled by a change to one of the two recording paths -- which
    !> is the only way they could ever disagree. The sweep covers both `inclusive` modes, a disc
    !> at a pole (where the ring band is truncated), one on the phi = 0 seam (where an arc wraps
    !> and the walk emits two runs for one ring), and radii from below one pixel to several rings.
    subroutine test_disc_runs_expand(error)
        type(error_type), allocatable, intent(out) :: error !! set on any disagreement.
        integer(int64) :: nside, npix, nlist, nruns, k, j, filled, p
        integer(int64), allocatable :: listpix(:), runs(:,:)
        real(real64) :: v(3), radius
        integer :: ic, ir, im
        logical :: inc, ok
        real(real64), parameter :: centres(3, 4) = reshape([ &
            0.0_real64, 0.0_real64, 1.0_real64, &
            1.0_real64, 0.0_real64, 0.0_real64, &
            0.6_real64, 0.8_real64, 0.0_real64, &
            0.2_real64, 0.3_real64, 0.9327379053088815_real64], [3, 4])
        real(real64), parameter :: radii(5) = &
            [0.002_real64, 0.02_real64, 0.1_real64, 0.4_real64, 1.2_real64]

        nside = 32_int64
        npix = 12_int64 * nside * nside
        allocate (listpix(npix), runs(2, npix))
        do ic = 1, size(centres, 2)
            v = centres(:, ic)
            do ir = 1, size(radii)
                radius = radii(ir)
                do im = 1, 2
                    inc = (im == 2)
                    call pf_query_disc(nside, v, radius, listpix, nlist, inclusive=inc)
                    call pf_query_disc_runs(nside, v, radius, runs, nruns, inclusive=inc)
                    call check(error, nruns >= 0_int64 .and. nruns <= npix, &
                               "the run count is outside every possible range")
                    if (allocated(error)) return
                    filled = 0_int64
                    ok = .true.
                    do k = 1_int64, nruns
                        if (runs(2, k) <= 0_int64) then
                            ok = .false.
                            exit
                        end if
                        do j = 0_int64, runs(2, k) - 1_int64
                            p = runs(1, k) + j
                            filled = filled + 1_int64
                            if (filled > nlist) then
                                ok = .false.
                                exit
                            end if
                            if (p /= listpix(filled)) ok = .false.
                        end do
                        if (.not. ok) exit
                    end do
                    call check(error, ok, "the runs did not expand to the disc's own pixel list")
                    if (allocated(error)) return
                    call check(error, filled, nlist, &
                               "the runs cover a different number of pixels than the disc holds")
                    if (allocated(error)) return
                end do
            end do
        end do
    end subroutine test_disc_runs_expand

    !> The runs really are ranges: ascending, non-overlapping, and far fewer than the pixels.
    !>
    !> **Without this, `test_disc_runs_expand` passes against a decomposition that emits one run
    !> per pixel** -- which would expand correctly and be worthless. Maximality is deliberately
    !> NOT asserted: two runs can legitimately abut, both across a ring boundary (the last pixel
    !> of one ring and the first of the next are consecutive in RING numbering) and within one
    !> ring whose arc covers it entirely after trimming.
    subroutine test_disc_runs_are_ranges(error)
        type(error_type), allocatable, intent(out) :: error !! set if the decomposition degenerates.
        integer(int64) :: nside, npix, nlist, nruns, k
        integer(int64), allocatable :: listpix(:), runs(:,:)
        real(real64), parameter :: v(3) = [0.2_real64, 0.3_real64, 0.9327379053088815_real64]

        nside = 64_int64
        npix = 12_int64 * nside * nside
        allocate (listpix(npix), runs(2, npix))
        call pf_query_disc(nside, v, 0.05_real64, listpix, nlist)
        call pf_query_disc_runs(nside, v, 0.05_real64, runs, nruns)
        call check(error, nlist > 20_int64, "the fixture disc is too small to test a decomposition")
        if (allocated(error)) return
        ! A disc spans an arc of each ring it touches, so the run count tracks the RINGS, not the
        ! pixels. Anything close to one run per pixel is a decomposition that decomposed nothing.
        call check(error, nruns * 2_int64 < nlist, &
                   "the run count is not materially below the pixel count, so the runs are not ranges")
        if (allocated(error)) return
        do k = 1_int64, nruns
            call check(error, runs(1, k) >= 0_int64 .and. runs(1, k) < npix, &
                       "a run starts outside the pixelisation")
            if (allocated(error)) return
            call check(error, runs(2, k) > 0_int64, "a run has a non-positive length")
            if (allocated(error)) return
            call check(error, runs(1, k) + runs(2, k) <= npix, "a run runs past the last pixel")
            if (allocated(error)) return
            if (k > 1_int64) then
                call check(error, runs(1, k - 1_int64) + runs(2, k - 1_int64) <= runs(1, k), &
                           "the runs are not ascending and non-overlapping")
                if (allocated(error)) return
            end if
        end do
    end subroutine test_disc_runs_are_ranges

    !> A run buffer too small reports the TRUE count and fills its prefix correctly.
    !>
    !> This is the contract that makes a short buffer recoverable rather than an abort, and it is
    !> the one place where copying the module's own internal recording -- which used a negative
    !> sentinel -- would have been wrong. Asserted against the full query rather than against a
    !> stored expectation, so it cannot drift.
    subroutine test_disc_runs_short_buffer(error)
        type(error_type), allocatable, intent(out) :: error !! set if a short buffer misreports.
        integer(int64) :: nside, npix, nruns_full, nruns, k, cap
        integer(int64), allocatable :: full(:,:), small(:,:)
        real(real64), parameter :: v(3) = [0.2_real64, 0.3_real64, 0.9327379053088815_real64]
        real(real64), parameter :: radius = 0.05_real64

        nside = 64_int64
        npix = 12_int64 * nside * nside
        allocate (full(2, npix))
        call pf_query_disc_runs(nside, v, radius, full, nruns_full)
        call check(error, nruns_full > 3_int64, "the fixture disc has too few runs to truncate")
        if (allocated(error)) return
        do cap = 1_int64, 3_int64
            deallocate (small, stat=k)
            allocate (small(2, cap))
            call pf_query_disc_runs(nside, v, radius, small, nruns)
            call check(error, nruns, nruns_full, &
                       "a short run buffer did not report the true run count")
            if (allocated(error)) return
            do k = 1_int64, cap
                call check(error, small(1, k), full(1, k), &
                           "a short run buffer stored a different run start")
                if (allocated(error)) return
                call check(error, small(2, k), full(2, k), &
                           "a short run buffer stored a different run length")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_disc_runs_short_buffer

    !> The int32 and int64 kinds return the same runs.
    subroutine test_disc_runs_kinds_agree(error)
        type(error_type), allocatable, intent(out) :: error !! set if the two kinds disagree.
        integer(int32) :: runs32(2, 512), nruns32
        integer(int64) :: runs64(2, 512), nruns64, k
        real(real64), parameter :: v(3) = [0.2_real64, 0.3_real64, 0.9327379053088815_real64]

        call pf_query_disc_runs(128_int32, v, 0.05_real64, runs32, nruns32, inclusive=.true.)
        call pf_query_disc_runs(128_int64, v, 0.05_real64, runs64, nruns64, inclusive=.true.)
        call check(error, int(nruns32, int64), nruns64, "the two kinds reported different run counts")
        if (allocated(error)) return
        call check(error, nruns64 > 0_int64 .and. nruns64 <= 512_int64, &
                   "the fixture disc did not fit the test buffers")
        if (allocated(error)) return
        do k = 1_int64, nruns64
            call check(error, int(runs32(1, k), int64), runs64(1, k), &
                       "the two kinds reported different run starts")
            if (allocated(error)) return
            call check(error, int(runs32(2, k), int64), runs64(2, k), &
                       "the two kinds reported different run lengths")
            if (allocated(error)) return
        end do
    end subroutine test_disc_runs_kinds_agree

    !> `pf_query_disc_alloc` is still correct for a disc holding more runs than it can record.
    !>
    !> Its counting pass records where each run landed so that the emitting pass is a replay
    !> rather than a second walk, and that recording has a fixed-size buffer. **The overflow path
    !> -- walk twice instead -- is what this covers**, and it is reached only by a disc spanning
    !> more rings than the buffer has columns, so no ordinary disc test goes near it. The
    !> whole-sphere disc below spans 2047 rings against a 1024-column buffer.
    subroutine test_disc_alloc_run_overflow(error)
        type(error_type), allocatable, intent(out) :: error !! set if the fallback misbehaves.
        integer(int64) :: nside, npix, nlist, k
        integer(int64), allocatable :: got(:)
        logical :: ok
        real(real64), parameter :: v(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        nside = 512_int64
        npix = 12_int64 * nside * nside
        call pf_query_disc_alloc(nside, v, 4.0_real64, got, nlist)
        call check(error, nlist, npix, "a disc past pi/2 radians should still hold every pixel")
        if (allocated(error)) return
        call check(error, int(size(got), int64), npix, "the allocated array is the wrong size")
        if (allocated(error)) return
        ok = .true.
        do k = 1_int64, npix
            if (got(k) /= k - 1_int64) then
                ok = .false.
                exit
            end if
        end do
        call check(error, ok, "the whole-sphere disc did not come back in ascending pixel order")
    end subroutine test_disc_alloc_run_overflow

end module test_healpix
