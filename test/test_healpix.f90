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
        ieee_invalid, ieee_divide_by_zero, ieee_overflow, ieee_value, ieee_positive_inf
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

    !> The step across each of the eight neighbours, in the order `pf_neighbours_nest` returns
    !> them: `x` then `y` in the face's own coordinates.
    integer, parameter :: nb_dx(8) = [-1, -1, 0, 1, 1, 1, 0, -1]
    integer, parameter :: nb_dy(8) = [0, 1, 1, 1, 0, -1, -1, -1] !! see `nb_dx`.

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
            new_unittest("every int32 arithmetic entry point agrees with its int64 twin", &
                         test_arith_int32_agrees_with_int64), &
            new_unittest("the int32 arithmetic entry points have their own domain bounds", &
                         test_arith_int32_out_of_domain), &
            new_unittest("the int32 scalar conversions agree with their int64 twins", &
                         test_core_int32_agrees_with_int64), &
            new_unittest("every int32 bulk entry point agrees with its int64 twin", &
                         test_bulk_int32_agrees_with_int64), &
            new_unittest("every bulk entry point accepts a zero-length input", &
                         test_bulk_empty_input_is_a_no_op), &
            new_unittest("query_disc_count and query_disc_alloc agree in the int32 kind", &
                         test_query_disc_int32_kind), &
            new_unittest("neighbours are symmetric over every pixel", test_neighbours_symmetric), &
            new_unittest("exactly 24 corners are missing, at the 3-face vertices", &
                         test_neighbours_missing_corners), &
            new_unittest("the neighbour tables match the pixelisation", &
                         test_neighbour_tables_match_the_pixelisation), &
            new_unittest("a neighbour-based disc walk returns query_disc's inclusive set", &
                         test_neighbour_disc_matches_query_disc), &
            new_unittest("neighbours agree across the two kinds, and ring is the nest2ring image", &
                         test_neighbours_kinds_and_ring) &
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

    !> Every int32 arithmetic entry point, A/B'd against its int64 twin over the whole int32 range.
    !!
    !! **The int32 forms are not casts of the int64 ones.** Each carries its OWN domain bound --
    !! `hpx_nside_max_i32` is 8192, where the int64 bound is 2**29 -- and each rewrites the range
    !! guards in int32 before widening to do the arithmetic. Three things can go wrong in that
    !! rewrite and none of them is visible from the int64 side: the wrong cap (so nside 16384 is
    !! accepted and overflows), a guard that compares in int32 where the product `12*nside*nside`
    !! does not fit, and a narrowing cast applied before the bound check rather than after.
    !!
    !! The int64 twin is the oracle because it is the same mathematics with room to spare, and it
    !! is exercised by the round-trip tests above. Agreement across every power-of-two nside up to
    !! the int32 ceiling, at pixels chosen at both ends and in the middle of each map, is what
    !! makes a divergence a real one rather than a rounding artifact -- these are exact integer
    !! answers apart from the two areal quantities, which are compared exactly because both sides
    !! evaluate the identical real64 expression.
    subroutine test_arith_int32_agrees_with_int64(error)
        type(error_type), allocatable, intent(out) :: error !! set on any disagreement.
        integer(int32) :: ns32, ip32, ir32
        integer(int64) :: ns64, ip64, npix
        integer :: e, k
        integer(int64) :: probes(5)
        integer(int32) :: ends(3)

        do e = 0, 13
            ns32 = ishft(1_int32, e)
            ns64 = int(ns32, int64)
            npix = 12_int64 * ns64 * ns64

            call check(error, pf_nside2pixarea(ns32) == pf_nside2pixarea(ns64), &
                "nside2pixarea disagrees across kinds")
            if (allocated(error)) return
            call check(error, pf_nside2resol(ns32) == pf_nside2resol(ns64), &
                "nside2resol disagrees across kinds")
            if (allocated(error)) return

            ! Both ends of the map and the middle: the polar caps and the equatorial belt are
            ! different formulae inside, so one probe would exercise only one of them.
            probes = [0_int64, 1_int64, npix / 2_int64, npix - 2_int64, npix - 1_int64]
            do k = 1, 5
                ip64 = probes(k)
                if (ip64 < 0_int64 .or. ip64 >= npix) cycle
                ip32 = int(ip64, int32)
                call check(error, int(pf_pix2ring_ring(ns32, ip32), int64) == &
                    pf_pix2ring_ring(ns64, ip64), "pix2ring_ring disagrees across kinds")
                if (allocated(error)) return
                call check(error, int(pf_pix2ring_nest(ns32, ip32), int64) == &
                    pf_pix2ring_nest(ns64, ip64), "pix2ring_nest disagrees across kinds")
                if (allocated(error)) return
            end do

            ! Every ring of the map at the small nsides; the two ends and the equator beyond that,
            ! since 4*8192-1 rings is more than this assertion needs to be convincing.
            if (ns32 <= 32_int32) then
                do ir32 = 1_int32, 4_int32 * ns32 - 1_int32
                    call check(error, pf_ring2z(ns32, ir32) == pf_ring2z(ns64, int(ir32, int64)), &
                        "ring2z disagrees across kinds")
                    if (allocated(error)) return
                end do
            else
                ends = [1_int32, 2_int32 * ns32, 4_int32 * ns32 - 1_int32]
                do k = 1, 3
                    ir32 = ends(k)
                    call check(error, pf_ring2z(ns32, ir32) == pf_ring2z(ns64, int(ir32, int64)), &
                        "ring2z disagrees across kinds at an end or the equator")
                    if (allocated(error)) return
                end do
            end if
        end do

        ! The vacuity guard: the sweep above would be satisfied by five functions that all
        ! returned their out-of-domain sentinel every time.
        call check(error, pf_nside2pixarea(4_int32) > 0.0_real64 .and. &
            pf_pix2ring_ring(4_int32, 100_int32) > 0_int32 .and. &
            pf_ring2z(4_int32, 5_int32) > -2.0_real64, &
            "the swept entry points must answer in domain, not their sentinels")
    end subroutine test_arith_int32_agrees_with_int64

    !> Each int32 entry point's OWN out-of-domain arms, including the int32-only ceiling.
    !!
    !! `nside = 16384` is the case the int64 form accepts and the int32 form must not: it is a
    !! valid nside, and perfectly representable, but above `hpx_nside_max_i32`. A cap copied from
    !! the int64 side would accept it here and compute `12*16384*16384`, which overflows int32 --
    !! so this is the assertion that the two bounds really are different, and it is asserted
    !! against the int64 form ANSWERING for the same nside, which is what pins it as a deliberate
    !! API ceiling rather than a shared domain limit.
    subroutine test_arith_int32_out_of_domain(error)
        type(error_type), allocatable, intent(out) :: error !! set on any disagreement.
        integer(int32), parameter :: NS = 16_int32
        integer(int32), parameter :: NPIX = 12_int32 * NS * NS

        ! Above the int32 ceiling: refused here, accepted on the int64 side.
        call check(error, pf_nside2pixarea(16384_int32) == -1.0_real64, &
            "nside above the int32 ceiling must give the area sentinel")
        if (allocated(error)) return
        call check(error, pf_nside2pixarea(16384_int64) > 0.0_real64, &
            "...while the int64 form answers for the same nside, so the ceiling is the int32 API's")
        if (allocated(error)) return
        call check(error, pf_nside2resol(16384_int32) == -1.0_real64, &
            "nside above the int32 ceiling must give the resolution sentinel")
        if (allocated(error)) return

        ! Not a power of two, and zero: both refused by the domain test itself.
        call check(error, pf_nside2pixarea(3_int32) == -1.0_real64 .and. &
            pf_nside2resol(0_int32) == -1.0_real64, &
            "a non-power-of-two and a zero nside are out of domain")
        if (allocated(error)) return

        ! A pixel index off each end of a valid map, in both schemes.
        call check(error, pf_pix2ring_ring(NS, -1_int32) == -1_int32 .and. &
            pf_pix2ring_ring(NS, NPIX) == -1_int32, &
            "a RING pixel off either end of the map is out of domain")
        if (allocated(error)) return
        call check(error, pf_pix2ring_nest(NS, -1_int32) == -1_int32 .and. &
            pf_pix2ring_nest(NS, NPIX) == -1_int32, &
            "a NEST pixel off either end of the map is out of domain")
        if (allocated(error)) return
        call check(error, pf_pix2ring_nest(3_int32, 0_int32) == -1_int32, &
            "and a bad nside is refused before the pixel is looked at")
        if (allocated(error)) return

        ! A ring index off each end, and a bad nside.
        call check(error, pf_ring2z(NS, 0_int32) == -2.0_real64 .and. &
            pf_ring2z(NS, 4_int32 * NS) == -2.0_real64, &
            "a ring off either end is out of domain")
        if (allocated(error)) return
        call check(error, pf_ring2z(3_int32, 1_int32) == -2.0_real64, &
            "and a bad nside gives the ring sentinel too")
        if (allocated(error)) return
        ! The control: the rings just inside each end ARE in domain.
        call check(error, pf_ring2z(NS, 1_int32) < 1.0_real64 .and. &
            pf_ring2z(NS, 4_int32 * NS - 1_int32) > -1.0_real64, &
            "the rings just inside each end must answer")
    end subroutine test_arith_int32_out_of_domain

    !> The int32 scalar conversions, A/B'd against their int64 twins over a whole map.
    !!
    !! Each of these is a forwarder that widens, calls the int64 body and narrows the answer back
    !! -- which is exactly why they need driving: the narrowing cast is the only thing that can go
    !! wrong, it is invisible from the int64 side, and a forwarder wired to the wrong twin (ring
    !! where nest was meant, say) would still return a plausible pixel index.
    !!
    !! Every pixel of an nside-8 map is swept rather than a sample, because these are exact
    !! integer maps: a disagreement anywhere is a real one, and a whole map is cheap at 768
    !! pixels. The vector forms are compared componentwise and exactly -- both kinds evaluate the
    !! identical real64 expression, so anything but equality is a wiring fault, not rounding.
    subroutine test_core_int32_agrees_with_int64(error)
        type(error_type), allocatable, intent(out) :: error !! set on any disagreement.
        integer(int32), parameter :: NS32 = 8_int32
        integer(int64), parameter :: NS64 = 8_int64
        integer(int32) :: p32, r32, n32, back32
        integer(int64) :: p64, r64, n64, npix, back64
        real(real64) :: v32(3), v64(3)

        npix = 12_int64 * NS64 * NS64
        do p64 = 0_int64, npix - 1_int64
            p32 = int(p64, int32)

            call pf_ring2nest(NS32, p32, n32)
            call pf_ring2nest(NS64, p64, n64)
            call check(error, int(n32, int64) == n64, "ring2nest disagrees across kinds")
            if (allocated(error)) return

            call pf_nest2ring(NS32, p32, r32)
            call pf_nest2ring(NS64, p64, r64)
            call check(error, int(r32, int64) == r64, "nest2ring disagrees across kinds")
            if (allocated(error)) return

            call pf_pix2vec_ring(NS32, p32, v32)
            call pf_pix2vec_ring(NS64, p64, v64)
            call check(error, all(v32 == v64), "pix2vec_ring disagrees across kinds")
            if (allocated(error)) return

            call pf_pix2vec_nest(NS32, p32, v32)
            call pf_pix2vec_nest(NS64, p64, v64)
            call check(error, all(v32 == v64), "pix2vec_nest disagrees across kinds")
            if (allocated(error)) return

            ! vec2pix_ring closes the loop: the centre of every pixel must land back in it, in
            ! the int32 kind as in the int64 one. That is a stronger claim than agreement alone,
            ! since two forwarders wired to the same wrong twin would agree with each other.
            call pf_pix2vec_ring(NS64, p64, v64)
            call pf_vec2pix_ring(NS32, v64, back32)
            call pf_vec2pix_ring(NS64, v64, back64)
            call check(error, int(back32, int64) == back64, &
                "vec2pix_ring disagrees across kinds")
            if (allocated(error)) return
            call check(error, back64 == p64, "a pixel centre must land back in its own pixel")
            if (allocated(error)) return
        end do
    end subroutine test_core_int32_agrees_with_int64

    !> Every BULK entry point in its int32 kind, against the int64 twin of the same call.
    !!
    !! The bulk forms are not loops over the scalar ones from the caller's point of view: each
    !! validates its own array sizes, resolves its own thread team and carries its own `nside`
    !! domain bound -- `hpx_nside_max_i32` here, where the int64 form uses the larger one. Eight
    !! of them, and the int32 half of six was reached by nothing.
    !!
    !! The same inputs go through both kinds and the answers must match element for element.
    !! Directions are taken from pixel centres so the answers are exact rather than near a rim.
    subroutine test_bulk_int32_agrees_with_int64(error)
        type(error_type), allocatable, intent(out) :: error !! set on any disagreement.
        integer, parameter :: N = 24
        integer(int32), parameter :: NS32 = 8_int32
        integer(int64), parameter :: NS64 = 8_int64
        integer(int32) :: ip32(N), got32(N)
        integer(int64) :: ip64(N), got64(N)
        real(real64) :: th32(N), ph32(N), th64(N), ph64(N)
        real(real64) :: v32(3, N), v64(3, N)
        integer :: k
        integer(int64) :: step, npix

        npix = 12_int64 * NS64 * NS64
        step = npix / int(N, int64)
        do k = 1, N
            ip64(k) = (int(k, int64) - 1_int64) * step
            ip32(k) = int(ip64(k), int32)
        end do

        ! pix2ang, both schemes: the int32 and int64 forms must produce identical angles.
        call pf_pix2ang_ring_bulk(NS32, ip32, th32, ph32)
        call pf_pix2ang_ring_bulk(NS64, ip64, th64, ph64)
        call check(error, all(th32 == th64) .and. all(ph32 == ph64), &
            "pix2ang_ring_bulk disagrees across kinds")
        if (allocated(error)) return
        call pf_pix2ang_nest_bulk(NS32, ip32, th32, ph32)
        call pf_pix2ang_nest_bulk(NS64, ip64, th64, ph64)
        call check(error, all(th32 == th64) .and. all(ph32 == ph64), &
            "pix2ang_nest_bulk disagrees across kinds")
        if (allocated(error)) return

        ! ang2pix_nest closes that round trip in both kinds.
        call pf_ang2pix_nest_bulk(NS32, th64, ph64, got32)
        call pf_ang2pix_nest_bulk(NS64, th64, ph64, got64)
        call check(error, all(int(got32, int64) == got64), &
            "ang2pix_nest_bulk disagrees across kinds")
        if (allocated(error)) return
        call check(error, all(got64 == ip64), &
            "and the round trip must return the pixels it started from")
        if (allocated(error)) return

        ! pix2vec, both schemes, then vec2pix back.
        call pf_pix2vec_ring_bulk(NS32, ip32, v32)
        call pf_pix2vec_ring_bulk(NS64, ip64, v64)
        call check(error, all(v32 == v64), "pix2vec_ring_bulk disagrees across kinds")
        if (allocated(error)) return
        call pf_pix2vec_nest_bulk(NS32, ip32, v32)
        call pf_pix2vec_nest_bulk(NS64, ip64, v64)
        call check(error, all(v32 == v64), "pix2vec_nest_bulk disagrees across kinds")
        if (allocated(error)) return

        call pf_pix2vec_ring_bulk(NS64, ip64, v64)
        call pf_vec2pix_ring_bulk(NS32, v64, got32)
        call pf_vec2pix_ring_bulk(NS64, v64, got64)
        call check(error, all(int(got32, int64) == got64), &
            "vec2pix_ring_bulk disagrees across kinds")
        if (allocated(error)) return
        call check(error, all(got64 == ip64), &
            "and every pixel centre lands back in its own pixel")
        if (allocated(error)) return
        call pf_vec2pix_nest_bulk(NS32, v64, got32)
        call pf_vec2pix_nest_bulk(NS64, v64, got64)
        call check(error, all(int(got32, int64) == got64), &
            "vec2pix_nest_bulk disagrees across kinds")
    end subroutine test_bulk_int32_agrees_with_int64

    !> Every bulk entry point accepts a ZERO-LENGTH input and does nothing with it.
    !!
    !! Each bulk routine returns before resolving a thread team or checking `nside` when there is
    !! no work, so this arm is ahead of every other guard in the procedure -- which means a bulk
    !! call on an empty array must not abort even for an `nside` that would otherwise be refused.
    !! That is the property asserted here, in both kinds, for all eight routines: nothing raises,
    !! and the deliberately invalid `nside` proves the early return really is first.
    subroutine test_bulk_empty_input_is_a_no_op(error)
        type(error_type), allocatable, intent(out) :: error !! set on any disagreement.
        integer(int32) :: ip32(0), got32(0)
        integer(int64) :: ip64(0), got64(0)
        real(real64) :: th(0), ph(0), v(3, 0)
        integer(int32), parameter :: BAD32 = 3_int32
        integer(int64), parameter :: BAD64 = 3_int64

        ! nside 3 is not a power of two, so every one of these would abort on a non-empty array.
        call pf_ang2pix_ring_bulk(BAD32, th, ph, got32)
        call pf_ang2pix_ring_bulk(BAD64, th, ph, got64)
        call pf_ang2pix_nest_bulk(BAD32, th, ph, got32)
        call pf_ang2pix_nest_bulk(BAD64, th, ph, got64)
        call pf_pix2ang_ring_bulk(BAD32, ip32, th, ph)
        call pf_pix2ang_ring_bulk(BAD64, ip64, th, ph)
        call pf_pix2ang_nest_bulk(BAD32, ip32, th, ph)
        call pf_pix2ang_nest_bulk(BAD64, ip64, th, ph)
        call pf_vec2pix_ring_bulk(BAD32, v, got32)
        call pf_vec2pix_ring_bulk(BAD64, v, got64)
        call pf_vec2pix_nest_bulk(BAD32, v, got32)
        call pf_vec2pix_nest_bulk(BAD64, v, got64)
        call pf_pix2vec_ring_bulk(BAD32, ip32, v)
        call pf_pix2vec_ring_bulk(BAD64, ip64, v)
        call pf_pix2vec_nest_bulk(BAD32, ip32, v)
        call pf_pix2vec_nest_bulk(BAD64, ip64, v)

        ! Reaching here is the assertion -- every call above would have ended the process had its
        ! nside been looked at. The check keeps the test from reading as a no-op to a human.
        call check(error, size(got32) == 0 .and. size(got64) == 0, &
            "every bulk entry point must return from a zero-length input before it checks nside")
    end subroutine test_bulk_empty_input_is_a_no_op

    !> `pf_query_disc_count` and `pf_query_disc_alloc` in their int32 kind, against the int64 twin.
    !!
    !! Both carry their own `hpx_nside_max_i32` domain check and their own narrowing of the count,
    !! and `..._alloc` has two paths of its own beneath that: a REPLAY of the recorded runs, and a
    !! second full walk for a disc whose runs overflowed the recording buffer. The replay writes
    !! through an int32 output that nothing else reaches. Both paths are driven here -- a small
    !! disc for the replay, and the whole sphere for the fallback, which is the same shape
    !! `test_disc_alloc_run_overflow` uses for the int64 kind.
    !!
    !! Both schemes, because the replay converts RING runs to NEST indices on the way out and the
    !! int32 arm of that conversion is its own line.
    subroutine test_query_disc_int32_kind(error)
        type(error_type), allocatable, intent(out) :: error !! set on any disagreement.
        integer(int32), parameter :: NS32 = 256_int32
        integer(int64), parameter :: NS64 = 256_int64
        integer(int32) :: n32
        integer(int64) :: n64, npix, k
        integer(int32), allocatable :: got32(:)
        integer(int64), allocatable :: got64(:)
        real(real64), parameter :: V(3) = [0.6_real64, -0.3_real64, 0.7416198487095663_real64]
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        integer :: isch, scheme
        logical :: ok

        do isch = 1, 2
            scheme = merge(PF_HP_RING, PF_HP_NEST, isch == 1)

            call pf_query_disc_count(NS32, V, 0.05_real64, n32, scheme=scheme)
            call pf_query_disc_count(NS64, V, 0.05_real64, n64, scheme=scheme)
            call check(error, int(n32, int64), n64, "query_disc_count disagreed across kinds")
            if (allocated(error)) return
            call check(error, n64 > 0_int64, "the fixture disc must hold pixels (vacuity guard)")
            if (allocated(error)) return

            ! The replay path: a small disc records runs, and they are replayed into the int32
            ! output array.
            call pf_query_disc_alloc(NS32, V, 0.05_real64, got32, n32, scheme=scheme)
            call pf_query_disc_alloc(NS64, V, 0.05_real64, got64, n64, scheme=scheme)
            call check(error, int(size(got32), int64), int(size(got64), int64), &
                       "query_disc_alloc allocated different sizes across kinds")
            if (allocated(error)) return
            call check(error, all(int(got32, int64) == got64), &
                       "query_disc_alloc returned different pixels across kinds")
            if (allocated(error)) return
            call check(error, int(n32, int64) == n64 .and. int(size(got32), int64) == n64, &
                       "query_disc_alloc must allocate to exactly nlist")
            if (allocated(error)) return
        end do

        ! The fallback path: a disc past pi/2 overflows the run buffer, so the answer is built by
        ! a second walk rather than a replay. Asserted in RING, where the whole sphere comes back
        ! in ascending pixel order and any gap is visible.
        npix = 12_int64 * NS64 * NS64
        call pf_query_disc_alloc(NS32, NORTH, 4.0_real64, got32, n32)
        call check(error, int(n32, int64), npix, &
                   "an int32 disc past pi/2 radians should still hold every pixel")
        if (allocated(error)) return
        call check(error, int(size(got32), int64), npix, "the allocated int32 array is the wrong size")
        if (allocated(error)) return
        ok = .true.
        do k = 1_int64, npix
            if (int(got32(k), int64) /= k - 1_int64) then
                ok = .false.
                exit
            end if
        end do
        call check(error, ok, "the whole-sphere int32 disc did not come back in ascending order")
    end subroutine test_query_disc_int32_kind

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

    ! ---- Neighbours ----
    !
    ! The Morton codec and the face-neighbour tables are derived here again, from the pixelisation
    ! itself at a finer resolution, so that the oracle shares nothing with the module's own tables.

    !> Spreads the bits of a within-face coordinate to the even positions: the test's own copy of
    !> the Morton codec, so that nothing below reaches the module's.
    pure function nb_spread(v) result(s)
        integer(int64), intent(in) :: v !! a within-face coordinate.
        integer(int64) :: s !! its bits, spread to the even positions.

        s = iand(v, int(z'00000000FFFFFFFF', int64))
        s = iand(ior(s, ishft(s, 16)), int(z'0000FFFF0000FFFF', int64))
        s = iand(ior(s, ishft(s, 8)), int(z'00FF00FF00FF00FF', int64))
        s = iand(ior(s, ishft(s, 4)), int(z'0F0F0F0F0F0F0F0F', int64))
        s = iand(ior(s, ishft(s, 2)), int(z'3333333333333333', int64))
        s = iand(ior(s, ishft(s, 1)), int(z'5555555555555555', int64))
    end function nb_spread

    !> Gathers the even bits back into a coordinate; the inverse of `nb_spread`.
    pure function nb_compact(v) result(c)
        integer(int64), intent(in) :: v !! a Morton code, or one shifted right by one for `y`.
        integer(int64) :: c !! the coordinate.

        c = iand(v, int(z'5555555555555555', int64))
        c = iand(ior(c, ishft(c, -1)), int(z'3333333333333333', int64))
        c = iand(ior(c, ishft(c, -2)), int(z'0F0F0F0F0F0F0F0F', int64))
        c = iand(ior(c, ishft(c, -4)), int(z'00FF00FF00FF00FF', int64))
        c = iand(ior(c, ishft(c, -8)), int(z'0000FFFF0000FFFF', int64))
        c = iand(ior(c, ishft(c, -16)), int(z'00000000FFFFFFFF', int64))
    end function nb_compact

    !> A NEST pixel's face and within-face coordinates.
    pure subroutine nb_nest2xyf(nside, ipix, ix, iy, face)
        integer(int64), intent(in) :: nside !! resolution parameter.
        integer(int64), intent(in) :: ipix !! a NEST pixel.
        integer(int64), intent(out) :: ix !! its `x` within the face.
        integer(int64), intent(out) :: iy !! its `y` within the face.
        integer(int64), intent(out) :: face !! its face, 0 .. 11.
        integer(int64) :: raw

        face = ipix / (nside * nside)
        raw = ipix - face * nside * nside
        ix = nb_compact(raw)
        iy = nb_compact(ishft(raw, -1))
    end subroutine nb_nest2xyf

    !> The NEST pixel at within-face coordinates `(ix, iy)` of `face`.
    pure function nb_xyf2nest(nside, ix, iy, face) result(ipix)
        integer(int64), intent(in) :: nside !! resolution parameter.
        integer(int64), intent(in) :: ix !! `x` within the face.
        integer(int64), intent(in) :: iy !! `y` within the face.
        integer(int64), intent(in) :: face !! the face, 0 .. 11.
        integer(int64) :: ipix !! the NEST pixel.

        ipix = face * nside * nside + ior(nb_spread(ix), ishft(nb_spread(iy), 1))
    end function nb_xyf2nest

    !> Which of flip-x (1), flip-y (2) and swap (4) map the wrapped `(x, y)` onto `(xq, yq)`,
    !> preferring `prefer` when it fits; -1 when no combination does.
    pure function nb_match_bits(nside, x, y, xq, yq, prefer) result(okbits)
        integer(int64), intent(in) :: nside !! resolution parameter.
        integer(int64), intent(in) :: x !! the wrapped `x`.
        integer(int64), intent(in) :: y !! the wrapped `y`.
        integer(int64), intent(in) :: xq !! `x` of the pixel actually found across the edge.
        integer(int64), intent(in) :: yq !! its `y`.
        integer, intent(in) :: prefer !! the bits already recorded for this entry, or a negative value.
        integer :: okbits !! the transform bits, or -1.
        integer(int64) :: xt, yt, tt
        integer :: bits

        okbits = -1
        do bits = 0, 7
            xt = x
            yt = y
            if (iand(bits, 1) /= 0) xt = nside - xt - 1_int64
            if (iand(bits, 2) /= 0) yt = nside - yt - 1_int64
            if (iand(bits, 4) /= 0) then
                tt = xt
                xt = yt
                yt = tt
            end if
            if (xt == xq .and. yt == yq) then
                if (okbits < 0 .or. bits == prefer) okbits = bits
            end if
        end do
    end function nb_match_bits

    !> Records one derived table entry, counting a lookup that disagrees with an earlier one.
    subroutine nb_record(fa, sw, nseen, nbad, f, nbn, face_found, bits_found)
        integer, intent(inout) :: fa(0:11, 0:8) !! the face across each step class, being derived.
        integer, intent(inout) :: sw(0:11, 0:8) !! the transform bits, being derived.
        integer, intent(inout) :: nseen(0:11, 0:8) !! lookups recorded per entry.
        integer, intent(inout) :: nbad(0:11, 0:8) !! lookups that disagreed per entry.
        integer(int64), intent(in) :: f !! the face stepped from.
        integer, intent(in) :: nbn !! the step class, `4 + dx + 3*dy`.
        integer, intent(in) :: face_found !! the face the step landed on, or -1.
        integer, intent(in) :: bits_found !! the transform bits that fitted, or -1.

        if (bits_found < 0 .and. face_found >= 0) then
            nbad(f, nbn) = nbad(f, nbn) + 1
        else if (nseen(f, nbn) == 0) then
            fa(f, nbn) = face_found
            sw(f, nbn) = bits_found
        else if (fa(f, nbn) /= face_found .or. sw(f, nbn) /= bits_found) then
            nbad(f, nbn) = nbad(f, nbn) + 1
        end if
        nseen(f, nbn) = nseen(f, nbn) + 1
    end subroutine nb_record

    !> The eight neighbours of `(face, ix, iy)` through a given pair of tables, in the order
    !> `pf_neighbours_nest` returns them.
    pure subroutine nb_from_tables(nside, face, ix, iy, fa, sw, nb)
        integer(int64), intent(in) :: nside !! resolution parameter.
        integer(int64), intent(in) :: face !! the pixel's face.
        integer(int64), intent(in) :: ix !! its `x` within the face.
        integer(int64), intent(in) :: iy !! its `y`.
        integer, intent(in) :: fa(0:11, 0:8) !! the face across each step class.
        integer, intent(in) :: sw(0:11, 0:8) !! the transform bits per entry.
        integer(int64), intent(out) :: nb(8) !! the neighbours, -1 where the table says none.
        integer(int64) :: x, y, f, t
        integer :: m, nbnum, bits

        do m = 1, 8
            x = ix + nb_dx(m)
            y = iy + nb_dy(m)
            nbnum = 4
            if (x < 0_int64) then
                x = x + nside
                nbnum = nbnum - 1
            else if (x >= nside) then
                x = x - nside
                nbnum = nbnum + 1
            end if
            if (y < 0_int64) then
                y = y + nside
                nbnum = nbnum - 3
            else if (y >= nside) then
                y = y - nside
                nbnum = nbnum + 3
            end if
            if (nbnum == 4) then
                nb(m) = nb_xyf2nest(nside, x, y, face)
                cycle
            end if
            f = int(fa(int(face), nbnum), int64)
            if (f < 0_int64) then
                nb(m) = -1_int64
                cycle
            end if
            bits = sw(int(face), nbnum)
            if (iand(bits, 1) /= 0) x = nside - x - 1_int64
            if (iand(bits, 2) /= 0) y = nside - y - 1_int64
            if (iand(bits, 4) /= 0) then
                t = x
                x = y
                y = t
            end if
            nb(m) = nb_xyf2nest(nside, x, y, f)
        end do
    end subroutine nb_from_tables

    !> Derives the face-neighbour tables from the pixelisation itself.
    !>
    !> Edge entries: for every non-corner boundary pixel of every face, the pixel across the edge
    !> is found at a 64x finer resolution -- the sub-pixel at the middle of the crossed edge is
    !> extrapolated one sub-pixel step outward through `pf_pix2vec_nest`, `pf_vec2pix_nest` names
    !> the pixel there, and dropping the six finest bits of its coordinates gives the coarse pixel;
    !> the face and the transform mapping the wrapped `(x, y)` onto it are read off and required to
    !> agree along the whole edge. Corner entries: from the corner pixel, the point reflected
    !> through the vertex; when it lands in one of the two edge neighbours, or in the pixel itself,
    !> the vertex is one where three faces meet and the entry is -1.
    subroutine nb_derive_tables(nside, fa, sw, nbad, nseen)
        integer(int64), intent(in) :: nside !! the coarse resolution the tables are derived at.
        integer, intent(out) :: fa(0:11, 0:8) !! the face across each step class; -1 none, -2 never reached.
        integer, intent(out) :: sw(0:11, 0:8) !! the transform bits per entry.
        integer, intent(out) :: nbad(0:11, 0:8) !! lookups that disagreed with an earlier one.
        integer, intent(out) :: nseen(0:11, 0:8) !! lookups recorded per entry.
        integer(int64), parameter :: sub = 64_int64
        integer(int64) :: f, ix, iy, x, y, q, fq, xq, yq, nsub, sx, sy, qs, xs, ys, k, e1, e2, nbtmp(8)
        integer :: m, nbnum, okbits, dx, dy
        real(real64) :: c0(3), cm(3), cx(3), cy(3), qv(3), vtx(3)

        nsub = nside * sub
        fa = -2
        sw = -2
        nseen = 0
        nbad = 0
        ! ---- edge entries ----
        do f = 0_int64, 11_int64
            do m = 1, 7, 2
                dx = nb_dx(m)
                dy = nb_dy(m)
                do k = 1_int64, nside - 2_int64
                    if (dx /= 0) then
                        ix = merge(nside - 1_int64, 0_int64, dx > 0)
                        iy = k
                    else
                        ix = k
                        iy = merge(nside - 1_int64, 0_int64, dy > 0)
                    end if
                    sx = sub * ix + sub / 2_int64
                    sy = sub * iy + sub / 2_int64
                    if (dx > 0) sx = sub * ix + sub - 1_int64
                    if (dx < 0) sx = sub * ix
                    if (dy > 0) sy = sub * iy + sub - 1_int64
                    if (dy < 0) sy = sub * iy
                    call pf_pix2vec_nest(nsub, nb_xyf2nest(nsub, sx, sy, f), c0)
                    call pf_pix2vec_nest(nsub, nb_xyf2nest(nsub, sx - dx, sy - dy, f), cm)
                    qv = 2.0_real64 * c0 - cm
                    qv = qv / sqrt(sum(qv * qv))
                    call pf_vec2pix_nest(nsub, qv, qs)
                    call nb_nest2xyf(nsub, qs, xs, ys, fq)
                    xq = xs / sub
                    yq = ys / sub
                    x = ix + dx
                    y = iy + dy
                    nbnum = 4
                    if (x < 0_int64) then
                        x = x + nside
                        nbnum = nbnum - 1
                    else if (x >= nside) then
                        x = x - nside
                        nbnum = nbnum + 1
                    end if
                    if (y < 0_int64) then
                        y = y + nside
                        nbnum = nbnum - 3
                    else if (y >= nside) then
                        y = y - nside
                        nbnum = nbnum + 3
                    end if
                    okbits = nb_match_bits(nside, x, y, xq, yq, sw(f, nbnum))
                    call nb_record(fa, sw, nseen, nbad, f, nbnum, int(fq), okbits)
                end do
            end do
        end do
        ! ---- corner entries ----
        do f = 0_int64, 11_int64
            do m = 2, 8, 2
                dx = nb_dx(m)
                dy = nb_dy(m)
                ix = merge(nside - 1_int64, 0_int64, dx > 0)
                iy = merge(nside - 1_int64, 0_int64, dy > 0)
                sx = merge(sub * ix + sub - 1_int64, sub * ix, dx > 0)
                sy = merge(sub * iy + sub - 1_int64, sub * iy, dy > 0)
                call pf_pix2vec_nest(nsub, nb_xyf2nest(nsub, sx, sy, f), c0)
                call pf_pix2vec_nest(nsub, nb_xyf2nest(nsub, sx - dx, sy, f), cx)
                call pf_pix2vec_nest(nsub, nb_xyf2nest(nsub, sx, sy - dy, f), cy)
                ! The vertex is half a sub-step beyond the corner sub-pixel in both directions.
                vtx = c0 + 0.5_real64 * ((c0 - cx) + (c0 - cy))
                qv = 2.0_real64 * vtx - c0
                qv = qv / sqrt(sum(qv * qv))
                call pf_vec2pix_nest(nsub, qv, qs)
                call nb_nest2xyf(nsub, qs, xs, ys, fq)
                xq = xs / sub
                yq = ys / sub
                q = nb_xyf2nest(nside, xq, yq, fq)
                ! The two edge neighbours of the corner pixel, from the entries derived above.
                call nb_from_tables(nside, f, ix, iy, fa, sw, nbtmp)
                e1 = nbtmp(m - 1)
                e2 = nbtmp(mod(m, 8) + 1)
                nbnum = 4 + dx + 3 * dy
                x = modulo(ix + dx, nside)
                y = modulo(iy + dy, nside)
                if (q == e1 .or. q == e2 .or. q == nb_xyf2nest(nside, ix, iy, f)) then
                    call nb_record(fa, sw, nseen, nbad, f, nbnum, -1, 0)
                else
                    okbits = nb_match_bits(nside, x, y, xq, yq, -2)
                    call nb_record(fa, sw, nseen, nbad, f, nbnum, int(fq), okbits)
                end if
            end do
        end do
    end subroutine nb_derive_tables

    !> A disc by breadth-first search over neighbours from the containing pixel, accepting a pixel
    !> when its centre lies within `radius + pf_max_pixrad` of `vec`: the inclusive-by-centre rule
    !> `pf_query_disc` publishes, reached through nothing but neighbour steps.
    subroutine nb_disc_bfs(nside, vec, radius, list, n)
        integer(int64), intent(in) :: nside !! resolution parameter.
        real(real64), intent(in) :: vec(3) !! the disc centre, a unit vector.
        real(real64), intent(in) :: radius !! the disc radius, radians.
        integer(int64), intent(inout) :: list(:) !! the pixels found, in the order found.
        integer(int64), intent(out) :: n !! how many.
        integer(int64) :: p0, head, nb(8), q, j
        real(real64) :: cosr, c(3)
        integer :: m
        logical :: seen

        cosr = cos(min(radius + pf_max_pixrad(nside), pi))
        call pf_vec2pix_nest(nside, vec, p0)
        n = 1_int64
        list(1) = p0
        head = 1_int64
        do while (head <= n)
            call pf_neighbours_nest(nside, list(head), nb)
            head = head + 1_int64
            do m = 1, 8
                q = nb(m)
                if (q < 0_int64) cycle
                seen = .false.
                do j = 1_int64, n
                    if (list(j) == q) seen = .true.
                end do
                if (seen) cycle
                call pf_pix2vec_nest(nside, q, c)
                if (c(1) * vec(1) + c(2) * vec(2) + c(3) * vec(3) >= cosr) then
                    n = n + 1_int64
                    if (n > size(list, kind=int64)) error stop "nb_disc_bfs: the list overflowed"
                    list(n) = q
                end if
            end do
        end do
    end subroutine nb_disc_bfs

    !> Sorts the first `n` entries of `a` ascending, in place: an insertion sort, for short lists.
    pure subroutine nb_sort(a, n)
        integer(int64), intent(inout) :: a(:) !! the list.
        integer(int64), intent(in) :: n !! how many leading entries to sort.
        integer(int64) :: i, j, key

        do i = 2_int64, n
            key = a(i)
            j = i - 1_int64
            do while (j >= 1_int64)
                if (a(j) <= key) exit
                a(j + 1_int64) = a(j)
                j = j - 1_int64
            end do
            a(j + 1_int64) = key
        end do
    end subroutine nb_sort

    !> Every neighbour relation is symmetric -- `q` is among `p`'s eight exactly when `p` is among
    !> `q`'s -- and the eight are distinct pixels other than `p` itself. Exhaustive over every
    !> pixel at each resolution; a mutation of any table entry breaks it somewhere along that
    !> face's edge.
    subroutine test_neighbours_symmetric(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first asymmetric entry.
        integer(int64), parameter :: sides(5) = [1_int64, 2_int64, 4_int64, 16_int64, 64_int64]
        integer(int64) :: nside, npix, p, q, nb(8), nb2(8)
        integer :: k, m, m2, nbad, nchecked
        logical :: found
        character(len=160) :: detail

        nbad = 0
        nchecked = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            do p = 0_int64, npix - 1_int64
                call pf_neighbours_nest(nside, p, nb)
                do m = 1, 8
                    q = nb(m)
                    if (q < 0_int64) cycle
                    nchecked = nchecked + 1
                    found = q < npix .and. q /= p
                    if (found) then
                        call pf_neighbours_nest(nside, q, nb2)
                        found = .false.
                        do m2 = 1, 8
                            if (nb2(m2) == p) found = .true.
                        end do
                    end if
                    do m2 = 1, 8
                        if (m2 /= m .and. nb(m2) == q) found = .false.
                    end do
                    if (.not. found) then
                        nbad = nbad + 1
                        if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,i0)') &
                            "nside=", nside, " pixel=", p, " step=", m, " neighbour=", q
                    end if
                end do
            end do
        end do
        call check(error, nchecked > 0, "no neighbour was checked -- the assertion did nothing")
        if (allocated(error)) return
        call check(error, nbad, 0, "a neighbour relation was not symmetric, or an entry repeated: " // trim(detail))
    end subroutine test_neighbours_symmetric

    !> Exactly 24 corner neighbours are missing over the whole sphere at any resolution -- three at
    !> each of the eight vertices where three faces meet -- and nowhere else: every -1 sits at a
    !> corner step of a face-corner pixel whose two edge neighbours across that corner exist and lie
    !> on two other, distinct faces. A -1 replaced by a face, or a face by a -1, moves the count.
    subroutine test_neighbours_missing_corners(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first fault.
        integer(int64), parameter :: sides(5) = [1_int64, 2_int64, 4_int64, 16_int64, 64_int64]
        integer(int64) :: nside, npix, p, nb(8), ix, iy, face, e1, e2, f1, f2
        integer :: k, m, nmissing, nbad
        character(len=160) :: detail

        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            nmissing = 0
            nbad = 0
            do p = 0_int64, npix - 1_int64
                call pf_neighbours_nest(nside, p, nb)
                do m = 1, 8
                    if (nb(m) >= 0_int64) cycle
                    nmissing = nmissing + 1
                    call nb_nest2xyf(nside, p, ix, iy, face)
                    if (nb_dx(m) == 0 .or. nb_dy(m) == 0) then
                        nbad = nbad + 1
                    else if (ix /= merge(nside - 1_int64, 0_int64, nb_dx(m) > 0) .or. &
                             iy /= merge(nside - 1_int64, 0_int64, nb_dy(m) > 0)) then
                        nbad = nbad + 1
                    else
                        e1 = nb(m - 1)
                        e2 = nb(mod(m, 8) + 1)
                        f1 = e1 / (nside * nside)
                        f2 = e2 / (nside * nside)
                        if (e1 < 0_int64 .or. e2 < 0_int64 .or. f1 == face .or. f2 == face .or. f1 == f2) &
                            nbad = nbad + 1
                    end if
                    if (nbad == 1 .and. len_trim(detail) == 0) write (detail, '(a,i0,a,i0,a,i0)') &
                        "nside=", nside, " pixel=", p, " step=", m
                end do
            end do
            call check(error, nmissing, 24, "the number of missing corner neighbours over the sphere is not 24")
            if (allocated(error)) return
            call check(error, nbad, 0, "a missing neighbour was not at a corner where three faces meet: " // &
                trim(detail))
            if (allocated(error)) return
        end do
    end subroutine test_neighbours_missing_corners

    !> The module's face tables agree with tables derived from the pixelisation itself: at
    !> `nside` 8 and 16 the derivation of `nb_derive_tables` is self-consistent along every edge,
    !> reaches every entry, and reproduces `pf_neighbours_nest` for every pixel -- interior fast
    !> path and boundary tables alike, through the test's own Morton codec. Any table entry or
    !> transform bit changed, or any interior step, breaks it.
    subroutine test_neighbour_tables_match_the_pixelisation(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: sides(2) = [8_int64, 16_int64]
        integer :: fa(0:11, 0:8), sw(0:11, 0:8), nbad(0:11, 0:8), nseen(0:11, 0:8)
        integer(int64) :: nside, npix, p, ix, iy, face, want(8), got(8)
        integer :: k, ndiff
        character(len=160) :: detail

        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            call nb_derive_tables(nside, fa, sw, nbad, nseen)
            call check(error, sum(nbad), 0, "the derivation disagreed with itself along an edge")
            if (allocated(error)) return
            ! Every entry but the twelve of the stay-on-face class, which no crossing step records.
            call check(error, count(nseen > 0), 12 * 8, "the derivation did not reach every crossing entry")
            if (allocated(error)) return
            ndiff = 0
            do p = 0_int64, npix - 1_int64
                call nb_nest2xyf(nside, p, ix, iy, face)
                call nb_from_tables(nside, face, ix, iy, fa, sw, want)
                call pf_neighbours_nest(nside, p, got)
                if (any(got /= want)) then
                    ndiff = ndiff + 1
                    if (ndiff == 1) write (detail, '(a,i0,a,i0,a,8(1x,i0),a,8(1x,i0))') &
                        "nside=", nside, " pixel=", p, " got", got, " derived", want
                end if
            end do
            call check(error, ndiff, 0, "pf_neighbours_nest disagrees with the derived tables: " // trim(detail))
            if (allocated(error)) return
        end do
    end subroutine test_neighbour_tables_match_the_pixelisation

    !> A disc grown from the containing pixel by neighbour steps, under the inclusive-by-centre
    !> rule, is exactly the set `pf_query_disc` returns for `inclusive=.true.` in the NEST scheme
    !> -- so the neighbours connect every pixel of the sphere to every other, and an interior
    !> Morton step that lands one pixel off shows as a missing or a spurious pixel. Disagreements
    !> on a centre within rounding of the rim are not counted, as in the disc tests above.
    subroutine test_neighbour_disc_matches_query_disc(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first real disagreement.
        integer(int64), parameter :: sides(3) = [8_int64, 64_int64, 1024_int64]
        real(real64), parameter :: rfrac(4) = [0.2_real64, 0.5_real64, 1.0_real64, 3.0_real64]
        integer(int64) :: nside, lib(2048), bfs(2048), nlib, nbfs, i, j, q
        real(real64) :: v0(3), c(3), z, ph, radius, resol, cosr, dot
        integer :: k, cc, ir, nbad, nchecked
        logical :: same, inlib, inbfs
        character(len=160) :: detail

        nbad = 0
        nchecked = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            resol = pf_nside2resol(nside)
            do cc = 1, 40
                z = -1.0_real64 + 2.0_real64 * (real(cc, real64) - 0.5_real64) / 40.0_real64
                ph = 2.399963229728653_real64 * real(cc, real64)
                v0 = [sqrt(1.0_real64 - z * z) * cos(ph), sqrt(1.0_real64 - z * z) * sin(ph), z]
                do ir = 1, size(rfrac)
                    radius = rfrac(ir) * resol
                    call pf_query_disc(nside, v0, radius, lib, nlib, scheme=PF_HP_NEST, inclusive=.true.)
                    call nb_disc_bfs(nside, v0, radius, bfs, nbfs)
                    call nb_sort(lib, nlib)
                    call nb_sort(bfs, nbfs)
                    nchecked = nchecked + 1
                    same = nlib == nbfs
                    if (same) same = all(lib(1:nlib) == bfs(1:nbfs))
                    if (same) cycle
                    ! Every pixel in one list and not the other must be a rim case.
                    cosr = cos(min(radius + pf_max_pixrad(nside), pi))
                    do i = 1_int64, nlib + nbfs
                        if (i <= nlib) then
                            q = lib(i)
                        else
                            q = bfs(i - nlib)
                        end if
                        inlib = .false.
                        inbfs = .false.
                        do j = 1_int64, nlib
                            if (lib(j) == q) inlib = .true.
                        end do
                        do j = 1_int64, nbfs
                            if (bfs(j) == q) inbfs = .true.
                        end do
                        if (inlib .eqv. inbfs) cycle
                        call pf_pix2vec_nest(nside, q, c)
                        dot = c(1) * v0(1) + c(2) * v0(2) + c(3) * v0(3)
                        if (abs(dot - cosr) <= rim_tol) cycle
                        nbad = nbad + 1
                        if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0,a,l1,a,l1)') &
                            "nside=", nside, " case=", cc, " pixel=", q, " walk=", inlib, " bfs=", inbfs
                    end do
                end do
            end do
        end do
        call check(error, nchecked > 0, "no disc was compared -- the assertion did nothing")
        if (allocated(error)) return
        call check(error, nbad, 0, "the neighbour walk and the ring walk disagree away from the rim: " // trim(detail))
    end subroutine test_neighbour_disc_matches_query_disc

    !> The int32 specifics answer as the int64 ones, and the RING form names the same pixels as the
    !> NEST form mapped through `pf_nest2ring`, missing corners included. A kind conversion
    !> dropped, or a ring conversion skipped on one entry, shows here.
    subroutine test_neighbours_kinds_and_ring(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: sides(2) = [4_int64, 64_int64]
        integer(int64), parameter :: steps(2) = [1_int64, 7_int64]
        integer(int64) :: nside, npix, p, pr, want, nb64(8), rb64(8)
        integer(int32) :: nb32(8), rb32(8)
        integer :: k, m, nbad, nchecked
        character(len=160) :: detail

        nbad = 0
        nchecked = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            do p = 0_int64, npix - 1_int64, steps(k)
                call pf_neighbours_nest(nside, p, nb64)
                call pf_neighbours_nest(int(nside, int32), int(p, int32), nb32)
                call pf_nest2ring(nside, p, pr)
                call pf_neighbours_ring(nside, pr, rb64)
                call pf_neighbours_ring(int(nside, int32), int(pr, int32), rb32)
                nchecked = nchecked + 1
                do m = 1, 8
                    want = -1_int64
                    if (nb64(m) >= 0_int64) call pf_nest2ring(nside, nb64(m), want)
                    if (int(nb32(m), int64) /= nb64(m) .or. rb64(m) /= want .or. int(rb32(m), int64) /= want) then
                        nbad = nbad + 1
                        if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') "nside=", nside, " pixel=", p, " step=", m
                    end if
                end do
            end do
        end do
        call check(error, nchecked > 0, "no pixel was checked -- the assertion did nothing")
        if (allocated(error)) return
        call check(error, nbad, 0, "the kinds or the schemes disagree about a neighbour: " // trim(detail))
    end subroutine test_neighbours_kinds_and_ring

end module test_healpix
