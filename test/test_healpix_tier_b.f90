!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tier B of `parquet_healpix`: the grid arithmetic, the vector forms, the disc siblings and the
!> bulk forms.
!>
!> **Most of what this suite asserts needs no new oracle, and that is the point.** Tier A froze a
!> table of reference pixels, pixel centres and disc results derived from an independent model;
!> almost every Tier B name can be tied back to those rather than to a fresh set of numbers. So
!> `pf_vec2pix_*` is checked against the frozen `pf_ang2pix_*` values rather than against itself,
!> `pf_query_disc_count` against the frozen disc lists, and the bulk forms against the scalar forms
!> those tables already pin.
!>
!> The exceptions -- the exhaustive integer identities of the grid arithmetic, the geometric
!> identity behind `pf_ud_pix_nest`, and the chord relations -- are exact rather than approximate,
!> so they need no oracle either. `test_healpix_reference.f90` carries the handful of Tier B values
!> that genuinely are frozen.
!>
!> **A round trip proves less than a tie to frozen data**, and where both are available this suite
!> asserts both: a round trip passes happily against a consistently wrong implementation, which is
!> the first risk the feature document names.
module test_healpix_tier_b
    use parquet_healpix
    use test_healpix_vectors
    use iso_fortran_env, only : int32, int64, real64
    use ieee_arithmetic, only : ieee_get_flag, ieee_set_flag, ieee_support_flag, ieee_underflow
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_tests_healpix_tier_b

    !> pi, to the precision a `real64` literal carries.
    real(real64), parameter :: pi = 3.141592653589793238462643_real64
    !> 2*pi.
    real(real64), parameter :: twopi = 2.0_real64 * pi

contains

    !> Registers every Tier B test.
    subroutine collect_tests_healpix_tier_b(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("vec2pix agrees with the frozen ang2pix values", &
                         test_vec2pix_matches_frozen_ang2pix), &
            new_unittest("vec2pix inverts pix2vec over every pixel", &
                         test_vec2pix_inverts_pix2vec), &
            new_unittest("vec2pix is invariant under the vector's scale", &
                         test_vec2pix_scale_invariant), &
            new_unittest("ang2vec and vec2ang round trip", &
                         test_ang_vec_round_trip), &
            new_unittest("ang2vec reproduces pix2vec at every pixel centre", &
                         test_ang2vec_matches_pix2vec), &
            new_unittest("vec2ang answers the documented value at the poles and at zero", &
                         test_vec2ang_degenerate), &
            new_unittest("the grid arithmetic is exact at every order", &
                         test_grid_arithmetic_exact), &
            new_unittest("the grid arithmetic reports -1 outside its domain", &
                         test_grid_arithmetic_sentinels), &
            new_unittest("pixel areas sum to the whole sphere", &
                         test_pixarea_sums_to_sphere), &
            new_unittest("pix2ring and ring2z agree with the pixel's own centre", &
                         test_rings_agree_with_centres), &
            new_unittest("ud_pix_nest is a resolution change on the sphere", &
                         test_ud_pix_nest_geometric), &
            new_unittest("ud_pix_nest partitions a pixel into its children", &
                         test_ud_pix_nest_partition), &
            new_unittest("ud_pix_nest reports -1 outside its domain", &
                         test_ud_pix_nest_sentinels), &
            new_unittest("the chord pair round trips and matches angdist", &
                         test_chord_round_trip), &
            new_unittest("query_disc_count equals query_disc's own count", &
                         test_disc_count_matches), &
            new_unittest("query_disc_alloc returns exactly what query_disc does", &
                         test_disc_alloc_matches), &
            new_unittest("query_disc_alloc allocates a zero-length array for an empty disc", &
                         test_disc_alloc_empty), &
            new_unittest("query_disc_alloc agrees whether or not its run record overflows", &
                         test_disc_alloc_run_record), &
            new_unittest("query_disc_alloc holds a disc the fixed-buffer form could not", &
                         test_disc_alloc_beyond_a_buffer), &
            new_unittest("every bulk form equals its scalar form at every thread count", &
                         test_bulk_matches_scalar), &
            new_unittest("every bulk form accepts an empty and a one-element array", &
                         test_bulk_degenerate_sizes), &
            new_unittest("the bulk forms agree across the int32 and int64 kinds", &
                         test_bulk_kind_agreement) &
            ]
    end subroutine collect_tests_healpix_tier_b

    ! ---- B.1/B.2: vectors ----

    !> `pf_vec2pix_*` against the pixels the reference vectors froze for `pf_ang2pix_*`.
    !>
    !> **The tie that makes the vector form more than self-consistent.** Its companion below is a
    !> round trip, which a consistently wrong implementation passes; this one compares against
    !> values an independent model produced, reached through `pf_ang2vec`.
    subroutine test_vec2pix_matches_frozen_ang2pix(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: k, j, nbad, ncase, nskip
        integer(int64) :: nside, got, idx
        real(real64) :: v(3)
        character(len=200) :: detail

        nbad = 0
        ncase = 0
        nskip = 0
        detail = ""
        do k = 1, hv_n_nside
            nside = hv_nside(k)
            do j = 1, hv_n_pos
                idx = int((k - 1) * hv_n_pos + j, int64)
                ! Skip where the pixelisation is finer than a double can address FROM A VECTOR --
                ! see `resolvable` below. Nothing about the implementation is being excused here:
                ! the two rings in question hold z values less than an ulp apart, so no double
                ! precision code can tell them apart from a direction, and asserting otherwise
                ! would pin one particular rounding.
                if (.not. resolvable(nside, hv_pix_ring(idx))) then
                    nskip = nskip + 1
                    cycle
                end if
                call pf_ang2vec(hv_theta(j), hv_phi(j), v)
                ncase = ncase + 1
                call pf_vec2pix_ring(nside, v, got)
                if (got /= hv_pix_ring(idx)) then
                    nbad = nbad + 1
                    if (detail == "") write (detail, '(a,i0,a,i0,a,i0,a,i0)') "RING nside=", &
                        nside, " pos=", j, " expected=", hv_pix_ring(idx), " got=", got
                end if
                call pf_vec2pix_nest(nside, v, got)
                if (got /= hv_pix_nest(idx)) then
                    nbad = nbad + 1
                    if (detail == "") write (detail, '(a,i0,a,i0,a,i0,a,i0)') "NEST nside=", &
                        nside, " pos=", j, " expected=", hv_pix_nest(idx), " got=", got
                end if
            end do
        end do
        ! A vacuity guard, and a sharp one: if the resolvability test ever starts rejecting
        ! everything, this test would otherwise pass while asserting nothing at all.
        call check(error, ncase > hv_n_nside * hv_n_pos / 2, &
                   "more than half the reference positions were skipped as unresolvable")
        if (allocated(error)) return
        call check(error, nbad, 0, "vec2pix disagreed with the frozen ang2pix pixels: " // &
                   trim(detail))

    contains

        !> Whether a pixel's ring can be told from its neighbour given only a direction vector.
        !>
        !> **A limit of double precision, not of this module.** A ring's latitude is its `z`, and
        !> near a pole consecutive rings' `z` values converge: the gap between ring `i` and ring
        !> `i+1` is `(2i+1)/(3*nside**2)`, which at `nside = 2**29` is **3.5e-18 at ring 1 and
        !> 1.5e-16 at ring 65** -- 0.02 and 0.68 of an ulp of `z = 1`. Two adjacent rings there are
        !> not distinct double-precision numbers, so recovering `z` from a unit vector cannot land
        !> on the right one and no implementation can make it.
        !>
        !> `pf_ang2pix_*` is unaffected and is asserted at every resolution by
        !> `test_healpix_reference.f90`: it works from `theta`, where the same rings are millions
        !> of ulps apart. Only the vector route loses them, which is why this test and not that one
        !> needs the guard. The four-ulp margin is a working bound rather than a measured one --
        !> the recovery costs a handful of roundings -- and it excludes 30 of the 384 cases here,
        !> every one of them at `nside = 2**29`.
        logical function resolvable(ns, ipix)
            integer(int64), intent(in) :: ns !! the resolution parameter.
            integer(int64), intent(in) :: ipix !! the RING pixel the direction should land on.
            integer(int64) :: iring
            real(real64) :: z_here, z_next

            resolvable = .true.
            iring = pf_pix2ring_ring(ns, ipix)
            if (iring < 1_int64 .or. iring >= 4_int64 * ns - 1_int64) return
            z_here = pf_ring2z(ns, iring)
            z_next = pf_ring2z(ns, iring + 1_int64)
            resolvable = abs(z_here - z_next) > 4.0_real64 * spacing(1.0_real64)
        end function resolvable

    end subroutine test_vec2pix_matches_frozen_ang2pix

    !> `pf_vec2pix_*(nside, pf_pix2vec_*(nside, p)) == p`, over every pixel at small `nside`.
    subroutine test_vec2pix_inverts_pix2vec(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: sides(4) = [1_int64, 2_int64, 8_int64, 32_int64]
        integer :: k, nbad
        integer(int64) :: nside, p, npix, got
        real(real64) :: v(3)
        character(len=160) :: detail

        nbad = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            do p = 0_int64, npix - 1_int64
                call pf_pix2vec_ring(nside, p, v)
                call pf_vec2pix_ring(nside, v, got)
                if (got /= p) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') "RING nside=", nside, &
                        " p=", p, " got=", got
                end if
                call pf_pix2vec_nest(nside, p, v)
                call pf_vec2pix_nest(nside, v, got)
                if (got /= p) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,i0)') "NEST nside=", nside, &
                        " p=", p, " got=", got
                end if
            end do
        end do
        call check(error, nbad, 0, "a pixel centre did not map back to its own pixel: " // &
                   trim(detail))
    end subroutine test_vec2pix_inverts_pix2vec

    !> A direction names the same pixel however its components are scaled.
    !>
    !> This is the shape that found the original fault in `pf_query_disc`: squaring the components
    !> before scaling underflows `[1e-300, 0, 1e-300]` to zero and overflows `[1e300, 0, 1e300]` to
    !> infinity, each giving an answer wrong by 45 degrees rather than by a rounding. `healpy`
    !> 1.20.0 returns NaN for the first and pi/2 for the second.
    !>
    !> **Building the fixture raises `ieee_underflow`, and the library must not.** `base * 1e-300`
    !> is subnormal for every component below about 1e-8, which is most of them, so the scaling on
    !> this test's own line raises the flag -- and nagfor reports a raised flag as
    !> `Warning: Floating underflow occurred` at STOP, where it names neither a test nor a line and
    !> reads as a defect in the library. So the flag is saved on entry and restored on exit, and
    !> the same clear-and-read turns the nuisance into an assertion: `hpx_vec_unit` divides by the
    !> largest component before squaring anything, so no scale in this list may raise underflow
    !> inside `pf_vec2pix_*`. That claim was previously only a comment on that procedure.
    subroutine test_vec2pix_scale_invariant(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        real(real64), parameter :: scales(5) = [1.0e-300_real64, 1.0e-30_real64, 1.0_real64, &
                                                1.0e30_real64, 1.0e300_real64]
        integer(int64), parameter :: nside = 64_int64
        integer :: k, j, nbad, nflag
        integer(int64) :: want_r, want_n, got
        real(real64) :: base(3), v(3)
        logical :: watch, saved, raised
        character(len=160) :: detail, flagdetail

        watch = ieee_support_flag(ieee_underflow, 1.0_real64)
        saved = .false.
        if (watch) call ieee_get_flag(ieee_underflow, saved)

        nbad = 0
        nflag = 0
        detail = ""
        flagdetail = ""
        do j = 1, hv_n_pos
            call pf_ang2vec(hv_theta(j), hv_phi(j), base)
            call pf_vec2pix_ring(nside, base, want_r)
            call pf_vec2pix_nest(nside, base, want_n)
            do k = 1, size(scales)
                v = base * scales(k)
                ! A direction whose every component underflows to zero is the zero vector and no
                ! longer names anything; skip it rather than assert about it.
                if (all(v == 0.0_real64)) cycle
                if (.not. all(v == v)) cycle
                ! Clear whatever the scaling above raised, so that what is read back below names
                ! only the two library calls between here and the read.
                if (watch) call ieee_set_flag(ieee_underflow, .false.)
                call pf_vec2pix_ring(nside, v, got)
                if (got /= want_r) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,es10.2,a,i0,a,i0,a,i0)') "RING scale=", &
                        scales(k), " pos=", j, " expected=", want_r, " got=", got
                end if
                call pf_vec2pix_nest(nside, v, got)
                if (got /= want_n) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,es10.2,a,i0,a,i0,a,i0)') "NEST scale=", &
                        scales(k), " pos=", j, " expected=", want_n, " got=", got
                end if
                if (watch) then
                    call ieee_get_flag(ieee_underflow, raised)
                    if (raised) then
                        nflag = nflag + 1
                        if (nflag == 1) write (flagdetail, '(a,es10.2,a,i0)') &
                            "scale=", scales(k), " pos=", j
                    end if
                end if
            end do
        end do
        ! Restore what the caller had, rather than `saved .or. raised`: everything raised in
        ! between was either this test's own scaling or a failure already recorded above.
        if (watch) call ieee_set_flag(ieee_underflow, saved)

        call check(error, nbad, 0, "vec2pix was not invariant under the vector's scale: " // &
                   trim(detail))
        if (allocated(error)) return
        call check(error, nflag, 0, "vec2pix raised ieee_underflow on a scaled direction: " // &
                   trim(flagdetail))
    end subroutine test_vec2pix_scale_invariant

    !> `pf_vec2ang(pf_ang2vec(theta, phi))` returns the direction it was given.
    subroutine test_ang_vec_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: j, nbad
        real(real64) :: v(3), th, ph, dphi
        character(len=160) :: detail

        nbad = 0
        detail = ""
        do j = 1, hv_n_pos
            call pf_ang2vec(hv_theta(j), hv_phi(j), v)
            call pf_vec2ang(v, th, ph)
            if (abs(th - hv_theta(j)) > 1.0e-12_real64) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,es16.9,a,es16.9)') "theta pos=", j, &
                    " expected=", hv_theta(j), " got=", th
                cycle
            end if
            ! Longitude is undefined at a pole, so it is only asserted where the direction has a
            ! transverse component to define it.
            if (sin(hv_theta(j)) <= 1.0e-9_real64) cycle
            dphi = abs(modulo(ph - hv_phi(j) + pi, twopi) - pi)
            if (dphi > 1.0e-12_real64) then
                nbad = nbad + 1
                if (nbad == 1) write (detail, '(a,i0,a,es16.9,a,es16.9)') "phi pos=", j, &
                    " expected=", hv_phi(j), " got=", ph
            end if
        end do
        call check(error, nbad, 0, "ang2vec/vec2ang did not round trip: " // trim(detail))
    end subroutine test_ang_vec_round_trip

    !> `pf_ang2vec(pf_pix2ang_*(p))` reproduces `pf_pix2vec_*(p)`.
    !>
    !> Ties the new conversion to Tier A's primitive one. The tolerance is loose near a pole for
    !> the reason `pf_pix2ang_*` documents -- `theta` there is derived through an `acos` and loses
    !> about half its digits -- so this asserts agreement rather than an ulp.
    subroutine test_ang2vec_matches_pix2vec(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: sides(3) = [4_int64, 16_int64, 64_int64]
        integer :: k, nbad
        integer(int64) :: nside, p, npix
        real(real64) :: want(3), got(3), th, ph
        character(len=160) :: detail

        nbad = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            do p = 0_int64, npix - 1_int64
                call pf_pix2vec_ring(nside, p, want)
                call pf_pix2ang_ring(nside, p, th, ph)
                call pf_ang2vec(th, ph, got)
                if (maxval(abs(got - want)) > 1.0e-12_real64) then
                    nbad = nbad + 1
                    if (nbad == 1) write (detail, '(a,i0,a,i0,a,es12.4)') "nside=", nside, &
                        " p=", p, " maxdiff=", maxval(abs(got - want))
                end if
            end do
        end do
        call check(error, nbad, 0, "ang2vec(pix2ang) did not reproduce pix2vec: " // trim(detail))
    end subroutine test_ang2vec_matches_pix2vec

    !> The three inputs for which `atan2` alone has no answer, and what this module reports instead.
    !>
    !> `atan2(0, 0)` is prohibited by the standard and raises `IEEE_INVALID` on nagfor, so both
    !> poles and the zero vector are guarded rather than passed through. What they return is a
    !> documented convention, not an accident, and this is what pins it.
    subroutine test_vec2ang_degenerate(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        real(real64) :: th, ph
        ! Named rather than written inline at the call: `pf_vec2ang`'s `vec(3)` dummy is
        ! explicit-shape, so an array constructor -- a value with no address of its own -- is
        ! argument-associated through a temporary, which ifx reports as `warning (406)` on every
        ! call under --profile debug. See CLAUDE.md's ifx-specific gotchas.
        real(real64), parameter :: north(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        real(real64), parameter :: south(3) = [0.0_real64, 0.0_real64, -1.0_real64]
        real(real64), parameter :: long_north(3) = [0.0_real64, 0.0_real64, 7.5_real64]
        real(real64), parameter :: zero3(3) = [0.0_real64, 0.0_real64, 0.0_real64]
        real(real64), parameter :: near_north(3) = [1.0e-8_real64, 0.0_real64, 1.0_real64]

        call pf_vec2ang(north, th, ph)
        call check(error, th, 0.0_real64, "the north pole's colatitude is 0", thr=1.0e-15_real64)
        if (allocated(error)) return
        call check(error, ph, 0.0_real64, "the north pole reports longitude 0", thr=0.0_real64)
        if (allocated(error)) return

        call pf_vec2ang(south, th, ph)
        call check(error, th, pi, "the south pole's colatitude is pi", thr=1.0e-15_real64)
        if (allocated(error)) return
        call check(error, ph, 0.0_real64, "the south pole reports longitude 0", thr=0.0_real64)
        if (allocated(error)) return

        ! A non-unit polar vector must answer identically: the scale is divided out first.
        call pf_vec2ang(long_north, th, ph)
        call check(error, th, 0.0_real64, "a non-unit north pole still reports colatitude 0", &
                   thr=1.0e-15_real64)
        if (allocated(error)) return

        call pf_vec2ang(zero3, th, ph)
        call check(error, th, 0.0_real64, "the zero vector reports colatitude 0", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, ph, 0.0_real64, "the zero vector reports longitude 0", thr=0.0_real64)
        if (allocated(error)) return

        ! A hair off the pole, where an `acos(z/|v|)` would answer exactly 0 and this must not.
        call pf_vec2ang(near_north, th, ph)
        call check(error, th, 1.0e-8_real64, &
                   "a direction 1e-8 off the pole reports 1e-8, not 0 as an acos would", &
                   thr=1.0e-16_real64)
    end subroutine test_vec2ang_degenerate

    ! ---- B.4: grid arithmetic ----

    !> Every grid identity, over every order the module accepts, in exact integer arithmetic.
    subroutine test_grid_arithmetic_exact(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: o, nbad
        integer(int64) :: nside, npix
        integer(int32) :: nside32
        character(len=160) :: detail

        nbad = 0
        detail = ""
        do o = 0, 29
            nside = ishft(1_int64, o)
            npix = 12_int64 * nside * nside
            if (pf_nside2npix(nside) /= npix) nbad = nbad + 1
            if (pf_npix2nside(npix) /= nside) nbad = nbad + 1
            if (pf_nside2order(nside) /= int(o, int64)) nbad = nbad + 1
            if (pf_order2nside(int(o, int64)) /= nside) nbad = nbad + 1
            if (nbad > 0 .and. detail == "") write (detail, '(a,i0)') "first failure at order ", o
            ! The int32 family agrees wherever both kinds have the value in domain.
            if (o <= 13) then
                nside32 = int(nside, int32)
                if (pf_nside2npix(nside32) /= int(npix, int32)) nbad = nbad + 1
                if (pf_npix2nside(int(npix, int32)) /= nside32) nbad = nbad + 1
                if (pf_nside2order(nside32) /= o) nbad = nbad + 1
                if (pf_order2nside(o) /= nside32) nbad = nbad + 1
            end if
        end do
        call check(error, nbad, 0, "a grid identity failed: " // trim(detail))
    end subroutine test_grid_arithmetic_exact

    !> Out-of-domain arguments report the sentinel rather than a plausible wrong answer.
    subroutine test_grid_arithmetic_sentinels(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.

        ! Not a power of two, zero, negative.
        call check(error, pf_nside2npix(3_int64), -1_int64, "nside2npix(3) is -1")
        if (allocated(error)) return
        call check(error, pf_nside2npix(0_int64), -1_int64, "nside2npix(0) is -1")
        if (allocated(error)) return
        call check(error, pf_nside2npix(-4_int64), -1_int64, "nside2npix(-4) is -1")
        if (allocated(error)) return
        call check(error, pf_nside2order(3_int64), -1_int64, "nside2order(3) is -1")
        if (allocated(error)) return

        ! Above each kind's ceiling. 2**29 is representable in int32 and is still refused there,
        ! because no int32 entry point of this module would accept it.
        call check(error, pf_nside2npix(1073741824_int64), -1_int64, "nside2npix(2**30) is -1")
        if (allocated(error)) return
        call check(error, pf_nside2npix(16384_int32), -1_int32, "the int32 form stops at 8192")
        if (allocated(error)) return
        call check(error, pf_order2nside(14_int32), -1_int32, "the int32 order stops at 13")
        if (allocated(error)) return
        call check(error, pf_order2nside(13_int32), 8192_int32, "the int32 order reaches 13")
        if (allocated(error)) return
        call check(error, pf_order2nside(30_int64), -1_int64, "the int64 order stops at 29")
        if (allocated(error)) return
        call check(error, pf_order2nside(-1_int64), -1_int64, "a negative order is -1")
        if (allocated(error)) return

        ! Near-miss pixel counts. Each of these is exactly the shape a file could carry.
        call check(error, pf_npix2nside(0_int64), -1_int64, "npix2nside(0) is -1")
        if (allocated(error)) return
        call check(error, pf_npix2nside(47_int64), -1_int64, "npix2nside(47) is -1")
        if (allocated(error)) return
        call check(error, pf_npix2nside(49_int64), -1_int64, "npix2nside(49) is -1")
        if (allocated(error)) return
        ! 12*2**k for an ODD k: divisible by 12, a power of two after dividing, and still not a
        ! valid pixel count, because nside would not be an integer.
        call check(error, pf_npix2nside(24_int64), -1_int64, "npix2nside(12*2) is -1")
        if (allocated(error)) return
        call check(error, pf_npix2nside(96_int64), -1_int64, "npix2nside(12*8) is -1")
        if (allocated(error)) return
        ! There is deliberately NO case here for a valid pixel count above a kind's ceiling: the
        ! first one does not fit the kind. At int64 the ceiling is order 29 (npix
        ! 3458764513820540928) and order 30's count is 13835058055282163712, past `huge`; at int32
        ! the ceiling is order 13 (805306368) and order 14's is 3221225472, likewise past `huge`.
        ! So `hpx_npix2nside`'s ceiling clause is unreachable through the public API, is documented
        ! as such at the source, and cannot be asserted from here.
        call check(error, pf_npix2nside(805306368_int32), 8192_int32, &
                   "the int32 form reaches its own ceiling")
        if (allocated(error)) return
        call check(error, pf_npix2nside(3458764513820540928_int64), 536870912_int64, &
                   "the int64 form reaches its own ceiling")
        if (allocated(error)) return

        call check(error, pf_nside2pixarea(3_int64), -1.0_real64, "nside2pixarea(3) is -1", &
                   thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_nside2resol(0_int64), -1.0_real64, "nside2resol(0) is -1", &
                   thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_ring2z(4_int64, 0_int64), -2.0_real64, "ring2z below ring 1 is -2", &
                   thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_ring2z(4_int64, 16_int64), -2.0_real64, &
                   "ring2z past the last ring is -2", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_pix2ring_ring(4_int64, 192_int64), -1_int64, &
                   "pix2ring_ring past the last pixel is -1")
        if (allocated(error)) return
        call check(error, pf_pix2ring_nest(4_int64, -1_int64), -1_int64, &
                   "pix2ring_nest below pixel zero is -1")
    end subroutine test_grid_arithmetic_sentinels

    !> Every pixel has the same area and they sum to the sphere; the resolution is its square root.
    subroutine test_pixarea_sums_to_sphere(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: o, nbad
        integer(int64) :: nside
        real(real64) :: total, area, resol
        character(len=160) :: detail

        nbad = 0
        detail = ""
        do o = 0, 29
            nside = ishft(1_int64, o)
            area = pf_nside2pixarea(nside)
            resol = pf_nside2resol(nside)
            total = area * real(12_int64 * nside * nside, real64)
            if (abs(total - 4.0_real64 * pi) > 1.0e-12_real64) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,es16.9)') "order ", o, " sums to ", total
            end if
            if (abs(resol * resol - area) > 1.0e-15_real64 * area) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a)') "order ", o, ": resol**2 /= pixarea"
            end if
        end do
        call check(error, nbad, 0, "the pixel area is not exactly a twelfth of a quarter sphere: " &
                   // trim(detail))
    end subroutine test_pixarea_sums_to_sphere

    ! ---- B.6: rings ----

    !> A pixel's ring, and that ring's `z`, agree with the pixel centre Tier A computes.
    subroutine test_rings_agree_with_centres(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: sides(4) = [1_int64, 2_int64, 8_int64, 32_int64]
        integer :: k, nbad
        integer(int64) :: nside, p, npix, iring, nring
        real(real64) :: v(3), z
        character(len=160) :: detail

        nbad = 0
        detail = ""
        do k = 1, size(sides)
            nside = sides(k)
            npix = 12_int64 * nside * nside
            do p = 0_int64, npix - 1_int64
                iring = pf_pix2ring_ring(nside, p)
                if (iring < 1_int64 .or. iring > 4_int64 * nside - 1_int64) then
                    nbad = nbad + 1
                    if (detail == "") write (detail, '(a,i0,a,i0,a,i0)') "nside=", nside, " p=", &
                        p, " ring out of range: ", iring
                    cycle
                end if
                call pf_pix2vec_ring(nside, p, v)
                z = pf_ring2z(nside, iring)
                if (abs(z - v(3)) > 1.0e-15_real64) then
                    nbad = nbad + 1
                    if (detail == "") write (detail, '(a,i0,a,i0,a,es12.4)') "nside=", nside, &
                        " p=", p, " ring2z differs by ", abs(z - v(3))
                end if
                ! The same pixel reached through the NEST index must name the same ring.
                call pf_ring2nest(nside, p, nring)
                if (pf_pix2ring_nest(nside, nring) /= iring) then
                    nbad = nbad + 1
                    if (detail == "") write (detail, '(a,i0,a,i0,a)') "nside=", nside, " p=", p, &
                        ": pix2ring_nest disagrees with pix2ring_ring"
                end if
            end do
        end do
        call check(error, nbad, 0, "a ring accessor disagreed with the pixel centre: " // &
                   trim(detail))
    end subroutine test_rings_agree_with_centres

    ! ---- B.7: nested resolution change ----

    !> Coarsening a pixel obtained from a direction lands on that direction's coarser pixel.
    !>
    !> **The geometric identity, and the strong assertion of the three.** It says the shift really
    !> is a change of resolution on the sphere, rather than a self-consistent bit operation: both
    !> sides are reached from a direction, and only one of them goes through `pf_ud_pix_nest`.
    subroutine test_ud_pix_nest_geometric(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: pairs(2, 7) = reshape( &
            [0, 4, 4, 0, 10, 13, 13, 10, 2, 20, 20, 2, 7, 7], [2, 7])
        integer :: c, j, nbad, oin, oout, ncase
        integer(int64) :: pin, pout, got, span
        real(real64) :: v(3), th, ph
        character(len=160) :: detail

        nbad = 0
        ncase = 0
        detail = ""
        do c = 1, size(pairs, 2)
            oin = pairs(1, c)
            oout = pairs(2, c)
            do j = 1, hv_n_pos
                th = hv_theta(j)
                ph = hv_phi(j)
                call pf_ang2vec(th, ph, v)
                call pf_ang2pix_nest(ishft(1_int64, oin), th, ph, pin)
                call pf_ang2pix_nest(ishft(1_int64, oout), th, ph, pout)
                call pf_ud_pix_nest(pin, int(oin, int64), int(oout, int64), got)
                ncase = ncase + 1
                if (oout <= oin) then
                    if (got /= pout) then
                        nbad = nbad + 1
                        if (detail == "") write (detail, '(a,i0,a,i0,a,i0,a,i0)') "coarsen ", &
                            oin, "->", oout, " expected=", pout, " got=", got
                    end if
                else
                    span = ishft(1_int64, 2 * (oout - oin))
                    if (pout < got .or. pout >= got + span) then
                        nbad = nbad + 1
                        if (detail == "") write (detail, '(a,i0,a,i0,a,i0,a,i0)') "refine ", &
                            oin, "->", oout, " first child=", got, " does not bracket ", pout
                    end if
                end if
            end do
        end do
        call check(error, ncase > 0, "no resolution-change case was exercised")
        if (allocated(error)) return
        call check(error, nbad, 0, "ud_pix_nest is not a resolution change on the sphere: " // &
                   trim(detail))
    end subroutine test_ud_pix_nest_geometric

    !> The `4**k` children of a pixel are exactly the consecutive run beginning at its first child.
    subroutine test_ud_pix_nest_partition(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: orders(4) = [0, 1, 3, 5]
        integer, parameter :: steps(4) = [1, 2, 1, 3]
        integer :: c, nbad
        integer(int64) :: p, first, child, back, span, npix, lim
        character(len=160) :: detail

        nbad = 0
        detail = ""
        do c = 1, size(orders)
            npix = 12_int64 * ishft(1_int64, 2 * orders(c))
            span = ishft(1_int64, 2 * steps(c))
            lim = min(npix, 48_int64)
            do p = 0_int64, lim - 1_int64
                call pf_ud_pix_nest(p, int(orders(c), int64), &
                                    int(orders(c) + steps(c), int64), first)
                do child = first, first + span - 1_int64
                    call pf_ud_pix_nest(child, int(orders(c) + steps(c), int64), &
                                        int(orders(c), int64), back)
                    if (back /= p) then
                        nbad = nbad + 1
                        if (detail == "") write (detail, '(a,i0,a,i0,a,i0)') "child ", child, &
                            " of pixel ", p, " coarsens to ", back
                    end if
                end do
                ! Down then up returns the FIRST child, not the original -- asserted because it is
                ! the behaviour a caller will otherwise assume away.
                call pf_ud_pix_nest(first, int(orders(c) + steps(c), int64), &
                                    int(orders(c), int64), back)
                if (back /= p) nbad = nbad + 1
            end do
        end do
        call check(error, nbad, 0, "the children of a pixel did not partition it: " // &
                   trim(detail))
    end subroutine test_ud_pix_nest_partition

    !> Out-of-domain orders and a negative pixel report -1 rather than shifting whatever they got.
    subroutine test_ud_pix_nest_sentinels(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64) :: got
        integer(int32) :: got32

        call pf_ud_pix_nest(5_int64, -1_int64, 2_int64, got)
        call check(error, got, -1_int64, "a negative order_in is -1")
        if (allocated(error)) return
        call pf_ud_pix_nest(5_int64, 2_int64, 30_int64, got)
        call check(error, got, -1_int64, "an order_out above 29 is -1")
        if (allocated(error)) return
        call pf_ud_pix_nest(-1_int64, 2_int64, 4_int64, got)
        call check(error, got, -1_int64, "a negative ipix is -1")
        if (allocated(error)) return
        call pf_ud_pix_nest(5_int64, 3_int64, 3_int64, got)
        call check(error, got, 5_int64, "equal orders are the identity")
        if (allocated(error)) return
        ! The int32 form caps at its own nside ceiling, order 13, not at 29.
        call pf_ud_pix_nest(5_int32, 2_int32, 14_int32, got32)
        call check(error, got32, -1_int32, "the int32 form stops at order 13")
        if (allocated(error)) return
        call pf_ud_pix_nest(5_int32, 2_int32, 13_int32, got32)
        call check(error, got32, ishft(5_int32, 22), "the int32 form reaches order 13")
    end subroutine test_ud_pix_nest_sentinels

    ! ---- B.12: chords ----

    !> The chord pair round trips, and agrees with `pf_angdist` on real directions.
    subroutine test_chord_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        real(real64), parameter :: angles(9) = [0.0_real64, 1.0e-12_real64, 1.0e-6_real64, &
            0.001_real64, 0.5_real64, 1.0_real64, 2.0_real64, 3.0_real64, pi]
        integer :: k, j, nbad
        real(real64) :: c2, back, v1(3), v2(3), d, chord
        character(len=160) :: detail

        nbad = 0
        detail = ""
        do k = 1, size(angles)
            c2 = pf_chord2_from_angle(angles(k))
            back = pf_angle_from_chord2(c2)
            if (abs(back - angles(k)) > 1.0e-9_real64 * max(1.0_real64, angles(k))) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,es16.9,a,es16.9)') "angle=", angles(k), &
                    " came back as ", back
            end if
        end do
        call check(error, nbad, 0, "the chord pair did not round trip: " // trim(detail))
        if (allocated(error)) return

        ! The identity that makes the squared-chord substitution valid: for unit vectors, the
        ! squared chord between them IS the squared Euclidean distance.
        nbad = 0
        detail = ""
        do j = 1, hv_n_pos - 1
            call pf_ang2vec(hv_theta(j), hv_phi(j), v1)
            call pf_ang2vec(hv_theta(j + 1), hv_phi(j + 1), v2)
            call pf_angdist(v1, v2, d)
            chord = sum((v1 - v2) ** 2)
            if (abs(pf_chord2_from_angle(d) - chord) > 1.0e-12_real64) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,es12.4)') "pair ", j, " differs by ", &
                    abs(pf_chord2_from_angle(d) - chord)
            end if
        end do
        call check(error, nbad, 0, "chord2(angdist) is not the squared Euclidean distance: " // &
                   trim(detail))
        if (allocated(error)) return

        ! The clamps: a chord2 a rounding outside [0, 4] must yield 0 or pi rather than raise.
        call check(error, pf_angle_from_chord2(-1.0e-15_real64), 0.0_real64, &
                   "a slightly negative chord2 clamps to 0", thr=0.0_real64)
        if (allocated(error)) return
        call check(error, pf_angle_from_chord2(4.0_real64 + 1.0e-12_real64), pi, &
                   "a chord2 slightly above 4 clamps to pi", thr=1.0e-15_real64)
    end subroutine test_chord_round_trip

    ! ---- B.8/B.9: the disc siblings ----

    !> `pf_query_disc_count` answers exactly the `nlist` `pf_query_disc` reports.
    subroutine test_disc_count_matches(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: c, nbad, ncase
        integer(int64) :: nside, nlist, ncount
        integer(int64) :: listpix(70000)
        real(real64) :: v(3), r
        logical :: inc
        character(len=160) :: detail

        nbad = 0
        ncase = 0
        detail = ""
        do c = 1, hv_n_disc
            nside = hv_disc_nside(c)
            ! Only the resolutions whose whole-sphere pixel count fits the buffer above, since the
            ! fixed-buffer form is one half of the comparison.
            if (12_int64 * nside * nside > int(size(listpix), int64)) cycle
            v = hv_disc_vec(3 * (c - 1) + 1:3 * (c - 1) + 3)
            r = hv_disc_radius(c)
            inc = hv_disc_inclusive(c)
            call pf_query_disc(nside, v, r, listpix, nlist, inclusive=inc)
            call pf_query_disc_count(nside, v, r, ncount, inclusive=inc)
            ncase = ncase + 1
            if (ncount /= nlist) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,i0,a,i0)') "disc ", c, " list=", nlist, &
                    " count=", ncount
            end if
            ! The scheme must not change the count: a bijection cannot change how many there are.
            call pf_query_disc_count(nside, v, r, ncount, scheme=PF_HP_NEST, inclusive=inc)
            if (ncount /= nlist) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a)') "disc ", c, ": NEST count differs"
            end if
        end do
        call check(error, ncase > 0, "no disc fixture was small enough to compare")
        if (allocated(error)) return
        call check(error, nbad, 0, "query_disc_count disagreed with query_disc: " // trim(detail))
    end subroutine test_disc_count_matches

    !> `pf_query_disc_alloc` returns the same pixels, in the same order, sized exactly.
    !> Checks `pf_query_disc_alloc` against `pf_query_disc` on two discs chosen to take its two
    !> different emitting paths.
    !>
    !> **Why two discs rather than one.** The self-sizing form counts first, and on that counting
    !> walk it RECORDS where each run of pixels landed, so that the emitting pass replays those
    !> runs instead of walking the ring geometry a second time. The record is a fixed-size buffer
    !> (`hpx_alloc_runs_max`, 1024 runs), and a disc large enough to overflow it falls back to the
    !> second walk. Both paths must produce the same answer, and nothing in the public interface
    !> distinguishes them -- so the two cases below are chosen by measurement rather than by
    !> argument, and the measurement is recorded here because it is the only evidence that this
    !> test covers two paths and not one twice:
    !>
    !>   nside 128, radius 1.2, centre on the seam ->  794 runs recorded, replay path
    !>   nside 256, radius 1.2, same centre        -> 1400+ runs, record overflows, fallback path
    !>
    !> A centre on the `phi = 0` seam is what makes the run count high for the pixel count: nearly
    !> every ring's arc wraps through the seam and so arrives as two runs rather than one. If a
    !> future change to `hpx_alloc_runs_max` moves the boundary, re-measure and update both the
    !> numbers above and the resolutions below; a version of this test where both cases fit in the
    !> record would still pass while checking half of what it claims.
    !>
    !> **Confirmed by negative control, since neither path can be observed from outside.** Breaking
    !> only the replay of recorded runs fails this test at `nside=128` and nowhere else; breaking
    !> only the fall-back second walk fails it at `nside=256` and nowhere else. That is what
    !> establishes that the two cases take different paths, rather than the run counts above, which
    !> only predict it.
    subroutine test_disc_alloc_run_record(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        real(real64), parameter :: seam(3) = [1.0_real64, 0.0_real64, 0.0_real64]
        integer(int64), parameter :: cases(2) = [128_int64, 256_int64]
        integer(int64) :: nside, nlist, nalloc, k
        integer(int64), allocatable :: buf(:), got(:)
        integer :: c, sch, nbad
        character(len=200) :: detail

        nbad = 0
        detail = ""
        do c = 1, size(cases)
            nside = cases(c)
            do sch = 0, 1
                call pf_query_disc_count(nside, seam, 1.2_real64, nlist, scheme=sch)
                allocate (buf(max(1_int64, nlist)))
                call pf_query_disc(nside, seam, 1.2_real64, buf, nlist, scheme=sch)
                call pf_query_disc_alloc(nside, seam, 1.2_real64, got, nalloc, scheme=sch)
                if (nalloc /= nlist .or. int(size(got), int64) /= nlist) then
                    nbad = nbad + 1
                    if (detail == "") write (detail, '(a,i0,a,i0,a,i0,a,i0,a,i0)') &
                        "nside=", nside, " scheme=", sch, ": alloc gave ", nalloc, &
                        " (array ", size(got), ") against ", nlist
                else
                    do k = 1_int64, nlist
                        if (got(k) /= buf(k)) then
                            nbad = nbad + 1
                            if (detail == "") write (detail, '(a,i0,a,i0,a,i0,a,i0,a,i0)') &
                                "nside=", nside, " scheme=", sch, " element ", k, ": alloc gave ", &
                                got(k), " against ", buf(k)
                            exit
                        end if
                    end do
                end if
                deallocate (buf, got)
            end do
        end do
        call check(error, nbad, 0, "query_disc_alloc disagreed with query_disc: " // trim(detail))
    end subroutine test_disc_alloc_run_record

    subroutine test_disc_alloc_matches(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer :: c, nbad, ncase
        integer(int64) :: nside, nlist, nalloc
        integer(int64) :: listpix(70000)
        integer(int64), allocatable :: got(:)
        real(real64) :: v(3), r
        logical :: inc
        character(len=160) :: detail

        nbad = 0
        ncase = 0
        detail = ""
        do c = 1, hv_n_disc
            nside = hv_disc_nside(c)
            if (12_int64 * nside * nside > int(size(listpix), int64)) cycle
            v = hv_disc_vec(3 * (c - 1) + 1:3 * (c - 1) + 3)
            r = hv_disc_radius(c)
            inc = hv_disc_inclusive(c)
            call pf_query_disc(nside, v, r, listpix, nlist, inclusive=inc)
            call pf_query_disc_alloc(nside, v, r, got, nalloc, inclusive=inc)
            ncase = ncase + 1
            if (.not. allocated(got)) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a)') "disc ", c, ": listpix unallocated"
                cycle
            end if
            if (int(size(got), int64) /= nalloc .or. nalloc /= nlist) then
                nbad = nbad + 1
                if (detail == "") write (detail, '(a,i0,a,i0,a,i0,a,i0)') "disc ", c, " size=", &
                    size(got), " nalloc=", nalloc, " nlist=", nlist
                cycle
            end if
            if (nlist > 0_int64) then
                if (any(got(1:nlist) /= listpix(1:nlist))) then
                    nbad = nbad + 1
                    if (detail == "") write (detail, '(a,i0,a)') "disc ", c, ": pixels differ"
                end if
            end if
        end do
        call check(error, ncase > 0, "no disc fixture was small enough to compare")
        if (allocated(error)) return
        call check(error, nbad, 0, "query_disc_alloc disagreed with query_disc: " // trim(detail))
    end subroutine test_disc_alloc_matches

    !> An empty disc allocates a zero-length array, never an unallocated one.
    subroutine test_disc_alloc_empty(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64) :: nlist
        integer(int64), allocatable :: got(:)
        real(real64) :: v(3), th, ph

        ! A radius of exactly zero centred BETWEEN pixel centres: no centre lies within it.
        call pf_pix2ang_ring(64_int64, 100_int64, th, ph)
        call pf_ang2vec(th + 0.004_real64, ph + 0.004_real64, v)
        call pf_query_disc_alloc(64_int64, v, 0.0_real64, got, nlist)
        call check(error, nlist, 0_int64, "a zero-radius disc off a centre holds no pixels")
        if (allocated(error)) return
        call check(error, allocated(got), "listpix is allocated even when the disc is empty")
        if (allocated(error)) return
        call check(error, size(got), 0, "listpix is zero-length when the disc is empty")
    end subroutine test_disc_alloc_empty

    !> The form that removes the buffer-too-small failure: a disc larger than a plausible buffer.
    subroutine test_disc_alloc_beyond_a_buffer(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64) :: nlist, ncount
        integer(int64), allocatable :: got(:)
        ! Named rather than written inline at the call, so that the disc centre is not passed to
        ! an explicit-shape `vec(3)` dummy as a value with no address -- see the note in
        ! test_vec2ang_degenerate above.
        real(real64), parameter :: north(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        ! A whole-sphere disc at nside 128 is 196608 pixels -- far past anything a caller would
        ! declare on the stack, and exactly the case the five hand-rolled buffer guards downstream
        ! exist to police.
        call pf_query_disc_alloc(128_int64, north, pi, got, nlist)
        call check(error, nlist, 196608_int64, "a whole-sphere disc at nside 128 holds every pixel")
        if (allocated(error)) return
        call check(error, size(got), 196608, "listpix was allocated to exactly that many")
        if (allocated(error)) return
        call check(error, got(1), 0_int64, "the first pixel of a RING whole-sphere disc is 0")
        if (allocated(error)) return
        call check(error, got(196608), 196607_int64, "the last is npix-1")
        if (allocated(error)) return
        call pf_query_disc_count(128_int64, north, pi, ncount)
        call check(error, ncount, nlist, "the count form agrees on the whole sphere")
    end subroutine test_disc_alloc_beyond_a_buffer

    ! ---- B.10: the bulk forms ----

    !> Every bulk form reproduces its scalar form exactly, at every thread count.
    !>
    !> **Exact equality is the right bar here, and that is worth saying because in this workspace
    !> the default assumption is the opposite.** An OpenMP reduction over reals is not
    !> bit-reproducible, so a test asserting `==` on one is wrong -- but there is no reduction
    !> here: every element is a function of its own inputs alone and the schedule is static, so the
    !> result cannot depend on how the loop was divided.
    subroutine test_bulk_matches_scalar(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: n = 4000
        integer, parameter :: threadings(4) = [1, 2, 4, 8]
        integer(int64), parameter :: nside = 256_int64
        integer :: k, t, nbad
        real(real64) :: theta(n), phi(n)
        real(real64) :: s_vec(3, n), s_pvec(3, n), s_th(n), s_ph(n), s_vth(n), s_vph(n)
        integer(int64) :: s_ring(n), s_nest(n), s_vring(n), s_vnest(n)
        real(real64) :: g_vec(3, n), g_th(n), g_ph(n)
        integer(int64) :: g_pix(n)
        character(len=160) :: detail

        ! A fixture spanning both caps, the belt, both poles and the seam. It is deliberately NOT
        ! perturbed away from pixel boundaries, unlike the reference vectors -- every comparison
        ! below is between two paths through the SAME arithmetic, so a position sitting exactly on
        ! a boundary is answered identically by both and is worth including rather than avoiding.
        do k = 1, n
            theta(k) = pi * real(k - 1, real64) / real(n - 1, real64)
            phi(k) = modulo(2.399963_real64 * real(k, real64), twopi)
        end do

        ! ---- The scalar answers, each from the scalar form of the very procedure under test ----
        !
        ! Note what is NOT done here: `pf_vec2pix_ring_bulk` is compared against a loop of
        ! `pf_vec2pix_ring`, never against `pf_ang2pix_ring` on the angles the vectors came from.
        ! Those two agree only where a position is not within a rounding of a pixel boundary, so
        ! asserting it would be asserting a round-trip identity this fixture deliberately breaks.
        call pf_ang2pix_ring(nside, theta, phi, s_ring)
        call pf_ang2pix_nest(nside, theta, phi, s_nest)
        call pf_pix2ang_ring(nside, s_ring, s_th, s_ph)
        do k = 1, n
            call pf_ang2vec(theta(k), phi(k), s_vec(:, k))
        end do
        do k = 1, n
            call pf_vec2pix_ring(nside, s_vec(:, k), s_vring(k))
            call pf_vec2pix_nest(nside, s_vec(:, k), s_vnest(k))
            call pf_pix2vec_ring(nside, s_ring(k), s_pvec(:, k))
            call pf_vec2ang(s_vec(:, k), s_vth(k), s_vph(k))
        end do

        nbad = 0
        detail = ""
        do t = 1, size(threadings)
            call pf_ang2pix_ring_bulk(nside, theta, phi, g_pix, threads=threadings(t))
            call note(any(g_pix /= s_ring), "ang2pix_ring_bulk", threadings(t), nbad, detail)
            call pf_ang2pix_nest_bulk(nside, theta, phi, g_pix, threads=threadings(t))
            call note(any(g_pix /= s_nest), "ang2pix_nest_bulk", threadings(t), nbad, detail)
            call pf_pix2ang_ring_bulk(nside, s_ring, g_th, g_ph, threads=threadings(t))
            call note(any(g_th /= s_th) .or. any(g_ph /= s_ph), "pix2ang_ring_bulk", &
                      threadings(t), nbad, detail)
            call pf_ang2vec_bulk(theta, phi, g_vec, threads=threadings(t))
            call note(any(g_vec /= s_vec), "ang2vec_bulk", threadings(t), nbad, detail)
            call pf_vec2pix_ring_bulk(nside, s_vec, g_pix, threads=threadings(t))
            call note(any(g_pix /= s_vring), "vec2pix_ring_bulk", threadings(t), nbad, detail)
            call pf_vec2pix_nest_bulk(nside, s_vec, g_pix, threads=threadings(t))
            call note(any(g_pix /= s_vnest), "vec2pix_nest_bulk", threadings(t), nbad, detail)
            call pf_pix2vec_ring_bulk(nside, s_ring, g_vec, threads=threadings(t))
            call note(any(g_vec /= s_pvec), "pix2vec_ring_bulk", threadings(t), nbad, detail)
            call pf_vec2ang_bulk(s_vec, g_th, g_ph, threads=threadings(t))
            call note(any(g_th /= s_vth) .or. any(g_ph /= s_vph), "vec2ang_bulk", &
                      threadings(t), nbad, detail)
        end do
        ! And once more with the thread count resolved automatically rather than requested.
        call pf_ang2pix_ring_bulk(nside, theta, phi, g_pix)
        call note(any(g_pix /= s_ring), "ang2pix_ring_bulk (auto)", 0, nbad, detail)
        call check(error, nbad, 0, "a bulk form disagreed with its scalar form: " // trim(detail))

    contains

        !> Records a disagreement, keeping the first one's name and thread count.
        subroutine note(bad, name, nt, count, first)
            logical, intent(in) :: bad !! whether this comparison disagreed.
            character(len=*), intent(in) :: name !! the bulk form compared.
            integer, intent(in) :: nt !! the thread count requested; 0 means automatic.
            integer, intent(inout) :: count !! running disagreement count.
            character(len=*), intent(inout) :: first !! the first disagreement's description.

            if (.not. bad) return
            count = count + 1
            if (first == "") write (first, '(a,a,i0)') trim(name), " at threads=", nt
        end subroutine note

    end subroutine test_bulk_matches_scalar

    !> A zero-sized array is a defined no-op; a one-element array is not a special case.
    subroutine test_bulk_degenerate_sizes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: nside = 16_int64
        real(real64) :: t0(0), p0(0), v0(3, 0), t1(1), p1(1), v1(3, 1)
        integer(int64) :: i0(0), i1(1), want

        ! Zero-sized: must return without touching anything.
        call pf_ang2pix_ring_bulk(nside, t0, p0, i0)
        call pf_vec2pix_nest_bulk(nside, v0, i0)
        call pf_pix2vec_ring_bulk(nside, i0, v0)
        call pf_ang2vec_bulk(t0, p0, v0)
        call pf_vec2ang_bulk(v0, t0, p0)
        call check(error, size(i0), 0, "a zero-sized bulk call is a defined no-op")
        if (allocated(error)) return

        t1(1) = 1.0_real64
        p1(1) = 2.0_real64
        call pf_ang2pix_ring(nside, t1(1), p1(1), want)
        call pf_ang2pix_ring_bulk(nside, t1, p1, i1)
        call check(error, i1(1), want, "a one-element bulk call agrees with the scalar form")
        if (allocated(error)) return
        call pf_ang2vec_bulk(t1, p1, v1)
        call pf_vec2pix_ring_bulk(nside, v1, i1)
        call check(error, i1(1), want, "a one-element vector round trip agrees too")
    end subroutine test_bulk_degenerate_sizes

    !> The int32 and int64 bulk specifics answer identically wherever both are in domain.
    subroutine test_bulk_kind_agreement(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: n = 500
        integer(int32), parameter :: nside32 = 512_int32
        integer :: k
        real(real64) :: theta(n), phi(n), vec(3, n)
        integer(int32) :: got32(n)
        integer(int64) :: got64(n)

        do k = 1, n
            theta(k) = pi * real(k, real64) / real(n + 1, real64)
            phi(k) = modulo(1.7_real64 * real(k, real64), twopi)
        end do
        call pf_ang2pix_ring_bulk(nside32, theta, phi, got32)
        call pf_ang2pix_ring_bulk(int(nside32, int64), theta, phi, got64)
        call check(error, all(int(got32, int64) == got64), &
                   "the two ang2pix_ring_bulk kinds disagree")
        if (allocated(error)) return
        call pf_ang2vec_bulk(theta, phi, vec)
        call pf_vec2pix_nest_bulk(nside32, vec, got32)
        call pf_vec2pix_nest_bulk(int(nside32, int64), vec, got64)
        call check(error, all(int(got32, int64) == got64), &
                   "the two vec2pix_nest_bulk kinds disagree")
    end subroutine test_bulk_kind_agreement

end module test_healpix_tier_b
