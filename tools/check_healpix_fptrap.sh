#!/usr/bin/env bash
#
# Proves `parquet_healpix` raises no floating-point exception, in a build where raising one is
# FATAL rather than merely recorded.
#
# WHY THIS EXISTS SEPARATELY FROM THE TEST SUITE. `test/test_healpix.f90`'s
# test_no_ieee_exceptions sweeps the same entry points and reads the IEEE flags afterwards, which
# is a real check and runs everywhere. It is not the same check as this one:
#
#   * it observes a flag that was RAISED AND IGNORED, where a caller running under
#     `-ffpe-trap=zero,invalid` would already be dead. A build that halts is the situation the
#     module's promise is actually about, and only a trapping build reproduces it.
#   * it runs inside the whole test binary, where anything else in the process may have raised a
#     flag first. This runs one program that does nothing else.
#   * it links the whole library. This compiles seven Fortran files with a bare compiler and no
#     Arrow anywhere, so it also demonstrates -- rather than asserting -- that the tier really is
#     free of the reader/writer stack. That is the same thing tools/check_argsort_standalone.sh
#     proves one tier over, and for the same reason: nothing in `fpm test` can see it, because the
#     library obviously has Arrow.
#
# The promise this checks is the module's whole reason for existing. `libhealpix`'s disc query
# raises inside the library, so every downstream caller building with the traps enabled has to
# suspend the halting modes around each call -- a guard measured at 4.05 microseconds under ifx,
# which is more than the query it guards. An implementation that raised would keep that guard
# alive and lose most of the benefit, and nothing else would notice.
#
# Usage:  tools/check_healpix_fptrap.sh
#
#   FC=<compiler>   Fortran compiler to use (default: gfortran).
#
# Only gfortran and compilers accepting its `-ffpe-trap=` spelling are exercised; the script says
# so and skips rather than reporting a pass it did not earn.

set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
FC="${FC:-gfortran}"

# A completion flag plus an EXIT trap, so that a failure part-way through can never be read as a
# pass: this script inspects exit statuses deliberately, so `set -e` is unavailable.
finished=0
trap '[ "$finished" = "1" ] || { echo "check_healpix_fptrap.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

work="$(mktemp -d "${TMPDIR:-/tmp}/pf-hpxtrap.XXXXXX")"
cleanup() { rm -rf "$work"; }

if ! command -v "$FC" > /dev/null 2>&1; then
    echo "check_healpix_fptrap.sh: SKIPPED -- '$FC' not found" >&2
    finished=1
    cleanup
    exit 0
fi

# Probe with the flags that will actually be used, never by asking whether the name exists: a
# compiler present but rejecting -ffpe-trap would otherwise be reported as a pass.
cat > "$work/probe.f90" <<'EOF'
program probe
    print *, "ok"
end program probe
EOF
if ! "$FC" -ffpe-trap=zero,invalid,overflow -o "$work/probe" "$work/probe.f90" > "$work/probe.log" 2>&1; then
    echo "check_healpix_fptrap.sh: SKIPPED -- $FC does not accept -ffpe-trap=zero,invalid,overflow" >&2
    finished=1
    cleanup
    exit 0
fi

cat > "$work/driver.f90" <<'EOF'
!> Sweeps every parquet_healpix entry point over the inputs whose arithmetic is most exposed.
!>
!> Built with the floating-point traps ENABLED, so raising an exception aborts the process rather
!> than setting a flag. The program's own arithmetic is deliberately trivial; anything that halts
!> halts inside the library.
program healpix_fptrap
    use parquet_healpix
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none

    real(real64), parameter :: pi = 3.141592653589793238462643_real64
    real(real64), parameter :: two_thirds = 2.0_real64 / 3.0_real64
    integer(int64), allocatable :: listpix(:), alloclist(:)
    integer(int64) :: nside, ipix, nlist, sink, p
    real(real64) :: bt(2048), bp(2048), bv(3, 2048)
    integer(int64) :: bi(2048)
    integer(int32) :: ipix32
    real(real64) :: theta, phi, vec(3), other(3), dist
    integer :: k, c, g
    type(pf_healpix_grid) :: grid, unbuilt
    integer(int64) :: gi
    real(real64) :: gtheta, gphi, gra, gdec

    allocate (listpix(12_int64 * 64_int64 * 64_int64))
    sink = 0_int64

    ! Every resolution the module accepts, at the latitudes where its branches meet: the poles,
    ! the cap/belt boundary, the equator, and the seam in longitude.
    do k = 0, 29
        nside = ishft(1_int64, k)
        do c = 1, 10
            select case (c)
            case (1); theta = 0.0_real64;                  phi = 0.0_real64
            case (2); theta = pi;                          phi = 0.0_real64
            case (3); theta = 1.0e-15_real64;              phi = 1.0e-15_real64
            case (4); theta = pi - 1.0e-15_real64;         phi = 2.0_real64 * pi
            case (5); theta = acos(two_thirds);            phi = 0.0_real64
            case (6); theta = acos(two_thirds) + 1.0e-15_real64; phi = 0.5_real64 * pi
            case (7); theta = acos(-two_thirds);           phi = -3.0_real64
            case (8); theta = 0.5_real64 * pi;             phi = 400.0_real64
            case (9); theta = 0.5_real64 * pi;             phi = 2.0_real64 * pi - 1.0e-15_real64
            case default; theta = 1.234_real64;            phi = 5.678_real64
            end select
            call pf_ang2pix_ring(nside, theta, phi, ipix)
            sink = sink + ipix
            call pf_ang2pix_nest(nside, theta, phi, ipix)
            sink = sink + ipix
            call pf_pix2ang_ring(nside, ipix, theta, phi)
            call pf_pix2ang_nest(nside, ipix, theta, phi)
            call pf_pix2vec_ring(nside, ipix, vec)
            call pf_pix2vec_nest(nside, ipix, other)
            call pf_ring2nest(nside, ipix, p)
            sink = sink + p
            call pf_nest2ring(nside, ipix, p)
            sink = sink + p
            call pf_angdist(vec, other, dist)
            call pf_angdist(vec, vec, dist)
            call pf_angdist(vec, -vec, dist)
            ! The RA/Dec separation, over the shapes that would raise if anything did: the seam,
            ! the poles, coincident and antipodal positions, and a right ascension outside [0,360).
            dist = pf_angdist_deg(359.999_real64, 0.0_real64, 0.001_real64, 0.0_real64)
            dist = dist + pf_angdist_deg(0.0_real64, 90.0_real64, 123.5_real64, -90.0_real64)
            dist = dist + pf_angdist_deg(45.0_real64, 45.0_real64, 45.0_real64, 45.0_real64)
            dist = dist + pf_angdist_deg(0.0_real64, 0.0_real64, 180.0_real64, 0.0_real64)
            dist = dist + pf_angdist_deg(-725.5_real64, 89.9_real64, 1085.25_real64, -89.9_real64)
            if (nside <= 8192_int64) then
                call pf_ang2pix_ring(int(nside, int32), theta, phi, ipix32)
                sink = sink + int(ipix32, int64)
            end if
        end do
    end do

    ! ---- Tier B ----
    !
    ! The grid arithmetic, the vector conversions and the chord pair, over the inputs that would
    ! divide by zero, take a square root of a negative, invert a cosine outside its domain, or
    ! reach ATAN2(0, 0) -- prohibited by the standard, and fatal under nagfor.
    do k = 0, 30
        nside = ishft(1_int64, k)
        sink = sink + max(-1_int64, pf_nside2npix(nside))
        sink = sink + max(-1_int64, pf_nside2order(nside))
        sink = sink + max(-1_int64, pf_order2nside(int(k, int64)))
        sink = sink + max(-1_int64, pf_npix2nside(12_int64 * nside * nside))
        dist = pf_nside2pixarea(nside)
        dist = pf_nside2resol(nside)
        dist = pf_ring2z(nside, 1_int64)
        dist = pf_ring2z(nside, 4_int64 * nside - 1_int64)
        dist = pf_ring2z(nside, 0_int64)
        sink = sink + max(-1_int64, pf_pix2ring_ring(nside, 0_int64))
        sink = sink + max(-1_int64, pf_pix2ring_nest(nside, 0_int64))
        call pf_ud_pix_nest(5_int64, int(k, int64), 0_int64, p)
        sink = sink + p
        call pf_ud_pix_nest(5_int64, 0_int64, int(k, int64), p)
        sink = sink + p
    end do
    ! The sentinel inputs, which take every early-return branch.
    dist = pf_nside2pixarea(0_int64)
    dist = pf_nside2resol(-1_int64)
    dist = pf_ring2z(3_int64, 1_int64)
    sink = sink + max(-1_int64, pf_npix2nside(0_int64))
    sink = sink + max(-1_int64, pf_npix2nside(47_int64))

    ! Vector conversions, including both poles and the zero vector -- the three inputs for which
    ! ATAN2 alone has no answer -- and components spanning 600 orders of magnitude.
    do c = 1, 8
        select case (c)
        case (1); vec = [0.0_real64, 0.0_real64, 1.0_real64]
        case (2); vec = [0.0_real64, 0.0_real64, -1.0_real64]
        case (3); vec = [0.0_real64, 0.0_real64, 0.0_real64]
        case (4); vec = [1.0_real64, 0.0_real64, 0.0_real64]
        case (5); vec = [1.0e-300_real64, 0.0_real64, 1.0e-300_real64]
        case (6); vec = [1.0e300_real64, 0.0_real64, 1.0e300_real64]
        case (7); vec = [1.0e-8_real64, 0.0_real64, 1.0_real64]
        case default; vec = [-0.3_real64, 0.7_real64, -0.5_real64]
        end select
        call pf_vec2ang(vec, theta, phi)
        call pf_ang2vec(theta, phi, other)
        call pf_vec2pix_ring(1024_int64, vec, ipix)
        sink = sink + ipix
        call pf_vec2pix_nest(1024_int64, vec, ipix)
        sink = sink + ipix
    end do

    ! The chord pair, at both ends of its range and a rounding outside each.
    do c = 1, 7
        select case (c)
        case (1); dist = 0.0_real64
        case (2); dist = pi
        case (3); dist = 1.0e-300_real64
        case (4); dist = -1.0e-15_real64
        case (5); dist = 4.0_real64
        case (6); dist = 4.0_real64 + 1.0e-9_real64
        case default; dist = 1.234_real64
        end select
        theta = pf_chord2_from_angle(dist)
        phi = pf_angle_from_chord2(dist)
        theta = pf_angle_from_chord2(pf_chord2_from_angle(dist))
    end do

    ! The bulk forms, over a fixture reaching both poles and the seam, threaded and serial.
    do c = 1, size(bt)
        bt(c) = pi * real(c - 1, real64) / real(size(bt) - 1, real64)
        bp(c) = modulo(2.399963_real64 * real(c, real64), 2.0_real64 * pi)
    end do
    call pf_ang2vec_bulk(bt, bp, bv)
    call pf_vec2ang_bulk(bv, bt, bp)
    call pf_ang2pix_ring_bulk(256_int64, bt, bp, bi)
    sink = sink + bi(1)
    call pf_ang2pix_nest_bulk(256_int64, bt, bp, bi, threads=2)
    sink = sink + bi(1)
    call pf_pix2ang_ring_bulk(256_int64, bi, bt, bp)
    call pf_pix2ang_nest_bulk(256_int64, bi, bt, bp, threads=2)
    call pf_vec2pix_ring_bulk(256_int64, bv, bi)
    sink = sink + bi(1)
    call pf_vec2pix_nest_bulk(256_int64, bv, bi, threads=2)
    sink = sink + bi(1)
    call pf_pix2vec_ring_bulk(256_int64, bi, bv)
    call pf_pix2vec_nest_bulk(256_int64, bi, bv, threads=2)

    ! Disc queries: every shape the walk has a branch for, in both schemes and both modes.
    do c = 1, 9
        select case (c)
        case (1); vec = [0.0_real64, 0.0_real64, 1.0_real64];  dist = 0.4_real64
        case (2); vec = [0.0_real64, 0.0_real64, -1.0_real64]; dist = 0.5_real64 * pi
        case (3); vec = [1.0_real64, 0.0_real64, 0.0_real64];  dist = 0.3_real64
        case (4); vec = [sqrt(1.0_real64 - two_thirds**2), 0.0_real64, two_thirds]
                  dist = 0.2_real64
        case (5); vec = [0.3_real64, 0.4_real64, 0.8660254037844386_real64]; dist = pi
        case (6); vec = [0.3_real64, 0.4_real64, 0.8660254037844386_real64]; dist = 0.0_real64
        case (7); vec = [1.0e-300_real64, 0.0_real64, 1.0e-300_real64];      dist = 0.25_real64
        case (8); vec = [1.0e300_real64, 0.0_real64, 1.0e300_real64];        dist = 0.25_real64
        case default
            vec = [cos(1.0e-14_real64), sin(1.0e-14_real64), 0.0_real64]
            dist = 1.9_real64
        end select
        call pf_query_disc(64_int64, vec, dist, listpix, nlist, scheme=PF_HP_RING)
        sink = sink + nlist
        call pf_query_disc(64_int64, vec, dist, listpix, nlist, scheme=PF_HP_NEST, inclusive=.true.)
        sink = sink + nlist
        call pf_query_disc(1_int64, vec, dist, listpix, nlist, scheme=PF_HP_NEST)
        sink = sink + nlist
        ! The two Tier B disc forms take the same walk; the alloc form takes it twice.
        call pf_query_disc_count(64_int64, vec, dist, nlist, inclusive=.true.)
        sink = sink + nlist
        call pf_query_disc_alloc(64_int64, vec, dist, alloclist, nlist, scheme=PF_HP_NEST)
        sink = sink + nlist
    end do

    ! ---- pf_healpix_grid: the object, and the RA/Dec layer that lives on it ----
    !
    ! The bindings delegate, so most of this re-walks ground already covered -- but the RA/Dec
    ! conversion is new arithmetic (a multiply and a subtract per angle) and the unbuilt-grid
    ! branches answer WITHOUT delegating, precisely so they cannot divide by nside = 0. Both
    ! halves have to be seen by a trapping build to be worth the promise.
    do c = 1, 2
        if (c == 1) then
            call grid%init(64_int64, PF_HP_RING, frame=PF_HP_DEC_NORTH)
        else
            call grid%init(64_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
        end if
        do g = 1, size(bt)
            call grid%ang2pix(bt(g), bp(g), gi)
            sink = sink + gi
            call grid%pix2ang(gi, gtheta, gphi)
            call grid%radec2pix(real(g, real64) * 7.0_real64, 89.0_real64 - real(g, real64), gi)
            sink = sink + gi
            call grid%pix2radec(gi, gra, gdec)
            call grid%radec2vec(gra, gdec, vec)
            call grid%vec2radec(vec, gra, gdec)
            call grid%vec2pix(vec, gi)
            sink = sink + gi
            call grid%pix2vec(gi, vec)
        end do
        call grid%radec2pix_bulk(bt, bp, bi, threads=2)
        sink = sink + bi(1)
        call grid%pix2radec_bulk(bi, bt, bp, threads=2)
        call grid%ang2pix_bulk(bt, bp, bi)
        sink = sink + bi(1)
        call grid%query_disc([0.3_real64, 0.4_real64, 0.8660254037844386_real64], 0.2_real64, &
                             listpix, nlist)
        sink = sink + nlist
        call grid%query_disc_radec(30.0_real64, -60.0_real64, 5.0_real64, listpix, nlist, &
                                   inclusive=.true.)
        sink = sink + nlist
        call grid%query_disc_radec_count(30.0_real64, -60.0_real64, 5.0_real64, nlist)
        sink = sink + nlist
        call grid%query_disc_radec_alloc(30.0_real64, -60.0_real64, 5.0_real64, alloclist, nlist)
        sink = sink + nlist
        sink = sink + int(nint(grid%pixarea() + grid%resol() + grid%max_pixrad()), int64)
    end do
    ! The unbuilt grid: every total binding answers from its own branch rather than by delegating,
    ! and none of them may divide by a zero nside on the way.
    do g = 1, 4
        call unbuilt%ang2pix(bt(g), bp(g), gi)
        sink = sink + gi
        call unbuilt%pix2ang(int(g, int64), gtheta, gphi)
        call unbuilt%pix2vec(int(g, int64), vec)
        call unbuilt%radec2pix(10.0_real64, 10.0_real64, gi)
        sink = sink + gi
        call unbuilt%pix2radec(int(g, int64), gra, gdec)
        sink = sink + int(nint(unbuilt%pixarea() + unbuilt%resol() + unbuilt%max_pixrad()), int64)
    end do

    if (sink == -1_int64) print *, sink
    print '(a)', "healpix_fptrap: no floating-point exception was raised"
end program healpix_fptrap
EOF

SRC="src/parquet_settings_base.f90 src/parquet_healpix.f90 src/parquet_healpix_core.f90 \
     src/parquet_healpix_arith.f90 src/parquet_healpix_query.f90 src/parquet_healpix_bulk.f90 \
     src/parquet_healpix_grid.f90"

status=0
for opt in -O0 -O2; do
    build="$work/build$opt"
    mkdir -p "$build"
    abs_src=""
    for f in $SRC; do
        abs_src="$abs_src $repo/$f"
    done
    # Compiled from the work directory with absolute source paths, so no .mod is left in the
    # repository root -- a stray one there shadows a later standalone build and the resulting
    # error blames the library.
    if ! ( cd "$build" && $FC $opt -ffpe-trap=zero,invalid,overflow -fopenmp -J. \
            -o driver $abs_src "$work/driver.f90" ) > "$build/compile.log" 2>&1; then
        echo "check_healpix_fptrap.sh: FAILED to build at $opt" >&2
        sed -n '1,40p' "$build/compile.log" >&2
        status=1
        continue
    fi
    out="$("$build/driver" 2>&1)"
    rc=$?
    if [ $rc -ne 0 ]; then
        echo "check_healpix_fptrap.sh: FAILED at $opt -- the driver died with status $rc" >&2
        echo "  A floating-point exception was raised inside parquet_healpix. That breaks the" >&2
        echo "  module's central promise: a caller building with -ffpe-trap must not need a" >&2
        echo "  guard around these calls." >&2
        echo "$out" | sed -n '1,20p' >&2
        status=1
        continue
    fi
    case "$out" in
        *"no floating-point exception was raised"*)
            echo "[ok] $opt: parquet_healpix raised nothing under -ffpe-trap=zero,invalid,overflow" ;;
        *)
            # The driver exiting 0 without its own line would mean it never reached the end.
            echo "check_healpix_fptrap.sh: FAILED at $opt -- the driver exited 0 but printed nothing" >&2
            echo "$out" | sed -n '1,20p' >&2
            status=1 ;;
    esac
done

cleanup
finished=1
if [ $status -ne 0 ]; then
    exit 1
fi
echo "check_healpix_fptrap.sh: parquet_healpix is trap-clean ($FC)"
exit 0
