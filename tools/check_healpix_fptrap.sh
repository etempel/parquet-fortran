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
#   * it links the whole library. This compiles four Fortran files with a bare compiler and no
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
    integer(int64), allocatable :: listpix(:)
    integer(int64) :: nside, ipix, nlist, sink, p
    integer(int32) :: ipix32
    real(real64) :: theta, phi, vec(3), other(3), dist
    integer :: k, c

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
            if (nside <= 8192_int64) then
                call pf_ang2pix_ring(int(nside, int32), theta, phi, ipix32)
                sink = sink + int(ipix32, int64)
            end if
        end do
    end do

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
    end do

    if (sink == -1_int64) print *, sink
    print '(a)', "healpix_fptrap: no floating-point exception was raised"
end program healpix_fptrap
EOF

SRC="src/parquet_settings_base.f90 src/parquet_healpix.f90 src/parquet_healpix_core.f90 src/parquet_healpix_query.f90"

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
