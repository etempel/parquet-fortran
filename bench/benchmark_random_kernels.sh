#!/usr/bin/env bash
# MEASUREMENT twin of tools/check_random_kernels.sh: that script ASSERTS the two route (e)
# kernels agree bit for bit and is run by CI; this one TIMES them against each other and is
# run by hand. Its driver, tools/benchmark_random_kernels.f90, deliberately stays under
# tools/ -- bench/ is an fpm source-dir and fpm must not build it (see that file's header).
#
# Time `parquet_random`'s TWO route (e) kernels against each other on one compiler: the 128-bit
# kernel (`#ifdef PF_INT128`) and the wrapping kernel (`#else`).
#
# Why this cannot go through fpm, and why the driver lives in tools/ rather than app/: forcing the
# other kernel needs `-U__GFORTRAN__`, which also flips src/parquet.f90's stringify branch, so the
# package does not compile with it. A standalone compile of the one module plus its single
# dependency is the only form that works -- the same constraint tools/check_random_kernels.sh
# works under, and this script deliberately mirrors its structure.
#
# THE CHECKSUM GATE IS THE POINT. The two kernels compute the same algorithm by different
# arithmetic, so they must agree bit for bit. gfortran is known to miscompile the wrapping kernel
# under LTO, and a timing from a miscompiled arm is a timing for something that is not this
# library. So this script REFUSES to report a comparison whose two halves disagree, rather than
# printing the numbers with a warning nobody reads.
#
# It also carries check_random_kernels.sh's vacuity guard, for the same reason that script does: if
# -U__GFORTRAN__ ever stops defeating the allowlist, this would build the SAME kernel twice and
# report a perfectly plausible 1.00x with nothing wrong on the surface. The two halves must report
# DIFFERENT kernel names or the run fails.
#
# Usage:
#   bench/benchmark_random_kernels.sh
#   FC=ifx bench/benchmark_random_kernels.sh          # already wrapping; reports the shipped arm only
#   N=50000000 ROUNDS=9 bench/benchmark_random_kernels.sh
#
# Config (env-overridable):
#   FC=gfortran     Compiler to use.
#   OPT="-O3 -funroll-loops"   Optimisation flags. LTO is deliberately NOT the default: gfortran
#                    miscompiles the wrapping kernel under it, so the checksum gate would (rightly)
#                    refuse the run.
#   N=10000000      Bulk fill length.
#   SCALAR=10000000 Scalar loop iterations.
#   ROUNDS=5        Rounds per arm; the best is kept.
#
# Bash 3.2 compatible (two of the three machines are macOS).
# ---------------------------------------------------------------------------------------------
# Times `parquet_random`'s two route (e) kernels against each
# other on one compiler -- the 128-bit kernel (`#ifdef PF_INT128`) and the wrapping kernel (`#else`)
# -- driving `tools/benchmark_random_kernels.f90`. Like `tools/check_random_kernels.sh`, and for the
# same reason, it cannot go through fpm and its driver is not under `app/`: forcing the other kernel
# needs `-U__GFORTRAN__`, which also flips `src/parquet.f90`'s stringify branch, so the package will
# not build that way at all. Only a standalone compile of `parquet_random` works -- which it does
# because that module reaches nothing but `iso_fortran_env` and two leaves of its own
# (`parquet_expkey`, `parquet_ziggurat`). `check_parquet_random_stays_leaf` is what enforces it, and
# the rule it enforces is the transitive one: every project module `parquet_random` reaches must
# itself reach nothing but compiler-supplied modules.
#
# Two gates decide whether it reports anything, and both exist because their failure mode is a
# plausible-looking number rather than an error. The vacuity guard requires the two halves to report
# different kernel names -- if `-U__GFORTRAN__` ever stops defeating the allowlist, this would build
# the same kernel twice and print a perfectly reasonable 1.00x. The checksum gate requires the two
# halves to agree bit for bit, which they must, being the same algorithm by different arithmetic;
# gfortran is known to miscompile the wrapping kernel under LTO, and a timing from a miscompiled arm
# is a timing for something that is not this library. LTO is therefore not the default `OPT`, and a
# disagreeing run is refused outright rather than printed with a warning.
#
# Measured on machine B, gfortran 15.2.1 at fpm's release flags, checksums identical throughout: the
# wrapping kernel is 1.58x faster than the shipped `int128` one on the `real64` bulk fill, 1.57x on
# `real32` and 1.93x on the scalar path (1.5-2.0x across `-O2`, `-O3` and `-O3 -funroll-loops`). The
# mechanism is visible in the object code and is not the multiply -- both arms emit the same 20
# `imul` in `random_block`, so the fork header's "the optimiser narrows it straight back to a native
# multiply" holds. What the `int128` arm adds is 20 `shrd` instructions extracting halves across a
# register pair whose upper 64 bits are provably zero, and 230 instructions against 167.
#
#     bench/benchmark_random_kernels.sh
#     OPT=-O2 bench/benchmark_random_kernels.sh
#     FC=ifx bench/benchmark_random_kernels.sh     # ships wrapping already; reports one arm and says so
# ---------------------------------------------------------------------------------------------
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

FC="${FC:-gfortran}"
OPT="${OPT:--O3 -funroll-loops}"
N="${N:-10000000}"
SCALAR="${SCALAR:-10000000}"
ROUNDS="${ROUNDS:-5}"

for arg in "$@"; do
    case "$arg" in
        -h|--help) sed -n '2,37p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) echo "benchmark_random_kernels.sh: unknown argument '$arg' (try --help)" >&2; exit 2 ;;
    esac
done

# A run that dies partway through must never look like a clean one. `set -e` is not usable here
# (build failures are inspected deliberately), so every deliberate exit happens after finished=1.
finished=0
trap '[ "$finished" = "1" ] || { echo "benchmark_random_kernels: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

# src/parquet_random.f90 imports nothing but iso_fortran_env and the leaf settings module (and
# omp_lib under _OPENMP), which is exactly what makes this standalone compile possible at all.
SRC="src/parquet_expkey.f90 src/parquet_ziggurat.f90 src/parquet_random.f90 tools/benchmark_random_kernels.f90"
ABS_SRC=""
for f in $SRC; do ABS_SRC="$ABS_SRC $ROOT_DIR/$f"; done

# Keep this in step with the `#if defined(...)` line at the top of src/parquet_random.f90.
case "$FC" in
    *flang*)        UNDEF="-U__flang__ -U__FLANG"; CPP="-cpp" ;;
    *ifx*|*ifort*)  UNDEF=""; CPP="-fpp" ;;    # already takes the wrapping arm; nothing to defeat
    *)              UNDEF="-U__GFORTRAN__"; CPP="-cpp" ;;
esac

WORK="$(mktemp -d)"
cleanup() { rm -rf "$WORK"; }

# Compile FROM a scratch directory with absolute sources. Compiling from the repo root would drop
# .mod files there, where they shadow the real modules and break every later fpm build with
# "Unexpected EOF" -- a failure that looks nothing like its cause.
build_and_run () {
    label="$1"; shift
    d="$WORK/$label"
    mkdir -p "$d"
    if ! out=$(cd "$d" && $FC $CPP $OPT "$@" $ABS_SRC -o drv 2>&1); then
        echo "  $label: BUILD-FAIL" >&2
        echo "$out" | head -5 | sed 's/^/        /' >&2
        return 1
    fi
    (cd "$d" && ./drv --n="$N" --scalar="$SCALAR" --rounds="$ROUNDS" 2>&1)
}

field () { echo "$1" | tr ' ' '\n' | grep "^$2=" | cut -d= -f2; }

echo "=============================================================================="
echo "benchmark_random_kernels.sh -- route (e) kernel A/B"
echo "  compiler : $FC  ($($FC --version 2>&1 | head -1))"
echo "  opt      : $OPT"
echo "  uname -m : $(uname -m)"
echo "  n=$N scalar=$SCALAR rounds=$ROUNDS"
echo "=============================================================================="
echo

shipped="$(build_and_run shipped)" || { cleanup; echo "shipped build failed" >&2; exit 1; }
echo "  shipped : $shipped"

if [ -z "$UNDEF" ]; then
    echo
    echo "  This compiler has no 128-bit integer kind, so it SHIPS the wrapping kernel and there is"
    echo "  no second arm to force. Nothing to compare on $FC; run this on gfortran or flang."
    cleanup
    finished=1
    exit 0
fi

forced="$(build_and_run forced $UNDEF)" || { cleanup; echo "forced build failed" >&2; exit 1; }
echo "  forced  : $forced"
echo

k_ship="$(field "$shipped" KERNEL)";  k_forc="$(field "$forced" KERNEL)"
c_ship="$(field "$shipped" CHECK)";   c_forc="$(field "$forced" CHECK)"

# Vacuity guard: two builds of the same kernel would report a plausible 1.00x with nothing visibly
# wrong. This is the check that makes the whole comparison mean anything.
if [ "$k_ship" = "$k_forc" ]; then
    echo "FAIL: both builds report KERNEL=$k_ship -- the forcing flag did not take effect." >&2
    echo "      This run proves nothing. Check the #if line in src/parquet_random.f90." >&2
    cleanup; finished=1; exit 1
fi

# Correctness gate: a timing from an arm that computes different values is not a timing for this
# library. gfortran miscompiles the wrapping kernel under LTO, which is precisely this case.
if [ "$c_ship" != "$c_forc" ]; then
    echo "FAIL: the two kernels DISAGREE (CHECK=$c_ship vs $c_forc)." >&2
    echo "      They compute the same algorithm and must match bit for bit, so one of them is" >&2
    echo "      miscompiled at '$OPT'. Refusing to report timings." >&2
    cleanup; finished=1; exit 1
fi

echo "  checksum agrees across kernels ($c_ship) -- comparison is valid."
echo

printf "  %-12s %12s %12s %12s\n" "kernel" "bulk r64" "bulk r32" "scalar r64"
printf "  %-12s %12s %12s %12s\n" "------------" "------------" "------------" "------------"
printf "  %-12s %12s %12s %12s\n" "$k_ship" \
    "$(field "$shipped" NS_R64)" "$(field "$shipped" NS_R32)" "$(field "$shipped" NS_SCALAR)"
printf "  %-12s %12s %12s %12s\n" "$k_forc" \
    "$(field "$forced" NS_R64)" "$(field "$forced" NS_R32)" "$(field "$forced" NS_SCALAR)"
echo
echo "  (nanoseconds per value, best of $ROUNDS rounds)"

awk -v a="$(field "$shipped" NS_R64)" -v b="$(field "$forced" NS_R64)" \
    -v c="$(field "$shipped" NS_R32)" -v d="$(field "$forced" NS_R32)" \
    -v e="$(field "$shipped" NS_SCALAR)" -v f="$(field "$forced" NS_SCALAR)" \
    -v ks="$k_ship" -v kf="$k_forc" \
    'BEGIN { printf "  ratio %s/%s : r64 %.2fx  r32 %.2fx  scalar %.2fx\n", kf, ks, b/a, d/c, f/e }'

cleanup
finished=1
