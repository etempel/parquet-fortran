#!/usr/bin/env bash
# Where a READ-TIME sort spends its time, phase by phase -- the measurement step of
# feature_sort.md's P13.
#
# Drives bench/benchmark_sort_readtime.f90, which opens a reader with `sort_by=` and reads the C++
# phase counters (parquet_debug_get_sort_*_nanos) around each stage of the install. It exists
# because P13's phase shares were DERIVED -- measured before R1 routed the permutation build to the
# Fortran engine, then rescaled by the measured engine ratio -- and a derived share is not a
# measurement.
#
#   bench/benchmark_sort_readtime.sh
#   SIZES=1000000,20000000 bench/benchmark_sort_readtime.sh
#   KEYS=2 bench/benchmark_sort_readtime.sh
#   READS=2 bench/benchmark_sort_readtime.sh          # the workflow arm; see below
#
# ---------------------------------------------------------------------------------------------
# SET THE ENVIRONMENT UP YOURSELF, BEFORE RUNNING THIS
# ---------------------------------------------------------------------------------------------
#
# This script sources nothing and chooses no compiler -- it measures whatever the shell it is
# invoked from is set up to build. On a machine carrying more than one toolchain, activate one, run
# this, then activate the other in a FRESH shell and run it again; the build trees are named after
# the compiler so the two do not collide, which is the trap CLAUDE.md records under "that build-tree
# name must vary by COMPILER as well as by configuration".
#
# It reports the toolchain it found and refuses a gfortran below 13, which is the one environment
# mistake that fails SILENTLY (gfortran <= 11 miscompiles this library).
#
# ---------------------------------------------------------------------------------------------
# WHAT THIS RUN CAN AND CANNOT SETTLE
# ---------------------------------------------------------------------------------------------
#
#   * `take` -- arrow::compute::Take over the reader's cached columns -- is P13's subject, and its
#     share is what this measures. Read it against the `column(s) taken` count in the heading, and
#     note what BOUNDS it: a sort may only be installed before any column is read
#     (parquet_reader_set_sort aborts otherwise), so the cache at that moment holds the sort's own
#     key columns and nothing else. The loop therefore scales with the number of KEYS, never with
#     how many columns the caller goes on to read. KEYS=2 is the arm that shows it.
#   * `info` and `bind` are the SAME reduction run twice -- parquet_reader_sort_key_info binds the
#     key to report its size, then parquet_reader_sort_key_fetch binds it again to copy it out.
#     Arrow's decode is shared (get_single_chunk_array caches), so `info` carries the decode and
#     `bind` is the repeat alone. That split is a finding of this instrumentation, not a design.
#   * `engine+open` is DERIVED, not measured: it is the wall clock minus the five C++ phases, so it
#     holds pf_argsort, the reader open itself, and any allocation between them. Do not quote it as
#     a sort-engine figure -- bench/benchmark_sort_engine.sh is what measures that.
#   * READS=N adds the WORKFLOW arm, and it is the one that answers P13's design question rather
#     than merely describing the open. A share of the OPEN is not what deferring the key's Take
#     could recover: a program opens once and then reads, and the reordering of a key column is
#     wasted only when the caller never reads that key. The arm times open-plus-N-payload-reads in
#     both shapes -- key never read, key read afterwards -- and prints `take` as a share of each.
#     The first row is the CEILING on any deferral design; the second is zero by construction and
#     is measured anyway, because it shows the key comes back from the cache rather than being
#     decoded a second time. The payload columns it reads are never sort keys.
#
# Config (environment; every one has a default so a bare run is meaningful):
#   SIZES=1000000,20000000   Row counts, comma-separated. The larger entry dominates runtime and
#                             disk: the fixture is ~56 bytes/row.
#   REPS=3                   Timed opens per figure; the best TOTAL is kept, with that run's own
#                             phases (a per-phase minimum would sum to a total nothing measured).
#   KEY=both                 int64 / string / both. The string key is the interesting one: Take on
#                             a variable-length column rebuilds offsets and copies the payload,
#                             where an int64 column is a fixed-stride gather.
#   KEYS=1                   Sort keys per run: 1, or 2 to add a second key. The Take loop runs
#                             once per KEY column and can never cover more -- a sort must be
#                             installed before any column is read, so nothing else is cached.
#   READS=0                  Payload columns read after the open, enabling the workflow arm.
#                             0 disables it and the output is the phase table alone. Capped at the
#                             number of payload columns the sort does not name (4, or 3 at KEYS=2).
#   KEEP=0                   1 leaves the fixtures under test_run/ for a re-run.
# ---------------------------------------------------------------------------------------------
# answers a different question again: where a read-time sort
# spends its time. It drives `bench/benchmark_sort_readtime.f90`, which opens a reader with `sort_by=`
# and reads the C++ phase counters around each stage of the install -- the key bind, the repeat
# bind, the copy-out to Fortran, the `arrow::Int64Array` build and the `arrow::compute::Take` loop
# -- so the shares are measured rather than inferred. It exists because those shares had previously
# been derived from a measurement taken before the sort was routed to the Fortran engine, and a
# derived share is not a measurement; see `feature_sort.md`'s P13 for what it found.
#
# Two things about reading its output. `engine+open` is DERIVED, being the wall clock minus the five
# C++ phases, so it holds `pf_argsort`, the reader open itself and the allocation between them --
# `benchmark_sort_engine.sh` is what measures the engine, and this row must not be quoted as an
# engine figure. And read `take` against the `column(s) taken` count in each heading: the Take loop
# runs once per column in the reader's cache, and a sort may only be installed before any column is
# read, so that cache holds the sort's own key columns and nothing else. `KEYS=2` is the arm that
# shows the scaling; there is deliberately no prefetch arm, because prefetch-then-sort is not a
# reachable shape.
#
# | variable | default | meaning |
# |---|---|---|
# | `SIZES` | `1000000,20000000` | row counts, comma-separated. The fixture is ~56 bytes/row on disk. |
# | `REPS` | `3` | timed opens per figure. The best total is kept, with that run's own phases -- a per-phase minimum would sum to a total nothing measured. |
# | `KEY` | `both` | `int64` / `string` / `both`. The string key is the interesting one: `Take` on a variable-length column rebuilds offsets and copies the payload. |
# | `KEYS` | `1` | sort keys per run; `2` adds a second key and doubles the Take loop. |
# | `READS` | `0` | payload columns read after the open, which switches on the workflow arm below. `0` prints the phase table alone. |
# | `KEEP` | `0` | `1` leaves the fixtures under `test_run/` for a re-run. |
# `READS=N` is the arm that sizes the `Take`, and the phase table alone cannot. A share of the open
# is not what deferring the key's `Take` would recover: a program opens once and then reads, and
# reordering a key column is wasted work only when the caller never reads that key. `READS=N` times
# open-plus-`N`-payload-reads in both shapes -- key never read, and key read afterwards -- and
# prints `take` as a share of each total. The first row is the ceiling on any deferral design; the
# second is zero by construction and is measured anyway, because it is what shows the key comes back
# out of the cache rather than being decoded a second time. The payload columns it reads are never
# sort keys, so the "key not read" shape really does not read one, and the arm asserts the key came
# back sorted before printing anything.
#
# Read the ceiling against both `READS` and `SIZES`: it falls as the caller reads more columns (the
# workflow grows while the `Take` does not) and rises with `n` (the gather falls out of cache). On
# one machine it spans 6.4% to 49.1% across that grid, so a single figure from it means nothing
# without both coordinates attached.
#
#     bench/benchmark_sort_readtime.sh
#     SIZES=20000000 KEY=string KEYS=2 bench/benchmark_sort_readtime.sh
#     READS=1 SIZES=1000000,20000000 bench/benchmark_sort_readtime.sh   # the workflow arm
# ---------------------------------------------------------------------------------------------
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

SIZES="${SIZES:-1000000,20000000}"
REPS="${REPS:-3}"
KEY="${KEY:-both}"
KEYS="${KEYS:-1}"
READS="${READS:-0}"
KEEP="${KEEP:-0}"

die() { echo "benchmark_sort_readtime.sh: $*" >&2; exit 1; }

FC="${FPM_FC:-gfortran}"
FC_BASE="$(basename "$FC")"
command -v "$FC" >/dev/null 2>&1 || die "the Fortran compiler '$FC' is not on PATH."

if [[ "$FC_BASE" == gfortran* ]]; then
    GVER="$("$FC" -dumpfullversion -dumpversion 2>/dev/null | head -n 1)"
    GMAJ="${GVER%%.*}"
    [[ -n "$GMAJ" ]] || die "could not read a version from '$FC'."
    (( GMAJ >= 13 )) || die "gfortran $GVER is below this project's minimum of 13 and MISCOMPILES it.
  A bare 'gfortran' is often an old system one; activate the toolchain you meant and re-run."
fi

compiler_defaults_to_optimised() {
    case "$FC_BASE" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}

echo "=============================================================="
echo "benchmark_sort_readtime.sh"
echo "=============================================================="
echo "  fortran     : $FC   ($("$FC" --version 2>&1 | head -n 1))"
echo "  arch        : $(uname -m)   $(uname -s)"
echo "  sizes       : $SIZES     reps: $REPS     key: $KEY     keys: $KEYS     reads: $READS"
echo "  load        : $(uptime | sed 's/.*load/load/')"

# A release profile that produced no optimisation flag is an -O0 run reported as a release one --
# measured 3.7x wrong on a headline figure when it last happened. Assert rather than assume.
FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]]; then
    echo "benchmark_sort_readtime.sh: could not read fortran_compile_flags from --show-model;" >&2
    echo "  refusing to produce numbers. SKIP_OPT_CHECK=1 overrides, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    echo "benchmark_sort_readtime.sh: '--profile release' produced NO optimisation flag:" >&2
    echo "  $FLAGS_LINE" >&2
    echo "  Append one yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):" >&2
    echo "      FPM_FFLAGS=\"\${FPM_FFLAGS:-} -O3\" bench/benchmark_sort_readtime.sh" >&2
    echo "  and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi

# THE SAME CHECK FOR THE C++ HALF, and it is not redundant: fpm derives the C/C++ compiler from the
# FORTRAN one's family, and for a family it does not recognise as a C family it emits NO profile
# flags for that half at all. Measured on this repository: `--profile release` gives
# cxx_compile_flags `-O3 -funroll-loops` under gfortran and NOTHING under nagfor or flang -- so the
# Arrow layer in src/parquet_wrapper.cpp compiles at the C++ compiler's own default, which is -O0
# for gcc and clang. Every phase this benchmark reports except `engine+open` is timed inside that file.
#     The failure mode is the one this project's tooling rules exist to prevent: the run passes the
# Fortran check above, prints a plausible table, and every C++-side figure in it is wrong.
# Demonstrated on benchmark_sort_readtime under nagfor at n = 10**6: the `bind` phase reported
# 25.9% of the open with an unoptimised C++ half and 2.9% with -O3, and the total went 97.45 ms to
# 36.91 ms. Nothing in the output distinguished the two.
CXX_FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
                  | grep -o 'cxx_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$CXX_FLAGS_LINE" ]]; then
    echo "benchmark_sort_readtime.sh: could not read cxx_compile_flags from --show-model;" >&2
    echo "  refusing to produce numbers. SKIP_OPT_CHECK=1 overrides, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$CXX_FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    echo "benchmark_sort_readtime.sh: '--profile release' produced NO optimisation flag for the C++ half:" >&2
    echo "  $CXX_FLAGS_LINE" >&2
    echo "  This is normal for nagfor and flang -- fpm gives their C/C++ half no profile flags." >&2
    echo "  Append one yourself (append, never assign -- FPM_CXXFLAGS carries Arrow's paths):" >&2
    echo "      FPM_CXXFLAGS=\"\${FPM_CXXFLAGS:-} -O3\" bench/benchmark_sort_readtime.sh" >&2
    echo "  and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi

mkdir -p test_run
fpm build --profile release >/dev/null || die "build failed."

for n in ${SIZES//,/ }; do
    fixture="test_run/bsr_${n}.parquet"
    args=(--n="$n" --reps="$REPS" --key="$KEY" --keys="$KEYS" --reads="$READS" --file="$fixture")
    [[ "$KEEP" == "1" ]] && args+=(--keep)
    fpm run benchmark_sort_readtime --profile release -- "${args[@]}" || die "run failed at n=$n."
done

echo ""
echo "Fixtures under test_run/ (KEEP=1 leaves them; otherwise each run removes its own)."
