#!/usr/bin/env bash
# The argsort A/B: the shipped C++ sort engine against the pure-Fortran one, on one machine.
#
# This is the measurement feature_sort.md's Stage 6 acceptance bar is judged on, taken one stage at
# a time. It drives tools/benchmark_sort_engine.sh twice -- ENGINE=cpp and ENGINE=fortran, the same
# build, the same data, the same seed -- and joins the two tables into one, so the ratio per arm is
# read off directly rather than by eye across two logs.
#
# It exists as its own script because an A/B has requirements a single run does not: the two arms
# must differ in nothing but the engine, and their checksums must agree or neither figure means
# anything.
#
#   tools/benchmark_sort_ab.sh
#   tools/benchmark_sort_ab.sh --test
#   SIZES=1000000,50000000 tools/benchmark_sort_ab.sh
#
# ---------------------------------------------------------------------------------------------
# SET THE ENVIRONMENT UP YOURSELF, BEFORE RUNNING THIS
# ---------------------------------------------------------------------------------------------
#
# This script sources nothing and chooses no compiler. It measures whatever the shell it is invoked
# from is set up to build -- so on a machine carrying more than one toolchain, activate one, run
# this, then activate the other in a FRESH shell and run it again. The two runs write build trees
# and log files named after the compiler, so they do not collide.
#
# It reports the toolchain it found, and refuses only one thing: a gfortran below 13. That is not a
# style check -- gfortran <= 11 miscompiles the optional allocatable character argument in
# schema%add_col_qc, and the symptom is a spurious "column not found" abort far from the cause. It
# is also the only way to get this wrong SILENTLY; a missing Arrow announces itself as
# "'arrow/api.h' file not found" and a missing compiler as a build error. Everything else about the
# environment is printed and left to you -- read the provenance block against what you intended
# before quoting any figure from it.
#
# ---------------------------------------------------------------------------------------------
# WHAT THIS RUN CAN AND CANNOT SETTLE, as of Stage 3
# ---------------------------------------------------------------------------------------------
#
# Only the full argsort is wired to the Fortran engine. So:
#
#   * its `thr` rows USED to be the same figure twice, because the engine ignored `threads=`
#     entirely, which made them a free negative control. **Stage 4 step 1 ended that** -- the engine
#     now honours the resolved thread count and threads its histogram and image build, so those rows
#     are expected to differ and are a measurement rather than a control. What replaced the control
#     is `parquet_debug_sort_threads_used`, which reports the count the engine actually resolved;
#     read it rather than inferring threading from the shape of the timings, since a first-touch
#     page-mapping artifact once read as a 2.3x threading win on an engine that did no threading at
#     all (see the untimed warm-up in app/benchmark_sort_engine.f90);
#   * the C++ threaded rows are not a defeat, they are the target the rest of Stage 4 has to reach;
#   * partial_argsort / nth_element / is_sorted / search / merge / group_offsets still cross into
#     C++ on BOTH arms (Stage 5), so `--mode=ops` would compare the C++ engine with itself. This
#     script therefore runs `--mode=argsort` only, deliberately.
#
# What it does settle: the per-arm serial ratio, which is what Stage 6's "every numeric arm must
# improve, a string regression is accepted" is measured against.
#
# ---------------------------------------------------------------------------------------------
# Config (environment or --flag; the flag wins)
# ---------------------------------------------------------------------------------------------
#   SIZES=...      Row counts. Default 1000000,5000000,20000000. Runtime is dominated by the
#                   largest entry and the serial arms are superlinear, so add 50000000 knowingly.
#   FAMILIES=...   Key families. Default all eight.
#   ROUNDS=5       Rounds per figure; the best is kept.
#   THREADS=64     High thread count for the C++ threaded rows. On a large NUMA machine, past
#                   roughly one socket the merge phase crosses nodes, so 64 is the documented
#                   default rather than "all of them". Set THREADS= (empty) for every core.
#   PERM=32        Permutation kind to ask the library for.
#   RUN_TESTS=0    Set to 1 (or pass --test) to run the full suite first, as a correctness gate.
#                   Note it does NOT then wait for the machine to settle -- the load is reported
#                   before timing and judging it is yours. CLAUDE.md's warning still stands: an
#                   ordered penalty biases one arm rather than adding symmetric noise, and
#                   best-of-N does not remove it, so a figure taken straight after a suite is worth
#                   re-taking if it would change a decision.
#
# Output is one joined table on stdout; the raw per-engine logs are left under test_run/ for the
# report. Redirect stdout to a file and return that file.
# ---------------------------------------------------------------------------------------------
# is the one to reach for when the question is "is the Fortran sort
# engine faster than the C++ one on this machine?". It runs `benchmark_sort_engine.sh
# --mode=argsort` twice -- `ENGINE=cpp` then `ENGINE=fortran`, same build, same data, same seed --
# and joins the two tables into one so the per-arm ratio is read off directly instead of by eye
# across two logs. It refuses to print the table at all if the two arms' checksums disagree, since
# the two engines return identical permutations by contract and a mismatch means either they do not
# or the arms did not see the same data.
#
# It sources nothing and chooses no compiler -- it measures whatever the shell it was invoked from
# is set up to build. On a machine carrying more than one toolchain, activate one, run it, then
# activate the other in a fresh shell and run it again; the build trees and log files are named
# after the compiler, so the two runs do not collide. It prints a provenance block describing that
# shell (compiler, every `FPM_*` variable, commit, load) rather than asking for one separately,
# because a provenance block collected in a different shell than the one that built the binary is
# not provenance.
#
# It refuses exactly one thing: a `gfortran` below 13. That is the only way to get this environment
# wrong silently -- such a compiler builds the library cleanly and miscompiles it (see Prerequisites
# (README.md#prerequisites)) -- whereas a missing Arrow announces itself as `'arrow/api.h' file not
# found` and a missing compiler as a build error. With `--test` it runs the full suite as a
# correctness gate before any timing. It reports the load and does not wait on it -- say what the
# load was in the report rather than leaving it to be inferred, and note that a figure taken
# straight after a suite is worth re-taking if it would change a decision, since an ordered penalty
# biases one arm rather than adding symmetric noise and best-of-N does not remove it.
#
#     tools/benchmark_sort_ab.sh                                    # whatever this shell builds
#     tools/benchmark_sort_ab.sh --test                             # with the correctness gate first
#     SIZES=1000000,50000000 tools/benchmark_sort_ab.sh
# ---------------------------------------------------------------------------------------------
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

SIZES="${SIZES:-1000000,5000000,20000000}"
FAMILIES="${FAMILIES:-i32,i64,i64lo,f32,f64,str,multi2,multi3}"
ROUNDS="${ROUNDS:-5}"
THREADS="${THREADS-64}"
PERM="${PERM:-32}"
RUN_TESTS="${RUN_TESTS:-0}"

for arg in "$@"; do
    case "$arg" in
        --sizes=*)    SIZES="${arg#*=}" ;;
        --families=*) FAMILIES="${arg#*=}" ;;
        --rounds=*)   ROUNDS="${arg#*=}" ;;
        --threads=*)  THREADS="${arg#*=}" ;;
        --perm=*)     PERM="${arg#*=}" ;;
        --test)       RUN_TESTS=1 ;;
        -h|--help)
            awk 'NR>1 { if ($0 !~ /^#/) exit; sub(/^# ?/, ""); print }' "${BASH_SOURCE[0]}"
            exit 0
            ;;
        *)
            echo "benchmark_sort_ab.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done

die() { echo "benchmark_sort_ab.sh: $*" >&2; exit 1; }

FC="${FPM_FC:-gfortran}"
FC_BASE="$(basename "$FC")"
command -v "$FC" >/dev/null 2>&1 || die "the Fortran compiler '$FC' is not on PATH."

# The one environment check worth failing on, because it is the one that fails SILENTLY -- see the
# header. A gfortran below 13 builds this library cleanly and miscompiles it.
if [[ "$FC_BASE" == gfortran* ]]; then
    GVER="$("$FC" -dumpfullversion -dumpversion 2>/dev/null | head -n 1)"
    GMAJ="${GVER%%.*}"
    [[ -n "$GMAJ" ]] || die "could not read a version from '$FC'."
    (( GMAJ >= 13 )) || die "gfortran $GVER is below this project's minimum of 13 and MISCOMPILES it.
  A bare 'gfortran' is often an old system one; activate the toolchain you meant and re-run."
fi

CORES="$( { nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null; } | head -n 1 )"
CORES="${CORES:-8}"

load1() { uptime | sed 's/.*load average[s]*: *//' | tr -d ',' | awk '{print $1}'; }

# The load is REPORTED, never waited on. An earlier version blocked until it fell below a
# threshold, which on a shared machine is a wait with no end in sight and no way to tell a busy
# machine from a stuck script. CLAUDE.md's own guidance is the right shape here: "Say what the load
# was, measure the floor, and let the floor decide" -- one report there ran at load 5.7-24 on 8
# cores and still measured a 1.5% floor, because the arms are single-threaded and every figure is
# best-of-N. So the number goes in the provenance block and the judgement is the reader's.

# --------------------------------------------------------------------------------------------
# Provenance. "The environment was already set up" is not reproducible.
#
# Everything below describes THE SHELL THIS RAN IN, which is the whole point of printing it here
# rather than collecting it separately: a provenance block taken in a different shell than the one
# that built the binary is not provenance, and reading one as a property of the machine has misled
# this project twice.
# --------------------------------------------------------------------------------------------
echo "=============================================================================="
echo "argsort A/B -- C++ engine vs Fortran engine (feature_sort.md Stage 3 state)"
echo "=============================================================================="
echo "  date        : $(date -u '+%Y-%m-%dT%H:%M:%SZ')    host: $(hostname)"
echo "  FPM_FC      : ${FPM_FC:-(unset -- fpm default)}"
echo "  compiler    : $("$FC" --version 2>/dev/null | head -n 1)"
echo "  FPM_CXXFLAGS: ${FPM_CXXFLAGS:-(unset)}"
echo "  FPM_FFLAGS  : ${FPM_FFLAGS:-(unset)}"
echo "  FPM_LDFLAGS : ${FPM_LDFLAGS:-(unset)}"
echo "  commit      : $(git rev-parse --short HEAD 2>/dev/null || echo '(not a git checkout)')"
echo "  dirty       : $(git status --porcelain 2>/dev/null | wc -l | tr -d ' ') file(s) modified"
echo "  cores       : $CORES      load now: $(load1)"
echo "  sizes       : $SIZES"
echo "  families    : $FAMILIES"
echo "  rounds      : $ROUNDS      perm: int$PERM      threads: ${THREADS:-max}"
echo "=============================================================================="
echo

if [[ -x tools/machine_report.sh ]]; then
    echo "--- tools/machine_report.sh (in THIS shell) ---"
    bash tools/machine_report.sh 2>&1 || true
    echo
fi

if [[ "$RUN_TESTS" == "1" ]]; then
    echo "--- correctness gate: full suite, BEFORE any timing ---"
    if fpm test >/dev/null 2>&1; then
        echo "fpm test: PASSED"
    else
        die "fpm test FAILED. No timing was taken; fix this before measuring anything."
    fi
    echo "load after the suite: $(load1)  (not waited on -- see the note in --help)"
    echo
fi

# --------------------------------------------------------------------------------------------
# The two arms.
# --------------------------------------------------------------------------------------------
mkdir -p test_run
CPP_LOG="test_run/sortab-${FC_BASE}-cpp.log"
FOR_LOG="test_run/sortab-${FC_BASE}-fortran.log"

run_arm() {
    local engine="$1" log="$2"
    local args=(--mode=argsort "--sizes=$SIZES" "--families=$FAMILIES" "--rounds=$ROUNDS" "--perm=$PERM")
    [[ -n "$THREADS" ]] && args+=("--threads=$THREADS")
    echo "--- ENGINE=$engine -> $log ---"
    if ! ENGINE="$engine" bash tools/benchmark_sort_engine.sh "${args[@]}" >"$log" 2>&1; then
        sed -n '$p;/error/Ip' "$log" | tail -n 20 >&2
        die "the ENGINE=$engine arm failed; see $log"
    fi
    grep -E '^# figures:' "$log" || true
}

# The C++ arm runs first only because something has to. Both arms are best-of-ROUNDS and each arm
# warms its own output pages (app/benchmark_sort_engine.f90 makes one untimed call per arm), so
# neither inherits an advantage from the other's allocations.
run_arm cpp "$CPP_LOG"
run_arm fortran "$FOR_LOG"
echo

# --------------------------------------------------------------------------------------------
# Checksum agreement -- the gate on the whole table.
#
# The two engines return IDENTICAL permutations; test/test_sorting.f90 asserts it directly and the
# benchmark's checksum is an independent end-to-end confirmation of the same thing. A mismatch
# means the arms did not see the same data or the engines genuinely disagree, and in either case
# no ratio below is worth reading -- so this refuses rather than warns.
# --------------------------------------------------------------------------------------------
CPP_SUM="$(grep -E '^# figures:' "$CPP_LOG" | sed 's/.*checksum: *//' | head -n 1)"
FOR_SUM="$(grep -E '^# figures:' "$FOR_LOG" | sed 's/.*checksum: *//' | head -n 1)"
echo "checksum  cpp=$CPP_SUM  fortran=$FOR_SUM"
if [[ -z "$CPP_SUM" || "$CPP_SUM" != "$FOR_SUM" ]]; then
    die "CHECKSUMS DISAGREE. The two engines did not compute the same answer, or the two arms did
  not see the same data. Every ratio would be meaningless; refusing to print the table.
  Logs: $CPP_LOG and $FOR_LOG"
fi
echo "checksums agree -- both engines returned the same permutations."
echo

# --------------------------------------------------------------------------------------------
# The joined table.
# --------------------------------------------------------------------------------------------
echo "=============================================================================="
echo "argsort: ns/element, best of $ROUNDS.  ratio < 1 means the Fortran engine is FASTER."
echo "=============================================================================="
awk '
    # A DATA row, not a header. The wrapper prints "  modes       : argsort" in its own banner,
    # whose third field is also "argsort" -- it was joined as a row until this was tightened, and
    # produced two rows of zeroes labelled "faster". Require the full 8-field shape with a numeric
    # row count and thread count.
    function isrow() { return (NF == 8 && $3 == "argsort" && $4 ~ /^[0-9]+$/ && $5 ~ /^[0-9]+$/) }
    FNR == NR {
        if (isrow()) cpp[$1 "|" $2 "|" $4 "|" $5] = $8
        next
    }
    isrow() {
        k = $1 "|" $2 "|" $4 "|" $5
        if (!(k in cpp)) next
        split(k, f, "|")
        r = (cpp[k] > 0) ? $8 / cpp[k] : 0
        mark = (r < 0.98) ? "faster" : (r > 1.02 ? "slower" : "parity")
        printf "  %-8s %-8s %12s %5s %12.2f %12.2f %10.3f  %s\n", f[1], f[2], f[3], f[4], cpp[k], $8, r, mark
        rows++
    }
    END { if (!rows) print "  (no argsort rows matched between the two logs -- check them by hand)" }
' "$CPP_LOG" "$FOR_LOG" | (
    printf "  %-8s %-8s %12s %5s %12s %12s %10s\n" family dist n thr cpp_ns fortran_ns ratio
    cat
)

cat <<EOF

------------------------------------------------------------------------------
Reading this table (Stage 3 state -- see feature_sort.md):

  * The Fortran engine is SERIAL. Its two \`thr\` rows for a given arm should agree to within this
    machine's noise; if they do not, the harness moved, not the engine.
  * i64lo is the integer COUNTING path on both sides. Stage 6 requires it not to regress at all.
  * f64/f32/i32/i64/multi* are the comparator path: serial introsort against std::sort.
  * str is expected to be slower, and a string regression is explicitly accepted.
  * The C++ threaded rows are the target Stage 4 has to reach, not a defeat.
  * Check the load in the provenance block. A busy machine is not automatically disqualifying --
    these arms are single-threaded and every figure is best-of-N -- but it widens the noise floor,
    so say what the load was rather than leaving it to be inferred.

Logs kept: $CPP_LOG
           $FOR_LOG
Return this whole output, including the provenance block at the top.
------------------------------------------------------------------------------
EOF
