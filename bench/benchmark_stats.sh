#!/usr/bin/env bash
# Drives bench/benchmark_stats.f90 -- the P5 "measure, then thread" campaign for parquet_stats.
#
# The phase this belongs to (feature_pandas_S4.md, P5) is deliberately "measure, THEN thread": a
# single O(n) reduction over a resident array may well be memory-bandwidth-bound, and if it is,
# `threads=` should enter no signature at all. This wrapper only builds and runs; the program
# decides what each mode times.
#
# Usage:
#   bench/benchmark_stats.sh                       # every mode
#   bench/benchmark_stats.sh --mode=thread         # one mode
#   bench/benchmark_stats.sh --mode=probit         # the normal kernel and what hides it
#   NROWS=50000000 bench/benchmark_stats.sh --mode=floor
#
# Config (env-overridable, matching this repo's other bench/*.sh wrappers):
#   NROWS=10000000  Population size. --mode=phases sweeps down from it by factors of ten, so it is
#                    the LARGEST size that mode uses, not its only one. Memory is 8*NROWS bytes for
#                    the population plus as much again for the engine's compaction buffer, and
#                    --mode=shapes allocates two further arrays of the same length.
#   ROUNDS=5        Rounds per figure; the best is kept, per this repo's benchmarking rules.
#   THREADS=        One explicit thread count for --mode=thread. Empty runs the ladder 1, 2, 4, ...
#                    up to min(omp_get_max_threads(), omp_get_num_procs()).
#
# WHICH FIGURE TO READ, per mode:
#
#   floor   The `x floor` column. The floor is a bare `s = s + x(i)` loop over the same warm array,
#           so it is the memory-bandwidth reference and nothing can beat it. Whether the four
#           library rows AGREE is the finding, not which is slowest: every entry point runs the
#           same two-pass engine, so `pf_sum` -- which needs nothing pass two computes -- pays for
#           the central moments too, and these rows are what sizes that.
#   phases  The `residual` column, which is `pf_sum` minus a one-pass library walk minus an
#           allocate-and-fill. It is a difference of measurements, so read it as a size rather than
#           an attribution.
#   shapes  The `x plain` column: how much more the general pass-one branch costs than the fast one.
#           If that gap is larger than what --mode=thread offers, the shape a caller passes matters
#           more than the thread count and the guide page should say so.
#   thread  The `speedup` column, but ONLY after both bit-exactness gates report yes. The program
#           exits nonzero if either fails, because a threaded arm that does not reproduce the serial
#           answer makes every speedup beside it worthless.
#   iqr     The `sort/select` column, which answers feature_pandas_S4.md's P6-1. Above 1 means
#           selecting the two order statistics beats sorting once, i.e. the shipped default is
#           right. Both arms are the shipped `pf_iqr` with the sort threshold moved either side of
#           2 by the debug override, so neither is a replica, and the two answers are compared bit
#           for bit before any timing is believed.
#   probit  Three tables, and each answers a different half of "what does the normal kernel cost?".
#           The first is the `x floor` column over a bare accumulation loop: `pf_probit` pays for
#           its refinement steps and the three forward functions pay for one erfc or one exp, so
#           this is where the guide's per-element claim comes from. The second is
#           `nscores/zscore`, two procedures of identical per-element shape except that one sorts
#           -- a ratio that GROWS with n says the ordering dominates and the kernel is noise. The
#           third is `fit/mad` over the three robust scale estimators, with a `sorts` column per
#           call; the program exits nonzero unless pf_probit_fit and pf_probit_scale each cost
#           exactly one ordering, since that is the property both are built on and a second sort
#           would look like an ordinary slow row. pf_mad reads 0 there because unweighted it
#           SELECTS rather than orders, which is why fit/mad compares two answers rather than two
#           implementations of one.
#   clip    The `ratio` and `sorts mad` columns, which answer P8-3. `stdfunc="mad_std"` orders the
#           DEVIATIONS once per clipping round on top of the single ordering of the values, so a
#           `sorts mad` that grows with the round count is the O(k n log n) shape the interval
#           property was chosen to avoid, and `ratio` is what it costs.
#   teamsweep
#           The `best` and `per-thread` columns, which are what STATS_MIN_PER_THREAD is derived
#           from. Neither of the two modes above can derive it: `thread` measures a replica over a
#           buffer allocated once outside the timed loop, so it reports a CEILING and never pays
#           what a real call pays, and `library` runs the shipped rule, so the very constant under
#           test censors the measurement -- every size below twice the floor reports exactly 1.00x
#           because the engine ran serially. This mode lifts the floor through the debug hook and
#           drives the team directly, so its crossover is the real one, on the code that ships.
#           Read the first row whose `best` exceeds 1.0: its `per-thread` value is what the floor
#           has to exceed. Asserts that a team actually opened, so a serial build cannot report a
#           table of 1.00x as though it had measured something.
#   library The same question asked of the SHIPPED procedure through its public `threads=`
#           argument, so this is what a caller actually sees. It is well below --mode=thread's
#           ceiling by construction: that one times pass two alone over an already-compacted
#           buffer, while this one includes pass one, which stays serial. The gap between the two
#           is the serial half of the call.
#
# The thread ladder measures a REPLICA of pass two over an already-compacted buffer, which is the
# best case: it pays none of pass one's exclusion or compaction, and none of the allocation. So its
# speedup is a CEILING on what threading the library could return, not a prediction. Read it against
# --mode=phases' residual column to see how much of a whole call that ceiling could apply to.
#
# Maintainer-only: nothing outside this repository runs it.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

NROWS="${NROWS:-10000000}"
ROUNDS="${ROUNDS:-5}"
THREADS="${THREADS:-}"

MODES=(floor phases shapes thread library teamsweep iqr clip probit)
for arg in "$@"; do
    case "$arg" in
        --mode=*) MODES=("${arg#--mode=}") ;;
        -h|--help)
            # Derived from the separator rather than a fixed line count: this header grows
            # every time a mode is added, and a stale `2,NNp` silently truncates the help
            # halfway through a paragraph with nothing to say it has.
            awk 'NR > 1 && /^# -{20,}$/ { exit } NR > 1 { sub(/^# ?/, ""); print }' \
                "${BASH_SOURCE[0]}"
            exit 0
            ;;
        *)
            echo "benchmark_stats.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done

echo "=============================================================================="
echo "benchmark_stats.sh"
echo "  build tree  : ${FPM_BUILD_DIR:-build}"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}"
echo "  arch        : $(uname -m)   $(uname -s)"
echo "  nrows=$NROWS rounds=$ROUNDS threads=${THREADS:-ladder}"
echo "  modes       : ${MODES[*]}"
echo "=============================================================================="
echo

# Some compilers optimise by DEFAULT, so "no -O in the flags" does not imply -O0 for them. This
# list is short and evidence-based on purpose: a wrong entry turns the check below into the silent
# -O0 run it exists to prevent. (ifx's own default is -O2; the check was a false positive on it
# once and blocked a whole machine's arm.)
compiler_defaults_to_optimised() {
    case "$(basename "${FPM_FC:-gfortran}")" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}

# --profile release is not optional, and asking for it is NOT the same as getting it: fpm applies
# no optimisation flags at all without a profile, and fpm 0.13.0 alpha has no release profile for
# flang, so `--profile release` there is an -O0 run that looks exactly like a valid one.
#
# This mode is unusually exposed to that. An -O0 build would report the bare accumulation loop and
# the library's two-pass engine at nearly the same cost -- the floor rises to meet the library
# rather than the library falling to the floor -- and the honest conclusion "the engine costs
# little above memory bandwidth" would be drawn from a run where nothing was optimised at all.
FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]]; then
    echo "benchmark_stats.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_stats.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

That would be an -O0 run reported as a release one. Here it is worse than usually: at -O0 the
memory floor and the library engine converge, so the run would support the conclusion "threading
is not worth it" for entirely the wrong reason.

If THIS compiler optimises by default (ifx does, at -O2), add it to compiler_defaults_to_optimised()
in this script rather than reaching for SKIP_OPT_CHECK -- an override and an appended -O3 are two
different configurations, and mixing them across machines is a comparability trap.

Fix by appending the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_stats.sh ...

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi

fpm build --profile release >/dev/null

# `${arr[@]+"${arr[@]}"}` and not a plain `"${arr[@]}"`: under `set -u`, bash 3.2 -- which is what
# macOS ships and what `#!/usr/bin/env bash` finds there -- treats an EMPTY array's expansion as an
# unbound variable and dies. `THREADS` is empty by default, so the plain form made this wrapper
# unrunnable on two of this project's three machines for every mode at once, with an error naming
# the array rather than the cause. See `.claude/rules/testing.md`, "`tools/*.sh` checks".
THREAD_FLAG=()
if [[ -n "$THREADS" ]]; then
    THREAD_FLAG=(--threads="$THREADS")
fi

for m in "${MODES[@]}"; do
    fpm run benchmark_stats --profile release -- \
        --mode="$m" --nrows="$NROWS" --rounds="$ROUNDS" ${THREAD_FLAG[@]+"${THREAD_FLAG[@]}"}
    echo
done
