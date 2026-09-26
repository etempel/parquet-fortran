#!/usr/bin/env bash
#
# Runs the PractRand statistical battery against parquet_random, one axis at a time.
#
# The generator is Philox4x32-10, whose statistical properties are published and whose three
# known-answer vectors the suite already reproduces bit for bit. A battery cannot add to that --
# a wrong constant would still be a strong mixing function and would very likely pass, while the
# KAT catches it on the first vector. What it CAN reach is this library's own mapping from
# (seed, stream, draw) onto the cipher's key and counter words, and the seed-derivation and
# permutation constructions built on top of it. That is what the axes below are for.
#
# Everything configurable is in the CONFIGURATION block. Each entry can also be overridden from the
# environment, e.g.  AXES="stream seedwalk" TLMAX=1TB bench/run_practrand.sh
#
# This is a maintainer tool and is never run by `fpm test` or by CI: a deep run takes hours, and a
# battery that emits marginal p-values by design is a flaky test waiting to happen.
# ---------------------------------------------------------------------------------------------
# Runs the PractRand (https://sourceforge.net/projects/pracrand/) battery
# against `parquet_random`, one axis at a time, feeding it from `app/probe_random_philox`'s sibling
# `app/probe_random_practrand`. It is not a way to check that Philox is a good generator -- that is
# published, and `test_kat_vectors` establishes bit-exactness to it far more sharply than any
# battery could, since a wrong constant would still be a strong mixing function and would very
# likely pass. What it reaches is the part the literature says nothing about: this library's own
# mapping from `(seed, stream, draw)` onto the cipher's key and counter words, and the constructions
# built on top of it. Hence the axes, ordered most-informative first: `stream` (the loop the guide
# tells users to write), `seedwalk` (seeds at Hamming distance 1, emulating PractRand's own
# `-ttseed64 -walk_greycode`, which cannot be aimed at an external RNG through `stdin`), `seed`,
# `key`, `perm` (the modular Feistel, 32-bit words) and `draw` (the sequential counter walk
# published results already cover, so a wiring check rather than a finding).
#
# It runs a known-weak generator through the identical pipe first and aborts if that does not fail,
# verifies `RNG_test` identifies itself as PractRand rather than merely being executable, and judges
# each run on the final result block only -- an anomaly at an interim length that is gone by the end
# is the signature of noise, since a real bias accumulates as the length doubles. PractRand ships
# only MSVC binaries, so it must be built once (`cd "$PRACTRAND_DIR/unix" && make`). Every parameter
# is in the script's own CONFIGURATION block.
#
#     bench/run_practrand.sh                                    # defaults: 6 axes x 3 seeds, 1GB each
#     AXES="stream seedwalk" TLMAX=1TB bench/run_practrand.sh   # the deep run worth leaving overnight
# Maintainer-only, and deliberately never in CI: a deep run takes hours, and a battery that emits
# marginal p-values by design is a flaky test waiting to happen.
#
# The forced half currently fails on gfortran under LTO, and that is expected.
# It is a gfortran bug affecting a kernel gfortran never ships (gfortran takes the protected arm and
# is clean; ifx ships the wrapping arm and is clean at every setting measured, including `-ipo`), so
# CI runs `--shipped-only` rather than going permanently red on it. Maintainer-only (stripped from
# the fpm-published package).
# ---------------------------------------------------------------------------------------------

set -euo pipefail

# ============================================================================
# CONFIGURATION -- edit here, or override from the environment
# ============================================================================

# Where PractRand is installed. It ships only MSVC binaries, so it must be built once:
#     cd "$PRACTRAND_DIR/unix" && make
# If your compiler needs the C++11 flag that Makefile only adds on Linux:
#     make CXXFLAGS="-std=c++11 -O2 -I../include -I../tools"
PRACTRAND_DIR="${PRACTRAND_DIR:-$HOME/usr/local/PractRand}"

# The battery binary. Derived from PRACTRAND_DIR unless you set it outright.
RNG_TEST="${RNG_TEST:-$PRACTRAND_DIR/bin/RNG_test}"

# Which axes to test, in order. See bench/probe_random_practrand.f90 for what each one streams.
#   draw     one stream, consecutive draws -- the sequential counter walk published Philox
#            results already cover, so this is a wiring check rather than a finding
#   stream   one draw from each consecutive stream -- THIS LIBRARY'S OWN mapping, and the loop
#            the guide tells users to write. The most informative axis.
#   seed     consecutive seeds, one draw each -- what `seed = base + rank` produces
#   seedwalk seeds at Hamming distance 1 (Gray code) -- the sharpest seed-mapping test, and an
#            emulation of PractRand's own -ttseed64 -walk_greycode, which cannot be aimed at an
#            external RNG through stdin
#   key      the pf_random_key fan-out, which exercises mix64 rather than the cipher
#   perm     the 4-round modular Feistel behind pf_random_perm_at -- the one construction here
#            with no published known-answer vectors (32-bit words)
AXES="${AXES:-stream seedwalk seed key perm draw}"

# Seeds to run each axis at. One seed is one sample; two or three is a much stronger statement.
SEEDS="${SEEDS:-20260817 1 8888888888888888}"

# How much data to test per (axis, seed). Accepts data (1GB, 32GB, 1TB) or time (30s, 10m, 2h).
# 1GB is a sanity run of seconds; the value of PractRand is in depth. Rough costs at the battery's
# ~150 MB/s: 32GB ~ 4 min, 256GB ~ 30 min, 1TB ~ 2 h -- per axis, per seed.
TLMAX="${TLMAX:-1GB}"

# Amount of data tested before interim results start printing. PractRand's own default is 1.5s.
TLMIN="${TLMIN:-1.5s}"

# Battery breadth. TF (-tf) folding: 0 raw only, 1 default, 2 wider set of transforms.
# TE (-te) expanded test set: 0 default (sensitivity per time), 1 expanded (sensitivity per bit).
TF="${TF:-1}"
TE="${TE:-0}"

# Use several cores for the battery (PractRand typically scales to about 5). The feeder is much
# faster than the battery, so this is where the wall-clock saving is.
MULTITHREADED="${MULTITHREADED:-yes}"

# Run a known-weak generator through the identical pipe first, to prove the setup can detect a bad
# generator at all. "No anomalies" is also what a broken feeder produces, so this is not optional
# in spirit -- turn it off only when you have just run it.
CONTROL="${CONTROL:-yes}"

# Where the logs go. One file per (axis, seed), plus a summary.
OUTDIR="${OUTDIR:-test_run/practrand}"

# fpm profile used to build the feeder. Release is not optional: at -O0 the feeder becomes the
# bottleneck instead of the battery.
PROFILE="${PROFILE:-release}"

# ============================================================================
# End of configuration
# ============================================================================

cd "$(dirname "$0")/.."

# A tool that stops early must not exit 0. `set -e` cannot express this on its own, because several
# steps below deliberately inspect non-zero exits, so completion is tracked explicitly.
finished=0
trap '[ "$finished" = "1" ] || { echo ""; echo "run_practrand.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

echo "=== parquet-fortran PractRand run ==="

# --- Pre-flight. A wrapper must FAIL rather than degrade when it cannot do what was asked. ------

if [ ! -x "$RNG_TEST" ]; then
    echo "run_practrand.sh: RNG_test not found or not executable at:" >&2
    echo "    $RNG_TEST" >&2
    echo "" >&2
    echo "PractRand ships only MSVC binaries. Build it once with:" >&2
    echo "    cd \"$PRACTRAND_DIR/unix\" && make" >&2
    echo "and if the link fails with missing vtables, add the C++11 flag its Makefile only sets" >&2
    echo "on Linux:  make CXXFLAGS=\"-std=c++11 -O2 -I../include -I../tools\"" >&2
    echo "" >&2
    echo "Set PRACTRAND_DIR or RNG_TEST if it lives somewhere else." >&2
    finished=1     # a deliberate, fully-reported exit -- not an early termination
    exit 1
fi

# Executable is not enough: anything on the PATH is executable, and a wrong binary here would
# run, exit 0, and produce logs this script would happily summarise as "clean". Make it prove
# what it is.
PR_VERSION="$("$RNG_TEST" -version 2>&1 | head -n 1)"
case "$PR_VERSION" in
    *PractRand*) : ;;
    *)
        echo "run_practrand.sh: $RNG_TEST does not identify itself as PractRand." >&2
        echo "  '-version' said: $PR_VERSION" >&2
        echo "Refusing to run: a wrong binary here exits 0 and every axis reads as clean." >&2
        finished=1     # deliberate
        exit 1
        ;;
esac

echo "PractRand : $PR_VERSION"
echo "machine   : $(uname -s) $(uname -m)"

# Build once, then invoke the BINARY directly rather than through `fpm run` for every axis. fpm's
# own chatter does go to stderr (checked), so a pipe through `fpm run` is not corrupted -- but a
# rebuild check per invocation is wasted, and a future fpm that printed one word to stdout would
# silently misalign every 64-bit word in the stream.
echo "building the feeder (--profile $PROFILE) ..."
fpm build --profile "$PROFILE" >/dev/null

FEEDER="$(find build -type f -perm -u+x -name probe_random_practrand | head -n 1)"
if [ -z "$FEEDER" ]; then
    echo "run_practrand.sh: could not find the built probe_random_practrand binary" >&2
    finished=1     # deliberate
    exit 1
fi
echo "feeder    : $FEEDER"

# The feeder verifies every axis against its own scalar entry point. A harness is untested code,
# and a feeder that silently re-emits one stream produces a battery result about nothing.
echo "self-check: $("$FEEDER" --selftest 2>&1)"

mkdir -p "$OUTDIR"
SUMMARY="$OUTDIR/summary.txt"
: > "$SUMMARY"

OPTS="-tlmin $TLMIN -tlmax $TLMAX -tf $TF -te $TE"
if [ "$MULTITHREADED" = "yes" ]; then
    OPTS="$OPTS -multithreaded"
fi
echo "options   : $OPTS"
echo "axes      : $AXES"
echo "seeds     : $SEEDS"
echo "logs      : $OUTDIR"
echo ""

# --- Negative control ---------------------------------------------------------------------------

if [ "$CONTROL" = "yes" ]; then
    RNG_OUTPUT="$(dirname "$RNG_TEST")/RNG_output"
    if [ -x "$RNG_OUTPUT" ]; then
        echo "--- negative control: xorshift32 through the identical stdin path ---"
        ctl_log="$OUTDIR/control_xorshift32.log"
        set +e
        "$RNG_OUTPUT" xorshift32 268435456 2>/dev/null \
            | "$RNG_TEST" stdin32 -tlmax 256MB >"$ctl_log" 2>&1
        set -e
        if grep -q "FAIL" "$ctl_log"; then
            echo "control FAILED as it must -- the pipe carries data and the battery has power"
            echo "control  xorshift32   DETECTED (as required)" >> "$SUMMARY"
        else
            echo "run_practrand.sh: the negative control did NOT fail." >&2
            echo "A known-weak generator passed, so this setup cannot detect a bad one and no" >&2
            echo "result below would mean anything. See $ctl_log" >&2
            finished=1     # deliberate
            exit 3
        fi
        echo ""
    else
        echo "note: RNG_output not found beside RNG_test, skipping the negative control" >&2
        echo "control  SKIPPED -- RNG_output not built" >> "$SUMMARY"
        echo ""
    fi
fi

# --- The runs -----------------------------------------------------------------------------------

overall=0
anomalies=0
for ax in $AXES; do
    # perm emits 32-bit words; every other axis emits 64-bit words.
    case "$ax" in
        perm) mode="stdin32" ;;
        *)    mode="stdin64" ;;
    esac

    for sd in $SEEDS; do
        log="$OUTDIR/${ax}_seed${sd}.log"
        printf '%-10s seed=%-20s %s ... ' "$ax" "$sd" "$mode"

        # The feeder is killed by SIGPIPE (141) when the battery stops at -tlmax, which is the
        # normal ending and not an error. The battery's own status is the one that matters, so
        # PIPESTATUS is inspected rather than letting pipefail decide.
        set +e
        "$FEEDER" --axis="$ax" --seed="$sd" 2>"$log.feeder" \
            | "$RNG_TEST" "$mode" $OPTS >"$log" 2>&1
        st=("${PIPESTATUS[@]}")
        set -e

        feeder_st="${st[0]}"
        test_st="${st[1]}"

        if [ "$feeder_st" != "0" ] && [ "$feeder_st" != "141" ]; then
            echo "FEEDER ERROR (exit $feeder_st)"
            sed -n '1,5p' "$log.feeder" >&2
            echo "$ax seed=$sd  FEEDER ERROR exit $feeder_st" >> "$SUMMARY"
            overall=1
            continue
        fi
        if [ "$test_st" != "0" ]; then
            echo "RNG_test ERROR (exit $test_st)"
            echo "$ax seed=$sd  RNG_test ERROR exit $test_st" >> "$SUMMARY"
            overall=1
            continue
        fi

        # **The verdict is the LAST result block, not the whole file.** PractRand prints one block
        # per interim length, and an anomaly at an interim length that is gone by the final one is
        # the signature of noise, not of a defect: a real bias accumulates as the length doubles,
        # so it can only get worse. Grepping the whole log conflates the two and reports a run that
        # finished clean as anomalous -- which is exactly what a 1 GB/2 GB `unusual` did on the
        # `stream` axis while 4 GB and 5 GB came back with no anomalies at all.
        #
        # Each block starts with a line beginning `rng=`, so resetting the buffer at every such
        # line and printing what is left at EOF yields the final block and nothing else.
        final="$(awk '/^rng=/ {buf = ""} {buf = buf $0 "\n"} END {printf "%s", buf}' "$log")"
        interim_only=""
        if printf '%s' "$final" | grep -q "no anomalies" && grep -q "suspicious\|unusual" "$log"; then
            interim_only=" (transient anomaly at an interim length -- clean at the end)"
        fi
        final_len="$(printf '%s' "$final" | grep -E '^length=' | tail -n 1 | sed 's/,.*//')"

        if printf '%s' "$final" | grep -q "FAIL"; then
            n=$(printf '%s' "$final" | grep -c "FAIL")
            echo "FAIL ($n) at $final_len -- see $log"
            echo "$ax seed=$sd  FAIL ($n failing tests at $final_len)" >> "$SUMMARY"
            overall=1
        elif printf '%s' "$final" | grep -q "suspicious\|unusual"; then
            # Survived to the final length. Still not a finding on its own -- see the closing note.
            echo "anomalies persisting at $final_len -- see $log"
            echo "$ax seed=$sd  ANOMALIES at $final_len (no outright failure)" >> "$SUMMARY"
            anomalies=$((anomalies + 1))
        elif printf '%s' "$final" | grep -q "no anomalies"; then
            verdict="$(printf '%s' "$final" | grep -E 'no anomalies' | tail -n 1 | sed 's/^ *//')"
            echo "$verdict at $final_len$interim_only"
            echo "$ax seed=$sd  clean -- $verdict at $final_len$interim_only" >> "$SUMMARY"
        else
            # No failure, no anomaly, and no "no anomalies" line either -- so the battery did not
            # actually report a result. Absence of a complaint is not a pass.
            echo "NO RESULT -- $RNG_TEST produced no recognisable verdict, see $log"
            echo "$ax seed=$sd  NO RESULT (unrecognised battery output)" >> "$SUMMARY"
            overall=1
        fi
    done
done

echo ""
echo "=== summary (also in $SUMMARY) ==="
cat "$SUMMARY"

finished=1
if [ "$overall" != "0" ]; then
    echo ""
    echo "run_practrand.sh: at least one axis failed or errored." >&2
    echo "Before concluding the generator is at fault, check WHICH tests failed and at what" >&2
    echo "length -- and re-run that axis at a different seed. A real defect repeats." >&2
    exit 4
fi
if [ "$anomalies" != "0" ]; then
    echo ""
    echo "$anomalies run(s) carried an anomaly THROUGH TO THE FINAL LENGTH. Still not a finding on"
    echo "its own, but stronger than a transient one -- read the log before deciding, and use:"
    echo ""
    echo "  1. Does R GROW with length?  R is a standardised deviation, so a real bias grows"
    echo "     roughly as sqrt(N): each doubling should multiply it by about 1.4. An R that sits"
    echo "     flat across a doubling is the same fluctuation being re-reported, not evidence"
    echo "     accumulating. This is the sharpest tell available without running deeper."
    echo "  2. How extreme is p, against how many tests?  The smallest of ~250 test results sits"
    echo "     near 1/250 = 4e-3 by chance alone, so a lone p of 1e-3 is unremarkable. Compare with"
    echo "     the control in this run: a genuinely broken generator gives R in the thousands."
    echo "  3. Does the SAME named test fire on the SAME axis at other seeds?  A real defect"
    echo "     repeats in one place; noise moves."
    echo ""
    echo "Then re-run that cell 10-100x deeper -- a real defect gets worse, noise disappears:"
    echo "    AXES=<axis> SEEDS=<seed> TLMIN=<10x> TLMAX=<100x> CONTROL=no bench/run_practrand.sh"
    exit 0
fi
echo ""
echo "All axes clean at $TLMAX."
