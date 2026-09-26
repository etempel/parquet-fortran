#!/usr/bin/env bash
# ---------------------------------------------------------------------------------------------
# Runs the undef-safe test runners under nagfor's `-C=undefined`, the check the runner split
# exists to make possible.
#
# WHAT `-C=undefined` IS FOR
#   It reports a read of a variable that was never assigned -- the one class of defect no other
#   compiler in this project's fleet can see. It is also all-or-nothing per build: the option is
#   stamped into every `.mod`, so it cannot be applied to part of a program. The only lever is
#   WHICH EXECUTABLE you run, which is why `test/run_tester*.f90` is five programs.
#
# WHY IT CANNOT COVER EVERYTHING
#   nagfor miscompiles every `bind(C)` call under this option -- its instrumentation interleaves a
#   definedness-map pointer after each argument, so a C callee with the plain signature receives
#   only argument 1 and garbage thereafter, silently. NAG's own manual states the restriction. So
#   any runner reaching the C++ layer is excluded by construction, not by preference.
#
# WHAT THIS SCRIPT RUNS
#   `run_tester_pf` and `run_tester` in full -- the two runners that execute no `bind(C)` call at
#   all -- then re-tries each `run_tester_noundef` suite one at a time and REPORTS any that now
#   pass. It never moves one: membership there is decided by a person after looking at why the
#   result changed.
#
# A COMPILE FAILURE HERE IS USUALLY A SHAPE, NOT A DEFECT
#   `fpm test` compiles every file of src/, test/, app/ and bench/ under this option, and nagfor 7.2
#   cannot compile a few legal source shapes under it -- one anywhere stops the run before a test
#   starts. The lint check `check_no_shape_nagfor_undefined_cannot_compile`
#   (tools/check_source_conventions.py) keeps the known ones out; a new one belongs in it.
#
# OMP_STACKSIZE IS LOAD-BEARING, NOT A TUNING KNOB
#   `healpix_tier_b` exhausts an OpenMP WORKER thread's stack under a checked build. A worker's
#   stack is `OMP_STACKSIZE`, not the process limit, so `ulimit -s` does not help and the default
#   is below the measured 1M floor. Without the export below the suite segfaults, which reads as a
#   library defect rather than a harness one.
#
# Usage:
#   tools/check_nag_undefined.sh            # everything above
#   FC=nagfor tools/check_nag_undefined.sh  # explicit compiler
# ---------------------------------------------------------------------------------------------

# A wrapper that stops early must never exit 0 (CLAUDE.md, "A tools/*.sh check must run under bash
# 3.2, and must never exit 0 having stopped early"). `set -e` is unavailable here because we
# inspect non-zero exits deliberately, so completion is tracked explicitly.
finished=0
trap '[ "$finished" = "1" ] || { echo ""; echo "check_nag_undefined.sh TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR" || exit 2

export FPM_FC="${FC:-nagfor}"
export PATH="$ROOT_DIR/tools/nagfor_fpm_shim:$PATH"
export FPM_BUILD_DIR="${FPM_BUILD_DIR:-test_run/nagundef}"

# See the banner: this is a floor, not a preference.
export OMP_STACKSIZE="${OMP_STACKSIZE:-2M}"

UNDEF_RUNNERS="run_tester_pf run_tester"
NOUNDEF_RUNNER="run_tester_noundef"

# The suites this run is expected to cover, derived rather than listed -- an enumerated list here
# would go stale in the direction that stops checking (CLAUDE.md).
expected=0
for r in $UNDEF_RUNNERS; do
    n=$(grep -cE 'new_testsuite\("[a-z0-9_]+"' "test/$r.f90" 2>/dev/null || echo 0)
    expected=$((expected + n))
done
if [ "$expected" -lt 1 ]; then
    echo "error: found no suites in $UNDEF_RUNNERS -- the runners moved or were renamed" >&2
    exit 2
fi

echo "=== compiler : $FPM_FC ($(command -v "$FPM_FC" 2>/dev/null || echo 'NOT FOUND'))"
echo "=== profile  : nagundef (-C=undefined)"
echo "=== build dir: $FPM_BUILD_DIR"
echo "=== OMP_STACKSIZE=$OMP_STACKSIZE"
echo "=== expecting $expected suite(s) across: $UNDEF_RUNNERS"
echo ""

rc=0
ran=0
for r in $UNDEF_RUNNERS; do
    echo "--- $r"
    if fpm test "$r" --profile nagundef; then
        n=$(grep -cE 'new_testsuite\("[a-z0-9_]+"' "test/$r.f90")
        ran=$((ran + n))
        echo "--- $r PASS ($n suites)"
    else
        echo "--- $r FAIL" >&2
        rc=1
    fi
    echo ""
done

# The curated list, re-tried one suite at a time so the report names what changed. Reporting only:
# a tool that promoted a suite on one green run would be deciding, on one machine and one
# compiler, something that needs a person to look at why it changed.
echo "--- $NOUNDEF_RUNNER: re-trying the curated list (report only, nothing is moved)"
promotable=""
for s in $(grep -oE 'new_testsuite\("[a-z0-9_]+"' "test/$NOUNDEF_RUNNER.f90" | sed -E 's/.*"([a-z0-9_]+)"/\1/'); do
    if fpm test "$NOUNDEF_RUNNER" --profile nagundef -- "$s" >/dev/null 2>&1; then
        echo "    $s: PASSES under -C=undefined now -- consider moving it by hand"
        promotable="$promotable $s"
    else
        echo "    $s: still fails, as expected"
    fi
done
echo ""

# Assert the run covered what it set out to, rather than merely not failing. A wrapper that
# silently ran zero suites otherwise reports success.
if [ "$ran" -ne "$expected" ]; then
    echo "error: ran $ran suite(s) but expected $expected -- this run proves nothing" >&2
    rc=2
fi

if [ "$rc" -eq 0 ]; then
    echo "check_nag_undefined.sh: PASS -- $ran suite(s) clean under -C=undefined"
    [ -n "$promotable" ] && echo "  note: reconsider run_tester_noundef membership for:$promotable"
else
    echo "check_nag_undefined.sh: FAIL" >&2
fi

finished=1
exit $rc
